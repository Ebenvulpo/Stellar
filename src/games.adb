--  Copyright 2024 Ebenvulpo
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.

with Ada.Numerics.Discrete_Random;
with AIs;
with Inputs;                       use Inputs;
with Unchecked_Deallocation;

package body Games is
   --------------------------
   --  Change_Attack_Mode  --
   --------------------------
   procedure Change_Attack_Mode
     (Game        : out Game_Type;
      Attack_Mode : in  Boolean)
   is
   begin
      Inputs.Change_Attacking_Soldiers (0);
      Game.Attack_Mode := Attack_Mode;
   end Change_Attack_Mode;

   ----------------------------------
   --  Check_Player_Solar_Systems  --
   ----------------------------------
   procedure Check_Player_Solar_Systems
     (Game   : in Game_Type;
      Player : in Player_Type)
   is
      Any_Solar_Systems : Boolean := False;

      Owner : Player_Access_Type;
   begin
      for I in Game.Solar_Systems'Range loop
         Owner := Get_Owner (Game.Solar_Systems (I));
         if Owner /= null then
            if Get_ID (Owner.all) = Get_ID (Player) then
               Any_Solar_Systems := True;
            end if;
         end if;
      end loop;

      Change_Is_Alive (Game.Players (Get_ID (Player)), Any_Solar_Systems);
   end Check_Player_Solar_Systems;

   ---------------------
   --  Check_Players  --
   ---------------------
   procedure Check_Players (Game : in out Game_Type) is
   begin
      for I in Game.Players'Range loop
         Check_Player_Solar_Systems (Game, Game.Players (I));
      end loop;

      if Is_Alive (Get_Human_Player (Game).all) = False then
         Game.Game_Over := True;
      end if;

      for I in Game.Players'First + 1 .. Game.Players'Last loop
         if Is_Alive (Game.Players (I)) then
            return;
         end if;
      end loop;

      Game.Game_Over := True;
   end Check_Players;

   --------------
   --  Create  --
   --------------
   procedure Create (Game : out Game_Type) is
   begin
      Game.Turns         := Turn_Type'First;
      Game.Players       := new Player_Array_Type       (1 .. 8);
      Game.Solar_Systems := new Solar_System_Array_Type (1 .. 64);

      Generate_Solar_Systems (Game);
      Generate_Players (Game);
   end Create;

   ---------------
   --  Destroy  --
   ---------------
   procedure Destroy (Game : in out Game_Type) is
   begin
      Free (Game.Players);
      Free (Game.Solar_Systems);
   end Destroy;

   ----------------
   --  End_Turn  --
   ----------------
   procedure End_Turn (Game : in out Game_Type) is
   begin
      if Game.Turns < Turn_Type'Last then
         Game.Turns := Game.Turns + 1;
         AIs.Tick (Game);
         Tick (Game);
      end if;

      --  Too many turns, so end the game here.
      if Game.Turns = Turn_Type'Last then
         Game.Game_Over := True;
      end if;
   end End_Turn;

   -------------------------
   --  Enter_Attack_Mode  --
   -------------------------
   procedure Enter_Attack_Mode
     (Game : in out Game_Type;
      X    : in     Coordinate_X_Type;
      Y    : in     Coordinate_Y_Type)
   is
      Solar_System : Solar_System_Access_Type;
   begin
      Solar_System := Get_Solar_System (Game, X, Y);
      if Solar_System /= null then
         if Get_Owner (Solar_System.all) = Get_Human_Player (Game) then
            Inputs.Change_Attacking_Solar_System (Solar_System);
            Inputs.Change_Attacking_Soldiers (0);

            Game.Attack_Mode := True;
         end if;
      else
         Game.Attack_Mode := False;
      end if;
   end Enter_Attack_Mode;

   ------------
   --  Free  --
   ------------
   procedure Free (Game : in out Game_Access_Type) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Object => Game_Type,
         Name   => Game_Access_Type);
   begin
      Free (Game.Players);
      Free (Game.Solar_Systems);

      Unchecked_Free (Game);
   end Free;

   ------------------------
   --  Generate_Players  --
   ------------------------
   procedure Generate_Players (Game : in out Game_Type) is
      package R is new Ada.Numerics.Discrete_Random (Aggressiveness_Type);

      X     : Coordinate_X_Type;
      Y     : Coordinate_Y_Type;
      G     : R.Generator;
      Owner : Player_Access_Type;
   begin
      R.Reset (G);
      for I in Game.Players'Range loop
         --  Do the human player first.
         if I = Player_ID_Type'First then
            Create (Game.Players (I), I, True, Medium);
         else
            Create (Game.Players (I), I, False, R.Random (G));
         end if;

         Owner := Game.Players (I)'Access;
         Change_Owner (Game.Solar_Systems (Long_Integer (I)), Owner);
      end loop;

      --  Move the cursor to the human player's solar system.
      Get_Location (Game.Solar_Systems (1), X, Y);
      Inputs.Change_Cursor_Position (X, Y);
   end Generate_Players;

   ------------------------------
   --  Generate_Solar_Systems  --
   ------------------------------
   procedure Generate_Solar_Systems (Game : in out Game_Type) is
      package XR is new Ada.Numerics.Discrete_Random (Coordinate_X_Type);
      package YR is new Ada.Numerics.Discrete_Random (Coordinate_Y_Type);

      X_Generator : XR.Generator;
      Y_Generator : YR.Generator;

      X : Coordinate_X_Type;
      Y : Coordinate_Y_Type;
   begin
      XR.Reset (X_Generator);
      YR.Reset (Y_Generator);
      for I in Game.Solar_Systems'Range loop
      <<Generate_Another_Position>>
         X := XR.Random (X_Generator);
         Y := YR.Random (Y_Generator);

         --  Make sure the newly generated solar system isn't on top of another
         --  solar system.
         if Get_Solar_System (Game, X, Y) /= null then
            goto Generate_Another_Position;
         end if;

         Create
           (Solar_System => Game.Solar_Systems (I),
            Owner        => null,
            Coordinate_X => X,
            Coordinate_Y => Y,
            Population   => 100,
            Soldiers     => 10,
            Resources    => 1000);
      end loop;
   end Generate_Solar_Systems;

   -----------------------
   --  Get_Attack_Mode  --
   -----------------------
   function Get_Attack_Mode (Game : in Game_Type) return Boolean is
   begin
      return Game.Attack_Mode;
   end Get_Attack_Mode;

   ------------------------
   --  Get_Human_Player  --
   ------------------------
   function Get_Human_Player (Game : in Game_Type) return Player_Access_Type is
   begin
      return Game.Players (1)'Access;
   end Get_Human_Player;

   ------------------
   --  Get_Player  --
   ------------------
   function Get_Player
     (Game   : in Game_Type;
      Player : in Player_ID_Type)
     return Player_Access_Type
   is
   begin
      if Player = Owner_None then
         return null;
      end if;

      return Game.Players (Player)'Access;
   end Get_Player;

   -------------------
   --  Get_Players  --
   -------------------
   function Get_Players
     (Game : in Game_Type)
     return Player_Array_Access_Type
   is
   begin
      return Game.Players;
   end Get_Players;

   ------------------------
   --  Get_Solar_System  --
   ------------------------
   function Get_Solar_System
     (Game : in Game_Type;
      X    : in Coordinate_X_Type;
      Y    : in Coordinate_Y_Type)
     return Solar_System_Access_Type
   is
      Solar_System_X : Coordinate_X_Type;
      Solar_System_Y : Coordinate_Y_Type;
   begin
      for I in Game.Solar_Systems'Range loop
         Get_Location (Game.Solar_Systems (I), Solar_System_X, Solar_System_Y);
         if X = Solar_System_X and Y = Solar_System_Y then
            return Game.Solar_Systems (I)'Access;
         end if;
      end loop;

      return null;
   end Get_Solar_System;

   -------------------------
   --  Get_Solar_Systems  --
   -------------------------
   function Get_Solar_Systems
     (Game : in Game_Type)
     return Solar_System_Array_Access_Type
   is
   begin
      return Game.Solar_Systems;
   end Get_Solar_Systems;

   -----------------
   --  Get_Turns  --
   -----------------
   function Get_Turns (Game : in Game_Type) return Turn_Type is
   begin
      return Game.Turns;
   end Get_Turns;

   -------------
   --  Input  --
   -------------
   procedure Input
     (Game  : in out Game_Type;
      Event : in     Event_Type)
   is
   begin
      case Get_Type (Event) is
         when Key_Down_Event => Keyboard_Input (Game, Event);
         when others => null;
      end case;
   end Input;

   ----------------------
   --  In_Attack_Mode  --
   ----------------------
   function In_Attack_Mode (Game : in Game_Type) return Boolean is
   begin
      return Game.Attack_Mode;
   end In_Attack_Mode;

   --------------------
   --  Is_Game_Over  --
   --------------------
   function Is_Game_Over (Game : in Game_Type) return Boolean is
   begin
      return Game.Game_Over;
   end Is_Game_Over;

   ----------------------------------
   --  Is_Human_Player_Last_Alive  --
   ----------------------------------
   function Is_Human_Player_Last_Alive (Game : in Game_Type) return Boolean is
      Human_Player : constant Player_Access_Type := Get_Human_Player (Game);

      Last_Alive : Boolean := Is_Alive (Human_Player.all);
   begin
      for I in Game.Players'First + 1 .. Game.Players'Last loop
         if Is_Alive (Game.Players (I)) then
            Last_Alive := False;
         end if;
      end loop;

      return Last_Alive;
   end Is_Human_Player_Last_Alive;

   ------------
   --  Tick  --
   ------------
   procedure Tick (Game : in out Game_Type) is
   begin
      for I in Game.Solar_Systems'Range loop
         Tick (Game.Solar_Systems (I));
      end loop;

      Check_Players (Game);
   end Tick;
end Games;
