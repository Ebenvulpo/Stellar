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

with Attacks;      use Attacks;
with Players;      use Players;
with SDL_Keycodes; use SDL_Keycodes;

package body Inputs is
   --------------------
   --  Attack_Input  --
   --------------------
   procedure Attack_Input (Game : in out Game_Type) is
      Solar_System : Solar_System_Access_Type;
   begin
      Solar_System := Get_Solar_System (Game, Cursor_X, Cursor_Y);
      if Solar_System /= null then
         Attack_Solar_System
           (Game     => Game,
            Attacker => Get_Human_Player (Game).all,
            Source   => Attacking_Solar_System.all,
            Target   => Solar_System.all,
            Soldiers => Attacking_Soldiers);

         Change_Attack_Mode (Game, False);
         Attacking_Soldiers := 0;
      end if;
   end Attack_Input;

   ---------------------------------
   --  Change_Attacking_Soldiers  --
   ---------------------------------
   procedure Change_Attacking_Soldiers (Soldiers : in Soldier_Type) is
   begin
      Attacking_Soldiers := Soldiers;
   end Change_Attacking_Soldiers;

   -------------------------------------
   --  Change_Attacking_Solar_System  --
   -------------------------------------
   procedure Change_Attacking_Solar_System
     (Solar_System : in Solar_System_Access_Type)
   is
   begin
      Attacking_Solar_System := Solar_System;
   end Change_Attacking_Solar_System;

   ------------------------------
   --  Change_Cursor_Position  --
   ------------------------------
   procedure Change_Cursor_Position
     (X : in  Coordinate_X_Type'Base;
      Y : in  Coordinate_Y_Type'Base)
   is
      X_First : constant Coordinate_X_Type'Base := Coordinate_X_Type'First;
      X_Last  : constant Coordinate_X_Type'Base := Coordinate_X_Type'Last;
      Y_First : constant Coordinate_Y_Type'Base := Coordinate_Y_Type'First;
      Y_Last  : constant Coordinate_Y_Type'Base := Coordinate_Y_Type'Last;
   begin
      if X > X_Last then
         Cursor_X := X_Last;
      elsif X < X_First then
         Cursor_X := X_First;
      else
         Cursor_X := X;
      end if;

      if Y > Y_Last then
         Cursor_Y := Y_Last;
      elsif Y < Y_First then
         Cursor_Y := Y_First;
      else
         Cursor_Y := Y;
      end if;
   end Change_Cursor_Position;

   ------------------------------------
   --  Decrement_Attacking_Soldiers  --
   ------------------------------------
   procedure Decrement_Attacking_Soldiers is
      Soldiers : constant Soldier_Type'Base := Attacking_Soldiers;
   begin
      if Soldiers - 10 <= 0 then
         Attacking_Soldiers := Soldier_Type'First;
      else
         Attacking_Soldiers := Soldiers - 10;
      end if;
   end Decrement_Attacking_Soldiers;

   ------------------------------
   --  Get_Attacking_Soldiers  --
   ------------------------------
   function Get_Attacking_Soldiers return Soldier_Type is
   begin
      return Attacking_Soldiers;
   end Get_Attacking_Soldiers;

   ----------------------------------
   --  Get_Attacking_Solar_System  --
   ----------------------------------
   function Get_Attacking_Solar_System return Solar_System_Access_Type is
   begin
      return Attacking_Solar_System;
   end Get_Attacking_Solar_System;

   ---------------------------
   --  Get_Cursor_Position  --
   ---------------------------
   procedure Get_Cursor_Position
     (X : out Coordinate_X_Type;
      Y : out Coordinate_Y_Type)
   is
   begin
      X := Cursor_X;
      Y := Cursor_Y;
   end Get_Cursor_Position;

   ------------------------------------
   --  Increment_Attacking_Soldiers  --
   ------------------------------------
   procedure Increment_Attacking_Soldiers is
   begin
      if
        Attacking_Soldiers + 10 <=
        Get_Soldiers (Attacking_Solar_System.all)
      then
         Attacking_Soldiers := Attacking_Soldiers + 10;
      else
         Attacking_Soldiers := Get_Soldiers (Attacking_Solar_System.all);
      end if;
   end Increment_Attacking_Soldiers;

   ----------------------
   --  Keyboard_Input  --
   ----------------------
   procedure Keyboard_Input
     (Game  : in out Game_Type;
      Event : in     Event_Type)
   is
   begin
      if Is_Game_Over (Game) = False and In_Attack_Mode (Game) = False then
         Keyboard_Input_Standard (Game, Event);
      elsif In_Attack_Mode (Game) then
         Keyboard_Input_Attack_Mode (Game, Event);
      elsif Is_Game_Over (Game) then
         Keyboard_Input_Game_Over (Event);
      end if;
   end Keyboard_Input;

   ----------------------------------
   --  Keyboard_Input_Attack_Mode  --
   ----------------------------------
   procedure Keyboard_Input_Attack_Mode
     (Game  : in out Game_Type;
      Event : in     Event_Type)
   is
   begin
      case Get_Keycode (Event) is
         when SDLK_Escape => Change_Attack_Mode (Game, False);
         when SDLK_1 => Decrement_Attacking_Soldiers;
         when SDLK_2 => Increment_Attacking_Soldiers;
         when SDLK_a => Move_Cursor (Left);
         when SDLK_d => Move_Cursor (Right);
         when SDLK_w => Move_Cursor (Up);
         when SDLK_s => Move_Cursor (Down);
         when SDLK_q => Attack_Input (Game);
         when others => null;
      end case;
   end Keyboard_Input_Attack_Mode;

   --------------------------------
   --  Keyboard_Input_Game_Over  --
   --------------------------------
   procedure Keyboard_Input_Game_Over (Event : in Event_Type)
   is
   begin
      case Get_Keycode (Event) is
         when others => null;
      end case;
   end Keyboard_Input_Game_Over;

   ------------------------------
   --  Keyboard_Input_Stanard  --
   ------------------------------
   procedure Keyboard_Input_Standard
     (Game  : in out Game_Type;
      Event : in     Event_Type)
   is
   begin
      case Get_Keycode (Event) is
         when SDLK_Space => End_Turn (Game);
         when SDLK_a => Move_Cursor (Left);
         when SDLK_d => Move_Cursor (Right);
         when SDLK_w => Move_Cursor (Up);
         when SDLK_s => Move_Cursor (Down);
         when SDLK_q => Enter_Attack_Mode (Game, Cursor_X, Cursor_Y);
         when others => null;
      end case;
   end Keyboard_Input_Standard;

   -------------------
   --  Move_Cursor  --
   -------------------
   procedure Move_Cursor (Input : in Cursor_Input_Type)
   is
   begin
      case Input is
         when Down  => Change_Cursor_Position (Cursor_X,     Cursor_Y + 1);
         when Up    => Change_Cursor_Position (Cursor_X,     Cursor_Y - 1);
         when Left  => Change_Cursor_Position (Cursor_X - 1, Cursor_Y);
         when Right => Change_Cursor_Position (Cursor_X + 1, Cursor_Y);
      end case;
   end Move_Cursor;
end Inputs;
