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

with Application;
with Attacks;     use Attacks;
with Inputs;      use Inputs;
with Videos;      use Videos;

pragma Elaborate (Application);
pragma Elaborate (Attacks);
pragma Elaborate (Videos);

package body Renders is
   --------------------
   --  Display_Cost  --
   --------------------
   procedure Display_Cost
     (Source   : in Solar_System_Type;
      Target   : in Solar_System_Type;
      Soldiers : in Soldier_Type)
   is
      Cost_String            : constant String := "Cost:";
      Insufficient_Resources : constant String := "(Insufficient Resources)";

      Cost : constant Resource_Type := Get_Attack_Cost
        (Source   => Source,
         Target   => Target,
         Soldiers => Soldiers);

      RSL : constant Long_Integer := Resource_Type'Image (Cost)'Length;

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      Draw_String (Video.all, Cost_String, 44, 0);
      Draw_String
        (Video => Video.all,
         Str   => Resource_Type'Image (Cost),
         X     => 44 + Cost_String'Length,
         Y     => 0);

      if Get_Resources (Source) < Cost then
         Draw_String
           (Video => Video.all,
            Str   => Insufficient_Resources,
            X     => 44 + Cost_String'Length + RSL,
            Y     => 0);
      end if;
   end Display_Cost;

   ------------------------
   --  Display_Distance  --
   ------------------------
   procedure Display_Distance
     (Source : in Solar_System_Type;
      Target : in Solar_System_Type)
   is
      Distance_String : constant String       := "Distance:";
      Distance        : constant Long_Integer := Get_Distance (Source, Target);

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      Draw_String (Video.all, Distance_String, 30, 0);
      Draw_String
        (Video => Video.all,
         Str   => Long_Integer'Image (Distance),
         X     => 30 + Distance_String'Length,
         Y     => 0);
   end Display_Distance;

   ------------------------
   --  Display_End_Game  --
   ------------------------
   procedure Display_End_Game (Game : in Game_Type) is
      Game_Over_String : constant String := "Game Over!";
      You_Won_String   : constant String := "You Won!";

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      if Is_Human_Player_Last_Alive (Game) then
         Draw_String
           (Video => Video.all,
            Str   => You_Won_String,
            X     => 40 - (You_Won_String'Length / 2),
            Y     => 0);
      else
         Draw_String
           (Video => Video.all,
            Str   => Game_Over_String,
            X     => 40 - (Game_Over_String'Length / 2),
            Y     => 0);
      end if;
   end Display_End_Game;

   --------------------------
   --  Display_Population  --
   --------------------------
   procedure Display_Population (Source : in Solar_System_Type) is
      Population_String : constant String          := "Population:";
      Population        : constant Population_Type := Get_Population (Source);

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      Draw_String (Video.all, Population_String, 30, 0);
      Draw_String
        (Video => Video.all,
         Str   => Population_Type'Image (Population),
         X     => 30 + Population_String'Length,
         Y     => 0);
   end Display_Population;

   -------------------------
   --  Display_Resources  --
   -------------------------
   procedure Display_Resources (Source : in Solar_System_Type) is
      Resources_String : constant String        := "Resources:";
      Resources        : constant Resource_Type := Get_Resources (Source);

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      Draw_String (Video.all, Resources_String, 63, 0);
      Draw_String
        (Video => Video.all,
         Str   => Resource_Type'Image (Resources),
         X     => 63 + Resources_String'Length,
         Y     => 0);
   end Display_Resources;

   ---------------------
   --  Display_Score  --
   ---------------------
   procedure Display_Score (Game : in Game_Type) is
      Score_String : constant String             := "Score:";
      Human_Player : constant Player_Access_Type := Get_Human_Player (Game);
      Score        : constant Score_Type := Get_Score (Human_Player.all);

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      Draw_String (Video.all, Score_String, 11, 0);
      Draw_String
        (Video => Video.all,
         Str   => Score_Type'Image (Score),
         X     => 11 + Score_String'Length,
         Y     => 0);
   end Display_Score;

   ------------------------
   --  Display_Soliders  --
   ------------------------
   procedure Display_Soldiers (Soldiers : in Soldier_Type) is
      Soldiers_String : constant String := "Soldiers:";

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      Draw_String (Video.all, Soldiers_String, 11, 0);
      Draw_String
        (Video => Video.all,
         Str   => Soldier_Type'Image (Soldiers),
         X     => 11 + Soldiers_String'Length,
         Y     => 0);
   end Display_Soldiers;

   -------------------------
   --  Display_Soliders2  --
   -------------------------
   procedure Display_Soldiers2 (Soldiers : in Soldier_Type) is
      Soldiers_String : constant String := "Soldiers:";

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      Draw_String (Video.all, Soldiers_String, 47, 0);
      Draw_String
        (Video => Video.all,
         Str   => Soldier_Type'Image (Soldiers),
         X     => 47 + Soldiers_String'Length,
         Y     => 0);
   end Display_Soldiers2;

   ----------------------------
   --  Get_Player_Character  --
   ----------------------------
   function Get_Player_Character
     (Player : in Player_ID_Type)
     return Character
   is
   begin
      case Player is
         when 0 => return 's';
         when 1 => return 'p';
         when 2 => return '1';
         when 3 => return '2';
         when 4 => return '3';
         when 5 => return '4';
         when 6 => return '5';
         when 7 => return '6';
         when 8 => return '7';
         when others => raise Program_Error;
      end case;
   end Get_Player_Character;

   ---------------------
   --  Render_Cursor  --
   ---------------------
   procedure Render_Cursor is
      Video : constant Video_Access_Type := Application.Get_Video;

      X : Coordinate_X_Type;
      Y : Coordinate_Y_Type;
   begin
      Inputs.Get_Cursor_Position (X, Y);
      Draw_Character (Video.all, 'X', X, Y);
   end Render_Cursor;

   ----------------------------
   --  Render_Solar_Systems  --
   ----------------------------
   procedure Render_Solar_Systems (Game : in Game_Type) is
      Video : constant Video_Access_Type := Application.Get_Video;

      X : Coordinate_X_Type;
      Y : Coordinate_Y_Type;

      Owner     : Player_Access_Type;
      Player_ID : Player_ID_Type;

      Solar_Systems          : Solar_System_Array_Access_Type;
      Solar_System_Character : Character;
   begin
      Solar_Systems := Get_Solar_Systems (Game);
      for I in Solar_Systems'Range loop
         Owner := Get_Owner (Solar_Systems (I));
         if Owner /= null then
            Player_ID := Get_ID (Owner.all);
            Solar_System_Character := Get_Player_Character (Player_ID);
         else
            Player_ID := Player_ID_Type'First;
            Solar_System_Character := Get_Player_Character (Player_ID);
         end if;

         Get_Location (Solar_Systems (I), X, Y);
         Draw_Character (Video.all, Solar_System_Character, X, Y);
      end loop;
   end Render_Solar_Systems;

   -------------------------
   --  Render_Status_Bar  --
   -------------------------
   procedure Render_Status_Bar (Game : in Game_Type) is
      Turn_String : constant String := "Turn:";
      Turn_Number : constant String := Turn_Type'Image (Get_Turns (Game));

      Video : constant Video_Access_Type := Application.Get_Video;
   begin
      Draw_String (Video.all, Turn_String, 0, 0);
      Draw_String (Video.all, Turn_Number, Turn_String'Length, 0);

      if not Is_Game_Over (Game) and not Get_Attack_Mode (Game) then
         Render_Status_Bar_Solar_System (Game);
      elsif Get_Attack_Mode (Game) then
         Render_Status_Bar_Attack_Mode (Game);
      else
         Render_Status_Bar_Score (Game);
      end if;
   end Render_Status_Bar;

   -------------------------------------
   --  Render_Status_Bar_Attack_Mode  --
   -------------------------------------
   procedure Render_Status_Bar_Attack_Mode (Game : in Game_Type) is
      X : Coordinate_X_Type;
      Y : Coordinate_Y_Type;

      Source : Solar_System_Access_Type;
      Target : Solar_System_Access_Type;
   begin
      Display_Soldiers (Inputs.Get_Attacking_Soldiers);

      Inputs.Get_Cursor_Position (X, Y);
      Target := Get_Solar_System (Game, X, Y);
      if Target /= null then
         Source := Inputs.Get_Attacking_Solar_System;
         if Target /= Source then
            Display_Cost     (Source.all, Target.all, Get_Attacking_Soldiers);
            Display_Distance (Source.all, Target.all);
         end if;
      end if;
   end Render_Status_Bar_Attack_Mode;

   -------------------------------
   --  Render_Status_Bar_Score  --
   -------------------------------
   procedure Render_Status_Bar_Score (Game : in Game_Type) is
   begin
      Display_Score    (Game);
      Display_End_Game (Game);
   end Render_Status_Bar_Score;

   --------------------------------------
   --  Render_Status_Bar_Solar_System  --
   --------------------------------------
   procedure Render_Status_Bar_Solar_System (Game : in Game_Type) is
      Location_X, Position_X : Coordinate_X_Type;
      Location_Y, Position_Y : Coordinate_Y_Type;

      Solar_Systems : Solar_System_Array_Access_Type;
   begin
      Inputs.Get_Cursor_Position (Position_X, Position_Y);

      Solar_Systems := Get_Solar_Systems (Game);
      --  Display the solar system details if the player is hovering over it.
      for S in Solar_Systems'Range loop
         Get_Location (Solar_Systems (S), Location_X, Location_Y);

         if
           Location_X = Position_X and
           Location_Y = Position_Y and
           Get_Owner (Solar_Systems (S)) = Get_Human_Player (Game)
         then
            Display_Population (Solar_Systems (S));
            Display_Soldiers2  (Get_Soldiers (Solar_Systems (S)));
            Display_Resources  (Solar_Systems (S));
         end if;
      end loop;
   end Render_Status_Bar_Solar_System;

   -------------
   --  Start  --
   -------------
   procedure Start (Game : in Game_Type) is
   begin
      Render_Solar_Systems (Game);

      if Is_Game_Over (Game) = False then
         Render_Cursor;
      end if;

      Render_Status_Bar (Game);
   end Start;
end Renders;
