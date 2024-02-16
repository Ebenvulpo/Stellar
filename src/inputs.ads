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

with Games;         use Games;
with SDL_Events;    use SDL_Events;
with Solar_Systems; use Solar_Systems;

package Inputs is
   procedure Change_Attacking_Soldiers (Soldiers : in Soldier_Type);

   procedure Change_Attacking_Solar_System
     (Solar_System : in Solar_System_Access_Type);

   procedure Change_Cursor_Position
     (X : in Coordinate_X_Type'Base;
      Y : in Coordinate_Y_Type'Base);

   function Get_Attacking_Soldiers     return Soldier_Type;
   function Get_Attacking_Solar_System return Solar_System_Access_Type;

   procedure Get_Cursor_Position
     (X : out Coordinate_X_Type;
      Y : out Coordinate_Y_Type);

   procedure Keyboard_Input
     (Game  : in out Game_Type;
      Event : in     Event_Type);

private
   procedure Attack_Input (Game : in out Game_Type);

   procedure Decrement_Attacking_Soldiers;
   procedure Increment_Attacking_Soldiers;

   procedure Keyboard_Input_Attack_Mode
     (Game  : in out Game_Type;
      Event : in     Event_Type);

   procedure Keyboard_Input_Game_Over (Event : in Event_Type);

   procedure Keyboard_Input_Standard
     (Game  : in out Game_Type;
      Event : in     Event_Type);

   type Cursor_Input_Type is (Down, Up, Left, Right);

   procedure Move_Cursor (Input : in Cursor_Input_Type);

   Attacking_Solar_System : Solar_System_Access_Type := null;
   Attacking_Soldiers     : Soldier_Type             := 0;

   Cursor_X : Coordinate_X_Type  := Coordinate_X_Type'First;
   Cursor_Y : Coordinate_Y_Type  := Coordinate_Y_Type'First;
end Inputs;
