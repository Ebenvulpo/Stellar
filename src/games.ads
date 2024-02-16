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

with Players;       use Players;
with SDL_Events;    use SDL_Events;
with Solar_Systems; use Solar_Systems;

pragma Elaborate (Players);
pragma Elaborate (Solar_Systems);

package Games is
   type Game_Type        is limited private;
   type Game_Access_Type is access Game_Type;

   type Turn_Type is new Long_Integer range 1 .. 100;

   procedure Change_Attack_Mode
     (Game        : out Game_Type;
      Attack_Mode : in  Boolean);

   procedure Check_Players (Game : in out Game_Type);
   procedure Create        (Game :    out Game_Type);
   procedure Destroy       (Game : in out Game_Type);
   procedure End_Turn      (Game : in out Game_Type);

   procedure Enter_Attack_Mode
     (Game : in out Game_Type;
      X    : in     Coordinate_X_Type;
      Y    : in     Coordinate_Y_Type);

   procedure Free (Game : in out Game_Access_Type);

   function  Get_Attack_Mode
     (Game : in Game_Type)
     return Boolean;

   function Get_Human_Player (Game : in Game_Type) return Player_Access_Type;

   function Get_Player
     (Game   : in Game_Type;
      Player : in Player_ID_Type)
     return Player_Access_Type;

   function Get_Players
     (Game : in Game_Type)
     return Player_Array_Access_Type;

   function Get_Solar_System
     (Game : in Game_Type;
      X    : in Coordinate_X_Type;
      Y    : in Coordinate_Y_Type)
     return Solar_System_Access_Type;

   function Get_Solar_Systems
     (Game : in Game_Type)
     return Solar_System_Array_Access_Type;

   function Get_Turns      (Game : in Game_Type) return Turn_Type;
   function In_Attack_Mode (Game : in Game_Type) return Boolean;

   procedure Input
     (Game  : in out Game_Type;
      Event : in     Event_Type);

   function Is_Game_Over               (Game : in Game_Type) return Boolean;
   function Is_Human_Player_Last_Alive (Game : in Game_Type) return Boolean;

private
   type Game_Type is limited
      record
         Attack_Mode    : Boolean   := False;
         Turns          : Turn_Type := Turn_Type'First;
         Game_Over      : Boolean   := False;
         Solar_Systems  : Solar_System_Array_Access_Type := null;
         Players        : Player_Array_Access_Type       := null;
      end record;

   procedure Check_Player_Solar_Systems
     (Game   : in Game_Type;
      Player : in Player_Type);

   procedure Generate_Players       (Game : in out Game_Type);
   procedure Generate_Solar_Systems (Game : in out Game_Type);
   procedure Tick                   (Game : in out Game_Type);
end Games;
