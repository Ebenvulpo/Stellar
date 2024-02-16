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
with Players;       use Players;
with Solar_Systems; use Solar_Systems;

pragma Elaborate (Players);

package Renders is
   procedure Start (Game : in Game_Type);

private
   procedure Display_Cost
     (Source   : in Solar_System_Type;
      Target   : in Solar_System_Type;
      Soldiers : in Soldier_Type);

   procedure Display_Distance
     (Source : in Solar_System_Type;
      Target : in Solar_System_Type);

   procedure Display_End_Game   (Game     : in Game_Type);
   procedure Display_Population (Source   : in Solar_System_Type);
   procedure Display_Resources  (Source   : in Solar_System_Type);
   procedure Display_Score      (Game     : in Game_Type);
   procedure Display_Soldiers   (Soldiers : in Soldier_Type);
   procedure Display_Soldiers2  (Soldiers : in Soldier_Type);

   function  Get_Player_Character
     (Player : in Player_ID_Type)
     return Character;

   procedure Render_Cursor;
   procedure Render_Solar_Systems (Game   : in Game_Type);
   procedure Render_Status_Bar    (Game   : in Game_Type);
   procedure Render_Status_Bar_Attack_Mode  (Game : in Game_Type);
   procedure Render_Status_Bar_Score        (Game : in Game_Type);
   procedure Render_Status_Bar_Solar_System (Game : in Game_Type);
end Renders;
