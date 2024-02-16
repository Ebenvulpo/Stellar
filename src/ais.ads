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
pragma Elaborate (Solar_Systems);

package AIs is
   procedure Tick (Game : in out Game_Type);

private
   procedure AI_Attack
     (Game   : in out Game_Type;
      Source : in out Solar_System_Type;
      Target : in out Solar_System_Type;
      AI     : in out Player_Type);

   procedure Do_AI_Turn
     (Game   : in out Game_Type;
      Source : in out Solar_System_Type;
      AI     : in out Player_Type);

   function Get_Nearby_Enemy_Solar_System
     (Game   : in Game_Type;
      Source : in Solar_System_Type)
     return Solar_System_Access_Type;

   function Pass_Threshold
     (AI     : in Player_Type;
      Source : in Solar_System_Type)
     return Boolean;
end AIs;
