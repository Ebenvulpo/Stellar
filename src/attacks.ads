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

pragma Elaborate (Games);
pragma Elaborate (Players);
pragma Elaborate (Solar_Systems);

package Attacks is
   procedure Attack_Solar_System
     (Game     : in out Game_Type;
      Attacker : in out Player_Type;
      Source   : in out Solar_System_Type;
      Target   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type);

   function Can_Attack
     (Attacker : in Player_Type;
      Source   : in Solar_System_Type;
      Target   : in Solar_System_Type;
      Soldiers : in Soldier_Type)
     return Boolean;

   function Get_Attack_Cost
     (Source   : in Solar_System_Type;
      Target   : in Solar_System_Type;
      Soldiers : in Soldier_Type)
     return Resource_Type;

   function Get_Max_Soldiers_That_Can_Be_Deployed
     (Source : in Solar_System_Type;
      Target : in Solar_System_Type)
     return Soldier_Type;

private
   procedure Attack_Solar_System_Battle_Loop
     (Game     : in out Game_Type;
      Attacker : in out Player_Type;
      Target   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type);

   procedure Attack_Solar_System_Cost
     (Source   : in out Solar_System_Type;
      Target   : in     Solar_System_Type;
      Soldiers : in     Soldier_Type);

   procedure Attack_Solar_System_Population_Lost
     (Target   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type);

   procedure Attacker_Won
     (Game     : in out Game_Type;
      Target   : in out Solar_System_Type;
      Attacker : in out Player_Type;
      Soldiers : in     Soldier_Type);

   procedure Defender_Won (Target : in Solar_System_Type);

   procedure Source_Solar_System_Soldiers_Lost
     (Source   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type);

   procedure Target_Solar_System_Soldiers_Lost
     (Target   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type);
end Attacks;
