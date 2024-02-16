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

with Attacks; use Attacks;

pragma Elaborate (Attacks);

package body AIs is
   -----------------
   --  AI_Attack  --
   -----------------
   procedure AI_Attack
     (Game   : in out Game_Type;
      Source : in out Solar_System_Type;
      Target : in out Solar_System_Type;
      AI     : in out Player_Type)
   is
      Soldiers : constant Soldier_Type := Get_Max_Soldiers_That_Can_Be_Deployed
        (Source => Source,
         Target => Target);
   begin
      case Get_Aggressiveness (AI) is
         when Low    =>
            Attack_Solar_System
              (Game     => Game,
               Attacker => AI,
               Source   => Source,
               Target   => Target,
               Soldiers => Soldiers / 4);
         when Medium =>
            Attack_Solar_System
              (Game     => Game,
               Attacker => AI,
               Source   => Source,
               Target   => Target,
               Soldiers => Soldiers / 2);
         when High   =>
            Attack_Solar_System
              (Game     => Game,
               Attacker => AI,
               Source   => Source,
               Target   => Target,
               Soldiers => Soldiers);
      end case;
   end AI_Attack;

   ------------------
   --  Do_AI_Turn  --
   ------------------
   procedure Do_AI_Turn
     (Game   : in out Game_Type;
      Source : in out Solar_System_Type;
      AI     : in out Player_Type)
   is
      Target : Solar_System_Access_Type;
   begin
      Target := Get_Nearby_Enemy_Solar_System (Game, Source);
      if Target /= null and Pass_Threshold (AI, Source) then
         AI_Attack (Game, Source, Target.all, AI);
      end if;
   end Do_AI_Turn;

   -------------------------------------
   --  Get_Nearby_Enemy_Solar_System  --
   -------------------------------------
   function Get_Nearby_Enemy_Solar_System
     (Game   : in Game_Type;
      Source : in Solar_System_Type)
     return Solar_System_Access_Type
   is
      Owner : constant Player_Access_Type := Get_Owner (Source);

      Distance      : Long_Integer := 0;
      Shortest      : Long_Integer := 0;
      Solar_Systems : Solar_System_Array_Access_Type;
      Target        : Solar_System_Access_Type := null;
   begin
      Solar_Systems := Get_Solar_Systems (Game);
      for I in Solar_Systems'Range loop
         if Get_Owner (Solar_Systems (I)) /= Owner then
            Distance := Get_Distance
              (Source => Source,
               Target => Solar_Systems (I));

            if Distance <= Shortest or Shortest = 0 then
               Shortest := Distance;
               Target := Solar_Systems (I)'Access;
            end if;
         end if;
      end loop;

      return Target;
   end Get_Nearby_Enemy_Solar_System;

   ------------
   --  Tick  --
   ------------
   procedure Tick (Game : in out Game_Type) is
      Owner         : Player_Access_Type;
      Solar_Systems : Solar_System_Array_Access_Type;
   begin
      Solar_Systems := Get_Solar_Systems (Game);
      for I in Solar_Systems'Range loop
         Owner := Get_Owner (Solar_Systems (I));
         if Owner /= null and Owner /= Get_Human_Player (Game) then
            Do_AI_Turn (Game, Solar_Systems (I), Owner.all);
         end if;
      end loop;
   end Tick;

   ----------------------
   --  Pass_Threshold  --
   ----------------------
   function Pass_Threshold
     (AI     : in Player_Type;
      Source : in Solar_System_Type)
     return Boolean
   is
      Resources : constant Resource_Type := Get_Resources (Source);
      Soldiers  : constant Soldier_Type  := Get_Soldiers (Source);
   begin
      case Get_Aggressiveness (AI) is
         when Low    =>
            if Soldiers >= 100 and Resources >= 1000 then
               return True;
            end if;
         when Medium =>
            if Soldiers >= 50 and Resources >= 750 then
               return True;
            end if;
         when High   =>
            if Soldiers >= 25 and Resources >= 500 then
               return True;
            end if;
      end case;

      return False;
   end Pass_Threshold;
end AIs;
