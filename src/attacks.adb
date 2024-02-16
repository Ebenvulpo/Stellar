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

package body Attacks is
   ---------------------------
   --  Attack_Solar_System  --
   ---------------------------
   procedure Attack_Solar_System
     (Game     : in out Game_Type;
      Attacker : in out Player_Type;
      Source   : in out Solar_System_Type;
      Target   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type)
   is
   begin
      if Can_Attack (Attacker, Source, Target, Soldiers) = False then
         return;
      end if;

      Forfeit_Growth (Target);

      Attack_Solar_System_Cost            (Source, Target, Soldiers);
      Attack_Solar_System_Population_Lost (Target, Soldiers);
      Source_Solar_System_Soldiers_Lost   (Source, Soldiers);

      Attack_Solar_System_Battle_Loop
        (Game     => Game,
         Attacker => Attacker,
         Target   => Target,
         Soldiers => Soldiers);
   end Attack_Solar_System;

   ---------------------------------------
   --  Attack_Solar_System_Battle_Loop  --
   ---------------------------------------
   procedure Attack_Solar_System_Battle_Loop
     (Game     : in out Game_Type;
      Attacker : in out Player_Type;
      Target   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type)
   is
      subtype Dice_Type is Soldier_Type range 1 .. 9;

      package Roll is new Ada.Numerics.Discrete_Random (Dice_Type);

      A, D  : Soldier_Type;
      S     : Soldier_Type'Base := Soldiers;
      Turns : Long_Integer      := 1;
      Dice  : Roll.Generator;
   begin
      Roll.Reset (Dice);

      --  Initial dice roll for the battle.
      A := Roll.Random (Dice);
      D := Roll.Random (Dice);
      loop
         if Turns > 4 then
            Turns := 1;

            A := Roll.Random (Dice);
            D := Roll.Random (Dice);
         else
            Turns := Turns + 1;
         end if;

         --  If the defender is successful in defeating the attacker, then
         --  give the defender some credit where credit is due.
         if S <= 0 then
            Defender_Won (Target);

            return;
         end if;

         Target_Solar_System_Soldiers_Lost (Target, A);
         --  If the attacker is successful in vanquishing all of the soliders
         --  in the target solar system, then take over the solar system.
         if Get_Soldiers (Target) = 0 then
            Attacker_Won (Game, Target, Attacker, S);

            return;
         end if;

         S := S - D;
      end loop;
   end Attack_Solar_System_Battle_Loop;

   --------------------------------
   --  Attack_Solar_System_Cost  --
   --------------------------------
   procedure Attack_Solar_System_Cost
     (Source   : in out Solar_System_Type;
      Target   : in     Solar_System_Type;
      Soldiers : in     Soldier_Type)
   is
      Resources : constant Resource_Type := Get_Resources (Source);
      Cost      : constant Resource_Type := Get_Attack_Cost
        (Source   => Source,
         Target   => Target,
         Soldiers => Soldiers);
   begin
      if Cost > Resources then
         return;
      end if;

      Change_Resources (Source, Resources - Cost);
   end Attack_Solar_System_Cost;

   -------------------------------------------
   --  Attack_Solar_System_Population_Lost  --
   -------------------------------------------
   procedure Attack_Solar_System_Population_Lost
     (Target   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type)
   is
      Lost : constant Float := Float'Ceiling (Float (Soldiers) / 10.0);

      Population : Population_Type'Base := Get_Population (Target);
   begin
      Population := Population - Population_Type'Base (Lost);
      if Population < Population_Type'First then
         Population := Population_Type'First;
      end if;

      Change_Population (Target, Population);
   end Attack_Solar_System_Population_Lost;

   --------------------
   --  Attacker_Won  --
   --------------------
   procedure Attacker_Won
     (Game     : in out Game_Type;
      Target   : in out Solar_System_Type;
      Attacker : in out Player_Type;
      Soldiers : in     Soldier_Type)
   is
      New_Owner : Player_Access_Type;
   begin
      New_Owner := Get_Player (Game, Get_ID (Attacker));

      Change_Score    (Attacker, Get_Score (Attacker) + 2);
      Change_Soldiers (Target, Soldiers);
      Change_Owner    (Target, New_Owner);

      Check_Players (Game);
   end Attacker_Won;

   ------------------
   --  Can_Attack  --
   ------------------
   function Can_Attack
     (Attacker : in Player_Type;
      Source   : in Solar_System_Type;
      Target   : in Solar_System_Type;
      Soldiers : in Soldier_Type)
     return Boolean
   is
      Owner     : constant Player_Access_Type := Get_Owner (Target);
      Resources : constant Long_Integer       := Get_Resources (Source);
      Cost      : constant Resource_Type      := Get_Attack_Cost
        (Source   => Source,
         Target   => Target,
         Soldiers => Soldiers);
   begin
      if Owner = null then
         return True;
      end if;

      if
        Get_ID (Owner.all) = Get_ID (Attacker) or
        Soldiers           = 0                 or
        Cost               > Resources
      then
         return False;
      end if;

      return True;
   end Can_Attack;

   --------------------
   --  Defender_Won  --
   --------------------
   procedure Defender_Won (Target : in Solar_System_Type)
   is
      Owner : constant Player_Access_Type := Get_Owner (Target);
   begin
      if Owner /= null then
         Change_Score (Owner.all, Get_Score (Owner.all) + 1);
      end if;
   end Defender_Won;

   -----------------------
   --  Get_Attack_Cost  --
   -----------------------
   function Get_Attack_Cost
     (Source   : in Solar_System_Type;
      Target   : in Solar_System_Type;
      Soldiers : in Soldier_Type)
     return Resource_Type
   is
      Distance : constant Long_Integer := Get_Distance (Source, Target);
   begin
      return Distance * Soldiers;
   end Get_Attack_Cost;

   ---------------------------------------------
   --  Get_Max_Soldiers_That_Can_Be_Deployed  --
   ---------------------------------------------
   function Get_Max_Soldiers_That_Can_Be_Deployed
     (Source : in Solar_System_Type;
      Target : in Solar_System_Type)
     return Soldier_Type
   is
      Distance     : constant Long_Integer := Get_Distance (Source, Target);
      Resources    : constant Long_Integer := Get_Resources (Source);
      Max_Soldiers : constant Soldier_Type := Resources / Distance;
   begin
      if Get_Soldiers (Source) < Max_Soldiers then
         return Get_Soldiers (Source);
      else
         return Max_Soldiers;
      end if;
   end Get_Max_Soldiers_That_Can_Be_Deployed;

   -----------------------------------------
   --  Source_Solar_System_Soldiers_Lost  --
   -----------------------------------------
   procedure Source_Solar_System_Soldiers_Lost
     (Source   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type)
   is
   begin
      Change_Soldiers (Source, Get_Soldiers (Source) - Soldiers);
   end Source_Solar_System_Soldiers_Lost;

   -----------------------------------------
   --  Target_Solar_System_Soldiers_Lost  --
   -----------------------------------------
   procedure Target_Solar_System_Soldiers_Lost
     (Target   : in out Solar_System_Type;
      Soldiers : in     Soldier_Type)
   is
      Target_Soldiers : constant Soldier_Type'Base := Get_Soldiers (Target);
   begin
      if Target_Soldiers - Soldiers < 0 then
         Change_Soldiers (Target, 0);
      else
         Change_Soldiers (Target, Target_Soldiers - Soldiers);
      end if;
   end Target_Solar_System_Soldiers_Lost;
end Attacks;
