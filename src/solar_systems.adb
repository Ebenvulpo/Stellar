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

with Ada.Numerics.Generic_Elementary_Functions;
with Unchecked_Deallocation;

package body Solar_Systems is
   ------------------------
   --  Change_Resources  --
   ------------------------
   procedure Change_Resources
     (Solar_System : in out Solar_System_Type;
      Resources    : in     Resource_Type)
   is
   begin
      Solar_System.Resources := Resources;
   end Change_Resources;

   --------------------
   --  Change_Owner  --
   --------------------
   procedure Change_Owner
     (Solar_System : in out Solar_System_Type;
      Owner        : in     Player_Access_Type)
   is
   begin
      Solar_System.Owner := Owner;
   end Change_Owner;

   -------------------------
   --  Change_Population  --
   -------------------------
   procedure Change_Population
     (Solar_System : in out Solar_System_Type;
      Population   : in     Population_Type)
   is
   begin
      Solar_System.Population := Population;
   end Change_Population;

   -----------------------
   --  Change_Soldiers  --
   -----------------------
   procedure Change_Soldiers
     (Solar_System : in out Solar_System_Type;
      Soldiers     : in     Soldier_Type)
   is
   begin
      Solar_System.Soldiers := Soldiers;
   end Change_Soldiers;

   --------------
   --  Create  --
   --------------
   procedure Create
     (Solar_System : out Solar_System_Type;
      Owner        : in  Player_Access_Type;
      Coordinate_X : in  Coordinate_X_Type;
      Coordinate_Y : in  Coordinate_Y_Type;
      Population   : in  Population_Type;
      Soldiers     : in  Soldier_Type;
      Resources    : in  Resource_Type)
   is
   begin
      Solar_System.Owner        := Owner;
      Solar_System.Coordinate_X := Coordinate_X;
      Solar_System.Coordinate_Y := Coordinate_Y;
      Solar_System.Population   := Population;
      Solar_System.Soldiers     := Soldiers;
      Solar_System.Resources    := Resources;
   end Create;

   ------------
   --  Free  --
   ------------
   procedure Free
     (Solar_System_Array : in out Solar_System_Array_Access_Type)
   is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Object => Solar_System_Array_Type,
         Name   => Solar_System_Array_Access_Type);
   begin
      Unchecked_Free (Solar_System_Array);
   end Free;

   ----------------------
   --  Forfeit_Growth  --
   ----------------------
   procedure Forfeit_Growth (Solar_System : in out Solar_System_Type) is
   begin
      Solar_System.Forfeit_Growth := True;
   end Forfeit_Growth;

   --------------------
   --  Get_Distance  --
   --------------------
   function Get_Distance
     (Source : in Solar_System_Type;
      Target : in Solar_System_Type)
     return Long_Integer
   is
      package EF is new Ada.Numerics.Generic_Elementary_Functions (Float);

      Distance  : Float;

      X, SX, TX : Coordinate_X_Type'Base;
      Y, SY, TY : Coordinate_Y_Type'Base;
   begin
      Get_Location (Source, SX, SY);
      Get_Location (Target, TX, TY);

      X := (SX - TX)**2;
      Y := (SY - TY)**2;

      Distance := Float'Ceiling (EF.Sqrt (Float (X + Y)));

      return Long_Integer (Distance);
   end Get_Distance;

   --------------------
   --  Get_Location  --
   --------------------
   procedure Get_Location
     (Solar_System : in  Solar_System_Type;
      Coordinate_X : out Coordinate_X_Type;
      Coordinate_Y : out Coordinate_Y_Type)
   is
   begin
      Coordinate_X := Solar_System.Coordinate_X;
      Coordinate_Y := Solar_System.Coordinate_Y;
   end Get_Location;

   -----------------
   --  Get_Owner  --
   -----------------
   function Get_Owner
     (Solar_System : in Solar_System_Type)
     return Player_Access_Type
   is
   begin
      return Solar_System.Owner;
   end Get_Owner;

   ----------------------
   --  Get_Population  --
   ----------------------
   function Get_Population
     (Solar_System : in Solar_System_Type)
     return Population_Type
   is
   begin
      return Solar_System.Population;
   end Get_Population;

   ---------------------
   --  Get_Resources  --
   ---------------------
   function Get_Resources
     (Solar_System : in Solar_System_Type)
     return Resource_Type
   is
   begin
      return Solar_System.Resources;
   end Get_Resources;

   --------------------
   --  Get_Soldiers  --
   --------------------
   function Get_Soldiers
     (Solar_System : in Solar_System_Type)
     return Soldier_Type
   is
   begin
      return Solar_System.Soldiers;
   end Get_Soldiers;

   ------------
   --  Tick  --
   ------------
   procedure Tick (Solar_System : in out Solar_System_Type) is
      Population : constant Population_Type'Base := Solar_System.Population;
      Soldiers   : constant Soldier_Type'Base    := Solar_System.Soldiers;
      Resources  : constant Resource_Type'Base   := Solar_System.Resources;
   begin
      --  Solar systems without an "owner" does not increase its population,
      --  resources, or army.
      if Solar_System.Owner = null then
         return;
      end if;

      if not Solar_System.Forfeit_Growth then
         Solar_System.Population := Population + 1;
      else
         Solar_System.Forfeit_Growth := False;
      end if;

      Solar_System.Soldiers  := Soldiers   + (Population / 10);
      Solar_System.Resources := Resources  + (Population / 2);
   end Tick;
end Solar_Systems;
