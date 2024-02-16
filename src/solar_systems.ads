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

with Players; use Players;

pragma Elaborate (Players);

package Solar_Systems is
   pragma Preelaborate (Solar_Systems);

   type Solar_System_Type        is limited private;
   type Solar_System_Access_Type is access all Solar_System_Type;

   type Solar_System_Array_Type is array (Long_Integer range <>) of
     aliased Solar_System_Type;

   type Solar_System_Array_Access_Type is access Solar_System_Array_Type;

   subtype Coordinate_X_Type is Long_Integer range 0 .. 79;
   subtype Coordinate_Y_Type is Long_Integer range 1 .. 47;

   subtype Population_Type is Long_Integer range 10 .. Long_Integer'Last;
   subtype Resource_Type   is Long_Integer range 0  .. Long_Integer'Last;
   subtype Soldier_Type    is Long_Integer range 0  .. Long_Integer'Last;

   procedure Change_Resources
     (Solar_System : in out Solar_System_Type;
      Resources    : in     Resource_Type);

   procedure Change_Owner
     (Solar_System : in out Solar_System_Type;
      Owner        : in     Player_Access_Type);

   procedure Change_Population
     (Solar_System : in out Solar_System_Type;
      Population   : in     Population_Type);

   procedure Change_Soldiers
     (Solar_System : in out Solar_System_Type;
      Soldiers     : in     Soldier_Type);

   procedure Create
     (Solar_System : out Solar_System_Type;
      Owner        : in  Player_Access_Type;
      Coordinate_X : in  Coordinate_X_Type;
      Coordinate_Y : in  Coordinate_Y_Type;
      Population   : in  Population_Type;
      Soldiers     : in  Soldier_Type;
      Resources    : in  Resource_Type);

   procedure Forfeit_Growth (Solar_System : in out Solar_System_Type);

   procedure Free (Solar_System_Array : in out Solar_System_Array_Access_Type);

   function Get_Distance
     (Source : in Solar_System_Type;
      Target : in Solar_System_Type)
     return Long_Integer;

   procedure Get_Location
     (Solar_System : in  Solar_System_Type;
      Coordinate_X : out Coordinate_X_Type;
      Coordinate_Y : out Coordinate_Y_Type);

   function Get_Owner
     (Solar_System : in Solar_System_Type)
     return Player_Access_Type;

   function Get_Population
     (Solar_System : in Solar_System_Type)
     return Population_Type;

   function Get_Resources
     (Solar_System : in Solar_System_Type)
     return Resource_Type;

   function Get_Soldiers
     (Solar_System : in Solar_System_Type)
     return Soldier_Type;

   procedure Tick (Solar_System : in out Solar_System_Type);

private
   type Solar_System_Type is limited
      record
         Owner          : Player_Access_Type := null;
         Coordinate_X   : Coordinate_X_Type  := Coordinate_X_Type'First;
         Coordinate_Y   : Coordinate_Y_Type  := Coordinate_Y_Type'First;
         Forfeit_Growth : Boolean            := False;
         Population     : Population_Type    := Population_Type'First;
         Soldiers       : Soldier_Type       := Soldier_Type'First;
         Resources      : Resource_Type      := Resource_Type'First;
      end record;
end Solar_Systems;
