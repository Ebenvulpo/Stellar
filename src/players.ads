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

package Players is
   pragma Preelaborate (Players);

   type Player_Type    is limited private;
   type Player_ID_Type is new Natural;

   Owner_None : constant Player_ID_Type := 0;

   type Player_Access_Type is access all Player_Type;

   type Player_Array_Type is array (Player_ID_Type range <>) of
     aliased Player_Type;

   type Player_Array_Access_Type is access Player_Array_Type;

   type Aggressiveness_Type is (Low, Medium, High);

   type Score_Type is new Long_Integer range 0 .. Long_Integer'Last;

   procedure Change_Is_Alive
     (Player   : out Player_Type;
      Is_Alive : in  Boolean);

   procedure Change_Score
     (Player : out Player_Type;
      Score  : in  Score_Type);

   procedure Create
     (Player         : out Player_Type;
      ID             : in  Player_ID_Type;
      Human          : in  Boolean;
      Aggressiveness : in  Aggressiveness_Type);

   procedure Free (Player_Array : in out Player_Array_Access_Type);

   function Get_Aggressiveness
     (Player : in Player_Type)
     return Aggressiveness_Type;

   function Get_ID    (Player : in Player_Type) return Player_ID_Type;
   function Get_Score (Player : in Player_Type) return Score_Type;
   function Is_Alive  (Player : in Player_Type) return Boolean;
   function Is_Human  (Player : in Player_Type) return Boolean;

private
   type Player_Type is limited
      record
         Alive          : Boolean             := False;
         ID             : Player_ID_Type      := Player_ID_Type'First;
         Score          : Score_Type          := Score_Type'First;
         Human          : Boolean             := False;
         Aggressiveness : Aggressiveness_Type := Medium;
      end record;
end Players;
