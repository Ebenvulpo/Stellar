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

with Unchecked_Deallocation;

package body Players is
   -----------------------
   --  Change_Is_Alive  --
   -----------------------
   procedure Change_Is_Alive
     (Player   : out Player_Type;
      Is_Alive : in  Boolean)
   is
   begin
      Player.Alive := Is_Alive;
   end Change_Is_Alive;

   --------------------
   --  Change_Score  --
   --------------------
   procedure Change_Score
     (Player : out Player_Type;
      Score  : in  Score_Type)
   is
   begin
      Player.Score := Score;
   end Change_Score;

   --------------
   --  Create  --
   --------------
   procedure Create
     (Player         : out Player_Type;
      ID             : in  Player_ID_Type;
      Human          : in  Boolean;
      Aggressiveness : in  Aggressiveness_Type)
   is
   begin
      Player.Alive := True;
      Player.ID    := ID;
      Player.Human := Human;
      Player.Aggressiveness := Aggressiveness;
   end Create;

   ------------
   --  Free  --
   ------------
   procedure Free (Player_Array : in out Player_Array_Access_Type) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Object => Player_Array_Type,
         Name   => Player_Array_Access_Type);
   begin
      Unchecked_Free (Player_Array);
   end Free;

   --------------------------
   --  Get_Aggressiveness  --
   --------------------------
   function Get_Aggressiveness
     (Player : in Player_Type)
     return Aggressiveness_Type
   is
   begin
      return Player.Aggressiveness;
   end Get_Aggressiveness;

   --------------
   --  Get_ID  --
   --------------
   function Get_ID (Player : in Player_Type) return Player_ID_Type is
   begin
      return Player.ID;
   end Get_ID;

   -----------------
   --  Get_Score  --
   -----------------
   function Get_Score (Player : in Player_Type) return Score_Type is
   begin
      return Player.Score;
   end Get_Score;

   ----------------
   --  Is_Alive  --
   ----------------
   function Is_Alive (Player : in Player_Type) return Boolean is
   begin
      return Player.Alive;
   end Is_Alive;

   ----------------
   --  Is_Human  --
   ----------------
   function Is_Human (Player : in Player_Type) return Boolean is
   begin
      return Player.Human;
   end Is_Human;
end Players;
