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

with Interfaces.C;
with SDL_Pixels;   use SDL_Pixels;
with System;

package SDL_Surfaces is
   type Surface_Type is limited private;

   type Byte_Type is mod 2**8;
   for Byte_Type'Size use 8;

   type BMP_Data_Type is array (Long_Integer range <>) of Byte_Type;
   pragma Pack (BMP_Data_Type);

   type BMP_Data_Access_Type is access constant BMP_Data_Type;

   procedure Free (Surface : in out Surface_Type);

   function Get_Pixel_Format
     (Surface : in Surface_Type)
     return Pixel_Format_Type;

   procedure Load_BMP
     (Surface : out Surface_Type;
      BMP     : in  BMP_Data_Access_Type);

   Surface_Error : exception;

private
   package C renames Interfaces.C;

   type C_Rectangle_Type is
      record
         x, y : aliased C.int;
         w, h : aliased C.int;
      end record;
   pragma Convention (C, C_Rectangle_Type);

   type C_Surface_Type is
      record
         Flags       : aliased C.unsigned;
         Format      : aliased Pixel_Format_Type;
         W, H        : aliased C.int;
         Pitch       : aliased C.int;
         Pixels      : aliased System.Address;
         UserData    : aliased System.Address;
         Locked      : aliased C.int;
         List_Bitmap : aliased System.Address;
         Clip_Rect   : aliased C_Rectangle_Type;
         Map         : aliased System.Address;
         RefCount    : aliased C.int;
      end record;
   pragma Convention (C, C_Surface_Type);

   type C_Surface_Access_Type is access C_Surface_Type;
   pragma Convention (C, C_Surface_Access_Type);

   type Surface_Type is limited
      record
         C_Surface : C_Surface_Access_Type := null;
      end record;
end SDL_Surfaces;
