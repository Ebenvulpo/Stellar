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

with Interfaces.C.Strings;

package body SDL_Surfaces is
   ----------------------------
   --  External C Functions  --
   ----------------------------
   procedure C_SDL_FreeSurface (Surface : in C_Surface_Access_Type);
   pragma Import (C, C_SDL_FreeSurface, "SDL_FreeSurface");

   function C_SDL_RWFromConstMem
     (Mem  : in System.Address;
      Size : in C.int)
     return System.Address;
   pragma Import (C, C_SDL_RWFromConstMem, "SDL_RWFromConstMem");

   function C_SDL_LoadBMP_RW
     (Src     : in System.Address;
      FreeSrc : in C.int)
     return C_Surface_Access_Type;
   pragma Import (C, C_SDL_LoadBMP_RW, "SDL_LoadBMP_RW");

   ------------
   --  Free  --
   ------------
   procedure Free (Surface : in out Surface_Type) is
   begin
      C_SDL_FreeSurface (Surface.C_Surface);
      Surface.C_Surface := null;
   end Free;

   ------------------------
   --  Get_Pixel_Format  --
   ------------------------
   function Get_Pixel_Format
     (Surface : in Surface_Type)
     return Pixel_Format_Type
   is
   begin
      return Surface.C_Surface.Format;
   end Get_Pixel_Format;

   ----------------
   --  Load_BMP  --
   ----------------
   procedure Load_BMP
     (Surface : out Surface_Type;
      BMP     : in  BMP_Data_Access_Type)
   is
      C_Mode : C.Strings.chars_ptr;
      Bitmap : System.Address;
   begin
      C_Mode := C.Strings.New_String ("rb");

      Bitmap := C_SDL_RWFromConstMem
        (Mem  => BMP (BMP'First)'Address,
         Size => BMP'Length);

      Surface.C_Surface := C_SDL_LoadBMP_RW (Bitmap, 1);

      C.Strings.Free (C_Mode);

      if Surface.C_Surface = null then
         raise Surface_Error;
      end if;
   end Load_BMP;
end SDL_Surfaces;
