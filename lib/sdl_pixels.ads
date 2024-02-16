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

with System;

package SDL_Pixels is
   type Pixel_Format_Type is new System.Address;

   type Red_Color_Type   is mod 2**8;
   type Green_Color_Type is mod 2**8;
   type Blue_Color_Type  is mod 2**8;
   type Alpha_Color_Type is mod 2**8;

   for Red_Color_Type'Size   use 8;
   for Green_Color_Type'Size use 8;
   for Blue_Color_Type'Size  use 8;
   for Alpha_Color_Type'Size use 8;

   type Color_Type is mod 2**32;
   for Color_Type'Size use 32;

   function Map_RGB
     (Format : in Pixel_Format_Type;
      Red    : in Red_Color_Type;
      Green  : in Green_Color_Type;
      Blue   : in Blue_Color_Type)
     return Color_Type;
   pragma Import (C, Map_RGB, "SDL_MapRGB");
end SDL_Pixels;
