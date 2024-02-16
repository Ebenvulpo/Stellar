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

package body SDL_Rectangles is
   ----------------
   --  C_Assign  --
   ----------------
   procedure C_Assign
     (C_Rectangle : out C_Rectangle_Type;
      X, Y        : in  Long_Integer;
      W, H        : in  Long_Integer)
   is
   begin
      C_Rectangle.x := C.int (X);
      C_Rectangle.y := C.int (Y);
      C_Rectangle.w := C.int (W);
      C_Rectangle.h := C.int (H);
   end C_Assign;

   --------------
   --  Create  --
   --------------
   procedure Create
     (Rectangle     : out Rectangle_Type;
      X, Y          : in  Long_Integer;
      Width, Height : in  Long_Integer)
   is
   begin
      C_Assign (Rectangle.C_SDL_Rectangle, X, Y, Width, Height);
   end Create;
end SDL_Rectangles;
