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

package SDL_Rectangles is
   pragma Pure (SDL_Rectangles);

   type Rectangle_Type is limited private;

   procedure Create
     (Rectangle     : out Rectangle_Type;
      X, Y          : in  Long_Integer;
      Width, Height : in  Long_Integer);

private
   package C renames Interfaces.C;

   type C_Rectangle_Type is
      record
         x, y : aliased C.int := 0;
         w, h : aliased C.int := 0;
      end record;
   pragma Convention (C, C_Rectangle_Type);

   type Rectangle_Type is limited
      record
         C_SDL_Rectangle : aliased C_Rectangle_Type;
      end record;

   procedure C_Assign
     (C_Rectangle : out C_Rectangle_Type;
      X, Y        : in  Long_Integer;
      W, H        : in  Long_Integer);
end SDL_Rectangles;
