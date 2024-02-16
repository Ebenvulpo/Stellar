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

with SDL_Rectangles; use SDL_Rectangles;
with SDL_Renders;    use SDL_Renders;
with SDL_Surfaces;   use SDL_Surfaces;
with System;         use System;

pragma Elaborate (SDL_Renders);
pragma Elaborate (SDL_Surfaces);

package SDL_Textures is
   pragma Elaborate_Body (SDL_Textures);

   type Texture_Type is limited private;

   procedure Copy
     (Texture     : in Texture_Type;
      Renderer    : in Renderer_Type;
      Source      : in Rectangle_Type;
      Destination : in Rectangle_Type);

   procedure Create_From_Surface
     (Texture  : out Texture_Type;
      Renderer : in  Renderer_Type;
      Surface  : in  Surface_Type);

   procedure Destroy (Texture : in out Texture_Type);

   Texture_Error : exception;

private
   type Texture_Type is limited
      record
         C_Texture : System.Address := System.Null_Address;
      end record;
end SDL_Textures;
