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

with SDL_Renders;  use SDL_Renders;
with SDL_Textures; use SDL_Textures;
with SDL_Windows;  use SDL_Windows;

pragma Elaborate (SDL_Renders);
pragma Elaborate (SDL_Textures);
pragma Elaborate (SDL_Windows);

package Videos is
   type Video_Type        is limited private;
   type Video_Access_Type is access Video_Type;

   procedure Create  (Video :    out Video_Type);
   procedure Destroy (Video : in out Video_Type);

   subtype Character_Location_X_Type is Long_Integer range -32768 .. 32768;
   subtype Character_Location_Y_Type is Long_Integer range -32768 .. 32768;

   procedure Draw_Character
     (Video : in Video_Type;
      Char  : in Character;
      X     : in Character_Location_X_Type;
      Y     : in Character_Location_Y_Type);

   procedure Draw_String
     (Video : in Video_Type;
      Str   : in String;
      X     : in Character_Location_X_Type;
      Y     : in Character_Location_Y_Type);

   procedure Finish_Render (Video : in     Video_Type);
   procedure Free          (Video : in out Video_Access_Type);
   procedure Start_Render  (Video : in     Video_Type);

private
   type Video_Type is limited
      record
         Fontset  : Texture_Type;
         Renderer : Renderer_Type;
         Window   : Window_Type;
      end record;
end Videos;
