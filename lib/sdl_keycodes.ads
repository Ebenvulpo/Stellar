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

package SDL_Keycodes is
   pragma Pure (SDL_Keycodes);

   type Keycode_Type is new Interfaces.C.int;

   SDLK_Escape : constant Keycode_Type := 16#1B#;
   SDLK_Space  : constant Keycode_Type := 16#20#;
   SDLK_1      : constant Keycode_Type := 16#31#;
   SDLK_2      : constant Keycode_Type := 16#32#;
   SDLK_a      : constant Keycode_Type := 16#61#;
   SDLK_d      : constant Keycode_Type := 16#64#;
   SDLK_q      : constant Keycode_Type := 16#71#;
   SDLK_s      : constant Keycode_Type := 16#73#;
   SDLK_w      : constant Keycode_Type := 16#77#;
end SDL_Keycodes;
