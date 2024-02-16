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

package SDL is
   type Initialize_Flag is mod 2**32;
   for Initialize_Flag'Size use 32;

   Init_Audio  : constant Initialize_Flag := 16#0000_0010#;
   Init_Video  : constant Initialize_Flag := 16#0000_0020#;
   Init_Window : constant Initialize_Flag := 16#0000_4000#;

   procedure Deinitialize;
   pragma Import (C, Deinitialize, "SDL_Quit");

   procedure Initialize (Flags : in Initialize_Flag);

   Initialization_Error : exception;
end SDL;
