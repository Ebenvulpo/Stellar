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

with Unchecked_Conversion;

package body SDL_Events is
   ----------------------------
   --  External C Functions  --
   ----------------------------
   procedure C_SDL_WaitEvent (Event : access C_Event_Type);
   pragma Import (C, C_SDL_WaitEvent, "SDL_WaitEvent");

   -------------------
   --  Get_Keycode  --
   -------------------
   function Get_Keycode (Event : in Event_Type) return Keycode_Type is
      function To_Keyboard is new Unchecked_Conversion
        (Source => C_Event_Type,
         Target => C_SDL_KeyboardEvent);

      Keyboard : aliased C_SDL_KeyboardEvent;
   begin
      if
        Event.C_Event.Event_Tag < 16#300# or
        Event.C_Event.Event_Tag > 16#3FF#
      then
         raise Event_Error;
      end if;

      Keyboard := To_Keyboard (Event.C_Event);

      return Keyboard.Keysym.Sym;
   end Get_Keycode;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (Event : in Event_Type) return Event_Tag_Type is
   begin
      return Event.C_Event.Event_Tag;
   end Get_Type;

   ------------
   --  Wait  --
   ------------
   procedure Wait (Event : out Event_Type) is
   begin
      C_SDL_WaitEvent (Event.C_Event'Access);
   end Wait;
end SDL_Events;
