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
with SDL_Keycodes; use SDL_Keycodes;

pragma Warnings (Off, "bits of*");

package SDL_Events is
   pragma Pure (SDL_Events);

   package C renames Interfaces.C;

   type Event_Type     is limited private;
   type Event_Tag_Type is new C.int;

   Quit_Event     : constant Event_Tag_Type := 16#100#;
   Key_Down_Event : constant Event_Tag_Type := 16#300#;

   function  Get_Keycode (Event : in  Event_Type) return Keycode_Type;
   function  Get_Type    (Event : in  Event_Type) return Event_Tag_Type;
   procedure Wait        (Event : out Event_Type);

   Event_Error : exception;

private
   type Uint8 is mod 2**8;
   for Uint8'Size use 8;

   type Padding_Type is array (C.size_t range <>) of aliased Uint8;
   pragma Pack (Padding_Type);

   type C_SDL_Keysym is
      record
         Scancode : aliased C.int;
         Sym      : aliased Keycode_Type;
         Modifier : aliased C.unsigned_short;
         Unused   : aliased C.unsigned;
      end record;
   pragma Convention (C, C_SDL_Keysym);

   type C_SDL_KeyboardEvent is
      record
         Event_Tag : aliased Event_Tag_Type;
         Timestamp : aliased C.unsigned;
         WindowID  : aliased C.unsigned;
         State     : aliased Uint8;
         Repeat    : aliased Uint8;
         Padding2  : aliased Uint8;
         Padding3  : aliased Uint8;
         Keysym    : aliased C_SDL_Keysym;
      end record;
   for C_SDL_KeyboardEvent'Size use 8 * 64;
   pragma Convention (C, C_SDL_KeyboardEvent);

   type C_Event_Type is
      record
         Event_Tag : aliased Event_Tag_Type;
      end record;
   for C_Event_Type'Size use 8 * 64;
   pragma Convention (C, C_Event_Type);

   type Event_Type is limited
      record
         C_Event : aliased C_Event_Type;
      end record;
end SDL_Events;
