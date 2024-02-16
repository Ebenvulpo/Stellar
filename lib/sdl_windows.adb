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
with Interfaces.C.Strings;

package body SDL_Windows is
   package C renames Interfaces.C;

   type Window_Flag is mod 2**32;

   ----------------------------
   --  External C Functions  --
   ----------------------------
   function C_SDL_CreateWindow
     (Title : in C.Strings.chars_ptr;
      X, Y  : in C.int;
      W, H  : in C.int;
      Flags : in Window_Flag)
     return System.Address;
   pragma Import (C, C_SDL_CreateWindow, "SDL_CreateWindow");

   procedure C_SDL_DestroyWindow (Window : in System.Address);
   pragma Import (C, C_SDL_DestroyWindow, "SDL_DestroyWindow");

   procedure C_SDL_SetWindowMinimumSize
     (Window : in System.Address;
      Min_W  : in C.int;
      Min_H  : in C.int);
   pragma Import (C, C_SDL_SetWindowMinimumSize, "SDL_SetWindowMinimumSize");

   ---------------
   --  Create   --
   ---------------
   procedure Create
     (Window        : out Window_Type;
      Title         : in  String;
      Width, Height : in  Integer)
   is
      C_Window : System.Address;
      C_Title  : C.Strings.chars_ptr;
   begin
      C_Title := C.Strings.New_String (Title);

      C_Window := C_SDL_CreateWindow
        (Title  => C_Title,
         X      => 16#1FFF_0000#,
         Y      => 16#1FFF_0000#,
         W      => C.int (Width),
         H      => C.int (Height),
         Flags  => 16#0000_0020#);
      C.Strings.Free (C_Title);

      if C_Window = System.Null_Address then
         raise Window_Error;
      end if;

      Window.C_SDL_Window := C_Window;
   end Create;

   ---------------
   --  Destroy  --
   ---------------
   procedure Destroy (Window : in out Window_Type) is
   begin
      C_SDL_DestroyWindow (Window.C_SDL_Window);
      Window.C_SDL_Window := System.Null_Address;
   end Destroy;

   ------------------------
   --  Set_Minimum_Size  --
   ------------------------
   procedure Set_Minimum_Size
     (Window : in out Window_Type;
      Width  : in     Integer;
      Height : in     Integer)
   is
   begin
      C_SDL_SetWindowMinimumSize
        (Window => Window.C_SDL_Window,
         Min_W  => C.int (Width),
         Min_H  => C.int (Height));
   end Set_Minimum_Size;
end SDL_Windows;
