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

with System; use System;

package SDL_Windows is
   pragma Preelaborate (SDL_Windows);

   type Window_Type is limited private;

   procedure Create
     (Window        : out Window_Type;
      Title         : in  String;
      Width, Height : in  Integer);

   procedure Destroy (Window : in out Window_Type);

   procedure Set_Minimum_Size
     (Window : in out Window_Type;
      Width  : in     Integer;
      Height : in     Integer);

   Window_Error : exception;

private
   type Window_Type is limited
      record
         C_SDL_Window : System.Address := System.Null_Address;
      end record;
end SDL_Windows;
