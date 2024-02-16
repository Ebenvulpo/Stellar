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

with Interfaces.C;         use Interfaces.C;
with Unchecked_Conversion;

package body SDL_Renders is
   package C renames Interfaces.C;

   ---------------------
   --  Special Types  --
   ---------------------
   type Unchecked_Window_Type is
      record
         C_Window : System.Address := System.Null_Address;
      end record;

   ------------------------------------
   --  Special Conversion Functions  --
   ------------------------------------
   function W_To_UW is new Unchecked_Conversion
     (Source => Window_Type,
      Target => Unchecked_Window_Type);

   ----------------------------
   --  External C Functions  --
   ----------------------------
   function C_SDL_CreateRenderer
     (Window : in System.Address;
      Index  : in C.int;
      Flags  : in C.unsigned)
     return System.Address;
   pragma Import (C, C_SDL_CreateRenderer, "SDL_CreateRenderer");

   procedure C_SDL_DestroyRenderer (Renderer : in System.Address);
   pragma Import (C, C_SDL_DestroyRenderer, "SDL_DestroyRenderer");

   function C_SDL_RenderClear (Renderer : in System.Address) return C.int;
   pragma Import (C, C_SDL_RenderClear, "SDL_RenderClear");

   function C_SDL_RenderPresent (Renderer : in System.Address) return C.int;
   pragma Import (C, C_SDL_RenderPresent, "SDL_RenderPresent");

   function C_SDL_RenderSetLogicalSize
     (Renderer      : in System.Address;
      Width, Height : in C.int)
     return C.int;
   pragma Import (C, C_SDL_RenderSetLogicalSize, "SDL_RenderSetLogicalSize");

   function C_SDL_SetRenderDrawColor
     (Renderer : in System.Address;
      R        : in Red_Color_Type;
      G        : in Green_Color_Type;
      B        : in Blue_Color_Type;
      A        : in Alpha_Color_Type)
     return C.int;
   pragma Import (C, C_SDL_SetRenderDrawColor, "SDL_SetRenderDrawColor");

   -------------
   --  Clear  --
   -------------
   procedure Clear (Renderer : in Renderer_Type) is
      Error : C.int;
   begin
      Error := C_SDL_RenderClear (Renderer.C_Renderer);
      if Error < 0 then
         raise Renderer_Error;
      end if;
   end Clear;

   --------------
   --  Create  --
   --------------
   procedure Create
     (Renderer : out Renderer_Type;
      Window   : in  Window_Type)
   is
      UW : Unchecked_Window_Type;
   begin
      UW := W_To_UW (Window);
      Renderer.C_Renderer := C_SDL_CreateRenderer
        (Window => UW.C_Window,
         Index  => -1,
         Flags  => 16#0000_0001#);
      if Renderer.C_Renderer = System.Null_Address then
         raise Renderer_Error;
      end if;
   end Create;

   ---------------
   --  Destroy  --
   ---------------
   procedure Destroy (Renderer : in out Renderer_Type) is
   begin
      C_SDL_DestroyRenderer (Renderer.C_Renderer);
      Renderer.C_Renderer := System.Null_Address;
   end Destroy;

   ---------------
   --  Present  --
   ---------------
   procedure Present (Renderer : in Renderer_Type) is
      Error : C.int;
   begin
      Error := C_SDL_RenderPresent (Renderer.C_Renderer);
      if Error < 0 then
         raise Renderer_Error;
      end if;
   end Present;

   ----------------------
   --  Set_Draw_Color  --
   ----------------------
   procedure Set_Draw_Color
     (Renderer : in Renderer_Type;
      Red      : in Red_Color_Type;
      Green    : in Green_Color_Type;
      Blue     : in Blue_Color_Type;
      Alpha    : in Alpha_Color_Type)
   is
      Error : C.int;
   begin
      Error := C_SDL_SetRenderDrawColor
        (Renderer => Renderer.C_Renderer,
         R        => Red,
         G        => Green,
         B        => Blue,
         A        => Alpha);
      if Error < 0 then
         raise Renderer_Error;
      end if;
   end Set_Draw_Color;

   ------------------------
   --  Set_Logical_Size  --
   ------------------------
   procedure Set_Logical_Size
     (Renderer      : in Renderer_Type;
      Width, Height : in Integer)
   is
      Error : C.int;
   begin
      Error := C_SDL_RenderSetLogicalSize
        (Renderer => Renderer.C_Renderer,
         Width    => C.int (Width),
         Height   => C.int (Height));
      if Error < 0 then
         raise Renderer_Error;
      end if;
   end Set_Logical_Size;
end SDL_Renders;
