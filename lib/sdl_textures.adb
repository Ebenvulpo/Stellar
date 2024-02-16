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

package body SDL_Textures is
   package C renames Interfaces.C;

   ---------------------
   --  Special Types  --
   ---------------------
   type Unchecked_C_Rectangle_Type is
      record
         x, y : aliased C.int;
         w, h : aliased C.int;
      end record;
   pragma Convention (C, Unchecked_C_Rectangle_Type);

   type Unchecked_Rectangle_Type is
      record
         C_SDL_Rectangle : aliased Unchecked_C_Rectangle_Type;
      end record;

   type Unchecked_Renderer_Type is
      record
         C_Renderer : System.Address := System.Null_Address;
      end record;

   type Unchecked_Surface_Type is
      record
         C_Surface : System.Address := System.Null_Address;
      end record;

   ------------------------------------
   --  Special Conversion Functions  --
   ------------------------------------
   function R_To_UR is new Unchecked_Conversion
     (Source => Renderer_Type,
      Target => Unchecked_Renderer_Type);

   function RA_To_URA is new Unchecked_Conversion
     (Source => Rectangle_Type,
      Target => Unchecked_Rectangle_Type);

   function S_To_US is new Unchecked_Conversion
     (Source => Surface_Type,
      Target => Unchecked_Surface_Type);

   ----------------------------
   --  External C Functions  --
   ----------------------------
   procedure C_SDL_DestroyTexture (Texture : in System.Address);
   pragma Import (C, C_SDL_DestroyTexture, "SDL_DestroyTexture");

   function C_SDL_RenderCopy
     (Renderer : in System.Address;
      Texture  : in System.Address;
      SrcRect  : in System.Address;
      DstRect  : in System.Address)
     return C.int;
   pragma Import (C, C_SDL_RenderCopy, "SDL_RenderCopy");

   function C_SDL_CreateTextureFromSurface
     (Renderer : in System.Address;
      Surface  : in System.Address)
     return System.Address;
   pragma Import
     (Convention    => C,
      Entity        => C_SDL_CreateTextureFromSurface,
      External_Name => "SDL_CreateTextureFromSurface");

   ------------
   --  Copy  --
   ------------
   procedure Copy
     (Texture     : in Texture_Type;
      Renderer    : in Renderer_Type;
      Source      : in Rectangle_Type;
      Destination : in Rectangle_Type)
   is
      Error : C.int;
      CS    : Unchecked_Rectangle_Type;
      CD    : Unchecked_Rectangle_Type;
      UR    : Unchecked_Renderer_Type;
   begin
      UR := R_To_UR   (Renderer);
      CS := RA_To_URA (Source);
      CD := RA_To_URA (Destination);

      Error := C_SDL_RenderCopy
        (Renderer => UR.C_Renderer,
         Texture  => Texture.C_Texture,
         SrcRect  => CS.C_SDL_Rectangle'Address,
         DstRect  => CD.C_SDL_Rectangle'Address);
      if Error < 0 then
         raise Texture_Error;
      end if;
   end Copy;

   ---------------------------
   --  Create_From_Surface  --
   ---------------------------
   procedure Create_From_Surface
     (Texture  : out Texture_Type;
      Renderer : in  Renderer_Type;
      Surface  : in  Surface_Type)
   is
      UR : Unchecked_Renderer_Type;
      US : Unchecked_Surface_Type;
   begin
      UR := R_To_UR (Renderer);
      US := S_To_US (Surface);
      Texture.C_Texture := C_SDL_CreateTextureFromSurface
        (Renderer => UR.C_Renderer,
         Surface  => US.C_Surface);
      if Texture.C_Texture = System.Null_Address then
         raise Texture_Error;
      end if;
   end Create_From_Surface;

   ---------------
   --  Destroy  --
   ---------------
   procedure Destroy (Texture : in out Texture_Type) is
   begin
      C_SDL_DestroyTexture (Texture.C_Texture);
      Texture.C_Texture := System.Null_Address;
   end Destroy;
end SDL_Textures;
