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

with Fontset;                use Fontset;
with SDL_Rectangles;         use SDL_Rectangles;
with SDL_Surfaces;           use SDL_Surfaces;
with Unchecked_Deallocation;

pragma Elaborate (SDL_Surfaces);

package body Videos is
   --------------
   --  Create  --
   --------------
   procedure Create (Video : out Video_Type) is
      Screen_Width  : constant Integer := 8 * 80;
      Screen_Height : constant Integer := 8 * 48;

      Fontset_Surface : Surface_Type;
   begin
      Create (Video.Window, "Stellar", Screen_Width * 2, Screen_Height * 2);
      Set_Minimum_Size (Video.Window, Screen_Width, Screen_Height);

      Create (Video.Renderer, Video.Window);
      Set_Logical_Size (Video.Renderer, Screen_Width, Screen_Height);

      Load_BMP (Fontset_Surface, Fontset_Memory'Access);

      Create_From_Surface (Video.Fontset, Video.Renderer, Fontset_Surface);
      Free (Fontset_Surface);
   end Create;

   ---------------
   --  Destroy  --
   ---------------
   procedure Destroy (Video : in out Video_Type) is
   begin
      Destroy (Video.Fontset);
      Destroy (Video.Renderer);
      Destroy (Video.Window);
   end Destroy;

   ----------------------
   --  Draw_Character  --
   ----------------------
   procedure Draw_Character
     (Video : in Video_Type;
      Char  : in Character;
      X     : in Character_Location_X_Type;
      Y     : in Character_Location_Y_Type)
   is
      Character_Value      : Long_Integer;
      Character_Location_X : Long_Integer;
      Character_Location_Y : Long_Integer;

      Source      : Rectangle_Type;
      Destination : Rectangle_Type;
   begin
      Character_Value := Character'Pos (Char);

      Character_Location_X := Character_Value mod 16;
      Character_Location_Y := Character_Value  /  16;

      Create
        (Rectangle => Source,
         X         => Character_Location_X * 8,
         Y         => Character_Location_Y * 8,
         Width     => 8,
         Height    => 8);

      Create (Destination, X * 8, Y * 8, 8, 8);

      Copy (Video.Fontset, Video.Renderer, Source, Destination);
   end Draw_Character;

   -------------------
   --  Draw_String  --
   -------------------
   procedure Draw_String
     (Video : in Video_Type;
      Str   : in String;
      X     : in Character_Location_X_Type;
      Y     : in Character_Location_Y_Type)
   is
   begin
      for I in Str'Range loop
         Draw_Character
           (Video => Video,
            Char  => Str (I),
            X     => X + (Character_Location_X_Type (I) - 1),
            Y     => Y);
      end loop;
   end Draw_String;

   ---------------------
   --  Finish_Render  --
   ---------------------
   procedure Finish_Render (Video : in Video_Type) is
   begin
      Present (Video.Renderer);
   end Finish_Render;

   ------------
   --  Free  --
   ------------
   procedure Free (Video : in out Video_Access_Type) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Object => Video_Type,
         Name   => Video_Access_Type);
   begin
      Unchecked_Free (Video);
   end Free;

   --------------------
   --  Start_Render  --
   --------------------
   procedure Start_Render (Video : in Video_Type) is
   begin
      Set_Draw_Color (Video.Renderer, 16#00#, 16#00#, 16#00#, 16#FF#);
      Clear (Video.Renderer);
   end Start_Render;
end Videos;
