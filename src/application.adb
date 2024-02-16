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

with Renders;
with SDL_Events;   use SDL_Events;
with SDL_Keycodes; use SDL_Keycodes;

package body Application is
   ------------------------
   --  Application_Loop  --
   ------------------------
   procedure Application_Loop is
   begin
      while App.Running loop
         Input;
         Render;
      end loop;
   end Application_Loop;

   --------------------
   --  Deinitialize  --
   --------------------
   procedure Deinitialize is
   begin
      Destroy (App.Game.all);
      Destroy (App.Video.all);

      Free (App.Game);
      Free (App.Video);
   end Deinitialize;

   -----------------
   --  Get_Video  --
   -----------------
   function Get_Video return Video_Access_Type is
   begin
      return App.Video;
   end Get_Video;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize is
   begin
      App.Video := new Video_Type;
      App.Game  := new Game_Type;

      Create (App.Video.all);
      App.Running := True;

      Create (App.Game.all);
   end Initialize;

   -------------
   --  Input  --
   -------------
   procedure Input is
      Event : Event_Type;
   begin
      Wait (Event);
      case Get_Type (Event) is
         when Quit_Event     => App.Running := False;
         when Key_Down_Event =>
            if
              Is_Game_Over (App.Game.all) and
              Get_Keycode (Event) = SDLK_Escape
            then
               App.Running := False;
            end if;
         when others => null;
      end case;

      Input (App.Game.all, Event);
   end Input;

   --------------
   --  Render  --
   --------------
   procedure Render is
   begin
      Start_Render  (App.Video.all);
      Renders.Start (App.Game.all);
      Finish_Render (App.Video.all);
   end Render;
end Application;
