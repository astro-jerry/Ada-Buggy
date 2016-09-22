---------------------------------------------------------------------------
--
-- Name:   Text_IO_Utils
-- Date:   8-07-14
-- Author: Jerry Petrey
-- Desc:   Utilities to supplement the ZFP/SFP Text_IO package.
--
---------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;

package body Text_IO_Utils is

   --
   function Strip_Space (This : String) return String is
      Str : String (1 .. (This'Length - 1));
   begin
      Str := This ((This'First + 1) .. This'Last);
      return Str;
   end Strip_Space;
   
   
   --
   procedure Erase_Screen is
   
   begin
      Put (ASCII.ESC & "[2J");
   end Erase_Screen;
   
   
   --
   procedure Erase_To_EOL is
   
   begin
      Put (ASCII.ESC & '[' & 'K');
   end Erase_To_EOL;
   
   
   --
   procedure Cursor_To (Line : in Natural;
                        Col  : in Natural) is
      Line_Str : constant String := Natural'Image (Line);
      Col_Str  : constant String := Natural'Image (Col);
   begin
      Put (ASCII.ESC & '[' & Strip_Space (Line_Str) 
                     & ';' & Strip_Space (Col_Str) & 'H');
   end Cursor_To;
   
   
   --
   procedure Cursor_Forward (Cols : in Natural) is
      Cols_Str : constant String := Natural'Image (Cols);
   begin
      Put (ASCII.ESC & '[' & Strip_Space (Cols_Str) & 'C');
   end Cursor_Forward;
   
   
   --
   procedure Cursor_Backward (Cols : in Natural) is
      Cols_Str : constant String := Natural'Image (Cols);
   begin
      Put (ASCII.ESC & '[' & Strip_Space (Cols_Str) & 'D');
   end Cursor_Backward;
   
   
   --
   procedure Cursor_Up (Lines : in Natural) is
      Lines_Str : constant String := Natural'Image (Lines);
   begin
      Put (ASCII.ESC & '[' & Strip_Space (Lines_Str) & 'A');
   end Cursor_Up;
   
   
   --
   procedure Cursor_Down (Lines : in Natural) is
      Lines_Str : constant String := Natural'Image (Lines);
   begin
      Put (ASCII.ESC & '[' & Strip_Space (Lines_Str) & 'B');
   end Cursor_Down;
   
   
   --
   procedure Bold_On is
   
   begin
      Put (ASCII.ESC & '1' & 'm');
   end Bold_On;
   
   
   --
   procedure Blink_On is
   
   begin
      Put (ASCII.ESC & '5' & 'm');
   end Blink_On;
   
   
   --
   procedure Reverse_On is
   
   begin
      Put (ASCII.ESC & '7' & 'm');
   end Reverse_On;
   
   
   --
   procedure All_Off is
   
   begin
      Put (ASCII.ESC & '0' & 'm');
   end All_Off;
   
   
   --
   procedure Set_FG_Color (Color : in Console_Colors) is
      Color_Str : String (1 .. 2);
   begin
      case Color is
         when Black   => Color_Str := "30";
         when Red     => Color_Str := "31";
         when Green   => Color_Str := "32";
         when Yellow  => Color_Str := "33";
         when Blue    => Color_Str := "34";
         when Magenta => Color_Str := "35";
         when Cyan    => Color_Str := "36";
         when White   => Color_Str := "37";
      end case;
      Put (ASCII.ESC & '[' & Color_Str & 'm');
   end Set_FG_Color;


   --
   procedure Set_BG_Color (Color : in Console_Colors) is
      Color_Str : String (1 .. 2);
   begin
      case Color is
         when Black   => Color_Str := "40";
         when Red     => Color_Str := "41";
         when Green   => Color_Str := "42";
         when Yellow  => Color_Str := "43";
         when Blue    => Color_Str := "44";
         when Magenta => Color_Str := "45";
         when Cyan    => Color_Str := "46";
         when White   => Color_Str := "47";
      end case;
      Put (ASCII.ESC & '[' & Color_Str & 'm');
   end Set_BG_Color;
   

begin
   null;
end Text_IO_Utils;
