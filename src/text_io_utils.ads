------------------------------------------------------------------------------
--
-- Name:   Text_IO_Utils
-- Date:   8-07-14
-- Author: Jerry Petrey
-- Desc:   Utilities to supplement the ZFP/SFP Text_IO package.
--
------------------------------------------------------------------------------

package Text_IO_Utils is


   type Console_Colors is (Black, Red, Green, Yellow,
                           Blue, Magenta, Cyan, White);


   procedure Erase_Screen;

   procedure Erase_To_EOL;

   procedure Cursor_To (Line : in Natural;
                        Col  : in Natural);

   procedure Cursor_Forward (Cols : in Natural);

   procedure Cursor_Backward (Cols : in Natural);

   procedure Cursor_Up (Lines : in Natural);

   procedure Cursor_Down (Lines : in Natural);

   procedure Bold_On;

   procedure Blink_On;

   procedure Reverse_On;

   procedure All_Off;

   procedure Set_FG_Color (Color : in Console_Colors);

   procedure Set_BG_Color (Color : in Console_Colors);


end Text_IO_Utils;
