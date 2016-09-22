------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
--  Note this version is for use with the ravenscar-sfp runtime.

with STM32.Board;              use STM32.Board;
with Ada.Real_Time;            use Ada.Real_Time;
with OLED1351;                 use OLED1351;
with System.Storage_Elements;  use System.Storage_Elements;
with Ada.Unchecked_Conversion;

package body Last_Chance_Handler is


   type Char_Pointer is access all Character;

   function As_Char_Pointer is
            new Ada.Unchecked_Conversion (Source => Integer_Address,
                                          Target => Char_Pointer);

   -------------------------
   -- Last_Chance_Handler --
   -------------------------
   procedure Last_Chance_Handler (Msg  : System.Address;
                                  Line : Integer) is
      --pragma Unreferenced (Msg, Line);
      P        : Integer_Address;
      Location : String (1 .. 160) := (others => ' ');
      N        : Positive := 1; -- string Location index
   begin
       P := To_Integer (Msg);
      loop
         exit when (As_Char_Pointer (P).all = ASCII.Nul);
         Location (N) := (As_Char_Pointer (P).all);
         P := P + 1;
         N := N + 1;
      end loop;

      -- show error on LCD
      Fill_Screen (SSD1351_COLOR_Red);
      Put_String (2,
                  10,
                  "EXCEPTION Occurred in",
                  SSD1351_COLOR_White,
                  SSD1351_COLOR_Red);
      if N < 21 then
         Put_String (2,
                     25,
                     (Location (1 .. N)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
      elsif N < 41 then  -- print on two lines
         Put_String (2,
                     25,
                     (Location (1 .. 20)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
         Put_String (2,
                     40,
                     (Location (21 .. N)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
      elsif N < 61 then -- print on 3 lines
         Put_String (2,
                     25,
                     (Location (1 .. 20)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
         Put_String (2,
                     40,
                     (Location (21 .. 40)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
         Put_String (2,
                     55,
                     (Location (41 .. N)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
      else -- print on 4 lines
         Put_String (2,
                     25,
                     (Location (1 .. 20)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
         Put_String (2,
                     40,
                     (Location (21 .. 40)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
         Put_String (2,
                     55,
                     (Location (41 .. 60)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
         Put_String (2,
                     70,
                     (Location (61 .. N)),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
      end if;
      if Line /= 0 then
         Put_String (2,
                     85,
                     "at Line:" & Integer'Image (Line),
                     SSD1351_COLOR_White,
                     SSD1351_COLOR_Red);
      end if;

      --  No-return procedure...
      loop
         -- flash LED2 on unhandled exception
         Turn_On (LCH_LED);
         delay until Clock + Milliseconds (70);
         Turn_Off (LCH_LED);
         delay until Clock + Milliseconds (70);
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
