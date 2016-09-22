------------------------------------------------------------------------------
-- Buzzer
--
-- Support for Piezo Buzzer Click module or my Click module with small
-- speaker connected to PWM pin.
--
-- Created by Jerry Petrey 05-23-15
-- Last Modification:      06-02-16
--
------------------------------------------------------------------------------
with STM32_SVD.RCC;  use STM32_SVD.RCC;

package Buzzer is

   pragma Elaborate_Body;

   -- for sounds via buzzer click board
   ---------------------------------------------------------------------------
   subtype Freq_Type is Natural range 0 .. 20_000;

   FULL_NOTE         : constant := 1200; -- in milliseconds
   HALF_DOT_NOTE     : constant :=  900;
   HALF_NOTE         : constant :=  600;
   QUARTER_DOT_NOTE  : constant :=  450;
   QUARTER_NOTE      : constant :=  300;
   EIGHTH_DOT_NOTE   : constant :=  225;
   EIGHTH_NOTE       : constant :=  150;
   SIXTINTH_DOT_NOTE : constant :=  112;
   SIXTINTH_NOTE     : constant :=   75;

   -- Scales : Scales_Type :=
   --  1      2     3     4     5     6     7     8    9     10    11     12
   --  C5  C5s/D5f  D5  D5s/E5f E5    F5  F5s/G5f G5  G5s/A5f A5  A5s/B5f B5
   -- 523,  554,  587,  622,  659,  698,  740,  784,  831,  880,  932,  988,
   --  C6  C6s/D6f  D6  D6s/E6f E6    F6  F6s/G6f G6  G6s/A6f A6  A6s/B6f B6
   -- 1047, 1109, 1175, 1245, 1319, 1397, 1480, 1568, 1661, 1760, 1865, 1976,
   --  C7  C7s/D7f  D7  D7s/E7f E7    F7  F7s/G7f G7  G7s/A7f A7  A7s/B7f B7
   -- 2093, 2218, 2349, 2489, 2637, 2794, 2960, 3136, 3322, 3520, 3729, 3951
   -- next would be C8 4186

   C5  : constant := 523;
   C5s : constant := 554;
   D5f : constant := 554;
   D5  : constant := 587;
   D5s : constant := 622;
   E5f : constant := 622;
   E5  : constant := 659;
   F5  : constant := 698;
   F5s : constant := 740;
   G5f : constant := 740;
   G5  : constant := 784;
   G5s : constant := 831;
   A5f : constant := 831;
   A5  : constant := 880;
   A5s : constant := 931;
   B5f : constant := 931;
   B5  : constant := 988;
   --
   C6  : constant := 1047;
   C6s : constant := 1109;
   D6f : constant := 1109;
   D6  : constant := 1175;
   D6s : constant := 1245;
   E6f : constant := 1245;
   E6  : constant := 1319;
   F6  : constant := 1397;
   F6s : constant := 1480;
   G6f : constant := 1480;
   G6  : constant := 1568;
   G6s : constant := 1661;
   A6f : constant := 1661;
   A6  : constant := 1760;
   A6s : constant := 1865;
   B6f : constant := 1865;
   B6  : constant := 1976;
   --
   C7  : constant := 2093;
   C7s : constant := 2218;
   D7f : constant := 2218;
   D7  : constant := 2349;
   D7s : constant := 2489;
   E7f : constant := 2489;
   E7  : constant := 2637;
   F7  : constant := 2794;
   F7s : constant := 2960;
   G7f : constant := 2960;
   G7  : constant := 3136;
   G7s : constant := 3322;
   A7f : constant := 3322;
   A7  : constant := 3520;
   A7s : constant := 3729;
   B7f : constant := 3729;
   B7  : constant := 3951;

   ---------------------------------------------------------------------------
   -- subprograms
   ---------------------------------------------------------------------------

   procedure Play_Sound (Freq      : in Natural;
                         Length    : in Natural;
                         Pause     : in Natural := 0);

end Buzzer;
