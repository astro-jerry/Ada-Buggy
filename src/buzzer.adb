------------------------------------------------------------------------------
-- Buzzer
--
-- Support for Piezo Buzzer on a Timer PWM pin.
--
-- Created by Jerry Petrey 05-23-15
-- Last Modification:      08-31-16
--
------------------------------------------------------------------------------
with STM32;            use STM32;
with Ada.Real_Time;    use Ada.Real_Time;
with STM32.PWM;        use STM32.PWM;
with STM32.Timers;     use STM32.Timers;
with STM32.Device;     use STM32.Device;

package body Buzzer is

   -- Buggy using Clicker2 MB2 uses Timer 4, Ch1, PP2, PD12
   -- setup for F407 Clicker2 Board
   Timer          : aliased PWM_Timer (Timer_4'Access);
   Output         : PWM_Modulator;
   Output_Channel : constant Timer_Channel  := Channel_1;
   PWM_Timer_AF   : constant GPIO_Alternate_Function := GPIO_AF_2_TIM4;

   ----------------------------------------------------------------------
   -- possible useful routine for some apps, not used here
   ----------------------------------------------------------------------
   --  approximation to the sine function for use with SFP runtime
   --  function Sine (Input : Long_Float) return Long_Float is
   --      Pi : constant Long_Float := 3.14159_26535_89793_23846;
   --      X  : constant Long_Float := Long_Float'Remainder(Input, Pi * 2.0);
   --      B  : constant Long_Float := 4.0 / Pi;
   --      C  : constant Long_Float := (-4.0) / (Pi * Pi);
   --      Y  : constant Long_Float := B * X + C * X * abs (X);
   --      P  : constant Long_Float := 0.225;
   --  begin
   --   return P * (Y * abs (Y) - Y) + Y;
   --  end Sine;


   ---------------------------------------------------------------------------
   --  Play_Sound
   ---------------------------------------------------------------------------
   procedure Play_Sound (Freq      : in Natural;
                         Length    : in Natural;
                         Pause     : in Natural := 0) is
   begin
      Initialise_PWM_Timer (This                   => Timer,
                            Requested_Frequency    => Float (Freq));

      Attach_PWM_Channel (Timer'Access,
                          Output,
                          Output_Channel,
                          (GPIO_D'Access, 12),
                          PWM_Timer_AF);

      Enable_PWM (Output);

      delay until Clock + Milliseconds (1);

      --  create freq
      Set_Duty_Cycle (Output, 50);
      delay until Clock + Milliseconds (Length);
      Set_Duty_Cycle (Output, 0);

      if Pause > 0 then
         delay until Clock + Milliseconds (Pause);
      end if;
   end Play_Sound;


begin
   null;
end Buzzer;
