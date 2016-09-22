------------------------------------------------------------------------------
-- buggy.adb
--
-- Handles control of Buggy chassis using Clicker 2 (or Mikromedia board)
--
-- Created by Jerry Petrey 02-07-15
-- Last Modification:      08-31-16
------------------------------------------------------------------------------
with STM32;            use STM32;
with STM32.GPIO;       use STM32.GPIO;
with STM32.PWM;        use STM32.PWM;
with STM32.Timers;     use STM32.Timers;
with STM32.Device;      use STM32.Device;
with Ada.Real_Time;    use Ada.Real_Time;
-- for DEBUG
--with OLED1351;          use OLED1351;

package body Buggy is

   -- Left side motors control (PWM)
   -- PWM-A PB9  -- backward
   -- PWM-B PB8  -- forward
   -- Right side motors control (PWM)
   -- PWM-C PE5  -- backward
   -- PWM-D PB0  -- forward
   
   -- Brake lights
   -- PE1
   -- Left Signal lights
   -- PC4
   -- Right Signal lights
   -- PE2
   -- Headlights (low beam)
   -- PE3
   -- Headlights (main beam)
   -- PB6

   -- Misc
   -- SPI2 
   -- SCK  PB13
   -- MISO PB14
   -- MOSI PB15
   -- UART
   -- TX PA0
   -- RX PA1
   -- Battery sense
   -- PC3
   -- Battery Status
   -- PD0
   
   ---------------------------------------------------------------------------
   -- Motor PWM values
   ---------------------------------------------------------------------------
   -- Right motor - forward
   R_Motor_F_Timer          : aliased PWM_Timer (Timer_3'Access);
   R_Motor_F_Output         : PWM_Modulator;  -- PB0
   R_Motor_F_Output_Channel : constant Timer_Channel  := Channel_3;
   R_Motor_F_PWM_Timer_AF   : constant GPIO_Alternate_Function := GPIO_AF_2_TIM3;
   
   -- Right motor - backwards
   R_Motor_B_Timer          : aliased PWM_Timer (Timer_9'Access);
   R_Motor_B_Output         : PWM_Modulator;  -- PE5
   R_Motor_B_Output_Channel : constant Timer_Channel  := Channel_1;
   R_Motor_B_PWM_Timer_AF   : constant GPIO_Alternate_Function := GPIO_AF_3_TIM9;
   
   -- Left motor - forwards
   L_Motor_F_Timer          : aliased PWM_Timer (Timer_4'Access);
   L_Motor_F_Output         : PWM_Modulator;  -- PB8
   L_Motor_F_Output_Channel : constant Timer_Channel  := Channel_3;
   L_Motor_F_PWM_Timer_AF   : constant GPIO_Alternate_Function := GPIO_AF_2_TIM4;
   
   -- Left motor - backwards
   L_Motor_B_Timer          : aliased PWM_Timer (Timer_4'Access);
   L_Motor_B_Output         : PWM_Modulator;  -- PB9
   L_Motor_B_Output_Channel : constant Timer_Channel  := Channel_4;
   L_Motor_B_PWM_Timer_AF   : constant GPIO_Alternate_Function := GPIO_AF_2_TIM4;
   
   
   ---------------------------------------------------------------------------
   -- procedures to control MB chip selects
   ---------------------------------------------------------------------------
   -- PD10 CS for Buggy MB1, PB1 for MB2, PE4 for MB3
   procedure Disable_Buggy_MB1_CS is
   begin
      Set (PD10);
   end Disable_Buggy_MB1_CS;

   
   procedure Enable_Buggy_MB1_CS is
   begin
      Clear (PD10);
   end Enable_Buggy_MB1_CS;
   pragma Unreferenced (Enable_Buggy_MB1_CS);
   
   
   procedure Disable_Buggy_MB2_CS is
   begin
      Set (PB1);
   end Disable_Buggy_MB2_CS;

   
   procedure Enable_Buggy_MB2_CS is
   begin
      Clear (PB1);
   end Enable_Buggy_MB2_CS;
   pragma Unreferenced (Enable_Buggy_MB2_CS);
   
   procedure Disable_Buggy_MB3_CS is
   begin
      Set (PE4);
   end Disable_Buggy_MB3_CS;

   
   procedure Enable_Buggy_MB3_CS is
   begin
      Clear (PE4);
   end Enable_Buggy_MB3_CS;
   pragma Unreferenced (Enable_Buggy_MB3_CS);
   
   ---------------------------------------------------------------------------
   -- Setup_Motor_PWM
   ---------------------------------------------------------------------------
   --                    Motors
   --      left side                     right side
   -- Back PWM-A PB9 TIM4_CH4_PP1 AF2    PWM-C PE5 TIM9_CH1_PP2 AF3
   -- Fwd  PWM-B PB8 TIM4_CH3_PP1 AF2    PWM-D PB0 TIM3_CH3_PP1 AF2
   --
   -- Timer clocks:
   -- Timers 2,3,4,5,6,7,12,13,14 --  84 MHz from APB1
   -- Timers 1,8,9,10,11          -- 168 MHz from APB2
   --
   ---------------------------------------------------------------------------
   procedure Setup_Motor_PWM is
   begin
      --Put_String (2, 30, "  Setup_Motor_PWM");
      ------------------------------------------------------------------------
      -- init Timer 3, Ch3 for RIGHT motor Fwd - PB0
      ------------------------------------------------------------------------
      --Put_String (2, 40, "  R_Motor_F");
      Initialise_PWM_Timer (This                 => R_Motor_F_Timer,
                            Requested_Frequency  => 3000.0); -- arbitrary

      Attach_PWM_Channel (R_Motor_F_Timer'Access,
                          R_Motor_F_Output,
                          R_Motor_F_Output_Channel,
                          (GPIO_B'Access, 0),
                          R_Motor_F_PWM_Timer_AF);

      Enable_PWM (R_Motor_F_Output);
      
      ------------------------------------------------------------------------
      -- init Timer 9, Ch1 for RIGHT motor Back - PE5
      ------------------------------------------------------------------------
      --Put_String (2, 50, "  R_Motor_B");
      Initialise_PWM_Timer (This                 => R_Motor_B_Timer,
                            Requested_Frequency  => 3000.0); -- arbitrary

      Attach_PWM_Channel (R_Motor_B_Timer'Access,
                          R_Motor_B_Output,
                          R_Motor_B_Output_Channel,
                          (GPIO_E'Access, 5),
                          R_Motor_B_PWM_Timer_AF);

      Enable_PWM (R_Motor_B_Output);
      
      
      ------------------------------------------------------------------------
      -- init Timer 4, Ch3 for LEFT motor Fwd - PB8
      ------------------------------------------------------------------------
      --Put_String (2, 60, "  L_Motor_F");
      Initialise_PWM_Timer (This                 => L_Motor_F_Timer,
                            Requested_Frequency  => 3000.0); -- arbitrary

      Attach_PWM_Channel (L_Motor_F_Timer'Access,
                          L_Motor_F_Output,
                          L_Motor_F_Output_Channel,
                          (GPIO_B'Access, 8),
                          L_Motor_F_PWM_Timer_AF);

      Enable_PWM (L_Motor_F_Output);

      
      ------------------------------------------------------------------------
      -- init Timer 4, Ch4 for LEFT motor Back - PB9
      ------------------------------------------------------------------------
      --Put_String (2, 70, "  L_Motor_B");
      Initialise_PWM_Timer (This                 => L_Motor_B_Timer,
                            Requested_Frequency  => 3000.0); -- arbitrary

      Attach_PWM_Channel (L_Motor_B_Timer'Access,
                          L_Motor_B_Output,
                          L_Motor_B_Output_Channel,
                          (GPIO_B'Access, 9),
                          L_Motor_B_PWM_Timer_AF);

      Enable_PWM (L_Motor_B_Output);
      
   end Setup_Motor_PWM;
   
   
   
   ---------------------------------------------------------------------------
   -- Setup_Buggy
   -- Initializes Buggy hardware
   ---------------------------------------------------------------------------
   procedure Setup_Buggy is
   begin
      --Put_String (2, 10, "Setup_Buggy ...");
      --  Enable clock for GPIO ports used
      Enable_Clock (GPIO_B);
      Enable_Clock (GPIO_C);
      Enable_Clock (GPIO_D);
      Enable_Clock (GPIO_E);
      
      -- setup other buggy functions
      
      --Put_String (2, 20, "  Config GPIOI");
      -- set outputs for Headlights Main beam
      Configure_IO (This   => PB6,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));

      -- set outputs for Headlights Low beam
      Configure_IO (This   => PE3,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));
      
      -- set outputs for Left Signals
      Configure_IO (This   => PC4,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));

      -- set outputs for Right Signals
      Configure_IO (This   => PE2,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));

      -- set outputs for Brake lights
      Configure_IO (This   => PE1,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));
      
      -- setup CS's for Buggy microbusses
      -- PD10 CS for Buggy MB1, PB1 for MB2, PE4 for MB3
      Configure_IO (This   => PD10,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));
      
      Configure_IO (This   => PB1,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));
      
      Configure_IO (This   => PE4,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));
      
      -- setup PWM for motors
      Setup_Motor_PWM;
      
      Disable_Buggy_MB1_CS;
      Disable_Buggy_MB2_CS;
      Disable_Buggy_MB3_CS;
   end Setup_Buggy;
   

   
   procedure Set_HeadLights (Which : in HeadLight_Type) is
   begin
      if Which = Low then
         Set (PE3);
         Clear (PB6);
      elsif Which = High then
         Set (PB6);
         Clear (PE3);
      elsif Which = Off then
         Clear (PE3);
         Clear (PB6);
      end if;
   end Set_HeadLights;
   
   
   procedure Set_BrakeLight (State : in State_Type) is
   begin
      if State = On then
         Set (PE1);
      elsif State = Off then
         Clear (PE1);
      end if;
   end Set_BrakeLight;

   
   procedure Set_Left_Turn (State : in State_Type) is
   begin
      if State = On then
         Set (PC4);
      elsif State = Off then
         Clear (PC4);
      end if;
   end Set_Left_Turn;

   
   procedure Set_Right_Turn (State : in State_Type) is
   begin
      if State = On then
         Set (PE2);
      elsif State = Off then
         Clear (PE2);
      end if;
   end Set_Right_Turn;
   
   
   procedure Set_Motor (Motor : in Motor_Type; 
                        State : in Motor_State_Type;
                        Speed : in Speed_Type := 0) is
   begin
      case Motor is
         when Left =>
            if State = Backward then
               Set_Duty_Cycle (L_Motor_B_Output, Speed);
            elsif State = Forward then
               Set_Duty_Cycle (L_Motor_F_Output, Speed);
            elsif State = Off then
               Set_Duty_Cycle (L_Motor_B_Output, 0);
               Set_Duty_Cycle (L_Motor_F_Output, 0);
            end if;
         
         when Right =>
            if State = Backward then
               Set_Duty_Cycle (R_Motor_B_Output, Speed);
            elsif State = Forward then
               Set_Duty_Cycle (R_Motor_F_Output, Speed);
            elsif State = Off then
               Set_Duty_Cycle (R_Motor_B_Output, 0);
               Set_Duty_Cycle (R_Motor_F_Output, 0);
            end if;
         end case;
   end Set_Motor;

   
   --------------------------------------------------------------------------
   -- Signal_Left_Turn
   --------------------------------------------------------------------------
   procedure Signal_Left_Turn is
   begin
      for I in 1 .. 5 loop
         Set_Left_Turn (On);
         delay until Clock + Milliseconds (500);
         Set_Left_Turn (Off);
         delay until Clock + Milliseconds (500);
      end loop;
   end Signal_Left_Turn;

   --------------------------------------------------------------------------
   -- Signal_Right_Turn
   --------------------------------------------------------------------------
   procedure Signal_Right_Turn is
   begin
      for I in 1 .. 5 loop
         Set_Right_Turn (On);
         delay until Clock + Milliseconds (500);
         Set_Right_Turn (Off);
         delay until Clock + Milliseconds (500);
      end loop;
   end Signal_Right_Turn;

   --------------------------------------------------------------------------
   -- Rotate_Left
   --------------------------------------------------------------------------
   procedure Rotate_Left (Speed : in Speed_Type;
                          Amt   : in Natural) is
   begin
      Set_BrakeLight (Off);
      Set_HeadLights (High);
      Set_Motor (Right, Forward, Speed);
      Set_Motor (Left, Backward, Speed);
      delay until Clock + Milliseconds (Amt);
      Set_Motor (Right, Off);
      Set_Motor (Left, Off);
   end Rotate_Left;

   --------------------------------------------------------------------------
   -- Rotate_Right
   --------------------------------------------------------------------------
   procedure Rotate_Right (Speed : in Speed_Type;
                           Amt   : in Natural) is
   begin
      Set_BrakeLight (Off);
      Set_HeadLights (High);
      Set_Motor (Right, Backward, Speed);
      Set_Motor (Left, Forward, Speed);
      delay until Clock + Milliseconds (Amt);
      Set_Motor (Right, Off);
      Set_Motor (Left, Off);
   end Rotate_Right;

   --------------------------------------------------------------------------
   -- Move_Forward
   --------------------------------------------------------------------------
   procedure Move_Forward (Speed : in Speed_Type) is
   begin
      Set_BrakeLight (Off);
      Set_HeadLights (High);
      Set_Motor (Right, Forward, Speed);
      Set_Motor (Left, Forward, Speed);
   end Move_Forward;

   --------------------------------------------------------------------------
   -- Move_Forward
   --------------------------------------------------------------------------
   procedure Move_Forward (Speed : in Speed_Type;
                           Amt   : in Natural) is
   begin
      Set_BrakeLight (Off);
      Set_HeadLights (High);
      Set_Motor (Right, Forward, Speed);
      Set_Motor (Left, Forward, Speed);
      delay until Clock + Milliseconds (Amt);
      Stop;
   end Move_Forward;
   
   
   --------------------------------------------------------------------------
   -- Move_Backward
   --------------------------------------------------------------------------
   procedure Move_Backward (Speed : in Speed_Type) is
   begin
      Set_HeadLights (High);
      Set_Motor (Right, Backward, Speed);
      Set_Motor (Left, Backward, Speed);
   end Move_Backward;

   --------------------------------------------------------------------------
   -- Move_Backward
   --------------------------------------------------------------------------
   procedure Move_Backward (Speed : in Speed_Type;
                            Amt   : in Natural) is
   begin
      Set_BrakeLight (Off);
      Set_HeadLights (High);
      Set_Motor (Right, Backward, Speed);
      Set_Motor (Left, Backward, Speed);
      delay until Clock + Milliseconds (Amt);
      Stop;
   end Move_Backward;
   
   
   --------------------------------------------------------------------------
   -- Stop
   --------------------------------------------------------------------------
   procedure Stop is
   begin
      Set_Motor (Right, Off);
      Set_Motor (Left, Off);
      Set_BrakeLight (On);
      Set_HeadLights (Off);
   end Stop;

   --------------------------------------------------------------------------
   -- Flash_Headlights
   --------------------------------------------------------------------------
   procedure Flash_Headlights is
   begin
      Set_HeadLights (Off);
      delay until Clock + Milliseconds (500);
      Set_HeadLights (High);
      delay until Clock + Milliseconds (500);
      Set_HeadLights (Off);
      delay until Clock + Milliseconds (500);
      Set_HeadLights (Low);
   end Flash_Headlights;


begin
   null;
end Buggy;
