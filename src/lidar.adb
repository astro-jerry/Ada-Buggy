------------------------------------------------------------------------------
-- LIDAR
--
-- Support for LIDAR Lite module on a Click board with a servo.
--
-- Created by Jerry Petrey 04-11-15
-- Last Modification:      08-31-16
--
------------------------------------------------------------------------------
with STM32;             use STM32;
with STM32.GPIO;        use STM32.GPIO;
with STM32.Timers;      use STM32.Timers;
with STM32.PWM;         use STM32.PWM;
with Ada.Real_Time;     use Ada.Real_Time;
with Interfaces;        use INterfaces;

package body LIDAR is

   The_Reg_ARR     : constant Word := 20000;  -- for good resolution
   --Min_CCR         : constant Word := 0;
   --Max_CCR         : constant Word := The_Reg_ARR / 10;
   --One_Degree      : constant Word := 6;    -- ~ 1000/2 /90
   Center          : constant Word := 1550; -- small correction to center
   -- Limit is number of One_Degree steps for ~ 90 degrees,
   -- determind experimentally
   Limit           : constant := 780;
   Current_CCR     : Word     := Center;
   Servo_Delay     : constant := 1;
   Offset          : constant := 7; -- to show readings from front of LIDAR

   --Output         : PWM_Modulator;
   --Output_Timer   : Timer renames Timer_2;
   --Output_Channel : constant Timer_Channel := Channel_1;
   --Output_Point   : constant GPIO_Point    := PA5;
   Timer          : aliased PWM_Timer (Timer_2'Access);
   Output         : PWM_Modulator;
   Output_Channel : constant Timer_Channel  := Channel_1;
   PWM_Timer_AF   : constant GPIO_Alternate_Function := GPIO_AF_1_TIM2;

   -- current direction servo is looking
   Current_Look   : Word;

   ---------------------------------------------------------------------------
   --                           Subprograms
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Enable_3_3   Power Enable PC0
   ---------------------------------------------------------------------------
   procedure Enable_3_3 is
   begin
      Set (PC0);
   end Enable_3_3;


   ---------------------------------------------------------------------------
   -- Enable_Sleep
   ---------------------------------------------------------------------------
   procedure Enable_Sleep is
   begin
      Clear (PC0);
   end Enable_Sleep;


   ---------------------------------------------------------------------------
   -- Stop_Measurements    Mode Select PA0
   ---------------------------------------------------------------------------
   procedure Mode_Sel_PWM_Out is
   begin
      -- set pin high
      Set (PA0);
   end Mode_Sel_PWM_Out;
   pragma Unreferenced (Mode_Sel_PWM_Out);

   ---------------------------------------------------------------------------
   -- Start_Measurements
   ---------------------------------------------------------------------------
   procedure Mode_Sel_Status_Trigger is
   begin
      -- set pin low
      Clear (PA0);
   end Mode_Sel_Status_Trigger;
   pragma Unreferenced (Mode_Sel_Status_Trigger);


   ---------------------------------------------------------------------------
   -- Restart_I2C   used when I2C error occurs
   ---------------------------------------------------------------------------
   procedure Restart_I2C is
   begin
      Enable_Sleep;
      Clear_I2C (I2C_Number => I2C_Num2);
      Enable_3_3;
      delay until Clock + Milliseconds (5);
   end Restart_I2C;


   ---------------------------------------------------------------------------
   -- Setup_LIDAR
   ---------------------------------------------------------------------------
   procedure Setup_LIDAR is
   begin
      --  Enable clock for GPIO used
      Enable_Clock (GPIO_A);
      Enable_Clock (GPIO_C);

      -- PC0 for Power Enable
      Configure_IO (This   => PC0,
                    Config => (Mode        => Mode_Out,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));

      -- PA0 for Mode Select, normally input, set as output for triggering
      -- set pin as output even though not currently used
--        Configure_IO (Port   => GPIO_A,
--                      Pin    => Pin_0,
--                      Config => (Mode        => Mode_Out,
--                                 Output_Type => Push_Pull,
--                                 Speed       => Speed_Very_High,
--                                 Resistors   => Floating));
      -- enable LIDAR
      Enable_3_3;

      delay until Clock + Milliseconds (5);

      -- setup the I2C port
      Setup_I2C_Port (Pin_Pack   => I2C_Pins,
                      CR1_Values => I2C_CR1_Setup_Record,
                      CR2_Values => I2C_CR2_Setup_Record);
      Setup_Servo;
   end Setup_LIDAR;


   ---------------------------------------------------------------------------
   -- Read_Distance
   ---------------------------------------------------------------------------
   function Read_Distance return HWord is
      An_Error1    : Boolean  := False;
      An_Error2    : Boolean  := False;
      An_Error3    : Boolean  := False;
      Num_Tries    : constant := 5;
      Reading_Good : Boolean  := False;
      --
      Dist_Array   : Array_Of_Bytes (1 .. 2) := (0, 0);
      Distance     : HWord    := 0;
      Connected    : Boolean  := True;
   begin
      Connected := Is_Device_Connected (I2C_Number => I2C_Num2,
                                        Slave_Addr => LIDAR_Slave_Addr);
      if not Connected then
         return 998;
      end if;
      -- since the LIDAR Lite is somewhat flakey on I2C, will try
      -- a few times for a good reading; if any step fails we will
      -- do a restart of I2C; if no complete reading is obtained
      -- after a few tries, we will just return a high value (999).
      for I in 1 .. Num_Tries loop
         Write_I2C_Byte (I2C_Number => I2C_Num2,
                         Slave_Addr => LIDAR_Slave_Addr,
                         Register   => LIDAR_Command_Control_Reg,
                         Data       => Measure_Value,
                         Error      => An_Error1);
         if An_Error1 then
            Restart_I2C;
            goto Retry;
         end if;
         delay until Clock + Milliseconds (20);

         Read_I2C_Byte (I2C_Number => I2C_Num2,
                        Slave_Addr => LIDAR_Slave_Addr,
                        Register   => LIDAR_Distance_LSB_Addr,
                        Data       => Dist_Array (2),
                        Error      => An_Error2);
         if An_Error2 then
            Restart_I2C;
            goto Retry;
         end if;
         delay until Clock + Milliseconds (5);

         Read_I2C_Byte (I2C_Number => I2C_Num2,
                        Slave_Addr => LIDAR_Slave_Addr,
                        Register   => LIDAR_Distance_MSB_Addr,
                        Data       => Dist_Array (1),
                        Error      => An_Error3);
         if An_Error3 then
            Restart_I2C;
            goto Retry;
         else
            -- got here without errors so use reading
            Reading_Good := True;
            exit;
         end if;

         <<Retry>>
         -- reset all error flags
         An_Error1 := False;
         An_Error2 := False;
         An_Error3 := False;
         delay until Clock + Milliseconds (5);
      end loop;

      if not Reading_Good then
         -- return large value
         return 999;
      else
         -- convert results to distance in cm
         Distance := 0;
         -- get high byte
         Distance := HWord (Dist_Array (1));
         -- shift to MSB
         Distance := Shift_Left (Distance, 8);
         -- add low byte
         Distance := Distance or HWord (Dist_Array (2));
         return (Distance - Offset);
      end if;
   end Read_Distance;



   ---------------------------------------------------------------------------
   -- Setup_Servo
   -- for LIDAR and servo module in MB1 of Buggy
   ---------------------------------------------------------------------------
   procedure Setup_Servo is
   begin
      -- setup the PWM
      Initialise_PWM_Timer
                    (Timer,
                     Requested_Frequency    => 1_000_000.0,
                     User_Period_Presc      => True,
                     Timer_Period           => The_Reg_ARR,
                     Timer_Prescaler        => 83);

      Attach_PWM_Channel (Timer'Access,
                          Output,
                          Output_Channel,
                          (GPIO_A'Access, 5),
                           PWM_Timer_AF);
      Enable_PWM (Output);
      
      Set_Duty_Cycle (Output, 0);
      Center_Servo;
   end Setup_Servo;


   ---------------------------------------------------------------------------
   -- Center_Servo
   ---------------------------------------------------------------------------
   procedure Center_Servo is
   begin
      Current_CCR  := Center;
      Set_Compare_Value (Timer_2, Channel_1, Current_CCR);
      Current_Look := Center;  -- update current looking direction
      delay until Clock + Milliseconds (250);
   end Center_Servo;


   ---------------------------------------------------------------------------
   -- Look_Right
   ---------------------------------------------------------------------------
   procedure Look_Right is
   begin
      -- rotate CW to right limit
      for I in 1 .. Limit loop
         Current_CCR  := Current_CCR + 1;
         Set_Compare_Value (Timer_2, Channel_1, Current_CCR);
         delay until Clock + Milliseconds (Servo_Delay);
      end loop;
      Current_Look := Current_CCR;  -- update current looking direction
   end Look_Right;


   ---------------------------------------------------------------------------
   -- Look_Left
   ---------------------------------------------------------------------------
   procedure Look_Left is
   begin
      -- rotate CCW to left limit
      for I in 1 .. Limit loop
         Current_CCR := Current_CCR - 1;
         Set_Compare_Value (Timer_2, Channel_1, Current_CCR);
         delay until Clock + Milliseconds (Servo_Delay);
      end loop;
      Current_Look := Current_CCR;  -- update current looking direction
   end Look_Left;


   ---------------------------------------------------------------------------
   -- Look_Forward
   ---------------------------------------------------------------------------
   procedure Look_Forward is
   begin
      if Current_Look > Center then
         -- rotate CCW back to center
         for I in 1 .. Limit loop
            Current_CCR := Current_CCR - 1;
            Set_Compare_Value (Timer_2, Channel_1, Current_CCR);
            delay until Clock + Milliseconds (Servo_Delay);
         end loop;

      elsif Current_Look < Center then
         -- rotate CW back to center
         for I in 1 .. Limit loop
            Current_CCR := Current_CCR + 1;
            Set_Compare_Value (Timer_2, Channel_1, Current_CCR);
            delay until Clock + Milliseconds (Servo_Delay);
         end loop;
      end if;
      Current_Look := Current_CCR;  -- update current looking direction
   end Look_Forward;


begin
   null;
end LIDAR;
