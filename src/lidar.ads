------------------------------------------------------------------------------
-- LIDAR
--
-- Support for LIDAR Lite module on a Click board with a servo.
-- In this case this board is in the Buggy MB1 slot.
-- Created by Jerry Petrey 04-11-15
-- Last Modification:      06-12-16
--
------------------------------------------------------------------------------
with HAL;                   use HAL;
with STM32.I2C;             use STM32.I2C;
with STM32.Device;          use STM32.Device;
with STM32F4_I2C_Registers; use STM32F4_I2C_Registers;

package LIDAR is

   package I2C renames STM32.I2C;

    I2C_CR1_Setup_Record : constant I2C_CR1_Setup_Record_Type :=
      (PE            => Disabled,
       SMBUS_Mode    => I2C_Mode,
       Resv1         => 0,
       SMBTYPE       => Device,
       ENARP         => Disabled,
       ENPEC         => Disabled,
       ENGC          => Disabled,
       NOSTRETCH     => Stretch_On,
       START         => Start_Disabled,
       STOP          => Stop_Disabled,
       ACK           => Ack_Disabled,
       POS           => Current_Byte,
       PEC           => Disabled,
       ALERT         => SMBA_High,
       Resv2         => 0,
       SWRST         => Not_Reset);

   I2C_CR2_Setup_Record : constant I2C_CR2_Setup_Record_Type :=
      (FREQ      => 42,
       Resv1     => 0,
       ITERREN   => Disabled,
       ITEVTEN   => Disabled,
       ITBUFEN   => Disabled,
       DMAEN     => Disabled,
       LAST_DMA  => Not_Last,
       Resv2     => 0);

   -- ************* Must be set correctly for I2C port used ***************
   I2C_Pins : constant Pins_Type := I2C2_Pin_Pack_1; -- used in all Buggy MBs


   LIDAR_Slave_Addr           : constant Byte := 16#62#;
   LIDAR_Command_Control_Reg  : constant Byte := 16#00#;  -- write
   LIDAR_Status_Reg           : constant Byte := 16#01#;

   LIDAR_Distance_MSB_Addr    : constant Byte := 16#0F#;  -- read (in cm)
   LIDAR_Distance_LSB_Addr    : constant Byte := 16#10#;  -- read (in cm)
   -- or 2-byte read from 16#8F#
   LIDAR_Distance_Addr        : constant Byte := 16#8F#;  -- read (in cm)

   LIDAR_I2C : I2C_Port renames I2C_2;

   -- Advanced features and control (partial list)
   LIDAR_Acq_Mode_Control_Reg : constant Byte := 16#04#;  -- write
   LIDAR_Power_Control_Reg    : constant Byte := 16#65#;  -- write
   LIDAR_Dual_Sig_Range_Reg   : constant Byte := 16#4B#;  -- write
   -- Select Second Return
   --  bit 0: 1 -> sw to alt return,
   --         0 -> sel data associated with detection criteria
   -- Select Range Criteria
   --  bit 1: 1 -> selects return data based on distance
   --         0 -> selects strongest return, regardless of distance
   -- Select Max Range
   --  bit 2: 1 -> selects the longer distance
   --         0 -> selects the shorter distance

   Measure_Value  : constant Byte := 16#04#;  -- to start ranging

   ---------------------------------------------------------------------------
   -- subprograms
   ---------------------------------------------------------------------------

   procedure Enable_3_3;

   procedure Enable_Sleep;

   procedure Restart_I2C;

   procedure Setup_LIDAR;

   procedure Setup_Servo;  -- called by Setup_LIDAR

   procedure Center_Servo;

   procedure Look_Right;

   procedure Look_Left;

   procedure Look_Forward;

   function Read_Distance return HWord;


end LIDAR;
