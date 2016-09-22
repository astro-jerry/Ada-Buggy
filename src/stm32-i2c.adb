------------------------------------------------------------------------------
-- STM32F4.I2C
--
-- Handles setting up the I2C ports and provides routines to use them.
--
-- Created by Jerry Petrey 04-11-15
-- Last Modification:      08-31-16
--
-- NOTE: small delays before error exception return makes routines work
--       much more reliable
------------------------------------------------------------------------------
with STM32;                 use STM32;
with STM32.Device;          use STM32.Device;
with Ada.Real_Time;         use Ada.Real_Time;
with STM32.GPIO;            use STM32.GPIO;
with STM32F4_I2C_Registers; use STM32F4_I2C_Registers;
with Ada.Unchecked_Conversion;

package body STM32.I2C is

   pragma Warnings (Off, "*is not referenced");
   pragma Warnings (Off, "*is assigned but never read");

   -- for I2C registers
   START_I2C           : constant HWORD  := 16#0100#;  -- in CR1
   STOP_I2C            : constant HWORD  := 16#0200#;  -- in CR1
   PE                  : constant HWord  := 16#0001#;  -- in CR1
   ACK_Bit             : constant HWord  := 16#0400#;  -- in CR1
   SWRST               : constant HWord  := 16#8000#;  -- in CR1
   Start_Bit           : constant HWORD  := 16#0001#;  -- in SR1
   ADDR_Bit            : constant HWORD  := 16#0002#;  -- in SR1
   BTF_Bit             : constant HWORD  := 16#0004#;  -- in SR1
   TXE_Bit             : constant HWORD  := 16#0080#;  -- in SR1
   TRA_Bit             : constant HWORD  := 16#0004#;  -- in SR2

   TRISE_Value         : HWord range 0 .. 63;

   I2C_Timeout_Value   : constant      := 50000;
   I2C_Addr_LSB        : constant Byte := 16#01#;

   Timeout             : exception;
   Error_Condition     : exception;


   function CR1_Record_To_HWord is
     new Ada.Unchecked_Conversion (Source => I2C_CR1_Setup_Record_Type,
                                   Target => HWord);

   function CR2_Record_To_HWord is
     new Ada.Unchecked_Conversion (Source => I2C_CR2_Setup_Record_Type,
                                   Target => HWord);


   ---------------------------------------------------------------------------
   -- Check_Event
   ---------------------------------------------------------------------------
   function Check_Event (Port  : I2C_Port;
                         Event : Word) return Event_Status_Type is

      Last_Event : Word := 0;
      Flag1      : Word := 0;
      Flag2      : Word := 0;
      Status     : Event_Status_Type := Error;

   begin
      -- read the status registers
      Flag1  := Word (Port.SR1);
      Flag2  := Word (Port.SR2);
      Flag2  := Shift_Left (Flag2, 16);
      -- get last event from I2C status register
      Last_Event := (Flag1 or Flag2) and Flag_Mask;
      if ((Last_Event and Event) = Event) then
         Status := Success;  -- event was there
      else
         Status := Error;
      end if;
      return Status;
   end Check_Event;


   ---------------------------------------------------------------------------
   -- Setup_I2C_Port
   --
   -- NOTE: assumes clock is PCLK1 42 MHz
   -- TODO: make work with whatever clock is set in CR2
   ---------------------------------------------------------------------------
   procedure Setup_I2C_Port (Pin_Pack   : in Pins_Type; -- number and pin pack
                             CR1_Values : in I2C_CR1_Setup_Record_Type;
                             CR2_Values : in I2C_CR2_Setup_Record_Type) is

      Conf : constant GPIO_Port_Configuration :=
                          (Mode        => Mode_AF,
                           Output_Type => Open_Drain,
                           Speed       => Speed_100MHz,
                           Resistors   => Floating);
   begin
      TRISE_Value := CR2_Values.FREQ + 1;

      case Pin_Pack is
         when I2C1_Pin_Pack_1 =>
            -- Enable clocks for GPIO and setup GPIO pins
            Enable_Clock (GPIO_B);
            -- SCK
            Configure_IO (This   => PB6,
                          Config => Conf);
            Configure_Alternate_Function (This   => PB6,
                                          AF     => GPIO_AF_4_I2C1);
            -- SDA
            Configure_IO (This   => PB7,
                          Config => Conf);
            Configure_Alternate_Function (This   => PB7,
                                          AF     => GPIO_AF_4_I2C1);
            -- SMBA (not used normally)
            -- enable clock for I2C
            Enable_Clock (I2C_1);
            -- reset I2C
            Reset (I2C_1);
            delay until Clock + Milliseconds (50);

            -- set CR registers with records passed in
            I2C_1.CR1   := CR1_Record_To_HWord (CR1_Values);
            I2C_1.CR2   := CR2_Record_To_HWord (CR2_Values);
            -- set TRISE register
            I2C_1.TRISE := TRISE_Value;
            I2C_1.OAR1  := Own_Addr;
            -- set CCR in I2C.CCR register for 100000 HZ operation
            -- CCR * 1/42000000 = 5000 ns (1/2 of 100000 period)
            I2C_1.CCR   := 16#D2#;  -- 210
            -- enable I2C
            I2C_1.CR1   := I2C_1.CR1 or PE;

         when I2C1_Pin_Pack_2 =>
            -- Enable clocks for GPIO and setup GPIO pins
            Enable_Clock (GPIO_B);
            -- SCK
            Configure_IO (This   => PB8,
                          Config => Conf);
            Configure_Alternate_Function (This   => PB8,
                                          AF     => GPIO_AF_4_I2C1);
            -- SDA
            Configure_IO (This   => PB9,
                          Config => Conf);
            Configure_Alternate_Function (This   => PB9,
                                          AF     => GPIO_AF_4_I2C1);
            -- SMBA (not used normally)
            -- enable clock for I2C
            Enable_Clock (I2C_1);
            -- reset I2C
            Reset (I2C_1);
            delay until Clock + Milliseconds (50);

            -- set CR registers with records passed in
            I2C_1.CR1   := CR1_Record_To_HWord (CR1_Values);
            I2C_1.CR2   := CR2_Record_To_HWord (CR2_Values);
            -- set TRISE register
            I2C_1.TRISE := TRISE_Value;
            I2C_1.OAR1  := Own_Addr;
            -- set CCR in I2C.CCR register for 100000 HZ operation
            -- CCR * 1/42000000 = 5000 ns (1/2 of 100000 period)
            I2C_1.CCR   := 16#D2#;  -- 210
            -- enable I2C
            I2C_1.CR1   := I2C_1.CR1 or PE;

         when I2C1_Pin_Pack_3 =>
            -- Enable clocks for GPIO and setup GPIO pins
            Enable_Clock (GPIO_B);
            -- SCK
            Configure_IO (This   => PB6,
                          Config => Conf);
            Configure_Alternate_Function (This   => PB6,
                                          AF     => GPIO_AF_4_I2C1);
            -- SDA
            Configure_IO (This   => PB9,
                          Config => Conf);
            Configure_Alternate_Function (This   => PB9,
                                          AF     => GPIO_AF_4_I2C1);
            -- SMBA (not used normally)
            -- enable clock for I2C
            Enable_Clock (I2C_1);
            -- reset I2C
            Reset (I2C_1);
            delay until Clock + Milliseconds (50);

            -- set CR registers with records passed in
            I2C_1.CR1   := CR1_Record_To_HWord (CR1_Values);
            I2C_1.CR2   := CR2_Record_To_HWord (CR2_Values);
            -- set TRISE register
            I2C_1.TRISE := TRISE_Value;
            I2C_1.OAR1  := Own_Addr;
            -- set CCR in I2C.CCR register for 100000 HZ operation
            -- CCR * 1/42000000 = 5000 ns (1/2 of 100000 period)
            I2C_1.CCR   := 16#D2#;  -- 210
            -- enable I2C
            I2C_1.CR1   := I2C_1.CR1 or PE;

         when I2C2_Pin_Pack_1 =>
            -- Enable clocks for GPIO and setup GPIO pins
            Enable_Clock (GPIO_B);
            -- SCK
            Configure_IO (This   => PB10,
                          Config => Conf);
            Configure_Alternate_Function (This   => PB10,
                                          AF     => GPIO_AF_4_I2C2);
            -- SDA
            Configure_IO (This   => PB11,
                          Config => Conf);
            Configure_Alternate_Function (This   => PB11,
                                          AF     => GPIO_AF_4_I2C2);
            -- SMBA (not used normally)
            -- enable clock for I2C
            Enable_Clock (I2C_2);
            -- reset I2C
            Reset (I2C_2);
            delay until Clock + Milliseconds (50);

            -- set CR registers with records passed in
            I2C_2.CR1   := CR1_Record_To_HWord (CR1_Values);
            I2C_2.CR2   := CR2_Record_To_HWord (CR2_Values);
            -- set TRISE register
            I2C_2.TRISE := TRISE_Value;
            I2C_2.OAR1  := Own_Addr;
            -- set CCR in I2C.CCR register for 100000 HZ operation
            -- CCR * 1/42000000 = 5000 ns (1/2 of 100000 period)
            I2C_2.CCR   := 16#D2#;  -- 210
            -- enable I2C
            I2C_2.CR1   := I2C_2.CR1 or PE;

         when I2C2_Pin_Pack_2 =>
            -- Enable clocks for GPIO and setup GPIO pins
            Enable_Clock (GPIO_F);
            -- SCK
            Configure_IO (This   => PF1,
                          Config => Conf);
            Configure_Alternate_Function (This   => PF1,
                                          AF     => GPIO_AF_4_I2C2);
            -- SDA
            Configure_IO (This   => PF0,
                          Config => Conf);
            Configure_Alternate_Function (This   => PF0,
                                          AF     => GPIO_AF_4_I2C2);
            -- SMBA (not used normally)
            -- enable clock for I2C
            Enable_Clock (I2C_2);
            -- reset I2C
            Reset (I2C_2);
            delay until Clock + Milliseconds (50);

            -- set CR registers with records passed in
            I2C_2.CR1   := CR1_Record_To_HWord (CR1_Values);
            I2C_2.CR2   := CR2_Record_To_HWord (CR2_Values);
            -- set TRISE register
            I2C_2.TRISE := TRISE_Value;
            I2C_2.OAR1  := Own_Addr;
            -- set CCR in I2C.CCR register for 100000 HZ operation
            -- CCR * 1/42000000 = 5000 ns (1/2 of 100000 period)
            I2C_2.CCR   := 16#D2#;  -- 210
            -- enable I2C
            I2C_2.CR1   := I2C_2.CR1 or PE;

         when I2C2_Pin_Pack_3 =>
            -- Enable clocks for GPIO and setup GPIO pins
            Enable_Clock (GPIO_H);
            -- SCK
            Configure_IO (This   => PH4,
                          Config => Conf);
            Configure_Alternate_Function (This   => PH4,
                                          AF     => GPIO_AF_4_I2C2);
            -- SDA
            Configure_IO (This   => PH5,
                          Config => Conf);
            Configure_Alternate_Function (This   => PH5,
                                          AF     => GPIO_AF_4_I2C2);
            -- SMBA (not used normally)
            -- enable clock for I2C
            Enable_Clock (I2C_2);
            -- reset I2C
            Reset (I2C_2);
            delay until Clock + Milliseconds (50);

            -- set CR registers with records passed in
            I2C_2.CR1   := CR1_Record_To_HWord (CR1_Values);
            I2C_2.CR2   := CR2_Record_To_HWord (CR2_Values);
            -- set TRISE register
            I2C_2.TRISE := TRISE_Value;
            I2C_2.OAR1  := Own_Addr;
            -- set CCR in I2C.CCR register for 100000 HZ operation
            -- CCR * 1/42000000 = 5000 ns (1/2 of 100000 period)
            I2C_2.CCR   := 16#D2#;  -- 210
            -- enable I2C
            I2C_2.CR1   := I2C_2.CR1 or PE;

         when I2C3_Pin_Pack_1 =>
            -- Enable clocks for GPIO and setup GPIO pins
            Enable_Clock (GPIO_A);
            Enable_Clock (GPIO_C);
            -- SCK
            Configure_IO (This   => PA8,
                          Config => Conf);
            Configure_Alternate_Function (This   => PA8,
                                          AF     => GPIO_AF_4_I2C3);
            -- SDA
            Configure_IO (This   => PC9,
                          Config => Conf);
            Configure_Alternate_Function (This   => PC9,
                                          AF     => GPIO_AF_4_I2C3);
            -- SMBA (not used normally)
            -- enable clock for I2C
            Enable_Clock (I2C_3);
            -- reset I2C
            Reset (I2C_3);
            delay until Clock + Milliseconds (50);

            -- set CR registers with records passed in
            I2C_3.CR1   := CR1_Record_To_HWord (CR1_Values);
            I2C_3.CR2   := CR2_Record_To_HWord (CR2_Values);
            -- set TRISE register
            I2C_3.TRISE := TRISE_Value;
            I2C_3.OAR1  := Own_Addr;
            -- set CCR in I2C.CCR register for 100000 HZ operation
            -- CCR * 1/42000000 = 5000 ns (1/2 of 100000 period)
            I2C_3.CCR   := 16#D2#;  -- 210
            -- enable I2C
            I2C_3.CR1   := I2C_3.CR1 or PE;

         when I2C3_Pin_Pack_2 =>
            -- Enable clocks for GPIO and setup GPIO pins
            Enable_Clock (GPIO_H);
            -- SCK
            Configure_IO (This   => PH7,
                          Config => Conf);
            Configure_Alternate_Function (This   => PH7,
                                          AF     => GPIO_AF_4_I2C3);
            -- SDA
            Configure_IO (This   => PH8,
                          Config => Conf);
            Configure_Alternate_Function (This   => PH8,
                                          AF     => GPIO_AF_4_I2C3);
            -- SMBA (not used normally)
            -- enable clock for I2C
            Enable_Clock (I2C_3);
            -- reset I2C
            Reset (I2C_3);
            delay until Clock + Milliseconds (50);

            -- set CR registers with records passed in
            I2C_3.CR1   := CR1_Record_To_HWord (CR1_Values);
            I2C_3.CR2   := CR2_Record_To_HWord (CR2_Values);
            -- set TRISE register
            I2C_3.TRISE := TRISE_Value;
            I2C_3.OAR1  := Own_Addr;
            -- set CCR in I2C.CCR register for 100000 HZ operation
            -- CCR * 1/42000000 = 5000 ns (1/2 of 100000 period)
            I2C_3.CCR   := 16#D2#;  -- 210
            -- enable I2C
            I2C_3.CR1   := I2C_3.CR1 or PE;
      end case;
      delay until Clock + Milliseconds (10);
   end Setup_I2C_Port;


   ---------------------------------------------------------------------------
   -- I2C_Write_Data
   ---------------------------------------------------------------------------
   procedure I2C_Write_Data (I2C_Number : in  I2C_Number_Type;
                             Data       : in  Byte;
                             Error      : out Boolean) is

      I2C_Timeout : Natural := I2C_Timeout_Value;
   begin
      case I2C_Number is
         when I2C_Num1 =>
            while ((I2C_1.SR1 and TXE_Bit) /= TXE_Bit) and (I2C_Timeout /= 0)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  raise Timeout;
               end if;
            end loop;
            I2C_1.DR    := HWord (Data);
            I2C_Timeout := I2C_Timeout_Value;
            while (Check_Event (I2C_1, Master_Byte_Transmitted) /= Success)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  raise Timeout;
               end if;
            end loop;

         when I2C_Num2 =>
            while ((I2C_2.SR1 and TXE_Bit) /= TXE_Bit) and (I2C_Timeout /= 0)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  raise Timeout;
               end if;
            end loop;
            I2C_2.DR    := HWord (Data);
            I2C_Timeout := I2C_Timeout_Value;
            while (Check_Event (I2C_2, Master_Byte_Transmitted) /= Success)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  raise Timeout;
               end if;
            end loop;

         when I2C_Num3 =>
            while ((I2C_3.SR1 and TXE_Bit) /= TXE_Bit) and (I2C_Timeout /= 0)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  raise Timeout;
               end if;
            end loop;
            I2C_3.DR    := HWord (Data);
            I2C_Timeout := I2C_Timeout_Value;
            while (Check_Event (I2C_3, Master_Byte_Transmitted) /= Success)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  raise Timeout;
               end if;
            end loop;

      end case;
   exception
      when Timeout =>
         delay until Clock + Milliseconds (1);
         return;
   end I2C_Write_Data;


   --
   -- Read byte with ACK
   --
   procedure I2C_Read_ACK (I2C_Number : in  I2C_Number_Type;
                           Data       : out Byte;
                           Error      : out Boolean) is
      I2C_Timeout : Natural := I2C_Timeout_Value;
   begin
      Error := False;
      case I2C_Number is
         when I2C_Num1 =>
            -- enable ACK
            I2C_1.CR1 := I2C_1.CR1 or ACK_Bit;
            -- wait until not received
            while ((Check_Event (I2C_1, Master_Byte_Received) /= Success))
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  Data  := 0;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            Data := Byte (I2C_1.DR);

         when I2C_Num2 =>
            -- enable ACK
            I2C_2.CR1 := I2C_2.CR1 or ACK_Bit;
            -- wait until not received
            while ((Check_Event (I2C_2, Master_Byte_Received) /= Success))
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  Data  := 0;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            Data := Byte (I2C_2.DR);

         when I2C_Num3 =>
            -- enable ACK
            I2C_3.CR1 := I2C_3.CR1 or ACK_Bit;
            -- wait until not received
            while ((Check_Event (I2C_3, Master_Byte_Received) /= Success))
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  Data  := 0;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            Data := Byte (I2C_3.DR);
      end case;

   exception
      when Timeout =>
         delay until Clock + Milliseconds (1);
         return;
   end I2C_Read_ACK;


   --
   -- Read byte without ACK
   --
   procedure I2C_Read_NACK (I2C_Number : in  I2C_Number_Type;
                            Data       : out Byte;
                            Error      : out Boolean) is
      I2C_Timeout : Natural := I2C_Timeout_Value;
   begin
      Error := False;
      case I2C_Number is
         when I2C_Num1 =>
            -- disable ACK
            I2C_1.CR1 := I2C_1.CR1 and (not ACK_Bit);
            -- generate stop
            I2C_1.CR1 := I2C_1.CR1 or STOP_I2C;
            -- wait until received
            while ((Check_Event (I2C_1, Master_Byte_Received) /= Success))
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  Data  := 0;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            Data := Byte (I2C_1.DR);

         when I2C_Num2 =>
            -- disable ACK
            I2C_2.CR1 := I2C_2.CR1 and (not ACK_Bit);
            -- generate stop
            I2C_2.CR1 := I2C_2.CR1 or STOP_I2C;
            -- wait until received
            while ((Check_Event (I2C_2, Master_Byte_Received) /= Success))
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  Data  := 0;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            Data := Byte (I2C_2.DR);

         when I2C_Num3 =>
            -- disable ACK
            I2C_3.CR1 := I2C_3.CR1 and (not ACK_Bit);
            -- generate stop
            I2C_3.CR1 := I2C_3.CR1 or STOP_I2C;
            -- wait until received
            while ((Check_Event (I2C_3, Master_Byte_Received) /= Success))
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error := True;
                  Data  := 0;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            Data := Byte (I2C_3.DR);
      end case;

   exception
      when Timeout =>
         delay until Clock + Milliseconds (1);
         return;
   end I2C_Read_NACK;


   --
   -- generate I2C start condition
   --
   procedure I2C_Start (I2C_Number : in  I2C_Number_Type;
                        Slave_Addr : in  Byte;
                        Direction  : in  Direction_Type;
                        Use_Ack    : in  Boolean;
                        Error      : out Boolean) is

      I2C_Timeout : Natural := I2C_Timeout_Value;
      SR2_Value   : HWord   := 0;
      Addr        : HWord   := 0;

   begin
      Error := False;
      case I2C_Number is
         when I2C_Num1 =>
            -- set Start bit
            I2C_1.CR1 := I2C_1.CR1 or Start_I2C;
            -- wait till I2C is busy
            I2C_Timeout := I2C_Timeout_Value;
            while (Check_Event (I2C_1, Master_Mode_Select) /= Success)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error       := True;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            -- enable ack if selected
            if Use_Ack then
               I2C_1.CR1 := I2C_1.CR1 or ACK_Bit;
            end if;
            -- send address with write/read bit
            if Direction = Transmit then
               Addr := HWord (Shift_Left (Slave_Addr, 1)
                              and (not I2C_Addr_LSB));
               I2C_1.DR    := Addr;
               I2C_Timeout := I2C_Timeout_Value;
               while (Check_Event (I2C_1, Master_Tx_Mode_Selected) /= Success)
               loop
                  I2C_Timeout := I2C_Timeout - 1;
                  if I2C_Timeout = 0 then
                     Error       := True;
                     I2C_Timeout := I2C_Timeout_Value;
                     raise Timeout;
                  end if;
               end loop;
            else  -- Receive
               Addr := HWord (Shift_Left (Slave_Addr, 1) or I2C_Addr_LSB);
               I2C_1.DR    := Addr;
               I2C_Timeout := I2C_Timeout_Value;
               while (Check_Event (I2C_1, Master_Rx_Mode_Selected) /= Success)
               loop
                  I2C_Timeout := I2C_Timeout - 1;
                  if I2C_Timeout = 0 then
                     Error       := True;
                     I2C_Timeout := I2C_Timeout_Value;
                     raise Timeout;
                  end if;
               end loop;
            end if;
            -- read SR2 to clear ADDR flag
            SR2_Value := I2C_1.SR2;

         when I2C_Num2 =>
            -- set Start bit
            I2C_2.CR1 := I2C_2.CR1 or Start_I2C;
            -- wait till I2C is busy
            I2C_Timeout := I2C_Timeout_Value;
            while (Check_Event (I2C_2, Master_Mode_Select) /= Success)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error       := True;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            -- enable ack if selected
            if Use_Ack then
               I2C_2.CR1 := I2C_2.CR1 or ACK_Bit;
            end if;
            -- send address with write/read bit
            if Direction = Transmit then
               Addr := HWord (Shift_Left (Slave_Addr, 1)
                              and (not I2C_Addr_LSB));
               I2C_2.DR    := Addr;
               I2C_Timeout := I2C_Timeout_Value;
               while (Check_Event (I2C_2, Master_Tx_Mode_Selected) /= Success)
               loop
                  I2C_Timeout := I2C_Timeout - 1;
                  if I2C_Timeout = 0 then
                     Error       := True;
                     I2C_Timeout := I2C_Timeout_Value;
                     raise Timeout;
                  end if;
               end loop;
            else  -- Receive
               Addr := HWord (Shift_Left (Slave_Addr, 1) or I2C_Addr_LSB);
               I2C_2.DR    := Addr;
               I2C_Timeout := I2C_Timeout_Value;
               while (Check_Event (I2C_2, Master_Rx_Mode_Selected) /= Success)
               loop
                  I2C_Timeout := I2C_Timeout - 1;
                  if I2C_Timeout = 0 then
                     Error       := True;
                     I2C_Timeout := I2C_Timeout_Value;
                     raise Timeout;
                  end if;
               end loop;
            end if;
            -- read SR2 to clear ADDR flag
            SR2_Value := I2C_2.SR2;

         when I2C_Num3 =>
            -- set Start bit
            I2C_3.CR1 := I2C_3.CR1 or Start_I2C;
            -- wait till I2C is busy
            I2C_Timeout := I2C_Timeout_Value;
            while (Check_Event (I2C_3, Master_Mode_Select) /= Success)
            loop
               I2C_Timeout := I2C_Timeout - 1;
               if I2C_Timeout = 0 then
                  Error       := True;
                  I2C_Timeout := I2C_Timeout_Value;
                  raise Timeout;
               end if;
            end loop;
            -- enable ack if selected
            if Use_Ack then
               I2C_3.CR1 := I2C_3.CR1 or ACK_Bit;
            end if;
            -- send address with write/read bit
            if Direction = Transmit then
               Addr := HWord (Shift_Left (Slave_Addr, 1)
                              and (not I2C_Addr_LSB));
               I2C_3.DR    := Addr;
               I2C_Timeout := I2C_Timeout_Value;
               while (Check_Event (I2C_3, Master_Tx_Mode_Selected) /= Success)
               loop
                  I2C_Timeout := I2C_Timeout - 1;
                  if I2C_Timeout = 0 then
                     Error       := True;
                     I2C_Timeout := I2C_Timeout_Value;
                     raise Timeout;
                  end if;
               end loop;
            else  -- Receive
               Addr := HWord (Shift_Left (Slave_Addr, 1) or I2C_Addr_LSB);
               I2C_3.DR    := Addr;
               I2C_Timeout := I2C_Timeout_Value;
               while (Check_Event (I2C_3, Master_Rx_Mode_Selected) /= Success)
               loop
                  I2C_Timeout := I2C_Timeout - 1;
                  if I2C_Timeout = 0 then
                     Error       := True;
                     I2C_Timeout := I2C_Timeout_Value;
                     raise Timeout;
                  end if;
               end loop;
            end if;
            -- read SR2 to clear ADDR flag
            SR2_Value := I2C_3.SR2;
      end case;

   exception when Timeout =>
         Error := True;
         delay until Clock + Milliseconds (1);
      return;

   end I2C_Start;


   procedure Clear_I2C (I2C_Number : in I2C_Number_Type) is
      CR1_Save   : HWord;
      CR2_Save   : HWord;
      TRISE_Save : HWord;
      OAR1_Save  : HWord;
      CCR_Save   : HWord;
   begin
      case I2C_Number is
         when I2C_Num1 =>
            -- generate stop
            I2C_1.CR1 := I2C_1.CR1 or STOP_I2C;
            delay until Clock + Milliseconds (10);
            -- disable and re-enable I2C
            I2C_1.CR1   := I2C_1.CR1 and (not PE);
            delay until Clock + Milliseconds (10);
            -- save values
            CR1_Save   := I2C_1.CR1;
            CR2_Save   := I2C_1.CR2;
            TRISE_Save := I2C_1.TRISE;
            OAR1_Save  := I2C_1.OAR1;
            CCR_Save   := I2C_1.CCR;
            -- reset I2C
            I2C_1.CR1 := I2C_1.CR1 or SWRST;
            delay until Clock + Milliseconds (10);
            I2C_1.CR1 := I2C_1.CR1 and (not SWRST);
            delay until Clock + Milliseconds (10);
            -- restore reg values
            I2C_1.CR1   := CR1_Save;
            I2C_1.CR2   := CR2_Save;
            I2C_1.TRISE := TRISE_Save;
            I2C_1.OAR1  := OAR1_Save;
            I2C_1.CCR   := CCR_Save;
            -- re-enable I2C
            I2C_1.CR1   := I2C_1.CR1 or PE;
            delay until Clock + Milliseconds (5);
         when I2C_Num2 =>
            -- generate stop
            I2C_2.CR1 := I2C_2.CR1 or STOP_I2C;
            delay until Clock + Milliseconds (10);
            -- disable and re-enable I2C
            I2C_2.CR1   := I2C_2.CR1 and (not PE);
            delay until Clock + Milliseconds (10);
            -- save values
            CR1_Save   := I2C_2.CR1;
            CR2_Save   := I2C_2.CR2;
            TRISE_Save := I2C_2.TRISE;
            OAR1_Save  := I2C_2.OAR1;
            CCR_Save   := I2C_2.CCR;
            -- reset I2C
            I2C_2.CR1 := I2C_2.CR1 or SWRST;
            delay until Clock + Milliseconds (10);
            I2C_2.CR1 := I2C_2.CR1 and (not SWRST);
            delay until Clock + Milliseconds (10);
            -- restore reg values
            I2C_2.CR1   := CR1_Save;
            I2C_2.CR2   := CR2_Save;
            I2C_2.TRISE := TRISE_Save;
            I2C_2.OAR1  := OAR1_Save;
            I2C_2.CCR   := CCR_Save;
            -- re-enable I2C
            I2C_2.CR1   := I2C_2.CR1 or PE;
            delay until Clock + Milliseconds (5);
         when I2C_Num3 =>
            -- generate stop
            I2C_3.CR1 := I2C_3.CR1 or STOP_I2C;
            delay until Clock + Milliseconds (10);
            -- disable and re-enable I2C
            I2C_3.CR1   := I2C_3.CR1 and (not PE);
            delay until Clock + Milliseconds (10);
            -- save values
            CR1_Save   := I2C_3.CR1;
            CR2_Save   := I2C_3.CR2;
            TRISE_Save := I2C_3.TRISE;
            OAR1_Save  := I2C_3.OAR1;
            CCR_Save   := I2C_3.CCR;
            -- reset I2C
            I2C_3.CR1 := I2C_3.CR1 or SWRST;
            delay until Clock + Milliseconds (10);
            I2C_3.CR1 := I2C_3.CR1 and (not SWRST);
            delay until Clock + Milliseconds (10);
            -- restore reg values
            I2C_3.CR1   := CR1_Save;
            I2C_3.CR2   := CR2_Save;
            I2C_3.TRISE := TRISE_Save;
            I2C_3.OAR1  := OAR1_Save;
            I2C_3.CCR   := CCR_Save;
            -- re-enable I2C
            I2C_3.CR1   := I2C_3.CR1 or PE;
            delay until Clock + Milliseconds (5);
      end case;
   end Clear_I2C;


   --
   -- generate I2C stop condition
   --
   procedure I2C_Stop (I2C_Number : in  I2C_Number_Type;
                       Error      : out Boolean) is
      I2C_Timeout : Natural := I2C_Timeout_Value;
   begin
      Error := False;
      case I2C_Number is
         when I2C_Num1 =>
            -- wait until transmitter not empty
--              while ((I2C_1.SR1 and TXE_Bit) /= TXE_Bit) or
--                    ((I2C_1.SR1 and BTF_Bit) /= BTF_Bit)
--              loop
--                 I2C_Timeout := I2C_Timeout - 1;
--                 if I2C_Timeout = 0 then
--                    Error := True;
--                    I2C_Timeout := I2C_Timeout_Value;
--                    raise Timeout;
--                 end if;
--              end loop;
            -- generate stop
            I2C_1.CR1 := I2C_1.CR1 or STOP_I2C;

         when I2C_Num2 =>
            -- wait until transmitter not empty
--              while ((I2C_2.SR1 and TXE_Bit) /= TXE_Bit) or
--                    ((I2C_2.SR1 and BTF_Bit) /= BTF_Bit)
--              loop
--                 I2C_Timeout := I2C_Timeout - 1;
--                 if I2C_Timeout = 0 then
--                    Error := True;
--                    I2C_Timeout := I2C_Timeout_Value;
--                    raise Timeout;
--                 end if;
--              end loop;
            -- generate stop
            I2C_2.CR1 := I2C_2.CR1 or STOP_I2C;

         when I2C_Num3 =>
            -- wait until transmitter not empty
--              while ((I2C_3.SR1 and TXE_Bit) /= TXE_Bit) or
--                    ((I2C_3.SR1 and BTF_Bit) /= BTF_Bit)
--              loop
--                 I2C_Timeout := I2C_Timeout - 1;
--                 if I2C_Timeout = 0 then
--                    Error := True;
--                    I2C_Timeout := I2C_Timeout_Value;
--                    raise Timeout;
--                 end if;
--              end loop;
            -- generate stop
            I2C_3.CR1 := I2C_3.CR1 or STOP_I2C;
      end case;

   exception when Timeout =>
      Error := True;
      delay until Clock + Milliseconds (1);
      return;

   end I2C_Stop;


   --
   -- determine if a device is connected
   --
   function Is_Device_Connected (I2C_Number : in I2C_Number_Type;
                                 Slave_Addr : in Byte) return Boolean is
      An_Error1  : Boolean := False;
      An_Error2  : Boolean := False;
   begin
      -- try to start
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Transmit,
                 Use_Ack    => True,
                 Error      => An_Error1);

      I2C_Stop (I2C_Number => I2C_Number,
                Error      => An_Error2);

      if An_Error1 or An_Error2 then
         return False;
      else
         return True;
      end if;
   end Is_Device_Connected;


   --
   -- Read single byte from slave
   --
   procedure Read_I2C_Byte (I2C_Number : in  I2C_Number_Type;
                            Slave_Addr : in  Byte;
                            Register   : in  Byte;
                            Data       : out Byte;
                            Error      : out Boolean) is
      An_Error : Boolean := False;
   begin
      Error := False;
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Transmit,
                 Use_Ack    => True,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Write_Data (I2C_Number => I2C_Number,
                      Data       => Register,
                      Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Stop (I2C_Number => I2C_Number,
                Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Receive,
                 Use_Ack    => True,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Read_NACK (I2C_Number => I2C_Number,
                     Data       => Data,
                     Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

   exception
      when Error_Condition =>
         Error := True;
         delay until Clock + Milliseconds (1);
         return;
   end Read_I2C_Byte;


   --
   -- Read multi bytes from slave
   --
   procedure Read_I2C_Multi (I2C_Number : in  I2C_Number_Type;
                             Slave_Addr : in  Byte;
                             Register   : in  Byte;
                             Count      : in  Positive;
                             Data_Array : out Array_Of_Bytes;
                             Error      : out Boolean) is
      An_Error   : Boolean := False;
      The_Data   : Array_Of_Bytes (1 .. Count) := (others => 0);
   begin
      Error := False;
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Transmit,
                 Use_Ack    => True,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Write_Data (I2C_Number => I2C_Number,
                      Data       => Register,
                      Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Stop (I2C_Number => I2C_Number,
                Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Receive,
                 Use_Ack    => True,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      for I in 1 .. Count loop
         if I = Count then -- last byte
            I2C_Read_NACK (I2C_Number => I2C_Number,
                           Data       => The_Data (I),
                           Error      => An_Error);
         else
            I2C_Read_ACK (I2C_Number => I2C_Number,
                          Data       => The_Data (I),
                          Error      => An_Error);
         end if;
         if An_Error then
            raise Error_Condition;
         end if;
      end loop;
      Data_Array := The_Data;

   exception
      when Error_Condition =>
         Error := True;
         delay until Clock + Milliseconds (1);
         return;
   end Read_I2C_Multi;


   --
   -- Read byte from slave without specifying register address
   --
   procedure Get_I2C_Byte_No_Reg (I2C_Number : in  I2C_Number_Type;
                                  Slave_Addr : in  Byte;
                                  Data       : out Byte;
                                  Error      : out Boolean) is
      An_Error : Boolean := False;
   begin
      Error := False;
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Receive,
                 Use_Ack    => True,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      -- also a stop condition happens
      I2C_Read_NACK (I2C_Number => I2C_Number,
                     Data       => Data,
                     Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

   exception
      when Error_Condition =>
         Error := True;
         delay until Clock + Milliseconds (1);
         return;
   end Get_I2C_Byte_No_Reg;


   --
   -- Read multi bytes from slave without setting register from where
   -- to start read
   --
   procedure Read_I2C_Multi_No_Reg
                            (I2C_Number : in  I2C_Number_Type;
                             Slave_Addr : in  Byte;
                             Count      : in  Positive;
                             Data_Array : out Array_Of_Bytes;
                             Error      : out Boolean) is
      An_Error : Boolean := False;
   begin
      Error := False;
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Receive,
                 Use_Ack    => True,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      for I in 1 .. Count loop
         if I = Count then -- last byte
            I2C_Read_NACK (I2C_Number => I2C_Number,
                           Data       => Data_Array (I),
                           Error      => An_Error);
         else
            I2C_Read_ACK (I2C_Number => I2C_Number,
                          Data       => Data_Array (I),
                          Error      => An_Error);
         end if;
         if An_Error then
            raise Error_Condition;
         end if;
      end loop;

   exception
      when Error_Condition =>
         Error := True;
         delay until Clock + Milliseconds (1);
         return;
   end Read_I2C_Multi_No_Reg;


   --
   -- Write single byte to slave
   --
   procedure Write_I2C_Byte (I2C_Number : in  I2C_Number_Type;
                             Slave_Addr : in  Byte;
                             Register   : in  Byte;
                             Data       : in  Byte;
                             Error      : out Boolean) is
      An_Error : Boolean := False;
   begin
      Error := False;
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Transmit,
                 Use_Ack    => True,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Write_Data (I2C_Number => I2C_Number,
                      Data       => Register,
                      Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Write_Data (I2C_Number => I2C_Number,
                      Data       => Data,
                      Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Stop (I2C_Number => I2C_Number,
                Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

   exception
      when Error_Condition =>
         Error := True;
         delay until Clock + Milliseconds (1);
         return;
   end Write_I2C_Byte;


   --
   -- Write multi bytes to slave
   --
   procedure Write_I2C_Byte_Multi
                              (I2C_Number : in  I2C_Number_Type;
                               Slave_Addr : in  Byte;
                               Register   : in  Byte;
                               Count      : in  Positive;
                               Data_Array : in  Array_Of_Bytes;
                               Error      : out Boolean) is
      An_Error : Boolean := False;
   begin
      Error := False;
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Transmit,
                 Use_Ack    => False,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Write_Data (I2C_Number => I2C_Number,
                      Data       => Register,
                      Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      for I in 1 .. Count loop
         I2C_Write_Data (I2C_Number => I2C_Number,
                         Data       => Data_Array (I),
                         Error      => An_Error);
         if An_Error then
            raise Error_Condition;
         end if;
      end loop;

      I2C_Stop (I2C_Number => I2C_Number,
                Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

   exception
      when Error_Condition =>
         Error := True;
         delay until Clock + Milliseconds (1);
         return;
   end Write_I2C_Byte_Multi;


   --
   -- Write byte to slave without specifying register address
   --
   procedure Write_I2C_Byte_No_Reg (I2C_Number : in  I2C_Number_Type;
                                    Slave_Addr : in  Byte;
                                    Data       : in  Byte;
                                    Error      : out Boolean) is
      An_Error : Boolean := False;
   begin
      Error := False;
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Transmit,
                 Use_Ack    => False,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Write_Data (I2C_Number => I2C_Number,
                      Data       => Data,
                      Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      I2C_Stop (I2C_Number => I2C_Number,
                Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

   exception
      when Error_Condition =>
         Error := True;
         delay until Clock + Milliseconds (1);
         return;
   end Write_I2C_Byte_No_Reg;


   --
   -- Write multi bytes to slave without setting register from where
   -- to start write
   --
   procedure Write_I2C_Byte_Multi_No_Reg
                              (I2C_Number : in  I2C_Number_Type;
                               Slave_Addr : in  Byte;
                               Count      : in  Positive;
                               Data_Array : in  Array_Of_Bytes;
                               Error      : out Boolean) is
      An_Error : Boolean := False;
   begin
      Error := False;
      I2C_Start (I2C_Number => I2C_Number,
                 Slave_Addr => Slave_Addr,
                 Direction  => Transmit,
                 Use_Ack    => False,
                 Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

      for I in 1 .. Count loop
         I2C_Write_Data (I2C_Number => I2C_Number,
                         Data       => Data_Array (I),
                         Error      => An_Error);
         if An_Error then
            raise Error_Condition;
         end if;
      end loop;

      I2C_Stop (I2C_Number => I2C_Number,
                Error      => An_Error);
      if An_Error then
         raise Error_Condition;
      end if;

   exception
      when Error_Condition =>
         Error := True;
         delay until Clock + Milliseconds (1);
         return;
   end Write_I2C_Byte_Multi_No_Reg;

end STM32.I2C;
