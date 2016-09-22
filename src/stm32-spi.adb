------------------------------------------------------------------------------
-- spi.adb
--
-- Handles setting up the SPI ports and sending a single byte or half-word
-- at a time to the selected SPI port.
-- Will add multiple byte sends at some point.
--
-- Created by Jerry Petrey 10-14-14
-- Last Modification:      08-31-16
------------------------------------------------------------------------------
with STM32.GPIO;          use STM32.GPIO;
with STM32.Device;        use STM32.Device;
with Ada.Real_Time;       use Ada.Real_Time;
with Ada.Unchecked_Conversion;

package body STM32.SPI is

   package Device renames STM32.Device;

   function CR1_Record_To_HWord is
     new Ada.Unchecked_Conversion (Source => SPI_CR1_Setup_Record_Type,
                                   Target => HWord);

   function CR2_Record_To_HWord is
     new Ada.Unchecked_Conversion (Source => SPI_CR2_Setup_Record_Type,
                                   Target => HWord);


   ---------------------------------------------------------------------------
   -- Setup_SPI_Port
   ---------------------------------------------------------------------------
   procedure Setup_SPI_Port (SPI        : SPI_ID_Type;  -- number and pin pack
                             CR1_Values : SPI_CR1_Setup_Record_Type;
                             CR2_Values : SPI_CR2_Setup_Record_Type) is
      SPI_Conf : GPIO_Port_Configuration;
   begin
      case SPI is
         when SPI1_Pin_Pack_1 =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PA5;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PA6;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PA7;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_A);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_5_SPI1);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_5_SPI1);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_1);

               -- reset SPI1
               Device.Reset (Device.SPI_1);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_1.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_1.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_1.CRCPR := 7;

               -- enable SPI
               Device.SPI_1.CR1 := Device.SPI_1.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

         when SPI1_Pin_Pack_2 =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PB3;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PB4;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PB5;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_B);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_5_SPI1);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_5_SPI1);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_1);

               -- reset SPI1
               Device.Reset (Device.SPI_1);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_1.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_1.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_1.CRCPR := 7;

               -- enable SPI
               Device.SPI_1.CR1 := Device.SPI_1.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

         when SPI2_Pin_Pack_1 =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PB10;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PC2;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PC3;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_B);
               Enable_Clock (GPIO_C);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_5_SPI2);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_5_SPI2);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_2);

               -- reset SPI1
               Device.Reset (Device.SPI_2);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_2.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_2.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_2.CRCPR := 7;

               -- enable SPI
               Device.SPI_2.CR1 := Device.SPI_2.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

         when SPI2_Pin_Pack_2 =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PB13;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PB14;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PB15;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_B);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_5_SPI2);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_5_SPI2);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_2);

               -- reset SPI1
               Device.Reset (Device.SPI_2);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_2.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_2.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_2.CRCPR := 7;

               -- enable SPI
               Device.SPI_2.CR1 := Device.SPI_2.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

         when SPI2_Pin_Pack_2a =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PD3;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PB14;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PB15;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_D);
               Enable_Clock (GPIO_B);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_5_SPI2);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_5_SPI2);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_2);

               -- reset SPI1
               Device.Reset (Device.SPI_2);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_2.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_2.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_2.CRCPR := 7;

               -- enable SPI
               Device.SPI_2.CR1 := Device.SPI_2.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

         when SPI2_Pin_Pack_3 =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PI1;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PI2;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PI3;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_I);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_5_SPI2);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_5_SPI2);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_2);

               -- reset SPI1
               Device.Reset (Device.SPI_2);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_2.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_2.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_2.CRCPR := 7;

               -- enable SPI
               Device.SPI_2.CR1 := Device.SPI_2.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

         when SPI2_Pin_Pack_4 =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PI1;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PB14;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PB15;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_I);
               Enable_Clock (GPIO_B);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_5_SPI2);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_5_SPI2);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_2);

               -- reset SPI1
               Device.Reset (Device.SPI_2);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_2.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_2.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_2.CRCPR := 7;

               -- enable SPI
               Device.SPI_2.CR1 := Device.SPI_2.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

         when SPI3_Pin_Pack_1 =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PB3;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PB4;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PB5;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_B);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_6_SPI3);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_6_SPI3);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_3);

               -- reset SPI1
               Device.Reset (Device.SPI_3);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_3.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_3.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_3.CRCPR := 7;

               -- enable SPI
               Device.SPI_3.CR1 := Device.SPI_3.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

         when SPI3_Pin_Pack_2 =>
            declare
               SCK_Idle : Internal_Pin_Resistors;
               SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PC10;
               SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PC11;
               SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PC12;
               SPI_Pins : constant GPIO_Points := (SPI_MOSI, SPI_MISO);
               SPI_Pin  : constant GPIO_Point  := SPI_SCK;
            begin
               if CR1_Values.Mode = SPI_Mode_0 or
                  CR1_Values.Mode = SPI_Mode_1 then
                  SCK_Idle := Pull_Down;
               else
                  SCK_Idle := Pull_Up;
               end if;
               SPI_Conf := (Mode        => Mode_AF,
                            Output_Type => Push_Pull,
                            Speed       => Speed_Medium,
                            Resistors   => SCK_Idle);
               -- Enable clock for GPIO used
               Enable_Clock (GPIO_C);

               -- set up GPIO for SPI
               -- SCK
               Configure_IO (SPI_Pin, SPI_Conf);
               Configure_Alternate_Function (SPI_Pin, GPIO_AF_6_SPI3);

               -- MISO, MOSI
               Configure_IO (SPI_Pins, SPI_Conf);
               Configure_Alternate_Function (SPI_Pins, GPIO_AF_6_SPI3);

               -- enable clock for SPI1
               Device.Enable_Clock (Device.SPI_3);

               -- reset SPI1
               Device.Reset (Device.SPI_3);
               delay until Clock + Milliseconds (10);


               -- set CR registers with records passed in
               Device.SPI_3.CR1   := CR1_Record_To_HWord (CR1_Values);
               Device.SPI_3.CR2   := CR2_Record_To_HWord (CR2_Values);
               Device.SPI_3.CRCPR := 7;

               -- enable SPI
               Device.SPI_3.CR1 := Device.SPI_3.CR1 or SPI_EN;
               delay until Clock + Milliseconds (20);
            end;

      end case;

   end Setup_SPI_Port;


   ---------------------------------------------------------------------------
   -- SPI_Send byte
   ---------------------------------------------------------------------------
   procedure SPI_Send (SPI_Number : in SPI_Number_Type;
                       Data       : in Byte) is
   begin

      case SPI_Number is

         when SPI1 =>

            -- Fill output buffer with data
            Device.SPI_1.DR := HWord (Data);
            -- Wait for transmission to complete
            loop
               exit when (Device.SPI_1.SR and SPI_TXE) = SPI_TXE;
            end loop;
            -- Wait for SPI to be ready
            loop
               exit when ((Device.SPI_1.SR and SPI_BSY) = 0);
            end loop;

         when SPI2 =>

            -- Fill output buffer with data
            Device.SPI_2.DR := HWord (Data);
            -- Wait for transmission to complete
            loop
               exit when (Device.SPI_2.SR and SPI_TXE) = SPI_TXE;
            end loop;
            -- Wait for SPI to be ready
            loop
               exit when ((Device.SPI_2.SR and SPI_BSY) = 0);
            end loop;

         when SPI3 =>

            -- Fill output buffer with data
            Device.SPI_3.DR := HWord (Data);
            -- Wait for transmission to complete
            loop
               exit when (Device.SPI_3.SR and SPI_TXE) = SPI_TXE;
            end loop;
            -- Wait for SPI to be ready
            loop
               exit when ((Device.SPI_3.SR and SPI_BSY) = 0);
            end loop;

      end case;

   end SPI_Send;


   ---------------------------------------------------------------------------
   -- SPI_Send Hword
   ---------------------------------------------------------------------------
   procedure SPI_Send (SPI_Number : in     SPI_Number_Type;
                       Data       : in     Hword) is
   begin

      case SPI_Number is

         when SPI1 =>

            -- Fill output buffer with data
            Device.SPI_1.DR := Data;
            -- Wait for transmission to complete
            loop
               exit when (Device.SPI_1.SR and SPI_TXE) = SPI_TXE;
            end loop;
            -- Wait for SPI to be ready
            loop
               exit when ((Device.SPI_1.SR and SPI_BSY) = 0);
            end loop;

         when SPI2 =>

            -- Fill output buffer with data
            Device.SPI_2.DR := Data;
            -- Wait for transmission to complete
            loop
               exit when (Device.SPI_2.SR and SPI_TXE) = SPI_TXE;
            end loop;
            -- Wait for SPI to be ready
            loop
               exit when ((Device.SPI_2.SR and SPI_BSY) = 0);
            end loop;

         when SPI3 =>

            -- Fill output buffer with data
            Device.SPI_3.DR := Data;
            -- Wait for transmission to complete
            loop
               exit when (Device.SPI_3.SR and SPI_TXE) = SPI_TXE;
            end loop;
            -- Wait for SPI to be ready
            loop
               exit when ((Device.SPI_3.SR and SPI_BSY) = 0);
            end loop;

      end case;

   end SPI_Send;


   ---------------------------------------------------------------------------
   -- SPI_Send_Multi
   ---------------------------------------------------------------------------
   --procedure SPI_Send_Multi (Data  : in Byte_Array;
   --                          Count : in HWORD) is
   --begin
      -- TBD
   --   null;
   --end SPI_Send_Multi;


   ---------------------------------------------------------------------------
   -- SPI_Write_Multi
   ---------------------------------------------------------------------------
   --procedure SPI_Write_Multi (Data  : in Byte_Array;
   --                           Count : in HWORD) is
   --begin
      -- TBD
   --   null;
   --end SPI_Write_Multi;


   ---------------------------------------------------------------------------
   -- SPI_Read_Multi
   ---------------------------------------------------------------------------
   --procedure SPI_Read_Multi (Data  : out Byte_Array;
   --                          Count : in HWORD) is
   --begin
      -- TBD
   --   null;
   --end SPI_Read_Multi;

end STM32.SPI;
