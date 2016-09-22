------------------------------------------------------------------------------
-- STM32F4.I2C
--
-- Created by Jerry Petrey 04-09-15
-- Last Modification:      08-31-16
--
-- Allows using any of the I2Cs implemented on STM32F4 chips.
--
-- NOTE: See usage information below.
------------------------------------------------------------------------------

package STM32.I2C is

   ---------------------------------------------------------------------------
   -- ************************************************************************
   -- To select I2C, set the values you need in records like these below,
   -- and then call Setup_I2C_Port using these record variables as parameters
   -- for the CR register values; also supply I2C_Type (number and pin pack)
   -- parameter.
   --*************************************************************************
   ---------------------------------------------------------------------------
   --I2C_CR1_Setup_Record : I2C_CR1_Setup_Record_Type :=
   --  (PE            => Disabled,
   --   SMBUS_Mode    => I2C_Mode,
   --   Resv1         => 0,
   --   SMBTYPE       => Device,
   --   ENARP         => Disabled,
   --   ENPEC         => Disabled,
   --   ENGC          => Disabled,
   --   NOSTRETCH     => Stretch_On,
   --   START         => Start_Disabled,
   --   STOP          => Stop_Disabled,
   --   ACK           => Ack_Disabled,
   --   POS           => Current_Byte,
   --   PEC           => Disabled,
   --   ALERT         => SMBA_High,
   --   Resv2         => 0,
   --   SWRST         => Not_Reset);
   --
   --I2C_CR2_Setup_Record : I2C_CR2_Setup_Record_Type :=
   --  (FREQ      => 42,  -- PCLK1
   --   Resv1     => 0,
   --   ITERREN   => Disabled,
   --   ITEVTEN   => Disabled,
   --   ITBUFEN   => Disabled,
   --   DMAEN     => Disabled,
   --   LAST_DMA  => Not_Last,
   --   Resv2     => 0);

   -- Pinouts used by the I2Cs:
   --       |PINS PACK 1    |PINS PACK 2    |PINS PACK 3    |PINS PACK 4*
   -- I2CX  |SCL  SDA  SMBA |SCL  SDA  SMBA |SCL  SDA  SMBA
   ---------|---------------|---------------|---------------|-----------
   -- I2C1  |PB6  PB7  PB5  |PB8  PB9  ---  |PB6  PB9       |
   -- I2C2  |PB10 PB11 PB12 |PF1  PF0  PF2  |PH4  PH5  PH6  |
   -- I2C3  |PA8  PC9  PA9  |PH7  PH8  PH9  |               |
   -- *may be on some future chips (now only F7 chips):
   --*I2C4  |PD12 PD13      |PF1  PF0       |PF14 PF15      |PH11 PH12


   type Pins_Type is (I2C1_Pin_Pack_1, I2C1_Pin_Pack_2, I2C1_Pin_Pack_3,
                      I2C2_Pin_Pack_1, I2C2_Pin_Pack_2, I2C2_Pin_Pack_3,
                      I2C3_Pin_Pack_1, I2C3_Pin_Pack_2);
   -- for future chips
   --I2C4_Pin_Pack_1, I2C4_Pin_Pack_2, I2C4_Pin_Pack_3, I2C4_Pin_Pack_4);

   -- use to identify I2C in send data routines
   type I2C_Number_Type is (I2C_Num1, I2C_Num2, I2C_Num3);

   type Addr_Mode_Type is (Seven_Bit, Ten_Bit); -- only 7-bit currently used

   type Direction_Type is (Transmit, Receive);

   -- for OAR1 reg (bit 14 needs to always be 1; bit 11 set for enable ACK)
   Own_Addr  : HWord  := 16#4400#;  -- OAR1 ADD[7..1] = 0

   type Event_Status_Type is (Success, Error);

   ---------------------------------------------------------------------------
   --  I2C Events
   ---------------------------------------------------------------------------
   -- EV1
   Slave_Rx_Addr_Matched    : Word := 16#0002_0002#; -- Busy & Addr
   Slave_Tx_Addr_Matched    : Word := 16#0006_0082#; -- TRA, Busy, TXE & Addr
   -- dual addr and general call not used
   Slave_Rx_2ndAddr_Matched : Word := 16#0082_0000#; -- DUALF, Busy
   Slave_Tx_2ndAddr_Matched : Word := 16#0086_0080#; -- DUALF, TRA, Busy, TXE
   Slave_GC_Addr_Matched    : Word := 16#0012_0000#; -- GENCALL, Busy
   -- EV2
   Slave_Byte_Received      : Word := 16#0002_0040#; -- Busy & RXNE
   -- EV3
   Slave_Byte_Transmitted   : Word := 16#0006_0084#; -- TRA, Busy, TXE, BTF
   Slave_Byte_Transmitting  : Word := 16#0006_0080#; -- TRA, Busy, TXE
   -- EV3_2
   Slave_Ack_Failure        : Word := 16#0000_0400#; -- AF


   -- EV5
   Master_Mode_Select        : Word := 16#0003_0001#; -- Busy, MSL, SB
   -- EV6
   Master_Tx_Mode_Selected   : Word := 16#0007_0082#; -- Busy,MSL,ADDR,TXE,TRA
   Master_Rx_Mode_Selected   : Word := 16#0003_0002#; -- Busy, MSL, ADDR
   -- EV7
   Master_Byte_Received      : Word := 16#0003_0040#; -- Busy, MSL, RXNE
   -- EV8
   Master_Byte_Transmitting  : Word := 16#0007_0080#; -- TXA, Busy, MSL, TXE
   -- EV8_2
   --Master_Byte_Transmitted : Word := 16#0007_0084#; -- TXA,Busy,MSL,TXE,BTF
   Master_Byte_Transmitted   : Word := 16#0000_0084#; -- TXE,BTF works better!
   -- EV9
   Master_Mode_Address10     : Word := 16#0003_0008#; -- Busy, MSL, ADD10


   Flag_Mask                 : Word  := 16#00FF_FFFF#;
   CR1_Clear_Mask            : HWord := 16#FBF5#;


   ---------------------------------------------------------------------------
   -- I2C setup data
   ---------------------------------------------------------------------------

   -- CR1 register bits
   -- bit 15
   type I2C_SWRST is (Not_Reset, Reset);
   for I2C_SWRST use (Not_Reset => 0,
                      Reset     => 1);
   -- bit 13
   type I2C_ALERT is (SMBA_High, SMBA_Low);
   for I2C_ALERT use (SMBA_High => 0,
                      SMBA_Low  => 1);
   -- bit 12
   type I2C_PEC is (Disabled, Enabled);
   for I2C_PEC use (Disabled => 0,
                    Enabled  => 1);
   -- bit 11
   type I2C_POS is (Current_Byte, Next_Byte);
   for I2C_POS use (Current_Byte => 0,
                    Next_Byte    => 1);
   -- bit 10
   type I2C_ACK is (Ack_Disabled, Ack_Enabled);
   for I2C_ACK use (Ack_Disabled  => 0,
                    Ack_Enabled   => 1);
   -- bit 9
   type I2C_STOP_Bit is (Stop_Disabled, Stop_Enabled);
   for I2C_STOP_Bit use (Stop_Disabled => 0,
                         Stop_Enabled  => 1);
   -- bit 8
   type I2C_START_Bit is (Start_Disabled, Start_Enabled);
   for I2C_START_Bit use (Start_Disabled => 0,
                          Start_Enabled  => 1);
   -- bit 7
   type I2C_NOSTRETCH is (Stretch_On, Strech_Off);
   for I2C_NOSTRETCH use (Stretch_On  => 0,
                          Strech_Off  => 1);
   -- bit 6
   type I2C_ENGC is (Disabled, Enabled);
   for I2C_ENGC use (Disabled => 0,
                     Enabled  => 1);
   -- bit 5
   type I2C_ENPEC is (Disabled, Enabled);
   for I2C_ENPEC use (Disabled => 0,
                      Enabled  => 1);
   -- bit 4
   type I2C_ENARP is (Disabled, Enabled);
   for I2C_ENARP use (Disabled   => 0,
                      Enabled    => 1);
   -- bit 3
   type I2C_SMBTYPE is (Device, Host);
   for I2C_SMBTYPE use (Device  => 0,
                        Host    => 1);
   -- bit 1
   type I2C_SMBUS_Mode is (I2C_Mode,
                           SMBUS_Mode);
   for I2C_SMBUS_Mode use (I2C_Mode   => 0,
                           SMBUS_Mode => 1);
   -- bit 0
   type I2C_PE is (Disabled, Enabled);
   for I2C_PE use (Disabled   => 0,
                   Enabled    => 1);

   ---------------------------------------------------------------------------
   -- CR2 register bits
   -- bit 12
   type I2C_LAST_DMA is (Not_Last, Last);
   for I2C_LAST_DMA use (Not_Last  => 0,
                         Last      => 1);
   -- bit 11
   type I2C_DMAEN is (Disabled, Enabled);
   for I2C_DMAEN use (Disabled => 0,
                      Enabled  => 1);
   -- bit 10
   type I2C_ITBUFEN is (Disabled, Enabled);
   for I2C_ITBUFEN use (Disabled => 0,
                        Enabled  => 1);
   -- bit 9
   type I2C_ITEVTEN is (Disabled, Enabled);
   for I2C_ITEVTEN use (Disabled => 0,
                        Enabled  => 1);
   -- bit 8
   type I2C_ITERREN is (Disabled, Enabled);
   for I2C_ITERREN use (Disabled => 0,
                        Enabled  => 1);
   -- bits 5:0
   subtype I2C_FREQ is HWORD range 2 .. 42;
   -- FREQ must be configured with APB clk freq
   -- for 42 Mhz, TRISE should be set to 43



   type I2C_CR1_Setup_Record_Type is
   record
      PE            : I2C_PE          := Disabled;
      SMBUS_Mode    : I2C_SMBUS_Mode  := I2C_Mode;
      Resv1         : Bit             := 0;
      SMBTYPE       : I2C_SMBTYPE     := Device;
      ENARP         : I2C_ENARP       := Disabled;
      ENPEC         : I2C_ENPEC       := Disabled;
      ENGC          : I2C_ENGC        := Disabled;
      NOSTRETCH     : I2C_NOSTRETCH   := Stretch_On;
      START         : I2C_START_Bit   := Start_Disabled;
      STOP          : I2C_STOP_Bit    := Stop_Disabled;
      ACK           : I2C_ACK         := Ack_Disabled;
      POS           : I2C_POS         := Current_Byte;
      PEC           : I2C_PEC         := Disabled;
      ALERT         : I2C_ALERT       := SMBA_High;
      Resv2         : Bit             := 0;
      SWRST         : I2C_SWRST       := Not_Reset;
   end record;

   for I2C_CR1_Setup_Record_Type use
   record
      PE            at 0 range  0 ..  0;
      SMBUS_Mode    at 0 range  1 ..  1;
      Resv1         at 0 range  2 ..  2;
      SMBTYPE       at 0 range  3 ..  3;
      ENARP         at 0 range  4 ..  4;
      ENPEC         at 0 range  5 ..  5;
      ENGC          at 0 range  6 ..  6;
      NOSTRETCH     at 0 range  7 ..  7;
      START         at 0 range  8 ..  8;
      STOP          at 0 range  9 ..  9;
      ACK           at 0 range 10 .. 10;
      POS           at 0 range 11 .. 11;
      PEC           at 0 range 12 .. 12;
      ALERT         at 0 range 13 .. 13;
      Resv2         at 0 range 14 .. 14;
      SWRST         at 0 range 15 .. 15;
   end record;

   -- NOTE: The correct value of PCLK1 (APB1 bus)can be obtained by the
   -- initialization call to Get_RCC_Clocks in the Timer package but
   -- currently we just assume it will be 42.
   type I2C_CR2_Setup_Record_Type is
   record
      FREQ      : I2C_FREQ      := 42;  -- PCLK1 42 MHz on most 407 boards
      Resv1     : UInt2         := 0;
      ITERREN   : I2C_ITERREN   := Disabled;
      ITEVTEN   : I2C_ITEVTEN   := Disabled;
      ITBUFEN   : I2C_ITBUFEN   := Disabled;
      DMAEN     : I2C_DMAEN     := Disabled;
      LAST_DMA  : I2C_LAST_DMA  := Not_Last;
      Resv2     : UInt3         := 0;
   end record;

   for I2C_CR2_Setup_Record_Type use
   record
      FREQ      at 0 range  0 ..  5;
      Resv1     at 0 range  6 ..  7;
      ITERREN   at 0 range  8 ..  8;
      ITEVTEN   at 0 range  9 ..  9;
      ITBUFEN   at 0 range 10 .. 10;
      DMAEN     at 0 range 11 .. 11;
      LAST_DMA  at 0 range 12 .. 12;
      Resv2     at 0 range 13 .. 15;
   end record;

   Max_Rec_Bytes : constant := 20;

   type Byte_Array is array (1 .. Max_Rec_Bytes) of Byte;


   ---------------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------------

   -- NOTE: assumes clock is PCLK1 42 MHz
   -- TODO: make work with whatever clock is set in CR2
   procedure Setup_I2C_Port (Pin_Pack   : in Pins_Type;  -- number and pin pack
                             CR1_Values : in I2C_CR1_Setup_Record_Type;
                             CR2_Values : in I2C_CR2_Setup_Record_Type);

   -- Read byte with ACK
   procedure I2C_Read_ACK (I2C_Number : in  I2C_Number_Type;
                           Data       : out Byte;
                           Error      : out Boolean);

   -- Read byte without ACK
   procedure I2C_Read_NACK (I2C_Number : in  I2C_Number_Type;
                            Data       : out Byte;
                            Error      : out Boolean);

    -- generate I2C start condition
    --procedure I2C_Start (I2C_Number : in  I2C_Number_Type;
    --                     Slave_Addr : in  Byte;
    --                     Direction  : in  Direction_Type;
    --                     Use_Ack    : in  Boolean;
    --                     Error      : out Boolean);

   -- clear and restart I2C
   procedure Clear_I2C (I2C_Number : in I2C_Number_Type);

   -- generate I2C stop condition
   procedure I2C_Stop (I2C_Number : in  I2C_Number_Type;
                       Error      : out Boolean);

   -- determine if a device is connected
   function Is_Device_Connected (I2C_Number : in I2C_Number_Type;
                                 Slave_Addr : in Byte) return Boolean;

   -- Read single byte from slave
   procedure Read_I2C_Byte (I2C_Number : in  I2C_Number_Type;
                            Slave_Addr : in  Byte;
                            Register   : in  Byte;
                            Data       : out Byte;
                            Error      : out Boolean);

   -- Read multi bytes from slave
   procedure Read_I2C_Multi (I2C_Number : in  I2C_Number_Type;
                             Slave_Addr : in  Byte;
                             Register   : in  Byte;
                             Count      : in  Positive;
                             Data_Array : out Array_Of_Bytes;
                             Error      : out Boolean);

   -- Read byte from slave without specifying register address
   procedure Get_I2C_Byte_No_Reg (I2C_Number : in  I2C_Number_Type;
                                  Slave_Addr : in  Byte;
                                  Data       : out Byte;
                                  Error      : out Boolean);

   -- Read multi bytes from slave without setting register from where
   -- to start read
   procedure Read_I2C_Multi_No_Reg
                            (I2C_Number : in  I2C_Number_Type;
                             Slave_Addr : in  Byte;
                             Count      : in  Positive;
                             Data_Array : out Array_Of_Bytes;
                             Error      : out Boolean);

   -- Write single byte to slave
   procedure Write_I2C_Byte (I2C_Number : in  I2C_Number_Type;
                             Slave_Addr : in  Byte;
                             Register   : in  Byte;
                             Data       : in  Byte;
                             Error      : out Boolean);

   -- Write multi bytes to slave
   procedure Write_I2C_Byte_Multi
                              (I2C_Number : in  I2C_Number_Type;
                               Slave_Addr : in  Byte;
                               Register   : in  Byte;
                               Count      : in  Positive;
                               Data_Array : in  Array_Of_Bytes;
                               Error      : out Boolean);

   -- Write byte to slave without specifying register address
   procedure Write_I2C_Byte_No_Reg (I2C_Number : in  I2C_Number_Type;
                                    Slave_Addr : in  Byte;
                                    Data       : in  Byte;
                                    Error      : out Boolean);

   -- Write multi bytes to slave without setting register from where
   -- to start write
   procedure Write_I2C_Byte_Multi_No_Reg
                              (I2C_Number : in  I2C_Number_Type;
                               Slave_Addr : in  Byte;
                               Count      : in  Positive;
                               Data_Array : in  Array_Of_Bytes;
                               Error      : out Boolean);

end STM32.I2C;
