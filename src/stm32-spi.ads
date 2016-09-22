------------------------------------------------------------------------------
-- spi.ads
--
-- Created by Jerry Petrey 10-14-14
-- Last Modification:      08-22-16
--
-- Allows using any of the SPIs implemented on M3/M4 chips.
-- This version is faster than the AdaCore version so is sometimes better
-- to use.
--
-- NOTE: See usage information below.
------------------------------------------------------------------------------

package STM32.SPI is

   ---------------------------------------------------------------------------
   -- ************************************************************************
   -- To select SPI, set the values you need in records like these below,
   -- and then call Setup_SPI_Port using these record variables as parameters
   -- for the CR register values; also supply SPI_Type (number and pin pack)
   -- parameter.
   --*************************************************************************
   ---------------------------------------------------------------------------
   --SPI_CR1_Setup_Record : SPI_CR1_Setup_Record_Type :=
   --        (Mode          => SPI_Mode_0, -- CPOL CPHA bits
   --         MASTER        => Master,
   --         Baudrate      => Div_32,
   --         Enable        => Disabled,
   --         LSB_FIRST     => MSB_First;
   --         SSI_NSS_Bit   => High,
   --         SSM           => Enabled,
   --         RX_Only_Mode  => Full_Duplex,
   --         DF_Format     => DF_8_Bit,
   --         CRC_Next      => No_CRC_Phase,
   --         CRC_Enable    => Disabled,
   --         BiDir_OE      => Enabled,
   --         BiDir_Mode    => One_Line_BiDir);

   --SPI_CR2_Setup_Record : SPI_CR2_Setup_Record_Type :=
   --        (RX_DMA_Enable         => Disabled,
   --         TX_DMA_Enable         => Disabled,
   --         SS_Output_Enable      => SS_Output_Disabled,
   --         Reserved1             => 0,
   --         Frame_Format          => Motorola_Mode,
   --         Error_Interrupt       => Masked,
   --         RX_Buf_Not_Empty_Intr => Masked,
   --         TX_Buf_Empty_Intr     => Masked,
   --         Reserved2             => 0);

   -- Pinouts used by the SPIs: all on AF5 except * = AF6 for some SPI3 pin packs
   -- (not all chips have all pins, some are questionable on which go together,
   --  SPI2 and SPI3 seem to have additional isolated pins on some chips outside
   --  the normal pin packs on AF5)
--       |PINS PACK 1    |PINS PACK 2      |PINs PACK 2a  |PINS PACK 3   |PINS PACK 4   |       ?      | ?
-- SPIX  |MOSI MISO SCK  |MOSI MISO  SCK   |MOSI MISO SCK |MOSI MISO SCK |MOSI MISO SCK |MOSI MISO SCK | SCK
-- ----- |---------------|-----------------|--------------|--------------|--------------|--------------|-----
-- SPI1  |PA7  PA6  PA5  |PB5  PB4   PB3   |              |              |              |
-- SPI2  |PC3  PC2  PB10 |PB15 PB14  PB13  |PB15 PB14 PD3 |PI3  PI2  PI1 |PB15 PB14 PI1 |PC1       PD3 | PA9
-- SPI3 *|PB5  PB4  PB3 *|PC12 PC11  PC10 *|              |              |              |PD6 ?         |


   type SPI_ID_Type is
          (SPI1_Pin_Pack_1, SPI1_Pin_Pack_2,
           SPI2_Pin_Pack_1, SPI2_Pin_Pack_2, SPI2_Pin_Pack_2a, SPI2_Pin_Pack_3,
           SPI2_Pin_Pack_4,
           SPI3_Pin_Pack_1, SPI3_Pin_Pack_2);

   -- use to identify SPI in send data routines
   type SPI_Number_Type is (SPI1, SPI2, SPI3);


   ---------------------------------------------------------------------------
   -- SPI setup data
   ---------------------------------------------------------------------------

   -- CR1 register bits
   type SPI_BiDir_Mode is (Two_Line_UniDir, One_Line_BiDir);
   for SPI_BiDir_Mode use (Two_Line_UniDir => 0,
                           One_Line_BiDir  => 1);

   type SPI_BiDir_OE is (Disabled, Enabled);
   for SPI_BiDir_OE use (Disabled => 0,
                         Enabled  => 1);

   type SPI_CRC is (Disabled, Enabled);
   for SPI_CRC use (Disabled => 0,
                    Enabled  => 1);

   type SPI_CRC_Next is (No_CRC_Phase, CRC_Phase);
   for SPI_CRC_Next use (No_CRC_Phase => 0,
                         CRC_Phase    => 1);

   type SPI_DF_Format is (DF_8_Bit, DF_16_Bit);
   for SPI_DF_Format use (DF_8_Bit  => 0,
                          DF_16_Bit => 1);

   type SPI_RX_Only_Mode is (Full_Duplex, Rec_Only);
   for SPI_RX_Only_Mode use (Full_Duplex => 0,
                             Rec_Only    => 1);

   type SPI_SSM is (Disabled, Enabled);
   for SPI_SSM use (Disabled => 0,
                    Enabled  => 1);

   type SPI_SSI_NSS_Bit is (Low, High);  -- only if SSM Enabled
   for SPI_SSI_NSS_Bit use (Low  => 0,   -- value placed on NSS pin
                            High => 1);

   type SPI_LSB_FIRST is (MSB_First, LSB_First);
   for SPI_LSB_FIRST use (MSB_First => 0,
                          LSB_First => 1);

   type SPI_Enable is (Disabled, Enabled);
   for SPI_Enable use (Disabled => 0,
                       Enabled  => 1);

   type SPI_Baudrate is (Div_2, Div_4, Div_8, Div_16, Div_32,
                         Div_64, Div_128, Div_256);
   for SPI_Baudrate use (Div_2   => 0,
                         Div_4   => 1,
                         Div_8   => 2,
                         Div_16  => 3,
                         Div_32  => 4,
                         Div_64  => 5,
                         Div_128 => 6,
                         Div_256 => 7);

   type SPI_MASTER is (Slave, Master);
   for SPI_MASTER use (Slave  => 0,
                       Master => 1);

   type SPI_Mode is (SPI_Mode_0,  -- Clock polarity low, clock phase 1st edge
                     SPI_Mode_1,  -- Clock polarity low, clock phase 2nd edge
                     SPI_Mode_2,  -- Clock polarity high, clock phase 1st edge
                     SPI_Mode_3); -- Clock polarity high, clock phase 2nd edge
   for SPI_Mode use (SPI_Mode_0 => 2#00#,  -- CPOL CPHA bits
                     SPI_Mode_1 => 2#01#,
                     SPI_Mode_2 => 2#10#,
                     SPI_Mode_3 => 2#11#);

   -- CR2 register bits
   type SPI_TXEIE is (Masked, Enabled);
   for SPI_TXEIE use (Masked  => 0,
                      Enabled => 1);

   type SPI_RXNEIE is (Masked, Enabled);
   for SPI_RXNEIE use (Masked  => 0,
                       Enabled => 1);

   type SPI_ERRIE is (Masked, Enabled);
   for SPI_ERRIE use (Masked  => 0,
                      Enabled => 1);

   type SPI_FRF is (Motorola_Mode, TI_Mode);
   for SPI_FRF use (Motorola_Mode => 0,
                    TI_Mode       => 1);

   type SPI_SSOE is (SS_Output_Disabled, SS_Output_Enabled);
   for SPI_SSOE use (SS_Output_Disabled => 0,
                     SS_Output_Enabled  => 1);

   type SPI_TXDMA_Enable is (Disabled, Enabled);
   for SPI_TXDMA_Enable use (Disabled => 0,
                             Enabled  => 1);

   type SPI_RXDMA_Enable is (Disabled, Enabled);
   for SPI_RXDMA_Enable use (Disabled => 0,
                             Enabled  => 1);

   -- constants for SPI CR1 register
   SPI_EN              : constant HWORD  := 16#0040#;
   -- constants for SPI status register
   SPI_RXNE            : constant HWORD  := 16#0001#;
   SPI_TXE             : constant HWORD  := 16#0002#;
   SPI_CHSIDE          : constant HWORD  := 16#0004#;
   SPI_UDR             : constant HWORD  := 16#0008#;
   SPI_CRC_ERR         : constant HWORD  := 16#0010#;
   SPI_MODF            : constant HWORD  := 16#0020#;
   SPI_OVR             : constant HWORD  := 16#0040#;
   SPI_BSY             : constant HWORD  := 16#0080#;
   SPI_FRE             : constant HWORD  := 16#0100#;
   SPI_FRLVL_Mask      : constant HWORD  := 16#0600#;
   SPI_FTLVL_Mask      : constant HWORD  := 16#1800#;

   -- seems to run 9341 LCD on most boards with Div_2 clock
   type SPI_CR1_Setup_Record_Type is
   record                                                    -- Field name
      Mode            : SPI_Mode         := SPI_Mode_0;      -- CPOL:CPHA
      Master_Slave    : SPI_MASTER       := Master;          -- MSTR
      Baudrate        : SPI_Baudrate     := Div_2;           -- BR [2:0]
      Enable          : SPI_Enable       := Disabled;        -- SPE
      LSB_FIRST       : SPI_LSB_FIRST    := MSB_First;       -- LSBFIRST
      SSI_NSS_Bit     : SPI_SSI_NSS_Bit  := High;            -- SSI
      SSM             : SPI_SSM          := Enabled;         -- SSM
      RX_Only_Mode    : SPI_RX_Only_Mode := Full_Duplex;     -- RXONLY
      DF_Format       : SPI_DF_Format    := DF_8_Bit;        -- DFF
      CRC_Next        : SPI_CRC_Next     := No_CRC_Phase;    -- CRCNEXT
      CRC_Enable      : SPI_CRC          := Disabled;        -- CRCEN
      BiDir_OE        : SPI_BiDir_OE     := Enabled;         -- BIDIOE
      BiDir_Mode      : SPI_BiDir_Mode   := Two_Line_UniDir; -- BIDIMODE
   end record with
     Volatile,
     Size => 16;

   for SPI_CR1_Setup_Record_Type use
   record
      Mode            at 0 range  0 ..  1;
      Master_Slave    at 0 range  2 ..  2;
      Baudrate        at 0 range  3 ..  5;
      Enable          at 0 range  6 ..  6;
      LSB_FIRST       at 0 range  7 ..  7;
      SSI_NSS_Bit     at 0 range  8 ..  8;
      SSM             at 0 range  9 ..  9;
      RX_Only_Mode    at 0 range 10 .. 10;
      DF_Format       at 0 range 11 .. 11;
      CRC_Next        at 0 range 12 .. 12;
      CRC_Enable      at 0 range 13 .. 13;
      BiDir_OE        at 0 range 14 .. 14;
      BiDir_Mode      at 0 range 15 .. 15;
   end record;


   type SPI_CR2_Setup_Record_Type is
   record
      RX_DMA_Enable         : SPI_RXDMA_Enable := Disabled;
      TX_DMA_Enable         : SPI_TXDMA_Enable := Disabled;
      SS_Output_Enable      : SPI_SSOE         := SS_Output_Disabled;
      Reserved1             : Bit              := 0;
      Frame_Format          : SPI_FRF          := Motorola_Mode;
      Error_Interrupt       : SPI_ERRIE        := Masked;
      RX_Buf_Not_Empty_Intr : SPI_RXNEIE       := Masked;
      TX_Buf_Empty_Intr     : SPI_TXEIE        := Masked;
      Reserved2             : Byte             := 0;
   end record;

   for SPI_CR2_Setup_Record_Type use
   record
      RX_DMA_Enable         at 0 range  0 ..  0;
      TX_DMA_Enable         at 0 range  1 ..  1;
      SS_Output_Enable      at 0 range  2 ..  2;
      Reserved1             at 0 range  3 ..  3;
      Frame_Format          at 0 range  4 ..  4;
      Error_Interrupt       at 0 range  5 ..  5;
      RX_Buf_Not_Empty_Intr at 0 range  6 ..  6;
      TX_Buf_Empty_Intr     at 0 range  7 ..  7;
      Reserved2             at 0 range  8 .. 15;
   end record;


   Default_CR1 : SPI_CR1_Setup_Record_Type;
   Default_CR2 : SPI_CR2_Setup_Record_Type;


   ---------------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------------

   procedure Setup_SPI_Port (SPI        : SPI_ID_Type; -- number and pin pack
                             CR1_Values : SPI_CR1_Setup_Record_Type;
                             CR2_Values : SPI_CR2_Setup_Record_Type);

   -- for byte transfers
   procedure SPI_Send (SPI_Number : in SPI_Number_Type;
                       Data       : in Byte);

   -- for 16-bit transfers
   procedure SPI_Send (SPI_Number : in SPI_Number_Type;
                       Data       : in Hword);

   --procedure SPI_Send_Multi (Data  : in Byte_Array;
   --                          Count : in HWORD);

   --procedure SPI_Write_Multi (Data  : in Byte_Array;
   --                           Count : in HWORD);

   --procedure SPI_Read_Multi (Data  : out Byte_Array;
   --                          Count : in HWORD);

end STM32.SPI;
