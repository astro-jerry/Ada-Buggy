------------------------------------------------------------------------------
--                                                                          --
--                         STM32F4_SPI_Registers                            --
--                                                                          --
-- Created by Jerry Petrey  10-07-14                                        --
-- Last Modification:       06-09-16                                        --
------------------------------------------------------------------------------
--  This file provides register definitions for the ARM Cortex STM32F4xx
--  micro-controller from ST Microelectronics.
-----------------------------------------------------------------------------
pragma Restrictions (No_Elaboration_Code);
with HAL;    use HAL;

package STM32F4_SPI_Registers is
   pragma Preelaborate;

   -- the SPI registers
   type SPI_Port is record
      --                   default
      CR1     : HWord; -- 16#0000# Control Register 1 (not used in I2S mode)
      Resv1   : HWord; -- 16#0000#
      CR2     : HWord; -- 16#0000# Control Register 2
      Resv2   : HWord; -- 16#0000#
      SR      : HWord; -- 16#0002# Status Register
      Resv3   : HWord; -- 16#0000#
      DR      : HWord; -- 16#0000# Data Register
      Resv4   : HWord; -- 16#0000#
      CRCPR   : HWord; -- 16#0007# CRC Polynomial Register (not used in I2S)
      Resv5   : HWord; -- 16#0000#
      RXCRCR  : HWord; -- 16#0000# RX CRC Register (not used in I2S mode)
      Resv6   : HWord; -- 16#0000#
      TXCRCR  : HWord; -- 16#0000# TX CRC Register (not used in I2S mode)
      Resv7   : HWord; -- 16#0000#
      I2SCFGR : HWord; -- 16#0000# I2S Configuration Register
      Resv8   : HWord; -- 16#0000#
      I2SPR   : HWord; -- 16#0002# I2S Prescaler Register
      Resv9   : HWord; -- 16#0000#
   end record with
     Volatile,
     Size => 18 * 16;

   for SPI_Port use record
      CR1     at  0 range  0 .. 15;
      Resv1   at  2 range  0 .. 15;
      CR2     at  4 range  0 .. 15;
      Resv2   at  6 range  0 .. 15;
      SR      at  8 range  0 .. 15;
      Resv3   at 10 range  0 .. 15;
      DR      at 12 range  0 .. 15;
      Resv4   at 14 range  0 .. 15;
      CRCPR   at 16 range  0 .. 15;
      Resv5   at 18 range  0 .. 15;
      RXCRCR  at 20 range  0 .. 15;
      Resv6   at 22 range  0 .. 15;
      TXCRCR  at 24 range  0 .. 15;
      Resv7   at 26 range  0 .. 15;
      I2SCFGR at 28 range  0 .. 15;
      Resv8   at 30 range  0 .. 15;
      I2SPR   at 32 range  0 .. 15;
      Resv9   at 34 range  0 .. 15;
   end record;

end STM32F4_SPI_Registers;

