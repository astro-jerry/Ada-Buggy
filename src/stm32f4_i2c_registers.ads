------------------------------------------------------------------------------
--                                                                          --
--                         STM32F4_I2C_Registers                            --
--                                                                          --
--  This file provides register definitions for the ARM Cortex STM32F407    --
--  micro-controller from ST Microelectronics.                              --
--  Created by Jerry Petrey  06-12-16                                       --
--  Last Modification:       06-12-16                                       --
------------------------------------------------------------------------------
pragma Restrictions (No_Elaboration_Code);

with HAL;   use HAL;

package STM32F4_I2C_Registers is
   pragma Preelaborate;

   -- the I2C registers
   type I2C_Port is record
         CR1      : HWord; -- Control Register 1
         Resv1    : HWord; -- Reserved
         CR2      : HWord; -- Control Register 2
         Resv2    : HWord; -- Reserved
         OAR1     : HWord; -- Own Address Register 1
         Resv3    : HWord; -- Reserved
         OAR2     : HWord; -- Own Address Register 2
         Resv4    : HWord; -- Reserved
         DR       : HWord; -- Data Register
         Resv5    : HWord; -- Reserved
         SR1      : HWord; -- Status Register 1
         Resv6    : HWord; -- Reserved
         SR2      : HWord; -- Status Register 2
         Resv7    : HWord; -- Reserved
         CCR      : HWord; -- Clock Control Register
         Resv8    : HWord; -- Reserved
         TRISE    : HWord; -- TRISE (SCL rise time) Register
         Resv9    : HWord; -- Reserved
         FLTR     : HWord; -- FLTR (Filter) Register
         Resv10   : HWord; -- Reserved
      end record with Volatile, Size => 20 * 16;

      for I2C_Port use record
         CR1      at 16#0000# range  0 .. 15;
         Resv1    at 16#0002# range  0 .. 15;
         CR2      at 16#0004# range  0 .. 15;
         Resv2    at 16#0006# range  0 .. 15;
         OAR1     at 16#0008# range  0 .. 15;
         Resv3    at 16#000A# range  0 .. 15;
         OAR2     at 16#000C# range  0 .. 15;
         Resv4    at 16#000E# range  0 .. 15;
         DR       at 16#0010# range  0 .. 15;
         Resv5    at 16#0012# range  0 .. 15;
         SR1      at 16#0014# range  0 .. 15;
         Resv6    at 16#0016# range  0 .. 15;
         SR2      at 16#0018# range  0 .. 15;
         Resv7    at 16#001A# range  0 .. 15;
         CCR      at 16#001C# range  0 .. 15;
         Resv8    at 16#001E# range  0 .. 15;
         TRISE    at 16#0020# range  0 .. 15;
         Resv9    at 16#0022# range  0 .. 15;
         FLTR     at 16#0024# range  0 .. 15;
         Resv10   at 16#0026# range  0 .. 15;
      end record;

end STM32F4_I2C_Registers;
