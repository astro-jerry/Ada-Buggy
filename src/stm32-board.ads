------------------------------------------------------------------------------
--                                                                          --
--   File     STM32.Board                                                   --
--   Author   Jerry Petrey                                                  --
--   Version  V1.0                                                          --
--   Date     06-12-2016                                                    --
--   Modified 07-17-2016                                                    --
--   Brief    This file contains definitions for STM32F407 Clicker2 Board   --
--             LEDs, push-buttons hardware resources.                       --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Interrupts.Names;  use Ada.Interrupts;
with STM32.Device;          use STM32.Device;
with STM32.GPIO;            use STM32.GPIO;
with STM32.SPI;             use STM32.SPI;
with STM32F4_SPI_Registers; use STM32F4_SPI_Registers;

package STM32.Board is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Point;

   Red1   : User_LED renames PE12;
   Red2   : User_LED renames PE15;
   
   LED1   : User_LED renames PE12;
   LED2   : User_LED renames PE15;

   All_LEDs : GPIO_Points := Red1 & Red2;
   LCH_LED  : GPIO_Point renames Red2;

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Turn_On (This : in out User_LED)
     renames STM32.GPIO.Set;
   procedure Turn_Off (This : in out User_LED)
     renames STM32.GPIO.Clear;

   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;
   procedure Toggle_LEDs (These : in out GPIO_Points)
     renames STM32.GPIO.Toggle;

   ---------------------------------------------------------------------------
   -- The Buggy F407 Clicker2 Board with 1.5" SSD1351 128x128 OLED
   -- display on prototype click board in MB slot 1, uses:
   -- SPI3 PinPack2  (MOSI=PC12, MISO=PC11, SCL=PC10)
   -- CS        PE8
   -- RST       PE7
   -- PWM (D/C) PE9
   ---------------------------------------------------------------------------
   LCD_SPI          : SPI_Port renames STM32.Device.SPI_3;
   LCD_SPI_Num      : SPI_Number_Type := SPI3;
   LCD_SPI_Pin_Pack : SPI_ID_Type     := SPI3_Pin_Pack_2;
   LCD_CS           : GPIO_Point      := PE8;
   LCD_DC           : GPIO_Point      := PE9;
   LCD_Reset        : GPIO_Point      := PE7;

   --  GPIO Pins for FMC
   FMC_A : constant GPIO_Points :=
             (PF0, PF1, PF2, PF3, PF4, PF5,
              PF12, PF13, PF14, PF15, PG0, PG1);

   FMC_D : constant GPIO_Points :=
             (PD14, PD15, PD0, PD1, PE7, PE8, PE9, PE10, PE11, PE12,
              PE13, PE14, PE15, PD8, PD9, PD10);

   FMC_SDNWE     : GPIO_Point renames PH5;
   FMC_SDNRAS    : GPIO_Point renames PF11;
   FMC_SDNCAS    : GPIO_Point renames PG15;
   FMC_SDCLK     : GPIO_Point renames PG8;
   FMC_BA0       : GPIO_Point renames PG4;
   FMC_BA1       : GPIO_Point renames PG5;
   FMC_SDNE0     : GPIO_Point renames PH3;
   FMC_SDCKE0    : GPIO_Point renames PC3;
   FMC_NBL0      : GPIO_Point renames PE0;
   FMC_NBL1      : GPIO_Point renames PE1;

   SDRAM_PINS    : constant GPIO_Points :=
                     GPIO_Points'
                       (FMC_SDNWE,
                        FMC_SDNRAS,
                        FMC_SDNCAS,
                        FMC_SDCLK,
                        FMC_BA0,
                        FMC_BA1,
                        FMC_SDNE0,
                        FMC_SDCKE0,
                        FMC_NBL0,
                        FMC_NBL1) &
                     FMC_A & FMC_D;

   --  User button

   User_Button1_Point     : GPIO_Point renames PE0;
   User_Button1_Interrupt : constant Interrupt_ID := Names.EXTI0_Interrupt;
   
   User_Button2_Point     : GPIO_Point renames PA10;
   User_Button2_Interrupt : constant Interrupt_ID := Names.EXTI15_10_Interrupt;

   procedure Configure_User_Buttons_GPIO;
   --  Configures the GPIO port/pin for the blue user button. Sufficient
   --  for polling the button, and necessary for having the button generate
   --  interrupts.

end STM32.Board;
