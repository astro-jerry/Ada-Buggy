------------------------------------------------------------------------------
-- Sharp_IR
--
-- Support for Sharp IR module.
--
-- Created by Jerry Petrey 05-23-15
-- Last Modification:      06-12-16
--
------------------------------------------------------------------------------
with HAL;              use HAL;
with STM32.GPIO;       use STM32.GPIO;
with STM32.Device;     use STM32.Device;
with STM32_SVD.GPIO;   use STM32_SVD.GPIO;
with Interfaces;       use Interfaces;
with Ada.Unchecked_Conversion;

package body Sharp_IR is


   function Port_To_HWord is
          new Ada.Unchecked_Conversion (Source => IDR_Field,
                                        Target => HWord);

   ---------------------------------------------------------------------------
   -- Setup_IR
   ---------------------------------------------------------------------------
   procedure Setup_IR is
   begin
      --  Enable clock for GPIO
      Enable_Clock (GPIO_C); -- for IR sens

      -- set PC1 for IR input on Buggy MB2
      Configure_IO (This   => PC1,
                    Config => (Mode        => Mode_In,
                               Output_Type => Push_Pull,
                               Speed       => Speed_Very_High,
                               Resistors   => Floating));

   end Setup_IR;


   ---------------------------------------------------------------------------
   -- IR_Object_Detected
   ---------------------------------------------------------------------------
   function IR_Object_Detected return Boolean is
      Pin1_Mask : constant HWord := 16#0002#;
      Detected  : Boolean        := False;
   begin
      Detected := (Port_To_HWord (GPIOC_Periph.IDR.IDR) and Pin1_Mask) = 0;
      return Detected;
   end IR_Object_Detected;


begin
   Setup_IR;
end Sharp_IR;
