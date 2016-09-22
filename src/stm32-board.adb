------------------------------------------------------------------------------
--                                                                          --
--   File     STM32.Board                                                   --
--   Author   Jerry Petrey                                                  --
--   Version  V1.0                                                          --
--   Date     06-12-2016                                                    --
--   Modified 06-12-2016                                                    --
--   Brief    This file contains definitions for STM32F407 Clicker2 Board   --
--             LEDs, push-buttons hardware resources.                       --
--                                                                          --
------------------------------------------------------------------------------

package body STM32.Board is

   ------------------
   -- All_LEDs_Off --
   ------------------
   procedure All_LEDs_Off is
   begin
      Clear (All_LEDs);
   end All_LEDs_Off;

   
   -----------------
   -- All_LEDs_On --
   -----------------
   procedure All_LEDs_On is
   begin
      Set (All_LEDs);
   end All_LEDs_On;

   
   ---------------------
   -- Initialize_LEDs --
   ---------------------
   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (All_LEDs);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (All_LEDs,
                    Config => Configuration);
   end Initialize_LEDs;

   
   --------------------------------
   -- Configure_User_Button_GPIO --
   --------------------------------
   procedure Configure_User_Buttons_GPIO is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (User_Button1_Point);
      Enable_Clock (User_Button2_Point);

      Config.Mode      := Mode_In;
      Config.Resistors := Floating;

      Configure_IO (User_Button1_Point, Config);
      Configure_IO (User_Button2_Point, Config);
   end Configure_User_Buttons_GPIO;


end STM32.Board;
