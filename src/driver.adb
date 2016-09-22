------------------------------------------------------------------------------
--                                                                          --
--                             Main program Driver                          --
--                                                                          --
--                                                                          --
-- Modified by:   Jerry Petrey                                              --
-- Original Date: 05-07-16                                                  --
-- Last Modified: 09-15-16                                                  --
------------------------------------------------------------------------------
with HAL;               use HAL;
with Ada.Real_Time;     use Ada.Real_Time;
with STM32.Board;       use STM32.Board;   -- Clicker2
with STM32.Device;      use STM32.Device;  -- STM32F407
with STM32.GPIO;        use STM32.GPIO;
with OLED1351;          use OLED1351;
with Buggy;             use Buggy;
with Buzzer;            use Buzzer;
with LIDAR;             use LIDAR;
with Sharp_IR;          use Sharp_IR;
with STM32.Button1;     use STM32.Button1;
with STM32.Button2;     use STM32.Button2;
with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;

package body Driver is

   pragma Warnings (Off, "*is not referenced");
                    
   package Button1 renames STM32.Button1;
   package Button2 renames STM32.Button2;
   package Board   renames STM32.Board;
   package Device  renames STM32.Device;
   package LCD     renames OLED1351;
                    
   ---------------------------------------------------------------------------
   Build_Num : constant Natural := 142; -- Displayed on LCD during startup
   ---------------------------------------------------------------------------

   -- LIDAR distance measurements
   Distance      : HWord      := 0;

   -- for motors
   Driving_State : Boolean := True;


   ---------------------------------------------------------------------------
   -- Initialize
   -- Sets up registers for peripherals used.
   ---------------------------------------------------------------------------
   procedure Initialize_LCD is
   begin
      Init_LCD;
      Fill_Screen (SSD1351_COLOR_YELLOW);
   end Initialize_LCD;


   ---------------------------------------------------------------------------
   -- Play_Fifth
   ---------------------------------------------------------------------------
   procedure Play_Fifth is
   begin
      Play_Sound (G5, EIGHTH_NOTE, 70);
      Play_Sound (G5, EIGHTH_NOTE, 70);
      Play_Sound (G5, EIGHTH_NOTE, 70);
      Play_Sound (E5f, HALF_NOTE, 70);
      delay until Clock + Milliseconds (500);
      Play_Sound (F5, EIGHTH_NOTE, 70);
      Play_Sound (F5, EIGHTH_NOTE, 70);
      Play_Sound (F5, EIGHTH_NOTE, 70);
      Play_Sound (D5, HALF_NOTE, 70);
      delay until Clock + Milliseconds (1000);
   end Play_Fifth;


   ---------------------------------------------------------------------------
   -- Play_Ode
   ---------------------------------------------------------------------------
   procedure Play_Ode is
   begin
      -- play Ode to Joy
      Play_Sound (G7,  QUARTER_NOTE, 70);
      Play_Sound (G7,  QUARTER_NOTE, 70);
      Play_Sound (G7s, QUARTER_NOTE, 70);
      Play_Sound (A7s, QUARTER_NOTE, 70);
      Play_Sound (A7s, QUARTER_NOTE, 70);
      Play_Sound (G7s, QUARTER_NOTE, 70);
      Play_Sound (G7,  QUARTER_NOTE, 70);
      Play_Sound (F7,  QUARTER_NOTE, 70);
      Play_Sound (D7s, QUARTER_NOTE, 70);
      Play_Sound (D7s, QUARTER_NOTE, 70);
      Play_Sound (F7,  QUARTER_NOTE, 70);
      Play_Sound (G7,  QUARTER_NOTE, 70);
      Play_Sound (G7,  QUARTER_DOT_NOTE, 70);
      Play_Sound (F7,  EIGHTH_NOTE, 70);
      Play_Sound (F7,  QUARTER_DOT_NOTE, 200);

      Play_Sound (G7,  QUARTER_NOTE, 70);
      Play_Sound (G7,  QUARTER_NOTE, 70);
      Play_Sound (G7s, QUARTER_NOTE, 70);
      Play_Sound (A7s, QUARTER_NOTE, 70);
      Play_Sound (A7s, QUARTER_NOTE, 70);
      Play_Sound (G7s, QUARTER_NOTE, 70);
      Play_Sound (G7,  QUARTER_NOTE, 70);
      Play_Sound (F7,  QUARTER_NOTE, 70);
      Play_Sound (D7s, QUARTER_NOTE, 70);
      Play_Sound (D7s, QUARTER_NOTE, 70);
      Play_Sound (F7,  QUARTER_NOTE, 70);
      Play_Sound (G7,  QUARTER_NOTE, 70);
      Play_Sound (F7,  QUARTER_DOT_NOTE, 70);
      Play_Sound (E7f, EIGHTH_NOTE, 70);
      Play_Sound (E7f, QUARTER_DOT_NOTE, 200);

      delay until Clock + Milliseconds (1000);
   end Play_Ode;


   ---------------------------------------------------------------------------
   -- Wait_For_Button
   ---------------------------------------------------------------------------
   procedure Wait_For_Button is
   begin
      loop
         if Button1.Has_Been_Pressed then
            exit;
         else
            delay until Clock + Milliseconds (100);
         end if;
      end loop;
   end Wait_For_Button;



   ---------------------------------------------------------------------------
   -- Show_Clocks
   ---------------------------------------------------------------------------
   procedure Show_Clocks is
      Clocks : Device.RCC_System_Clocks;
   begin
      Fill_Screen (SSD1351_COLOR_GREEN);
      delay until Clock + Milliseconds (100);
      Clocks := Device.System_Clock_Frequencies;
      Put_String (2, 10, "RCC Clocks:");
      Put_String (2, 20, " SYSCLK  =" &
                  Natural'Image (Natural (Clocks.SYSCLK)));
      Put_String (2, 30, " HCLK    =" &
                  Natural'Image (Natural (Clocks.HCLK)));
      Put_String (2, 40, " PCLK1   =" &
                  Natural'Image (Natural (Clocks.PCLK1)));
      Put_String (2, 50, " PCLK2   =" &
                  Natural'Image (Natural (Clocks.PCLK2)));
      Put_String (2, 60, " TIMCLK1 =" &
                  Natural'Image (Natural (Clocks.TIMCLK1)));
      Put_String (2, 70, " TIMCLK2 =" &
                  Natural'Image (Natural (Clocks.TIMCLK2)));
      loop
         exit when Button1.Has_Been_Pressed;
      end loop;
      Fill_Screen (SSD1351_COLOR_GREEN);
      delay until Clock + Milliseconds (100);
   end Show_Clocks;



   ---------------------------------------------------------------------------
   -- Controller
   --
   -- The main controller task to perform all the action.
   --
   ---------------------------------------------------------------------------
   task body Controller is
      Move_Shown     : Boolean        := False;
      Stop_Shown     : Boolean        := False;
      Was_Moving_Fwd : Boolean        := False;
      Limit1         : constant HWord := 50;
      Limit2         : constant HWord := 70;
      Left_Dist      : HWord          := 0;
      Right_Dist     : HWord          := 0;
      Fwd_Dist       : HWord          := 0;
   begin
      Initialize_LEDs;
      Configure_User_Buttons_GPIO;
      Button1.Initialize;
      Button2.Initialize;

      delay until Clock + Milliseconds (10);
      -- Init LCD
      Initialize_LCD;
      delay until Clock + Milliseconds (300);

      Fill_Screen (SSD1351_COLOR_GREEN);
      Set_Default_ForeGnd_Color (SSD1351_COLOR_WHITE);
      Set_Default_BackGnd_Color (SSD1351_COLOR_GREEN);
      Put_String (2, 60, "GNAT Ada build -" & Integer'Image (Build_Num));
      delay until Clock + Milliseconds (2000);
      Fill_Screen (SSD1351_COLOR_GREEN);

      Play_Fifth;

      Setup_Buggy;
      delay until Clock + Milliseconds (2000);
      Fill_Screen (SSD1351_COLOR_GREEN);
      delay until Clock + Milliseconds (200);

      Setup_LIDAR;
      
      Show_Clocks;  -- will wait for button 1 press to continue
      
      -------------------------------------------------------------------
      Main_Loop:
      -------------------------------------------------------------------
      loop
         -- flash LEDs
         Set (LED1);
         Clear (LED2);

         if Button1.Has_Been_Pressed then
            Driving_State := not Driving_State;
            if not Driving_State then
               Stop;
               Put_String (5, 30, "  Paused       ");
            end if;
         end if;

         if Driving_State then
            Distance := Read_Distance;
            if (not IR_Object_Detected) and not (Distance < Limit1) then
               Stop_Shown := False;
               Put_String (2, 100, "                      ");
               Put_String (2, 100, "Forward Dist = " &
                           Natural'Image (Natural (Distance)));
               if not Move_Shown then
                  Put_String (5, 30, "Moving Forward ");
                  Move_Shown := True;
               end if;
               if Driving_State then
                  Was_Moving_Fwd := True;
                  Move_Forward (60);  -- faster at first
                  delay until Clock + Milliseconds (75);
                  Move_Forward (30);  -- then slower
               end if;
            else -- object must have been detected
               Stop;
               if Was_Moving_Fwd then  -- back up a little if were moving fwd
                  Put_String (5, 30, "Backing Up     ");
                  delay until Clock + Milliseconds (200);
                  if Driving_State then
                     Move_Backward (50);
                     delay until Clock + Milliseconds (250);
                  end if;
                  Put_String (5, 30, "Stopped        ");
                  Stop;
                  delay until Clock + Milliseconds (250);
               end if;
               Move_Shown := False;
               if not Stop_Shown then
                  Put_String (5, 10, "OBJECT AHEAD:  ");
                  Put_String (5, 30, "STOPPED!       ");
                  Play_Sound (F5, HALF_NOTE, 70);
                  Stop_Shown := True;
                  delay until Clock + Milliseconds (250);
                  Put_String (5, 10, "               ");
               end if;
               Put_String (2, 100, "                      ");
   
               Look_Left;
               Left_Dist := Read_Distance;
               Put_String (2, 100, "Left Dist    = " &
                              Natural'Image (Natural (Left_Dist)));
               Look_Right; -- back to center
               Look_Right;
               Right_Dist := Read_Distance;
               Put_String (2, 100, "Right Dist   = " &
                             Natural'Image (Natural (Right_Dist)));
               Look_Forward;
   
               if Right_Dist > Limit2 and Driving_State then
                  Rotate_Right (90, 250);
                  delay until Clock + Milliseconds (100);
                  delay until Clock + Milliseconds (250);
                  Move_Forward (40);  -- faster at first
                  delay until Clock + Milliseconds (75);
                  Move_Forward (15);  -- then slower
               elsif Left_Dist > Limit2 and Driving_State then
                  Rotate_Left (90, 250);
                  delay until Clock + Milliseconds (100);
                  delay until Clock + Milliseconds (250);
                  Move_Forward (40);  -- faster at first
                  delay until Clock + Milliseconds (75);
                  Move_Forward (15);  -- then slower
               else
                  delay until Clock + Milliseconds (250);
                  -- try backing up and going left (or right if left is blocked)
                  Put_String (5, 30, "Backing Up     ");
                  delay until Clock + Milliseconds (1000);
                  if Driving_State then
                     Move_Backward (50);
                     delay until Clock + Milliseconds (400);
                  end if;
                  Put_String (5, 30, "Stopped        ");
                  Stop;
                  delay until Clock + Milliseconds (1000);
                  Put_String (5, 30, "Rotating Left  ");
                  --
                  if Driving_State then
                     Rotate_Left (90, 200);
                     delay until Clock + Milliseconds (50);
                  end if;
                  Distance := Read_Distance;
                  Put_String (2, 100, "Forward Dist = " &
                                Natural'Image (Natural (Distance)));
                  delay until Clock + Milliseconds (1000);
                  if Distance > Limit1 then
                     Put_String (5, 10, "               ");
                     Put_String (5, 30, "Moving Forward ");
                     if Driving_State then
                        Move_Forward (40);  -- faster at first
                        delay until Clock + Milliseconds (75);
                        Move_Forward (15);  -- then slower
                     end if;
                  else
                     Put_String (5, 10, "OBJECT AHEAD:  ");
                     Put_String (5, 30, "Rotating Right ");
                     if Driving_State then
                        Rotate_Right (90, 400);
                     end if;
                     delay until Clock + Milliseconds (250);
                     Put_String (5, 10, "               ");
                     Distance := Read_Distance;
                     Put_String (2, 100, "Forward Dist = " &
                                 Natural'Image (Natural (Distance)));
                     if Distance > Limit1 then
                        Put_String (5, 30, "Moving Forward ");
                        if Driving_State then
                           Move_Forward (60);  -- faster at first
                           delay until Clock + Milliseconds (75);
                           Move_Forward (40);  -- then slower
                        end if;
                     else
                        Put_String (5, 10, "OBJECT AHEAD:  ");
                        Stop;
                        delay until Clock + Milliseconds (250);
                        Put_String (5, 10, "               ");
                     end if;
                  end if;
               end if;
            end if;
         else -- stopped due to user button so just show distances
            Distance := Read_Distance;
            Put_String (2, 100, "                      ");
            Put_String (2, 100, "Forward Dist = " &
                        Natural'Image (Natural (Distance)));
            delay until Clock + Milliseconds (500);
         end if;
         delay until Clock + Milliseconds (100);
         Set (LED2);
         Clear (LED1);
      end loop Main_Loop;
      
   end Controller;

end Driver;
