------------------------------------------------------------------------------
-- oled1351.adb
--
-- Created by Jerry Petrey 03-19-15
-- Last Modification:      06-12-16
------------------------------------------------------------------------------
with Ada.Real_Time;      use Ada.Real_Time;
with OLED1351_Fonts;     use OLED1351_Fonts;
with STM32.Device;       use STM32.Device;
with STM32.Board;        use STM32.Board;
with STM32.GPIO;         use STM32.GPIO;
with STM32.SPI;          use STM32.SPI;
with Interfaces;         use Interfaces;

package body OLED1351 is

   pragma Warnings (Off, "*actuals for this call may be in wrong order");

   Selected_ForeGnd : HWord;
   Selected_BackGnd : HWord;

   -- initial position coordinates
   X   : Byte := 0;
   Y   : Byte := 0;

   -- for default font5x7
   Font_Width  : constant Byte := 5;
   Font_Height : constant Byte := 7;

   Rotation    : Rotation_Type  := Portrait1;
   Font_Size   : Font_Size_Type := 1;


   procedure Disable_OLED_CS with Inline;
   procedure Enable_OLED_CS  with Inline;
   procedure Enable_OLED_Reset with Inline;
   procedure Disable_OLED_Reset with Inline;
   procedure Enable_OLED_Data with Inline;
   procedure Enable_OLED_Cmd with Inline;


   ---------------------------------------------------------------------------
   -- Swap
   ---------------------------------------------------------------------------
   procedure Swap (A : in out Byte; B : in out Byte) is
      Temp : Byte;
   begin
      Temp := A;
      A    := B;
      B    := Temp;
   end Swap;

   procedure Swap (A : in out Integer; B : in out Integer) is
      Temp : Integer;
   begin
      Temp := A;
      A    := B;
      B    := Temp;
   end Swap;

   ---------------------------------------------------------------------------
   -- Set_Default_ForeGnd_Color
   ---------------------------------------------------------------------------
   procedure Set_Default_ForeGnd_Color (Color : in HWord) is
   begin
      Selected_ForeGnd := Color;
   end Set_Default_ForeGnd_Color;

   ---------------------------------------------------------------------------
   -- Set_Default_BackGnd_Color
   ---------------------------------------------------------------------------
   procedure Set_Default_BackGnd_Color (Color : in HWord) is
   begin
      Selected_BackGnd := Color;
   end Set_Default_BackGnd_Color;


   ---------------------------------------------------------------------------
   -- Init_LCD
   ---------------------------------------------------------------------------
   procedure Init_LCD is
      Config : constant GPIO_Port_Configuration :=
                        (Mode        => Mode_Out,
                         Output_Type => Push_Pull,
                         Resistors   => Floating,
                         Speed       => Speed_100MHz);
   begin
      if LCD_Initialized then
         return;
      end if;

      Enable_Clock (LCD_DC);
      Configure_IO (LCD_DC, Config);

      Enable_Clock (LCD_CS);
      Configure_IO (LCD_CS, Config);

      Enable_Clock (LCD_Reset);
      Configure_IO (LCD_Reset, Config);

      Disable_OLED_CS;

      Setup_SPI_Port (SPI        => LCD_SPI_Pin_Pack,
                      CR1_Values => Default_CR1,
                      CR2_Values => Default_CR2);

      -- hard reset of OLED
      Enable_OLED_Reset;
      delay until Clock + Milliseconds (5);
      Disable_OLED_Reset;
      delay until Clock + Milliseconds (200);

      -- Initialization sequence
      --
      Send_Command (SSD1351_CMD_COMMANDLOCK);
      Send_Data (16#12#); -- unlock OLED driver
      Send_Command (SSD1351_CMD_COMMANDLOCK);
      Send_Data (16#B1#); -- enable all inaccessible commands

      Send_Command (SSD1351_CMD_DISPLAYALLON); -- show all white screen
      delay until Clock + Milliseconds (200);

      -- 7:4 = Oscillator freq, 3:0 = CLK div ratio (A[3:0]+1 = 1..16)
      Send_Command (SSD1351_CMD_CLOCKDIV);
      Send_Data (16#F1#);  -- note: C code used Send_Command?

      Send_Command (SSD1351_CMD_MUXRATIO);
      Send_Data (127);

      Send_Command (SSD1351_CMD_SETREMAP);
      Send_Data (16#74#);

      Send_Command (SSD1351_CMD_SETCOLUMN);
      Send_Data (16#00#);
      Send_Data (16#7F#);
      Send_Command (SSD1351_CMD_SETROW);
      Send_Data (16#00#);
      Send_Data (16#7F#);

      Send_Command (SSD1351_CMD_STARTLINE);
      Send_Data (0);   -- for 128x128 1.5" OLED

      Send_Command (SSD1351_CMD_DISPLAYOFFSET);
      Send_Data (16#00#);

      Send_Command (SSD1351_CMD_SETGPIO);
      Send_Data (16#00#);

      Send_Command (SSD1351_CMD_FUNCTIONSELECT);
      Send_Data (16#01#);  -- internal (diode drop)

      Send_Command (SSD1351_CMD_PRECHARGE);
      Send_Data (16#32#);

      Send_Command (SSD1351_CMD_VCOMH);
      Send_Data (16#05#);

      Send_Command (SSD1351_CMD_CONTRASTABC);
      Send_Data (16#C8#);
      Send_Data (16#80#);
      Send_Data (16#C8#);

      Send_Command (SSD1351_CMD_CONTRASTMASTER);
      Send_Data (16#0F#);

      Send_Command (SSD1351_CMD_SETVSL);
      Send_Data (16#A0#);
      Send_Data (16#B5#);
      Send_Data (16#55#);

      Send_Command (SSD1351_CMD_USELUT);

      Send_Command (SSD1351_CMD_PRECHARGE2);
      Send_Data (16#01#);

      Send_Command (SSD1351_CMD_DISPLAYON);     -- sleep off
      Send_Command (SSD1351_CMD_NORMALDISPLAY); -- show DDRAM image

      delay until Clock + Milliseconds (20);
      Send_Command (SSD1351_CMD_DISPLAYENHANCE);
      Send_Data (16#A4#);                       -- enhanced display
      Send_Data (16#00#);
      Send_Data (16#00#);

      -- set default rotation
      Rotation := Landscape2;
      -- set initial coordinates
      X := 0;
      Y := 0;
      LCD_Initialized := True;
      delay until Clock + Milliseconds (20);
   end Init_LCD;


   ----------------------
   -- Disable_OLED_CS  --
   ----------------------
   procedure Disable_OLED_CS is
   begin
      Set (LCD_CS);
   end Disable_OLED_CS;


   ---------------------
   -- Enable_OLED_CS  --
   ---------------------
   procedure Enable_OLED_CS is
   begin
      Clear (LCD_CS);
   end Enable_OLED_CS;


   ------------------------
   -- Enable_OLED_Reset  --
   ------------------------
   procedure Enable_OLED_Reset is
   begin
      Clear (LCD_Reset);
   end Enable_OLED_Reset;


   -------------------------
   -- Disable_OLED_Reset  --
   -------------------------
   procedure Disable_OLED_Reset is
   begin
      Set (LCD_Reset);
   end Disable_OLED_Reset;


   -----------------------
   -- Enable_OLED_Data  --
   -----------------------
   procedure Enable_OLED_Data is
   begin
      Set (LCD_DC);
   end Enable_OLED_Data;


   ----------------------
   -- Enable_OLED_Cmd  --
   ----------------------
   procedure Enable_OLED_Cmd is
   begin
      Clear (LCD_DC);
   end Enable_OLED_Cmd;



   ---------------------------------------------------------------------------
   -- Send_Command
   ---------------------------------------------------------------------------
   procedure Send_Command (Cmd : in Byte) is
   begin
      Enable_OLED_Cmd;
      Enable_OLED_CS;
      SPI_Send (LCD_SPI_Num, Cmd);
      Disable_OLED_CS;
   end Send_Command;


   ---------------------------------------------------------------------------
   -- Send_Data
   ---------------------------------------------------------------------------
   procedure Send_Data (Data : in Byte) is
   begin
      Enable_OLED_Data;
      Enable_OLED_CS;
      SPI_Send (LCD_SPI_Num, Data);
      Disable_OLED_CS;
   end Send_Data;


   ---------------------------------------------------------------------------
   -- Go_To
   -- go to selected pixel location on display
   ---------------------------------------------------------------------------
   procedure Go_To (X : in Byte;
                    Y : in Byte) is
   begin
      Send_Command (SSD1351_CMD_SETCOLUMN);
      Send_Data (X);
      Send_Data (SSD1351_WIDTH - 1);

      Send_Command (SSD1351_CMD_SETROW);
      Send_Data (Y);
      Send_Data (SSD1351_HEIGHT - 1);

      Send_Command (SSD1351_CMD_WRITERAM);
   end Go_To;


   ---------------------------------------------------------------------------
   -- Invert_Display
   ---------------------------------------------------------------------------
   procedure Invert_Display (Invert : in Boolean) is
   begin
      if Invert then
         Send_Command (SSD1351_CMD_INVERTDISPLAY);
      else
         Send_Command (SSD1351_CMD_NORMALDISPLAY);
      end if;
   end Invert_Display;


   ---------------------------------------------------------------------------
   -- Set_Rotation
   ---------------------------------------------------------------------------
   procedure Set_Rotation (Value : in Rotation_Type) is
   begin
      Rotation := Value;
   end Set_Rotation;


   ---------------------------------------------------------------------------
   -- Get_Rotation
   ---------------------------------------------------------------------------
   function Get_Rotation return Rotation_Type is
   begin
      return Rotation;
   end Get_Rotation;


   ---------------------------------------------------------------------------
   -- Draw_Pixel
   ---------------------------------------------------------------------------
   procedure Draw_Pixel (X0    : in Byte;
                         Y0    : in Byte;
                         Color : in HWord) is
      Rec_Data : Byte;
      pragma Unreferenced (Rec_Data);
      X : Byte := X0;
      Y : Byte := Y0;
   begin
      -- transform x and y based on current rotation
      case Rotation is
         when Portrait1  =>  -- default, no rotation
            null;
         when Landscape1 =>  -- rotated 90 degrees clockwise
            Swap (X, Y);
            X := WIDTH - X - 1;
         when Portrait2  =>  -- rotated 180 degrees clockwise
            X := WIDTH - X - 1;
            Y := HEIGHT - Y - 1;
         when Landscape2 =>  -- rotated 270 degrees clockwise
            swap(X, Y);
            Y := HEIGHT - Y - 1;
      end case;

      -- Bounds check.
      if ((X >= SSD1351_WIDTH) or (Y >= SSD1351_HEIGHT)) then
         return;
      end if;

      Go_To (X, Y);
      Enable_OLED_Data;
      Enable_OLED_CS;
      SPI_Send (LCD_SPI_Num, Byte (Shift_Right (Color, 8)));
      SPI_Send (LCD_SPI_Num, Byte (Color and 16#FF#));
      Disable_OLED_CS;
   end Draw_Pixel;


   ---------------------------------------------------------------------------
   -- Fill_Screen
   ---------------------------------------------------------------------------
   procedure Fill_Screen (Color : in HWord) is
      X : constant Byte := 0;
      Y : constant Byte := 0;
   begin
      Fill_Rect (X, Y, SSD1351_WIDTH, SSD1351_HEIGHT, Color);
      -- set default background for use with future short Put_String
      Set_Default_BackGnd_Color (Color);
      delay until Clock + Milliseconds (20);
   end Fill_Screen;


   ---------------------------------------------------------------------------
   -- Raw_Fill_Rect
   -- Draw a filled rectangle with no rotation.
   ---------------------------------------------------------------------------
   procedure Raw_Fill_Rect (X     : in Byte;
                            Y     : in Byte;
                            W     : in out Byte;
                            H     : in out Byte;
                            Color : in Hword) is
      MSB : HWord;
      LSB : HWord;
   begin
      MSB := Shift_Right (Color, 8);
      LSB := Color and 16#FF#;
      -- bounds check
      if ((X >= SSD1351_WIDTH) or (Y >= SSD1351_HEIGHT)) then
         return;
      end if;
      if ((Y + H) > SSD1351_HEIGHT) then
         H := SSD1351_HEIGHT - Y - 1;
      end if;
      if ((X + W) > SSD1351_WIDTH) then
         W := SSD1351_WIDTH - X - 1;
      end if;

      Send_Command (SSD1351_CMD_SETCOLUMN);
      Send_Data (X);
      Send_Data (X + W - 1);
      Send_Command (SSD1351_CMD_SETROW);
      Send_Data (Y);
      Send_Data (Y + H - 1);
      -- fill
      Send_Command (SSD1351_CMD_WRITERAM);
      for I in Hword range 0 .. ((Hword (W) * Hword(H)) - 1) loop
         Send_Data (Byte (MSB));
         Send_Data (Byte (LSB));
      end loop;
      Send_Command (SSD1351_CMD_NOP);  -- end RAM write
   end Raw_Fill_Rect;


   ---------------------------------------------------------------------------
   -- Fill_Rect
   -- Draws a filled rectangle using HW acceleration
   ---------------------------------------------------------------------------
   procedure Fill_Rect (X0    : in Byte;
                        Y0    : in Byte;
                        Wt    : in Byte;
                        Ht    : in Byte;
                        Color : in Hword) is
      X : Byte := X0;
      Y : Byte := Y0;
      W : Byte := Wt;
      H : Byte := Ht;
   begin
      -- transform X and Y based on current rotation
      case Rotation is
         when Portrait1  =>  -- default, no rotation
            Raw_Fill_Rect (X, Y, W, H, Color);
         when Landscape1 =>  -- rotated 90 degrees clockwise
            Swap (X, Y);
            X := WIDTH - X - H;
            Raw_Fill_Rect (X, Y, H, W, Color); -- swap H and W
         when Portrait2  =>  -- rotated 180 degrees clockwise
            X := WIDTH - X - W;
            Y := HEIGHT - Y - H;
            Raw_Fill_Rect (X, Y, W, H, Color);
         when Landscape2 =>  -- rotated 270 degrees clockwise
            swap(X, Y);
            Y := HEIGHT - Y - W;
            Raw_Fill_Rect (X, Y, H, W, Color);  -- swap H and W
      end case;
   end Fill_Rect;


   ---------------------------------------------------------------------------
   -- Raw_Fast_HLine
   -- Draw a horizontal line ignoring any screen rotation.
   ---------------------------------------------------------------------------
   procedure Raw_Fast_HLine (X     : in Byte;
                             Y     : in Byte;
                             W     : in out Byte;
                             Color : in Hword) is
      MSB : HWord;
      LSB : HWord;
   begin
      MSB := Shift_Right (Color, 8);
      LSB := Color and 16#FF#;
      -- bounds check
      if ((X >= SSD1351_WIDTH) or (y >= SSD1351_HEIGHT)) then
         return;
      end if;
      if (X + W > SSD1351_WIDTH) then
         W := SSD1351_WIDTH - X - 1;
      end if;

      Send_Command (SSD1351_CMD_SETCOLUMN);
      Send_Data (X);
      Send_Data (X + W - 1);
      Send_Command (SSD1351_CMD_SETROW);
      Send_Data (Y);
      Send_Data (Y);
      -- draw
      Send_Command (SSD1351_CMD_WRITERAM);
      for I in 0 .. (W - 1) loop
         Send_Data (Byte (MSB));
         Send_Data (Byte (LSB));
      end loop;
      Send_Command (SSD1351_CMD_NOP);  -- end RAM write
   end Raw_Fast_HLine;


   ---------------------------------------------------------------------------
   -- Raw_Fast_VLine
   -- Draw a vertical line ignoring any screen rotation.
   ---------------------------------------------------------------------------
   procedure Raw_Fast_VLine (X     : in Byte;
                             Y     : in Byte;
                             H     : in out Byte;
                             Color : in Hword) is
      MSB : HWord;
      LSB : HWord;
   begin
      MSB := Shift_Right (Color, 8);
      LSB := Color and 16#FF#;
      -- bounds check
      if ((X >= SSD1351_WIDTH) or (y >= SSD1351_HEIGHT)) then
         return;
      end if;
      if (Y + H > SSD1351_HEIGHT) then
         H := SSD1351_HEIGHT - Y - 1;
      end if;

      Send_Command (SSD1351_CMD_SETCOLUMN);
      Send_Data (X);
      Send_Data (X);
      Send_Command (SSD1351_CMD_SETROW);
      Send_Data (Y);
      Send_Data (Y + H - 1);
      -- draw
      Send_Command (SSD1351_CMD_WRITERAM);
      for I in 0 .. (H - 1) loop
         Send_Data (Byte (MSB));
         Send_Data (Byte (LSB));
      end loop;
      Send_Command (SSD1351_CMD_NOP);  -- end RAM write
   end Raw_Fast_VLine;


   ---------------------------------------------------------------------------
   -- Draw_Fast_HLine
   -- Draw a horizontal line using any screen rotation.
   ---------------------------------------------------------------------------
   procedure Draw_Fast_HLine (X0    : in Byte;
                              Y0    : in Byte;
                              W     : in Byte;
                              Color : in Hword) is
      X  : Byte := X0;
      Y  : Byte := Y0;
      Wt : Byte := W;
   begin
      -- transform X and Y based on current rotation
      case Rotation is
         when Portrait1  =>  -- default, no rotation
            Raw_Fast_HLine (X, Y, Wt, Color);
         when Landscape1 =>  -- rotated 90 degrees clockwise
            Swap (X, Y);
            X := WIDTH - X - 1;
            Raw_Fast_VLine (X, Y, Wt, Color);
         when Portrait2  =>  -- rotated 180 degrees clockwise
            X := WIDTH - X - W;
            Y := HEIGHT - Y - 1;
            Raw_Fast_HLine (X, Y, Wt, Color);
         when Landscape2 =>  -- rotated 270 degrees clockwise
            swap(X, Y);
            Y := HEIGHT - Y - W;
            Raw_Fast_VLine (X, Y, Wt, Color);
      end case;
   end Draw_Fast_HLine;


   ---------------------------------------------------------------------------
   -- Draw_Fast_VLine
   -- Draw a vertical line using any screen rotation.
   ---------------------------------------------------------------------------
   procedure Draw_Fast_VLine (X0    : in Byte;
                              Y0    : in Byte;
                              H     : in Byte;
                              Color : in Hword) is
      X  : Byte := X0;
      Y  : Byte := Y0;
      Ht : Byte := H;
   begin
      -- transform X and Y based on current rotation
      case Rotation is
         when Portrait1  =>  -- default, no rotation
            Raw_Fast_VLine (X, Y, Ht, Color);
         when Landscape1 =>  -- rotated 90 degrees clockwise
            Swap (X, Y);
            X := WIDTH - X - Ht;
            Raw_Fast_HLine (X, Y, Ht, Color);
         when Portrait2  =>  -- rotated 180 degrees clockwise
            X := WIDTH - X - 1;
            Y := HEIGHT - Y - Ht;
            Raw_Fast_VLine (X, Y, Ht, Color);
         when Landscape2 =>  -- rotated 270 degrees clockwise
            swap(X, Y);
            Y := HEIGHT - Y - 1;
            Raw_Fast_HLine (X, Y, Ht, Color);
      end case;
   end Draw_Fast_VLine;



   ---------------------------------------------------------------------------
   -- Put_Char (for characters in a string)
   ---------------------------------------------------------------------------
   procedure Put_Char (Char    : in Character;
                       ForeGnd : in HWord;
                       BackGnd : in HWord) is

      B  : Byte := 0;

   begin
      -- use global X and Y coordinates previously set
      if ((X >= SSD1351_Width)   or    -- Clip right
          (Y >= SSD1351_Height)) then  -- Clip bottom
         return;
      end if;

      I_Loop:
      for I in Byte range 0 .. Font_Width loop
         if (I = Font_Width) then
            B := 16#00#;
         else
            B := Font5x7 ((Character'Pos (Char) * Natural (Font_Width))
                           + Natural (I));
         end if;
         -- look at each bit in the font column byte, except last
         -- (since height is only 7)
         J_Loop:
         for J in Byte range 0 .. (Font_Height - 1) loop
            if (B and 16#01#) = 1 then
               if (Font_Size = 1) then  -- default size
                  Draw_Pixel (X + I, Y + J, ForeGnd);
               else  -- big size
                  Fill_Rect (X + (I * Font_Size), Y +
                             (J * Font_Size), Font_Size,
                             Font_Size, ForeGnd);
               end if;
            elsif (BackGnd /= ForeGnd) then
               if (Font_Size = 1) then  -- default size
                 Draw_Pixel (X + I, Y + J, BackGnd);
               else   -- big size
                  Fill_Rect(X + I * Font_Size, Y + J * Font_Size,
                            Font_Size, Font_Size, BackGnd);
               end if;
            end if;
            B := Shift_Right (B, 1);
         end loop J_Loop;
      end loop I_Loop;

      X := X + (Font_Width * Font_Size) + 1;
   end Put_Char;


   ---------------------------------------------------------------------------
   -- Put_Char (for individual character)
   ---------------------------------------------------------------------------
   procedure Put_Char (X1      : in Byte;
                       Y1      : in Byte;
                       Char    : in Character;
                       ForeGnd : in HWord;
                       BackGnd : in HWord;
                       FSize   : in Font_Size_Type := 1) is
   begin
      -- set global X and Y coordinates & Font_Size
      X         := X1;
      Y         := Y1;
      Font_Size := FSize;
      Put_Char (Char    => Char,
                ForeGnd => ForeGnd,
                BackGnd => BackGnd);
   end Put_Char;


   ---------------------------------------------------------------------------
   -- Put_String
   ---------------------------------------------------------------------------
   procedure Put_String (X1      : in Byte;
                         Y1      : in Byte;
                         Str     : in String;
                         ForeGnd : in HWord;
                         BackGnd : in HWord;
                         FSize   : in Font_Size_Type := 1) is

      Start_X   : constant Byte  := X;
      I         : Positive       := Str'First;
      FH        : constant Byte  := Font_Height;

   begin
      -- set global X and Y coordinates & Font_Size
      X         := X1;
      Y         := Y1;
      Font_Size := FSize;

      while (I <= Str'Last) loop
         -- check for new line
         if Str (I) = ASCII.LF then
            Y := Y + FH + 1;
            -- if off page, go to top of page
            if Y > SSD1351_Height then
               Y := 0;
            end if;
            -- if after LF is also CR, than go to the left of the screen
            if Str (I+1) = ASCII.CR then
               X := 0;
               I := I + 1;
            else
               X := Start_X;
            end if;
            I := I + 1;
         elsif Str (I) = ASCII.CR then
            I := I + 1;
         end if;
         Put_Char ((Str (I)), ForeGnd, BackGnd);
         I := I + 1;
      end loop;
   end Put_String;


   ---------------------------------------------------------------------------
   -- Put_String (short form - uses default foreground and background colors)
   ---------------------------------------------------------------------------
   procedure Put_String (X1  : in Byte;
                         Y1  : in Byte;
                         Str : in String) is

   begin
      OLED1351.Put_String (X1      => X1,
                           Y1      => Y1,
                           Str     => Str,
                           ForeGnd => Selected_ForeGnd,
                           BackGnd => Selected_BackGnd);
   end Put_String;


   ---------------------------------------------------------------------------
   -- Get_String_Size
   ---------------------------------------------------------------------------
   procedure Get_String_Size (Str    : in String;
                              Width  : out Byte;
                              Height : out Byte) is
      W : Byte := 0;
   begin
      Height := Font_Height;
      for I in Str'First .. Str'Last loop
         W := W + Font_Width;
      end loop;
      Width := W;
   end Get_String_Size;


   ---------------------------------------------------------------------------
   -- Draw_Line
   -- Bresenham's algorithm
   ---------------------------------------------------------------------------
   procedure Draw_Line (X0    : in Byte;
                        Y0    : in Byte;
                        X1    : in Byte;
                        Y1    : in Byte;
                        Color : in HWord) is

      X_0    : Byte    := X0;
      X_1    : Byte    := X1;
      Y_0    : Byte    := Y0;
      Y_1    : Byte    := Y1;
      Steep  : constant Boolean := abs (Integer (Y_1) - Integer (Y_0)) >
                                   abs (Integer (X_1) - Integer (X_0));
      Dx     : Integer := 0;
      Dy     : Integer := 0;
      Y_Step : Integer;
      Err    : Integer := 0;

   begin
      -- Check for overflow
      if (X_0 >= SSD1351_Width) then
         X_0 := SSD1351_Width - 1;
      end if;
      if (X_1 >= SSD1351_Width) then
         X_1 := SSD1351_Width - 1;
      end if;
      if (Y_0 >= SSD1351_Height) then
         Y_0 := SSD1351_Height - 1;
      end if;
      if (Y_1 >= SSD1351_Height) then
         Y_1 := SSD1351_Height - 1;
      end if;

      if Steep then
         Swap (X_0, Y_0);
         Swap (X_1, Y_1);
      end if;

      if (X_0 > X_1) then
         Swap (X_0, X_1);
         Swap (Y_0, Y_1);
      end if;

      Dx  := Integer (X_1) - Integer (X_0);
      Dy  := abs (Integer (Y_1) - Integer (Y_0));
      Err := Dx / 2;

      if (Y_0 < Y_1) then
         Y_Step := 1;
      else
         Y_Step := -1;
      end if;

      while X_0 <= X_1 loop
         if Steep then
            Draw_Pixel (Y_0, X_0, Color);
         else
            Draw_Pixel (X_0, Y_0, Color);
         end if;
         Err := Err - Dy;
         if Err < 0 then
            Y_0 := Byte (Integer (Y_0) + Y_Step);
            Err := Err + Dx;
         end if;
         X_0 := X_0 + 1;
      end loop;
   end Draw_Line;


   ---------------------------------------------------------------------------
   -- Draw_Rectangle
   ---------------------------------------------------------------------------
   procedure Draw_Rectangle (X0    : in Byte;
                             Y0    : in Byte;
                             Wt    : in Byte;
                             Ht    : in Byte;
                             Color : in HWord) is
      X : constant Byte := X0;
      Y : constant Byte := Y0;
      W : constant Byte := Wt;
      H : constant Byte := Ht;
   begin
      Draw_Fast_HLine (X, Y, W, Color);
      Draw_Fast_HLine (X, Y + H - 1, W, Color);
      Draw_Fast_VLine (X, Y, H, Color);
      Draw_Fast_VLine (X + W - 1, Y, H, Color);
   end Draw_Rectangle;


   ---------------------------------------------------------------------------
   -- Draw_Triangle
   ---------------------------------------------------------------------------
   procedure Draw_Triangle (X0    : in Byte;
                            Y0    : in Byte;
                            X1    : in Byte;
                            Y1    : in Byte;
                            X2    : in Byte;
                            Y2    : in Byte;
                            Color : in HWord) is

   begin
      Draw_Line (X0, Y0, X1, Y1, Color);
      Draw_Line (X1, Y1, X2, Y2, Color);
      Draw_Line (X2, Y2, X0, Y0, Color);
   end Draw_Triangle;


   ---------------------------------------------------------------------------
   -- Fill_Triangle
   ---------------------------------------------------------------------------
   procedure Fill_Triangle (X0    : in Byte;
                            Y0    : in Byte;
                            X1    : in Byte;
                            Y1    : in Byte;
                            X2    : in Byte;
                            Y2    : in Byte;
                            Color : in HWord) is
      A    : Integer  := 0;
      B    : Integer  := 0;
      Yi   : Integer  := 0;
      X_0  : Byte     := X0;
      Y_0  : Byte     := Y0;
      X_1  : Byte     := X1;
      Y_1  : Byte     := Y1;
      X_2  : Byte     := X2;
      Y_2  : Byte     := Y2;
      Last : Byte     := 0;
      Dx01 : Integer  := 0;
      Dy01 : Integer  := 0;
      Dx02 : Integer  := 0;
      Dy02 : Integer  := 0;
      Dx12 : Integer  := 0;
      Dy12 : Integer  := 0;
      SA   : Integer  := 0;
      SB   : Integer  := 0;
   begin
      -- Sort coordinates by Y order (Y2 >= Y1 >= Y0)
      if (Y_0 > Y_1) then
         Swap (Y_0, Y_1);
         Swap (X_0, X_1);
      end if;
      if (Y_1 > Y_2) then
         Swap (Y_2, Y_1);
         Swap (X_2, X_1);
      end if;
      if (Y_0 > Y_1) then
         Swap (Y_0, Y_1);
         Swap (X_0, X_1);
      end if;

      -- Handle awkward all-on-same-line case as its own thing
      if (Y_0 = Y_2) then
         A := Integer (X_0);
         B := Integer (X_0);
         if (X_1 < Byte (A)) then
            A := Integer (X_1);
         elsif (X_1 > Byte (B)) then
            B := Integer (X_1);
         end if;
         if (X_2 < Byte (A)) then
            A := Integer (X_2);
         elsif (X_2 > Byte (B)) then
            B := Integer (X_2);
         end if;
         Draw_Fast_HLine (Byte (A), Y_0, Byte (B-A+1), Color);
         return;
      end if;

      Dx01 := Integer (X_1) - Integer (X_0);
      Dy01 := Integer (Y_1) - Integer (Y_0);
      Dx02 := Integer (X_2) - Integer (X_0);
      Dy02 := Integer (Y_2) - Integer (Y_0);
      Dx12 := Integer (X_2) - Integer (X_1);
      Dy12 := Integer (Y_2) - Integer (Y_1);
      SA   := 0;
      SB   := 0;

      -- For upper part of triangle, find scanline crossings for segments
      -- 0-1 and 0-2.  If y1=y2 (flat-bottomed triangle), the scanline y1
      -- is included here (and second loop will be skipped, avoiding a /0
      -- error there), otherwise scanline y1 is skipped here and handled
      -- in the second loop...which also avoids a /0 error here if y0=y1
      -- (flat-topped triangle).
      if (Y_1 = Y_2) then
         Last := Y_1;   -- Include Y_1 scanline
      else
         Last := Y_1-1; -- Skip it
      end if;

      Yi := Integer (Y_0);
      for Y in Byte range Y_0 .. Last loop
         A  := Integer (X_0) + SA / Dy01;
         B  := Integer (X_0) + SB / Dy02;
         SA := SA + Dx01;
         SB := SB + Dx02;
         -- longhand:
         -- a = x0 + (x1 - x0) * (y - y0) / (y1 - y0);
         -- b = x0 + (x2 - x0) * (y - y0) / (y2 - y0);
         if (A > B) then
            Swap (A, B);
         end if;
         Draw_Fast_HLine (Byte (A), Y, Byte (B-A+1), Color);
         Yi := Integer (Y);
      end loop;

      -- For lower part of triangle, find scanline crossings for segments
      -- 0-2 and 1-2.  This loop is skipped if y1=y2.
      SA := Dx12 * (Yi - Integer (Y_1));
      SB := Dx02 * (Yi - Integer (Y_0));
      while (Yi <= Integer (Y_2)) loop
         A  := Integer (X_1) + SA / Dy12;
         B  := Integer (X_0) + SB / Dy02;
         SA := SA + Dx12;
         SB := SB + Dx02;
         -- longhand:
         -- a = x1 + (x2 - x1) * (yi - y1) / (y2 - y1);
         -- b = x0 + (x2 - x0) * (yi - y0) / (y2 - y0);
         if (A > B) then
            Swap (A, B);
         end if;
         Draw_Fast_HLine (Byte (A), Byte (Yi), Byte (B-A+1), Color);
         Yi := Yi + 1;
      end loop;

   exception

      when others =>
         return;

   end Fill_Triangle;


   ---------------------------------------------------------------------------
   -- Draw_Filled_Rectangle
   ---------------------------------------------------------------------------
   procedure Draw_Filled_Rectangle (X0    : in Byte;
                                    Y0    : in Byte;
                                    Wt    : in Byte;
                                    Ht    : in Byte;
                                    Color : in HWord) is
      X : constant Byte := X0;
      Y : constant Byte := Y0;
      W : constant Byte := Wt;
      H : constant Byte := Ht;
   begin
      for I in X .. (X + W - 1) loop
         Draw_Fast_VLine (I, Y, H, Color);
      end loop;
   end Draw_Filled_Rectangle;



   ---------------------------------------------------------------------------
   -- Draw_Rounded_Rectangle
   ---------------------------------------------------------------------------
   procedure Draw_Rounded_Rectangle (X     : in Byte;
                                     Y     : in Byte;
                                     W     : in Byte;
                                     H     : in Byte;
                                     R     : in Byte;
                                     Color : in HWord) is
   begin
      Draw_Fast_HLine (X+R,   Y,     W-2*R, Color); -- Top
      Draw_Fast_HLine (X+R,   Y+H-1, W-2*R, Color); -- Bottom
      Draw_Fast_VLine (X,     Y+R,   H-2*R, Color); -- Left
      Draw_Fast_VLine (X+W-1, Y+R,   H-2*R, Color); -- Right
      -- draw four corners
      Draw_Circle_Helper (X+R    , Y+R    , R, A, Color);
      Draw_Circle_Helper (X+W-R-1, Y+R    , R, B, Color);
      Draw_Circle_Helper (X+W-R-1, Y+H-R-1, R, C, Color);
      Draw_Circle_Helper (X+R    , Y+H-R-1, R, D, Color);
   end Draw_Rounded_Rectangle;


   ---------------------------------------------------------------------------
   -- Fill_Rounded_Rectangle
   ---------------------------------------------------------------------------
   procedure Fill_Rounded_Rectangle (X     : in Byte;
                                     Y     : in Byte;
                                     W     : in Byte;
                                     H     : in Byte;
                                     R     : in Byte;
                                     Color : in HWord) is
   begin
      Fill_Rect (X+R, Y, W-2*R, H, Color);

      -- draw four corners
      Fill_Circle_Helper (X+W-R-1, Y+R, R, A, H-2*R-1, Color);
      Fill_Circle_Helper (X+R    , Y+R, R, B, H-2*R-1, Color);
   end Fill_Rounded_Rectangle;


   ---------------------------------------------------------------------------
   -- Draw_Circle
   ---------------------------------------------------------------------------
   procedure Draw_Circle (X0     : in Byte;
                          Y0     : in Byte;
                          Radius : in Byte;
                          Color  : in HWord) is

      X_0   : constant Integer := Integer (X0);
      Y_0   : constant Integer := Integer (Y0);
      F     : Integer := 1 - Integer (Radius);
      DdF_X : Integer := 1;
      DdF_Y : Integer := (-2) * Integer (Radius);
      X     : Integer := 0;
      R     : Integer := Integer (Radius);

   begin
      Draw_Pixel (X0, Y0 + Radius, Color);
      Draw_Pixel (X0, Y0 - Radius, Color);
      Draw_Pixel (X0 + Radius, Y0, Color);
      Draw_Pixel (X0 - Radius, Y0, Color);

      while (X < R) loop
         if (F >= 0) then
            R     := R - 1;
            DdF_Y := DdF_Y + 2;
            F     := F + DdF_Y;
         end if;
        X     := X + 1;
        DdF_X := DdF_X + 2;
        F     := F + DdF_X;

        Draw_Pixel (Byte (X_0 + X), Byte (Y_0 + R), Color);
        Draw_Pixel (Byte (X_0 - X), Byte (Y_0 + R), Color);
        Draw_Pixel (Byte (X_0 + X), Byte (Y_0 - R), Color);
        Draw_Pixel (Byte (X_0 - X), Byte (Y_0 - R), Color);

        Draw_Pixel (Byte (X_0 + R), Byte (Y_0 + X), Color);
        Draw_Pixel (Byte (X_0 - R), Byte (Y_0 + X), Color);
        Draw_Pixel (Byte (X_0 + R), Byte (Y_0 - X), Color);
        Draw_Pixel (Byte (X_0 - R), Byte (Y_0 - X), Color);
      end loop;
   end Draw_Circle;


   ---------------------------------------------------------------------------
   -- Draw_Circle_Helper
   ---------------------------------------------------------------------------
   procedure Draw_Circle_Helper (X0          : in Byte;
                                 Y0          : in Byte;
                                 Radius      : in Byte;
                                 Corner_Name : in Corner_Type;
                                 Color       : in HWord) is

      X_0   : constant Integer := Integer (X0);
      Y_0   : constant Integer := Integer (Y0);
      F     : Integer := 1 - Integer (Radius);
      DdF_X : Integer := 1;
      DdF_Y : Integer := (-2) * Integer (Radius);
      X     : Integer := 0;
      R     : Integer := Integer (Radius);

   begin
      while (X < R) loop
         if (F >= 0) then
            R     := R - 1;
            DdF_Y := DdF_Y + 2;
            F     := F + DdF_Y;
         end if;
        X     := X + 1;
        DdF_X := DdF_X + 2;
        F     := F + DdF_X;
        if Corner_Name = C then
           Draw_Pixel (Byte (X_0 + X), Byte (Y_0 + R), Color);
           Draw_Pixel (Byte (X_0 + R), Byte (Y_0 + X), Color);
        end if;
        if Corner_Name = B then
           Draw_Pixel (Byte (X_0 + X), Byte (Y_0 - R), Color);
           Draw_Pixel (Byte (X_0 + R), Byte (Y_0 - X), Color);
        end if;
        if Corner_Name = D then
           Draw_Pixel (Byte (X_0 - R), Byte (Y_0 + X), Color);
           Draw_Pixel (Byte (X_0 - X), Byte (Y_0 + R), Color);
        end if;
        if Corner_Name = A then
           Draw_Pixel (Byte (X_0 - R), Byte (Y_0 - X), Color);
           Draw_Pixel (Byte (X_0 - X), Byte (Y_0 - R), Color);
        end if;
      end loop;
   end Draw_Circle_Helper;


   ---------------------------------------------------------------------------
   -- Fill_Circle_Helper
   -- used to do circles and round rectangles
   ---------------------------------------------------------------------------
   procedure Fill_Circle_Helper (X0          : in Byte;
                                 Y0          : in Byte;
                                 Radius      : in Byte;
                                 Corner_Name : in Corner_Type;
                                 The_Delta   : in Byte;
                                 Color       : in HWord) is

      X_0   : constant Integer := Integer (X0);
      Y_0   : constant Integer := Integer (Y0);
      F     : Integer := 1 - Integer (Radius);
      DdF_X : Integer := 1;
      DdF_Y : Integer := (-2) * Integer (Radius);
      X     : Integer := 0;
      R     : Integer := Integer (Radius);
      H     : Byte;

   begin
      while (X < R) loop
         if (F >= 0) then
            R     := R - 1;
            DdF_Y := DdF_Y + 2;
            F     := F + DdF_Y;
         end if;
        X     := X + 1;
        DdF_X := DdF_X + 2;
        F     := F + DdF_X;
         if Corner_Name = A then
            H := 2*Byte (R)+1+The_Delta;
            Draw_Fast_VLine (Byte (X_0+X), Byte (Y_0-R), H, Color);
            H := 2*Byte (X)+1+The_Delta;
            Draw_Fast_VLine (Byte (X_0+R), Byte (Y_0-X), H, Color);
         end if;
         if Corner_Name = B then
            H := 2*Byte (R)+1+The_Delta;
            Draw_Fast_VLine (Byte (X_0-X), Byte (Y_0-R), H, Color);
            H := 2*Byte (X)+1+The_Delta;
            Draw_Fast_VLine (Byte (X_0-R), Byte (Y_0-X), H, Color);
        end if;
      end loop;
   end Fill_Circle_Helper;


   ---------------------------------------------------------------------------
   -- Draw_Filled_Circle
   ---------------------------------------------------------------------------
   procedure Draw_Filled_Circle (X0     : in Byte;
                                 Y0     : in Byte;
                                 Radius : in Byte;
                                 Color  : in HWord) is

      X_0   : constant Integer := Integer (X0);
      Y_0   : constant Integer := Integer (Y0);
      F     : Integer := 1 - Integer (Radius);
      DdF_X : Integer := 1;
      DdF_Y : Integer := (-2) * Integer (Radius);
      X     : Integer := 0;
      R     : Integer := Integer (Radius);

   begin
      Draw_Pixel (X0, Y0 + Radius, Color);
      Draw_Pixel (X0, Y0 - Radius, Color);
      Draw_Pixel (X0 + Radius, Y0, Color);
      Draw_Pixel (X0 - Radius, Y0, Color);
      Draw_Line (X0 - Radius, Y0, X0 + Radius, Y0, Color);

      while (X < R) loop
         if (F >= 0) then
            R     := R - 1;
            DdF_Y := DdF_Y + 2;
            F     := F + DdF_Y;
         end if;
        X     := X + 1;
        DdF_X := DdF_X + 2;
        F     := F + DdF_X;

        Draw_Line (Byte (X_0 - X), Byte (Y_0 + R),
                   Byte (X_0 + X), Byte (Y_0 + R), Color);
        Draw_Line (Byte (X_0 + X), Byte (Y_0 - R),
                   Byte (X_0 - X), Byte (Y_0 - R), Color);
        Draw_Line (Byte (X_0 + R), Byte (Y_0 + X),
                   Byte (X_0 - R), Byte (Y_0 + X), Color);
        Draw_Line (Byte (X_0 + R), Byte (Y_0 - X),
                   Byte (X_0 - R), Byte (Y_0 - X), Color);

      end loop;
   end Draw_Filled_Circle;


begin
   null;
end OLED1351;

