------------------------------------------------------------------------------
-- oled1351.ads
--
-- Created by Jerry Petrey 03-19-15
-- Last Modification:      06-12-16
------------------------------------------------------------------------------
with HAL;           use HAL;

package OLED1351 is

   pragma Elaborate_Body;

   -- LCD settings
   WIDTH                 : constant := 128;
   HEIGHT                : constant := 128;
   PIXELS                : constant := WIDTH * HEIGHT;
   -- these could change if another similar but rectangular display was used
   SSD1351_WIDTH         : Byte     := WIDTH;
   SSD1351_HEIGHT        : Byte     := HEIGHT;

   subtype Font_Size_Type is Byte range 1 .. 6;

   -- set after SPI & LCD initialized
   LCD_Initialized       : Boolean  := False;

   type Corner_Type is (A, B, C, D);
   for Corner_Type use (A => 1,
                        B => 2,
                        C => 4,
                        D => 8);

   -- Rotations
   type Rotation_Type is (Portrait1, Landscape1, Portrait2,  Landscape2);
   for Rotation_Type use (Portrait1  => 0,  -- 0,0 at UL under the SI pin
                          Landscape1 => 1,
                          Portrait2  => 2,
                          Landscape2 => 3);

   -- Colors - Mode RGB
   -- NOTE: chip interprets color values right to left, so for
   -- example, GRAY is 16#7BEF# which is 1111 0111 1101 1110 <--
   --                                     F     E    B   7
   SSD1351_COLOR_WHITE     : constant := 16#FFFF#;
   SSD1351_COLOR_BLACK     : constant := 16#0000#;
   SSD1351_COLOR_RED       : constant := 16#F800#;
   SSD1351_COLOR_GREEN     : constant := 16#0400#;
   SSD1351_COLOR_LIME      : constant := 16#07E0#;
   SSD1351_COLOR_BLUE      : constant := 16#001F#;
   SSD1351_COLOR_STEELBLUE : constant := 16#4416#;
   SSD1351_COLOR_YELLOW    : constant := 16#FFE0#;
   SSD1351_COLOR_ORANGE    : constant := 16#FD20#;
   SSD1351_COLOR_AQUA      : constant := 16#07FF#;
   SSD1351_COLOR_MAGENTA   : constant := 16#F81F#;
   SSD1351_COLOR_GRAY      : constant := 16#8410#;
   SSD1351_COLOR_BROWN     : constant := 16#A145#;
   SSD1351_COLOR_CORAL     : constant := 16#FBEA#;
   SSD1351_COLOR_CYAN      : constant := 16#07FF#;
   SSD1351_COLOR_DBLUE     : constant := 16#0011#;
   SSD1351_COLOR_DGREEN    : constant := 16#0320#;
   SSD1351_COLOR_DRED      : constant := 16#8800#;
   SSD1351_COLOR_GOLD      : constant := 16#FEA0#;
   SSD1351_COLOR_LBLUE     : constant := 16#AEDC#;
   SSD1351_COLOR_MAROON    : constant := 16#8000#;
   SSD1351_COLOR_PINK      : constant := 16#FE19#;
   SSD1351_COLOR_PURPLE    : constant := 16#8010#;
   SSD1351_COLOR_SILVER    : constant := 16#C618#;
   SSD1351_COLOR_SKYBLUE   : constant := 16#867D#;
   SSD1351_COLOR_TEAL      : constant := 16#8080#;
   SSD1351_COLOR_TAN       : constant := 16#D5B1#;
   SSD1351_COLOR_VIOLET    : constant := 16#EC1D#;
   SSD1351_COLOR_SEAGREEN  : constant := 16#2C4A#;
   SSD1351_COLOR_LGREEN    : constant := 16#9FD3#;
   SSD1351_COLOR_BEIGE     : constant := 16#7FBB#;
   SSD1351_COLOR_CRIMSON   : constant := 16#D8A7#;

   -- SSD1351 Commands
   SSD1351_CMD_SETCOLUMN       : constant := 16#15#;
   SSD1351_CMD_SETROW          : constant := 16#75#;
   SSD1351_CMD_WRITERAM        : constant := 16#5C#;
   SSD1351_CMD_READRAM         : constant := 16#5D#;
   SSD1351_CMD_SETREMAP        : constant := 16#A0#;
   SSD1351_CMD_STARTLINE       : constant := 16#A1#;
   SSD1351_CMD_DISPLAYOFFSET   : constant := 16#A2#;
   SSD1351_CMD_DISPLAYALLOFF   : constant := 16#A4#;
   SSD1351_CMD_DISPLAYALLON    : constant := 16#A5#;
   SSD1351_CMD_NORMALDISPLAY   : constant := 16#A6#;
   SSD1351_CMD_INVERTDISPLAY   : constant := 16#A7#;
   SSD1351_CMD_FUNCTIONSELECT  : constant := 16#AB#;
   SSD1351_CMD_DISPLAYOFF      : constant := 16#AE#;
   SSD1351_CMD_DISPLAYON       : constant := 16#AF#;
   SSD1351_CMD_PRECHARGE       : constant := 16#B1#;
   SSD1351_CMD_DISPLAYENHANCE  : constant := 16#B2#;
   SSD1351_CMD_CLOCKDIV        : constant := 16#B3#;
   SSD1351_CMD_SETVSL          : constant := 16#B4#;
   SSD1351_CMD_SETGPIO         : constant := 16#B5#;
   SSD1351_CMD_PRECHARGE2      : constant := 16#B6#;
   SSD1351_CMD_SETGRAY         : constant := 16#B8#;
   SSD1351_CMD_USELUT          : constant := 16#B9#;
   SSD1351_CMD_PRECHARGELEVEL  : constant := 16#BB#;
   SSD1351_CMD_VCOMH           : constant := 16#BE#;
   SSD1351_CMD_CONTRASTABC     : constant := 16#C1#;
   SSD1351_CMD_CONTRASTMASTER  : constant := 16#C7#;
   SSD1351_CMD_MUXRATIO        : constant := 16#CA#;
   SSD1351_CMD_NOP             : constant := 16#D1#;
   SSD1351_CMD_COMMANDLOCK     : constant := 16#FD#;
   SSD1351_CMD_HORIZSCROLL     : constant := 16#96#;
   SSD1351_CMD_STOPSCROLL      : constant := 16#9E#;
   SSD1351_CMD_STARTSCROLL     : constant := 16#9F#;


   -- subprograms
   procedure Set_Rotation (Value : in Rotation_Type);

   function Get_Rotation return Rotation_Type;

   procedure Set_Default_ForeGnd_Color (Color : in HWord);

   procedure Set_Default_BackGnd_Color (Color : in HWord);

   procedure Invert_Display (Invert : in Boolean);

   procedure Init_LCD;

   procedure Send_Command (Cmd : in Byte);

   procedure Send_Data (Data : in Byte);

   procedure Go_To (X : in Byte;
                    Y : in Byte);

   procedure Draw_Pixel (X0    : in Byte;
                         Y0    : in Byte;
                         Color : in HWord);

   procedure Fill_Screen (Color : in HWord);

   procedure Put_String (X1      : in Byte;
                         Y1      : in Byte;
                         Str     : in String;
                         ForeGnd : in HWord;
                         BackGnd : in HWord;
                         FSize   : in Font_Size_Type := 1);

   procedure Put_String (X1  : in Byte;
                         Y1  : in Byte;
                         Str : in String);

   procedure Get_String_Size (Str    : in String;
                              Width  : out Byte;
                              Height : out Byte);

   procedure Put_Char (X1      : in Byte;
                       Y1      : in Byte;
                       Char    : in Character;
                       ForeGnd : in HWord;
                       BackGnd : in HWord;
                       FSize   : in Font_Size_Type := 1);

   procedure Draw_Fast_HLine (X0    : in Byte;
                              Y0    : in Byte;
                              W     : in Byte;
                              Color : in Hword);

   procedure Draw_Fast_VLine (X0    : in Byte;
                              Y0    : in Byte;
                              H     : in Byte;
                              Color : in Hword);

   procedure Fill_Circle_Helper (X0          : in Byte;
                                 Y0          : in Byte;
                                 Radius      : in Byte;
                                 Corner_Name : in Corner_Type;
                                 The_Delta   : in Byte;
                                 Color       : in HWord);

   procedure Draw_Circle_Helper (X0          : in Byte;
                                 Y0          : in Byte;
                                 Radius      : in Byte;
                                 Corner_Name : in Corner_Type;
                                 Color       : in HWord);

   procedure Draw_Line (X0    : in Byte;
                        Y0    : in Byte;
                        X1    : in Byte;
                        Y1    : in Byte;
                        Color : in HWord);

   procedure Draw_Rectangle (X0    : in Byte;
                             Y0    : in Byte;
                             Wt    : in Byte;
                             Ht    : in Byte;
                             Color : in HWord);

   -- Draws a filled rectangle using HW acceleration
   procedure Fill_Rect (X0    : in Byte;
                        Y0    : in Byte;
                        Wt    : in Byte;
                        Ht    : in Byte;
                        Color : in Hword);

   procedure Draw_Filled_Rectangle (X0    : in Byte;
                                    Y0    : in Byte;
                                    Wt    : in Byte;
                                    Ht    : in Byte;
                                    Color : in HWord);

   procedure Draw_Rounded_Rectangle (X     : in Byte;
                                     Y     : in Byte;
                                     W     : in Byte;
                                     H     : in Byte;
                                     R     : in Byte;
                                     Color : in HWord);

   procedure Fill_Rounded_Rectangle (X     : in Byte;
                                     Y     : in Byte;
                                     W     : in Byte;
                                     H     : in Byte;
                                     R     : in Byte;
                                     Color : in HWord);

   procedure Draw_Triangle (X0    : in Byte;
                            Y0    : in Byte;
                            X1    : in Byte;
                            Y1    : in Byte;
                            X2    : in Byte;
                            Y2    : in Byte;
                            Color : in HWord);

   procedure Fill_Triangle (X0    : in Byte;
                            Y0    : in Byte;
                            X1    : in Byte;
                            Y1    : in Byte;
                            X2    : in Byte;
                            Y2    : in Byte;
                            Color : in HWord);

   procedure Draw_Circle (X0     : in Byte;
                          Y0     : in Byte;
                          Radius : in Byte;
                          Color  : in HWord);

   procedure Draw_Filled_Circle (X0     : in Byte;
                                 Y0     : in Byte;
                                 Radius : in Byte;
                                 Color  : in HWord);


end OLED1351;

