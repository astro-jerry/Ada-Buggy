------------------------------------------------------------------------------
-- buggy.ads
--
-- Handles control of Buggy chassis using Clicker 2 (or Mikromedia board)
--
-- Created by Jerry Petrey 02-07-15
-- Last Modification:      02-08-16
------------------------------------------------------------------------------

package Buggy is

   type HeadLight_Type is (Low, High, Off);

   type State_Type is (On, Off);

   type Motor_State_Type is (Forward, Backward, Off);

   type Motor_Type is (Left, Right);

   subtype Speed_Type is Natural range 0 .. 100; -- percent


   ---------------------------------------------------------------------------
   -- subprograms
   ---------------------------------------------------------------------------

   procedure Setup_Buggy;

   procedure Set_HeadLights (Which : in HeadLight_Type);

   procedure Set_BrakeLight (State : in State_Type);

   procedure Set_Left_Turn (State : in State_Type);

   procedure Set_Right_Turn (State : in State_Type);

   procedure Set_Motor (Motor : in Motor_Type;
                        State : in Motor_State_Type;
                        Speed : in Speed_Type := 0);

   procedure Signal_Left_Turn;

   procedure Signal_Right_Turn;

   procedure Rotate_Left (Speed : in Speed_Type;
                          Amt   : in Natural);

   procedure Rotate_Right (Speed : in Speed_Type;
                           Amt   : in Natural);

   procedure Move_Forward (Speed : in Speed_Type);

   procedure Move_Forward (Speed : in Speed_Type;
                           Amt   : in Natural);

   procedure Move_Backward (Speed : in Speed_Type);

   procedure Move_Backward (Speed : in Speed_Type;
                            Amt   : in Natural);

   procedure Stop;

   procedure Flash_Headlights;


end Buggy;
