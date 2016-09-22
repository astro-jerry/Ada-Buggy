------------------------------------------------------------------------------
-- Sharp_IR
--
-- Support for Sharp IR module.
--
-- Created by Jerry Petrey 05-23-15
-- Last Modification:      05-23-15
--
------------------------------------------------------------------------------

package Sharp_IR is

   pragma Elaborate_Body;

   ---------------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------------

   procedure Setup_IR;

   function IR_Object_Detected return Boolean;

end Sharp_IR;
