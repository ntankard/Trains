-- interrupt package for skeleton test program skel2 for trains
-- requires Swindows, Simrail2 (vsn 2.0.4 +), Raildefs, Halls2 &
-- Io_Ports for Simrail2.
--
-- Version: 1.0 extracted from skel2 for GNAT2008/9, 19-Feb-09
-- Version: 1.1 hide protected, 16-Mar-10
--
-- Copyright: Dr R. K Allen, faculty of ICT, Swinburne UT
--
with Swindows;

package Interrupt_Hdlr is

   procedure Init(Win : in Swindows.Window);

   procedure Install;  -- added v1.1

end Interrupt_Hdlr;
