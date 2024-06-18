# Audio latency measuring tool for Windows
Version: X 3.1.7

Description:
   This unit is designed to measure minimum, maximum and average execution times of a routine in MICROSECONDS.
   NOTE: You have to build a RELEASE version of your code including this code to get useful results.
         Running the code in the debugger will NOT give accurate results.

Usage:
  1) Add .."/MfPack/Tools/Latency" to your application search path.
  2) Add "MfPack.AudioLatencyTool" in the uses clause.
  3) Create the class in your application.
  4) Call Initialize within the method where measurment is needed.
  5) Call Start() just before the code to be measured.
  6) Call Stop() immediatly after the code been measured.
  7) Call FreeAndNil() when not needed anymore.


NOTES: 
 - This release is updated for compiler version 17 up to 35.
 - SDK version 10.0.22621.0 (Win 11)
 - Requires Windows 10 22H2 or later.
 - Minimum supported MfPack version: 3.1.6

Project: Media Foundation - MFPack - Tools
Project location: http://sourceforge.net/projects/MFPack

First release date: 29/05/2024
Final release date: 16/06/2024


Copyright © FactoryX. All rights reserved.
