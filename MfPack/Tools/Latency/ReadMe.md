# Latency measuring tool for Windows

&#x20;

Version: 4.1.0

&#x20;

Description:  
This unit is designed to measure minimum, maximum and average execution times of a routine in MICROSECONDS.  
NOTE: You have to build a RELEASE version of your code including this code to get useful results.  
Running the code in the debugger will NOT give accurate results.

&#x20;

Usage:

1. Add .."/MfPack/Tools/Latency" to your application search path.
2. Add "MfPack.LatencyTool" in the uses clause.
3. Create the class in your application.
4. Call Initialize within the method where measurement is needed.
5. Call Start() just before the code to be measured.
6. Call Stop() immediately after the code been measured.
7. Call FreeAndNil() when not needed anymore.

&#x20;

NOTES:

* This release is updated for compiler version 17 up to 35.
* SDK version 10.0.28000.2705 (Windows 11)
* Requires Windows 10 22H2 or later.
* Minimum supported MfPack version: 4.1.0

&#x20;

Project: MFPack - Tools

&#x20;

Project location: https://github.com/FactoryXCode/MfPack 

&#x20;

First release date: 02/09/2024  
Final release date: 17/11/2026

&#x20;

Copyright © FactoryX. All rights reserved.
