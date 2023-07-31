# Device Lost Sample
Version: X 3.1.5


NOTES: 
 - This release is updated for compiler version 17 up to 35.
 - SDK version 10.0.22621.0 (Win 11)
 - Requires Windows 7 or later.
 - Minimum supported MfPack version: 3.1.5

This example (based on the MfSimpleCapture example) shows, how you how to implement a deviceLoss handler
within your code.

Description

  This sample describes how to detect device loss when using a video capture device.
  It contains the following steps:

    1 Register for device notification.
    2 Get the Symbolic Link of the device.
    3 Implement a messagehandler to handle WM_DEVICECHANGE.
    4 Unregister for notification when you close your app.

Create FDeviceLoss object example:
   FDeviceLoss := TDeviceLoss.Create(Handle, DeviceEnumIndex, dmo_Message);
   Parameters:
   Handle = Application Window handle
   DeviceEnumIndex = The device index that has been picked from the MFEnumDevices activation pointer.
   dmo_Message = When a device is lost, FDeviceLoss sends a message to the Application Window.
   dmo_MessageBox = When a device is lost, FDeviceLoss shows a messagebox.
   dmo_Both = Send message and show messagebox.

Note:
   The Application Window must have a message handler to intercept the WM_DEVICELOST message when
   dmo_Message or dmo_Both are set.


Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
                  https://sourceforge.net/projects/MFPack

First release date: 25/02/2023
Final release date: 18/07/2023

Copyright © FactoryX. All rights reserved.