CaptureEngine video capture sample
==================================

This sample demonstrates how to use the [Media Foundation](http://msdn.microsoft.com/en-us/library/windows/desktop/ms694197) [**CaptureEngine**](http://msdn.microsoft.com/en-us/library/windows/desktop/hh162749) to capture video from a capture device, such as a webcam.

Note: function TCaptureManager.GetCaptureDeviceCaps is not working very well. Somehow m_pSource (IMFCaptureSource) returns
-1072875854 ( $C00D36B2) "The request is invalid in the current state."

Operating system requirements
-----------------------------

Client

Windows 8.1

Server

Windows Server 2012 R2



