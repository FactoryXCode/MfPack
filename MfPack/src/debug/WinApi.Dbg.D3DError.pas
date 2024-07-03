//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.WinError32
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Returns code definitions of the D3D errors.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 31/07/2023 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/debug/error-handling-reference
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.Dbg.D3DError;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {WinApiDebug}
  WinApi.Dbg.WinHResultTools;

const
  _FACD3D = $876;
  MAKE_D3DHRESULT_R = (1 shl 31) or (_FACD3D shl 16);
  MAKE_D3DSTATUS_R = (0 shl 31) or (_FACD3D shl 16);

  D3D_OK                                  = S_OK;
  D3DERR_WRONGTEXTUREFORMAT               = HResult(MAKE_D3DHRESULT_R or 2072);
  D3DERR_UNSUPPORTEDCOLOROPERATION        = HResult(MAKE_D3DHRESULT_R or 2073);
  D3DERR_UNSUPPORTEDCOLORARG              = HResult(MAKE_D3DHRESULT_R or 2074);
  D3DERR_UNSUPPORTEDALPHAOPERATION        = HResult(MAKE_D3DHRESULT_R or 2075);
  D3DERR_UNSUPPORTEDALPHAARG              = HResult(MAKE_D3DHRESULT_R or 2076);
  D3DERR_TOOMANYOPERATIONS                = HResult(MAKE_D3DHRESULT_R or 2077);
  D3DERR_CONFLICTINGTEXTUREFILTER         = HResult(MAKE_D3DHRESULT_R or 2078);
  D3DERR_UNSUPPORTEDFACTORVALUE           = HResult(MAKE_D3DHRESULT_R or 2079);
  D3DERR_CONFLICTINGRENDERSTATE           = HResult(MAKE_D3DHRESULT_R or 2081);
  D3DERR_UNSUPPORTEDTEXTUREFILTER         = HResult(MAKE_D3DHRESULT_R or 2082);
  D3DERR_CONFLICTINGTEXTUREPALETTE        = HResult(MAKE_D3DHRESULT_R or 2086);
  D3DERR_DRIVERINTERNALERROR              = HResult(MAKE_D3DHRESULT_R or 2087);

  D3DERR_NOTFOUND                         = HResult(MAKE_D3DHRESULT_R or 2150);
  D3DERR_MOREDATA                         = HResult(MAKE_D3DHRESULT_R or 2151);
  D3DERR_DEVICELOST                       = HResult(MAKE_D3DHRESULT_R or 2152);
  D3DERR_DEVICENOTRESET                   = HResult(MAKE_D3DHRESULT_R or 2153);
  D3DERR_NOTAVAILABLE                     = HResult(MAKE_D3DHRESULT_R or 2154);
  D3DERR_OUTOFVIDEOMEMORY                 = HResult(MAKE_D3DHRESULT_R or 380);
  D3DERR_INVALIDDEVICE                    = HResult(MAKE_D3DHRESULT_R or 2155);
  D3DERR_INVALIDCALL                      = HResult(MAKE_D3DHRESULT_R or 2156);
  D3DERR_DRIVERINVALIDCALL                = HResult(MAKE_D3DHRESULT_R or 2157);
  D3DERR_WASSTILLDRAWING                  = HResult(MAKE_D3DHRESULT_R or 540);
  D3DERR_DEVICEREMOVED                    = HResult(MAKE_D3DHRESULT_R or 2160);

  D3DOK_NOAUTOGEN                         = HResult(MAKE_D3DSTATUS_R or 2159);
  S_NOT_RESIDENT                          = HResult(MAKE_D3DSTATUS_R or 2165);
  S_RESIDENT_IN_SHARED_MEMORY             = HResult(MAKE_D3DSTATUS_R or 2166);
  S_PRESENT_MODE_CHANGED                  = HResult(MAKE_D3DSTATUS_R or 2167);
  S_PRESENT_OCCLUDED                      = HResult(MAKE_D3DSTATUS_R or 2168);
  D3DERR_DEVICEHUNG                       = HResult(MAKE_D3DHRESULT_R or 2164);

  D3DERR_UNSUPPORTEDOVERLAY               = HResult(MAKE_D3DHRESULT_R or 2171);
  D3DERR_UNSUPPORTEDOVERLAYFORMAT         = HResult(MAKE_D3DHRESULT_R or 2172);
  D3DERR_CANNOTPROTECTCONTENT             = HResult(MAKE_D3DHRESULT_R or 2173);
  D3DERR_UNSUPPORTEDCRYPTO                = HResult(MAKE_D3DHRESULT_R or 2174);
  D3DERR_PRESENT_STATISTICS_DISJOINT      = HResult(MAKE_D3DHRESULT_R or 2180);


  function GetD3DErrorDescription(const aHResult: HResult;
                                  out hrStr: string;
                                  out hrDescr: string;
                                  out RegionDescr: string;
                                  out HeaderFile: string;
                                  out Reference: TReferenceArray): HResult;

  function GetD3DRegion(aHResult: HResult;
                        out aRegion: string): HResult;

implementation

uses
  System.StrUtils,
  System.SysUtils;

const
  csNoDescriptionFound = 'Unknown HResult description.' + CRLF +
                         'Try https://learn.microsoft.com/en-us/windows/win32/api/winerror/ to find out.';
  csInfoHeader1 = 'Source of this result value:' + CRLF +
                  '======================' + CRLF + CRLF;
  csInfoHeader2 = 'Returned: ';


function GetD3DErrorDescription(const aHResult: HResult;
                                out hrStr: string;
                                out hrDescr: string;
                                out RegionDescr: string;
                                out HeaderFile: string;
                                out Reference: TReferenceArray): HResult;
var
  hr: HResult;

begin
  hr := S_OK;

  HeaderFile := 'd3d9.h';
  Reference[0] := 'https://learn.microsoft.com/en-us/windows/win32/direct3d9/d3derr';
  Reference[1] := 'https://learn.microsoft.com/en-us/windows/win32/direct3d9/driver-internal-errors';


  case aHResult of
    D3D_OK:                               begin
                                            hrStr := 'D3D_OK';
                                            hrDescr := 'Ok, no error.';
                                          end;
    D3DERR_WRONGTEXTUREFORMAT:            begin
                                            hrStr := 'D3DERR_WRONGTEXTUREFORMAT';
                                            hrDescr := 'The pixel format of the texture surface is not valid.';
                                          end;
    D3DERR_UNSUPPORTEDCOLOROPERATION:     begin
                                            hrStr := 'D3DERR_UNSUPPORTEDCOLOROPERATION';
                                            hrDescr := 'The device does not support a specified texture-blending operation for color values.';
                                          end;
    D3DERR_UNSUPPORTEDCOLORARG:           begin
                                            hrStr := 'D3DERR_UNSUPPORTEDCOLORARG';
                                            hrDescr := 'The device does not support a specified texture-blending argument for color values.';
                                          end;
    D3DERR_UNSUPPORTEDALPHAOPERATION:     begin
                                            hrStr := 'D3DERR_UNSUPPORTEDALPHAOPERATION';
                                            hrDescr := 'The device does not support a specified texture-blending operation for the alpha channel.';
                                          end;
    D3DERR_UNSUPPORTEDALPHAARG:           begin
                                            hrStr := 'D3DERR_UNSUPPORTEDALPHAARG';
                                            hrDescr := 'The device does not support a specified texture-blending argument for the alpha channel.';
                                          end;
    D3DERR_TOOMANYOPERATIONS:             begin
                                            hrStr := 'D3DERR_TOOMANYOPERATIONS';
                                            hrDescr := 'The application is requesting more texture-filtering operations than the device supports.';
                                          end;
    D3DERR_CONFLICTINGTEXTUREFILTER:      begin
                                            hrStr := 'D3DERR_UNSUPPORTEDFACTORVALUE';
                                            hrDescr := 'The current texture filters cannot be used together.';
                                          end;
    D3DERR_UNSUPPORTEDFACTORVALUE:        begin
                                            hrStr := 'D3D_OK';
                                            hrDescr := 'The device does not support the specified texture factor value. Not used; provided only to support older drivers.';
                                          end;
    D3DERR_CONFLICTINGRENDERSTATE:        begin
                                            hrStr := 'D3DERR_CONFLICTINGRENDERSTATE';
                                            hrDescr := 'The currently set render states cannot be used together.';
                                          end;
    D3DERR_UNSUPPORTEDTEXTUREFILTER:      begin
                                            hrStr := 'D3DERR_UNSUPPORTEDTEXTUREFILTER';
                                            hrDescr := 'The device does not support the specified texture filter.';
                                          end;
    D3DERR_CONFLICTINGTEXTUREPALETTE:     begin
                                            hrStr := 'D3DERR_DRIVERINTERNALERROR';
                                            hrDescr := 'The current textures cannot be used simultaneously.';
                                          end;
    D3DERR_DRIVERINTERNALERROR:           begin
                                            hrStr := 'D3D_OK';
                                            hrDescr := 'Internal driver error. Applications should destroy and recreate the device when receiving this error. For hints on debugging this error, see Driver Internal Errors (Direct3D 9). [3]';
                                          end;

    D3DERR_NOTFOUND:                      begin
                                            hrStr := 'D3DERR_NOTFOUND';
                                            hrDescr := 'The requested item was not found.';
                                          end;
    D3DERR_MOREDATA:                      begin
                                            hrStr := 'D3DERR_MOREDATA';
                                            hrDescr := 'There is more data available than the specified buffer size can hold.';
                                          end;
    D3DERR_DEVICELOST:                    begin
                                            hrStr := 'D3DERR_DEVICELOST';
                                            hrDescr := 'The device has been lost but cannot be reset at this time.' + crlf +
                                                       'Therefore, rendering is not possible.' + crlf +
                                                       'A Direct3D device object other than the one that returned this code caused the hardware adapter to be reset by the OS.' + crlf +
                                                       'Delete all video memory objects (surfaces, textures, state blocks) and call Reset() to ' + crlf +
                                                       'return the device to a default state. ' + crlf +
                                                       'If the application continues rendering without a reset, the rendering calls will succeed.';
                                          end;
    D3DERR_DEVICENOTRESET:                begin
                                            hrStr := 'D3DERR_DEVICENOTRESET';
                                            hrDescr := 'The device has been lost but can be reset at this time.';
                                          end;
    D3DERR_NOTAVAILABLE:                  begin
                                            hrStr := 'D3DERR_NOTAVAILABLE';
                                            hrDescr := 'This device does not support the queried technique.';
                                          end;
    D3DERR_OUTOFVIDEOMEMORY:              begin
                                            hrStr := 'D3DERR_OUTOFVIDEOMEMORY';
                                            hrDescr := 'Direct3D does not have enough display memory to perform the operation.' + crlf +
                                                       'The device is using more resources in a single scene than can fit simultaneously into video memory.' + crlf +
                                                       'Present, PresentEx, or CheckDeviceState can return this error.' + crlf +
                                                       'Recovery is similar to D3DERR_DEVICEHUNG, though the application may want to reduce' + crlf +
                                                       'its per-frame memory usage as well to avoid having the error recur.';
                                          end;
    D3DERR_INVALIDDEVICE:                 begin
                                            hrStr := 'D3DERR_INVALIDDEVICE';
                                            hrDescr := 'The requested device type is not valid.';
                                          end;
    D3DERR_INVALIDCALL:                   begin
                                            hrStr := 'D3DERR_INVALIDCALL';
                                            hrDescr := 'The method call is invalid. For example, a method''s parameter may not be a valid pointer.';
                                          end;
    D3DERR_DRIVERINVALIDCALL:             begin
                                            hrStr := 'D3DERR_WASSTILLDRAWING';
                                            hrDescr := 'Not used.';
                                          end;
    D3DERR_WASSTILLDRAWING:               begin
                                            hrStr := 'D3D_OK';
                                            hrDescr := 'The previous blit operation that is transferring information to or from this surface is incomplete.';
                                          end;
    D3DERR_DEVICEREMOVED:                 begin
                                            hrStr := 'D3DERR_DEVICEREMOVED';
                                            hrDescr := 'The hardware adapter has been removed.' + crlf +
                                                       'Application must destroy the device, do enumeration of adapters and create another Direct3D device.' + crlf +
                                                       'If application continues rendering without calling Reset, the rendering calls will succeed.' + crlf +
                                                       'Applies to Direct3D 9Ex only.';
                                          end;

    D3DOK_NOAUTOGEN:                      begin
                                            hrStr := 'D3DOK_NOAUTOGEN';
                                            hrDescr := 'This is a success code.' + crlf +
                                                       'However, the autogeneration of mipmaps is not supported for this format.' + crlf +
                                                       'This means that resource creation will succeed but the mipmap levels will not be automatically generated.';
                                          end;
    S_NOT_RESIDENT:                       begin
                                            hrStr := 'S_NOT_RESIDENT';
                                            hrDescr := 'At least one allocation that comprises the resources is on disk. Direct3D 9Ex only.';
                                          end;
    S_RESIDENT_IN_SHARED_MEMORY:          begin
                                            hrStr := 'S_RESIDENT_IN_SHARED_MEMORY';
                                            hrDescr := 'No allocations that comprise the resources are on disk.' + crlf +
                                                       'However, at least one allocation is not in GPU-accessible memory.' + crlf +
                                                       'Direct3D 9Ex only.';
                                          end;
    S_PRESENT_MODE_CHANGED:               begin
                                            hrStr := 'S_PRESENT_MODE_CHANGED';
                                            hrDescr := 'The desktop display mode has been changed.' + crlf +
                                                       'The application can continue rendering, but there might be color conversion/stretching.' + crlf +
                                                       'Pick a back buffer format similar to the current display mode, and call Reset to recreate the swap chains.' + crlf +
                                                       'The device will leave this state after a Reset is called.' + crlf +
                                                       'Direct3D 9Ex only.';
                                          end;
    S_PRESENT_OCCLUDED:                   begin
                                            hrStr := 'S_PRESENT_OCCLUDED';
                                            hrDescr := 'The presentation area is occluded.' + crlf +
                                                       'Occlusion means that the presentation window is minimized or another device entered the' + crlf +
                                                       'fullscreen mode on the same monitor as the presentation window and the presentation window is' + crlf +
                                                       'completely on that monitor.' + crlf +
                                                       'Occlusion will not occur if the client area is covered by another Window.' + crlf +
                                                       'Occluded applications can continue rendering and all calls will succeed,' + crlf +
                                                       'but the occluded presentation window will not be updated.' + crlf +
                                                       'Preferably the application should stop rendering to the presentation window using the device and' + crlf +
                                                       'keep calling CheckDeviceState until S_OK or S_PRESENT_MODE_CHANGED returns.' + crlf +
                                                       'Direct3D 9Ex only.';
                                          end;
    D3DERR_DEVICEHUNG:                    begin
                                            hrStr := 'D3DERR_DEVICEHUNG';
                                            hrDescr := 'The device that returned this code caused the hardware adapter to be reset by the OS.' + crlf +
                                                       'Applications that must continue should destroy all video memory objects (surfaces, textures, state blocks etc)' + crlf +
                                                       'and call Reset() to put the device in a default state.' + crlf +
                                                       'If the application then continues rendering in the same way, the device will return to this state.' + crlf +
                                                       'Applies to Direct3D 9Ex only.';
                                          end;
    D3DERR_UNSUPPORTEDOVERLAY:            begin
                                            hrStr := 'D3DERR_UNSUPPORTEDOVERLAY';
                                            hrDescr := 'The device does not support overlay for the specified size or display mode.' + crlf +
                                                       'Direct3D 9Ex under Windows 7 only.';
                                          end;
    D3DERR_UNSUPPORTEDOVERLAYFORMAT:      begin
                                            hrStr := 'D3DERR_UNSUPPORTEDOVERLAYFORMAT';
                                            hrDescr := 'The device does not support overlay for the specified surface format.' + crlf +
                                                       'Direct3D 9Ex under Windows 7 only.';
                                          end;
    D3DERR_CANNOTPROTECTCONTENT:          begin
                                            hrStr := 'D3DERR_CANNOTPROTECTCONTENT';
                                            hrDescr := 'The specified content cannot be protected.' + crlf +
                                                       'Direct3D 9Ex under Windows 7 only.';
                                          end;
    D3DERR_UNSUPPORTEDCRYPTO:             begin
                                            hrStr := 'D3DERR_UNSUPPORTEDCRYPTO';
                                            hrDescr := 'The specified cryptographic algorithm is not supported.' + crlf +
                                                       'Direct3D 9Ex under Windows 7 only.';
                                          end;
    D3DERR_PRESENT_STATISTICS_DISJOINT:   begin
                                            hrStr := 'D3DERR_PRESENT_STATISTICS_DISJOINT';
                                            hrDescr := 'The present statistics have no orderly sequence.' + crlf +
                                                       'Direct3D 9Ex under Windows 7 only.';
                                          end;
    else
      begin
        HrStr := 'Unknown identifier.';
        HrDescr := 'Unknown HResult code.';
        HeaderFile := 'Unknown.';
        FWinHResultCracker.ClearResults();
        hr := aHResult;
      end;
  end;
  Result := hr;
end;


function GetD3DRegion(aHResult: HResult;
                     out aRegion: string): HResult;
const
  D3D = 'D3D region.';

begin
  aRegion := D3D;
  Result := S_OK;
end;

end.
