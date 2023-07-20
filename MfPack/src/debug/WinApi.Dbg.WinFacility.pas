//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.WinFacility.pas
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: Unit to explore the Windows Facility codes.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 20/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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
unit WinApi.Dbg.WinFacility;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError;


  // Get the facilitycode from a function that returns a HResult.
  function GetFacilityDescription(const HResultCode: LongInt;
                                  out FacilityVal: LongInt;
                                  out FacilityTag: string;
                                  out FacilityDescr: string): HResult;

implementation


function GetFacilityDescription(const HResultCode: LongInt;
                                out FacilityVal: LongInt;
                                out FacilityTag: string;
                                out FacilityDescr: string): HResult;
const
  FACILITYSOURCE = 'The source of the HResult code is';

var
  hr: HResult;
  FacilityCode: LongInt;

begin
   hr := S_OK;
  // Get the facility code
  // Note: The facility is the originating API.
  FacilityCode := HRESULT_FACILITY(HResultCode);

  // Determen the code.
  case FacilityCode of
    FACILITY_NULL:      begin
                          FacilityTag := 'FACILITY_NULL';
                          FacilityDescr := 'The default facility code.';
                        end;
    FACILITY_RPC:       begin
                          FacilityTag := 'FACILITY_RPC';
                          FacilityDescr := FACILITYSOURCE + ' an RPC subsystem.';
                        end;
    FACILITY_DISPATCH:  begin
                          FacilityTag := 'FACILITY_DISPATCH';
                          FacilityDescr := FACILITYSOURCE + ' a COM Dispatch.';
                        end;
    FACILITY_STORAGE:   begin
                          FacilityTag := 'FACILITY_STORAGE';
                          FacilityDescr := FACILITYSOURCE + ' OLE Storage.';
                        end;
    FACILITY_ITF:       begin
                          FacilityTag := 'FACILITY_ITF';
                          FacilityDescr := FACILITYSOURCE + ' COM/OLE Interface management.';
                        end;
    FACILITY_WIN32:     begin
                          FacilityTag := 'FACILITY_WIN32';
                          FacilityDescr := 'This region is reserved to map undecorated HResult codes into HRESULTs.';
                        end;
    FACILITY_WINDOWS:   begin
                          FacilityTag := 'FACILITY_WINDOWS';
                          FacilityDescr := FACILITYSOURCE + ' the Windows subsystem.';
                        end;
    FACILITY_SECURITY:  begin
                          FacilityTag := 'FACILITY_SECURITY';
                          FacilityDescr := FACILITYSOURCE + ' the Security API layer.';
                        end;
    FACILITY_CONTROL:   begin
                          FacilityTag := 'FACILITY_CONTROL';
                          FacilityDescr := FACILITYSOURCE + ' the control mechanism.';
                        end;
    FACILITY_CERT:      begin
                          FacilityTag := 'FACILITY_CERT';
                          FacilityDescr := FACILITYSOURCE + ' a certificate client or server? ';
                        end;
    FACILITY_INTERNET:  begin
                          FacilityTag := 'FACILITY_INTERNET';
                          FacilityDescr := FACILITYSOURCE + ' Wininet related.';
                        end;
    FACILITY_MEDIASERVER:  begin
                             FacilityTag := 'FACILITY_MEDIASERVER';
                             FacilityDescr := FACILITYSOURCE + ' the Windows Media Server.';
                           end;
    FACILITY_MSMQ:         begin
                             FacilityTag := 'FACILITY_MSMQ';
                             FacilityDescr := FACILITYSOURCE + ' the Microsoft Message Queue.';
                           end;
    FACILITY_SETUPAPI:     begin
                             FacilityTag := 'FACILITY_SETUPAPI';
                             FacilityDescr := FACILITYSOURCE + ' the Setup API.';
                           end;
    FACILITY_SCARD:        begin
                             FacilityTag := 'FACILITY_SCARD';
                             FacilityDescr := FACILITYSOURCE + ' the Smart-card subsystem.';
                           end;
    FACILITY_COMPLUS:      begin
                             FacilityTag := 'FACILITY_COMPLUS';
                             FacilityDescr := FACILITYSOURCE + ' COM+ (COMPLUS).';
                           end;
    FACILITY_AAF:    begin
                       FacilityTag := 'FACILITY_AAF';
                       FacilityDescr := FACILITYSOURCE + ' the Microsoft agent.';
                     end;
    FACILITY_URT:    begin
                       FacilityTag := 'FACILITY_URT';
                       FacilityDescr := FACILITYSOURCE + ' .NET CLR.';
                     end;
    FACILITY_ACS:    begin
                       FacilityTag := 'FACILITY_ACS';
                       FacilityDescr := FACILITYSOURCE + ' the audit collection service.';
                     end;
    FACILITY_DPLAY:  begin
                       FacilityTag := 'FACILITY_DPLAY';
                       FacilityDescr := FACILITYSOURCE + ' Direct Play.';
                     end;
    FACILITY_UMI:    begin
                       FacilityTag := 'FACILITY_UMI';
                       FacilityDescr := FACILITYSOURCE + ' the ubiquitous memoryintrospection service.';
                     end;
    FACILITY_SXS:    begin
                       FacilityTag := 'FACILITY_SXS';
                       FacilityDescr := FACILITYSOURCE + ' Side-by-side servicing.';
                     end;
    FACILITY_WINDOWS_CE:               begin
                                         FacilityTag := 'FACILITY_WINDOWS_CE';
                                         FacilityDescr := 'The HResult code is specific to Windows CE.';
                                       end;
    FACILITY_HTTP:                     begin
                                          FacilityTag := 'FACILITY_HTTP';
                                          FacilityDescr := FACILITYSOURCE + ' HTTP support.';
                                       end;
    FACILITY_USERMODE_COMMONLOG:       begin
                                         FacilityTag := 'FACILITY_USERMODE_COMMONLOG';
                                         FacilityDescr := FACILITYSOURCE + ' common Logging support.';
                                       end;
    FACILITY_WER:                      begin
                                         FacilityTag := 'FACILITY_WER';
                                         FacilityDescr := FACILITYSOURCE + ' Windows Error Reporting.';
                                       end;
    FACILITY_USERMODE_FILTER_MANAGER:  begin
                                         FacilityTag := 'FACILITY_USERMODE_FILTER_MANAGER';
                                         FacilityDescr := FACILITYSOURCE + ' the user mode filter manager.';
                                       end;
    FACILITY_BACKGROUNDCOPY:           begin
                                         FacilityTag := 'FACILITY_BACKGROUNDCOPY';
                                         FacilityDescr := FACILITYSOURCE + ' background copy control';
                                       end;
    FACILITY_CONFIGURATION:            begin
                                         FacilityTag := 'FACILITY_CONFIGURATION';
                                         FacilityDescr := FACILITYSOURCE + ' configuration services.';
                                       end;
    FACILITY_STATE_MANAGEMENT:         begin
                                         FacilityTag := 'FACILITY_STATE_MANAGEMENT';
                                         FacilityDescr := FACILITYSOURCE + ' state management services.';
                                       end;
    FACILITY_METADIRECTORY:            begin
                                         FacilityTag := 'FACILITY_METADIRECTORY';
                                         FacilityDescr := FACILITYSOURCE + ' the Microsoft Identity Server.';
                                       end;
    FACILITY_WINDOWSUPDATE:            begin
                                         FacilityTag := 'FACILITY_WINDOWSUPDATE';
                                         FacilityDescr := FACILITYSOURCE + ' a Windows update.';
                                       end;
    FACILITY_DIRECTORYSERVICE:         begin
                                         FacilityTag := 'FACILITY_DIRECTORYSERVICE';
                                         FacilityDescr := FACILITYSOURCE + ' Active Directory.';
                                       end;
    FACILITY_GRAPHICS:                 begin
                                         FacilityTag := 'FACILITY_GRAPHICS';
                                         FacilityDescr := FACILITYSOURCE + ' the graphics drivers.';
                                       end;
    FACILITY_SHELL:                    begin
                                         FacilityTag := 'FACILITY_SHELL';
                                         FacilityDescr := FACILITYSOURCE + ' the user Shell.';
                                       end;
    FACILITY_TPM_SERVICES:             begin
                                         FacilityTag := 'FACILITY_TPM_SERVICES';
                                         FacilityDescr := FACILITYSOURCE + ' the Trusted Platform Module services.';
                                       end;
    FACILITY_TPM_SOFTWARE:             begin
                                         FacilityTag := 'FACILITY_TPM_SOFTWARE';
                                         FacilityDescr := FACILITYSOURCE + ' the Trusted Platform Module applications.';
                                       end;
    FACILITY_UI:                       begin
                                         FacilityTag := 'FACILITY_UI';
                                         FacilityDescr := FACILITYSOURCE + ' the User Interface service.';
                                       end;
    FACILITY_XAML:                     begin
                                         FacilityTag := 'FACILITY_XAML';
                                         FacilityDescr := FACILITYSOURCE + ' the XAML (eXtended Application Markup Language) service.';
                                       end;
    FACILITY_ACTION_QUEUE:             begin
                                         FacilityTag := 'FACILITY_ACTION_QUEUE';
                                         FacilityDescr := FACILITYSOURCE + ' the Work Queue service.';
                                       end;
    FACILITY_WINDOWS_SETUP:            begin
                                         FacilityTag := 'FACILITY_WINDOWS_SETUP';
                                         FacilityDescr := FACILITYSOURCE + ' the Windows Setup service.';
                                       end;
    FACILITY_FVE:                      begin
                                         FacilityTag := 'FACILITY_FVE';
                                         FacilityDescr := FACILITYSOURCE + ' Full volume encryption.';
                                       end;
    FACILITY_FWP:                      begin
                                         FacilityTag := 'FACILITY_FWP';
                                         FacilityDescr := FACILITYSOURCE + ' the Firewall Platform.';
                                       end;
    FACILITY_WINRM:                    begin
                                         FacilityTag := 'FACILITY_WINRM';
                                         FacilityDescr := FACILITYSOURCE + ' the Windows Resource Manager.';
                                       end;
    FACILITY_NDIS:                     begin
                                         FacilityTag := 'FACILITY_NDIS';
                                         FacilityDescr := FACILITYSOURCE + ' the Network Driver Interface.';
                                       end;
    FACILITY_USERMODE_HYPERVISOR:      begin
                                         FacilityTag := 'FACILITY_USERMODE_HYPERVISOR';
                                         FacilityDescr := FACILITYSOURCE + ' the Usermode Hypervisor components.';
                                       end;
    FACILITY_CMI:                      begin
                                         FacilityTag := 'FACILITY_CMI';
                                         FacilityDescr := FACILITYSOURCE + ' the Configuration Management Infrastructure.';
                                       end;
    FACILITY_USERMODE_VIRTUALIZATION:  begin
                                         FacilityTag := 'FACILITY_USERMODE_VIRTUALIZATION';
                                         FacilityDescr := FACILITYSOURCE + ' the user mode virtualization subsystem.';
                                       end;
    FACILITY_USERMODE_VOLMGR:          begin
                                         FacilityTag := 'FACILITY_USERMODE_VOLMGR';
                                         FacilityDescr := FACILITYSOURCE + '  the user mode volume manager';
                                       end;
    FACILITY_BCD:                      begin
                                         FacilityTag := 'FACILITY_BCD';
                                         FacilityDescr := FACILITYSOURCE + ' the Boot Configuration Database.';
                                       end;
    FACILITY_USERMODE_VHD:             begin
                                         FacilityTag := 'FACILITY_USERMODE_VHD';
                                         FacilityDescr := FACILITYSOURCE + ' user mode virtual hard disk support.';
                                       end;
    FACILITY_USERMODE_HNS:             begin
                                         FacilityTag := 'FACILITY_USERMODE_HNS';
                                         FacilityDescr := FACILITYSOURCE + ' the Host Network Service.';
                                       end;
    FACILITY_WEBSERVICES:              begin
                                         FacilityTag := 'FACILITY_WEBSERVICES';
                                         FacilityDescr := FACILITYSOURCE + ' the Web Services.';
                                       end;
    FACILITY_WPN:                      begin
                                         FacilityTag := 'FACILITY_WPN';
                                         FacilityDescr := FACILITYSOURCE + ' the Windows Notification Platform service.';
                                       end;
    FACILITY_WINDOWS_STORE:            begin
                                         FacilityTag := 'FACILITY_WINDOWS_STORE';
                                         FacilityDescr := FACILITYSOURCE + ' the Windows Store service.';
                                       end;
    FACILITY_INPUT:                    begin
                                         FacilityTag := 'FACILITY_INPUT';
                                         FacilityDescr := FACILITYSOURCE + ' the Input service.';
                                       end;
    FACILITY_QUIC:                     begin
                                         FacilityTag := 'FACILITY_QUIC';
                                         FacilityDescr := FACILITYSOURCE + ' the QUIC protocol service.';
                                       end;
    FACILITY_EAP:                      begin
                                         FacilityTag := 'FACILITY_EAP';
                                         FacilityDescr := FACILITYSOURCE + ' the Extensible Authentication Protocol service.';
                                       end;
    FACILITY_IORING:                   begin
                                         FacilityTag := 'FACILITY_IORING';
                                         FacilityDescr := FACILITYSOURCE + ' the In/Out ring service.';
                                       end;
    FACILITY_WINDOWS_DEFENDER:         begin
                                         FacilityTag := 'FACILITY_WINDOWS_DEFENDER';
                                         FacilityDescr := FACILITYSOURCE + ' a Windows Defender component.';
                                       end;
    FACILITY_OPC:                      begin
                                         FacilityTag := 'FACILITY_OPC';
                                         FacilityDescr := FACILITYSOURCE + ' the open connectivity service.';
                                       end;
    FACILITY_XPS:                      begin
                                         FacilityTag := 'FACILITY_XPS';
                                         FacilityDescr := FACILITYSOURCE + ' the XPS Document service.';
                                       end;
    FACILITY_POWERSHELL:               begin
                                         FacilityTag := 'FACILITY_POWERSHELL';
                                         FacilityDescr := FACILITYSOURCE + ' the Powershell service.';
                                       end;
    FACILITY_RAS:                      begin
                                         FacilityTag := 'FACILITY_RAS';
                                         FacilityDescr := FACILITYSOURCE + ' the Remote Access Service Administration service.';
                                       end;
    FACILITY_P2P_INT:                  begin
                                         FacilityTag := 'FACILITY_P2P_INT';
                                         FacilityDescr := FACILITYSOURCE + ' the Peer-to-Peer service.';
                                       end;
    FACILITY_P2P:                      begin
                                         FacilityTag := 'FACILITY_P2P';
                                         FacilityDescr := FACILITYSOURCE + ' the Peer-to-Peer service.';
                                       end;
    FACILITY_DAF:                      begin
                                         FacilityTag := 'FACILITY_DAF';
                                         FacilityDescr := FACILITYSOURCE + ' the Document Audit Facility service.';
                                       end;
    FACILITY_BLUETOOTH_ATT:            begin
                                         FacilityTag := 'FACILITY_BLUETOOTH_ATT';
                                         FacilityDescr := FACILITYSOURCE + ' the Bluetooth ATT (Attribute Protocol) service.';
                                       end;
    FACILITY_AUDIO:                    begin
                                         FacilityTag := 'FACILITY_AUDIO';
                                         FacilityDescr := FACILITYSOURCE + ' the audio service.';
                                       end;
    FACILITY_STATEREPOSITORY:          begin
                                         FacilityTag := 'FACILITY_STATEREPOSITORY';
                                         FacilityDescr := FACILITYSOURCE + ' the Windows State Repository service.';
                                       end;
    FACILITY_VISUALCPP:                begin
                                         FacilityTag := 'FACILITY_VISUALCPP';
                                         FacilityDescr := FACILITYSOURCE + ' the Visual C++ service.';
                                       end;
    FACILITY_SCRIPT:                   begin
                                         FacilityTag := 'FACILITY_SCRIPT';
                                         FacilityDescr := FACILITYSOURCE + ' the Facility Script service.';
                                       end;
    FACILITY_PARSE:                    begin
                                         FacilityTag := 'FACILITY_PARSE';
                                         FacilityDescr := FACILITYSOURCE + ' the parser service.';
                                       end;
    FACILITY_BLB:                      begin
                                         FacilityTag := 'FACILITY_BLB';
                                         FacilityDescr := FACILITYSOURCE + ' the System Diagnostics service.';
                                       end;
    FACILITY_BLB_CLI:                  begin
                                         FacilityTag := 'FACILITY_BLB_CLI';
                                         FacilityDescr := FACILITYSOURCE + ' the System Diagnostics service.';
                                       end;
    FACILITY_WSBAPP:                   begin
                                         FacilityTag := 'FACILITY_WSBAPP';
                                         FacilityDescr := FACILITYSOURCE + ' the System Diagnostics service.';
                                       end;
    FACILITY_BLBUI:                    begin
                                         FacilityTag := 'FACILITY_BLBUI';
                                         FacilityDescr := FACILITYSOURCE + ' the System Diagnostics service.';
                                       end;
    FACILITY_USN:                      begin
                                         FacilityTag := 'FACILITY_USN';
                                         FacilityDescr := FACILITYSOURCE + ' the Volume MAnagement service.';
                                       end;
    FACILITY_USERMODE_VOLSNAP:         begin
                                         FacilityTag := 'FACILITY_USERMODE_VOLSNAP';
                                         FacilityDescr := FACILITYSOURCE + ' the Volume Shadow Copy service.';
                                       end;
    FACILITY_TIERING:                  begin
                                         FacilityTag := 'FACILITY_TIERING';
                                         FacilityDescr := FACILITYSOURCE + ' the Storage Tiers Management service.';
                                       end;
    FACILITY_WSB_ONLINE:               begin
                                         FacilityTag := 'FACILITY_WSB_ONLINE';
                                         FacilityDescr := FACILITYSOURCE + ' the Windows Server Backup service.';
                                       end;
    FACILITY_ONLINE_ID:                begin
                                        FacilityTag := 'FACILITY_ONLINE_ID';
                                        FacilityDescr := FACILITYSOURCE + ' Online ID service.';
                                       end;
    FACILITY_DEVICE_UPDATE_AGENT:      begin
                                         FacilityTag := 'FACILITY_DEVICE_UPDATE_AGENT';
                                         FacilityDescr := FACILITYSOURCE + ' the Device Update Agent service.';
                                       end;
    FACILITY_DRVSERVICING:             begin
                                         FacilityTag := 'FACILITY_DRVSERVICING';
                                         FacilityDescr := FACILITYSOURCE + ' driving protocol service.';
                                       end;
    FACILITY_DLS:                      begin
                                         FacilityTag := 'FACILITY_DLS';
                                         FacilityDescr := FACILITYSOURCE + ' the Downloadable Sounds (DS) service.';
                                       end;
    FACILITY_DELIVERY_OPTIMIZATION:    begin
                                         FacilityTag := 'FACILITY_DELIVERY_OPTIMIZATION';
                                         FacilityDescr := FACILITYSOURCE + ' the Delivery Optimization service.';
                                       end;
    FACILITY_USERMODE_SPACES:          begin
                                         FacilityTag := 'FACILITY_USERMODE_SPACES';
                                         FacilityDescr := FACILITYSOURCE + ' the Storage Spaces service.';
                                       end;
    FACILITY_USER_MODE_SECURITY_CORE:  begin
                                         FacilityTag := 'FACILITY_USER_MODE_SECURITY_CORE';
                                         FacilityDescr := FACILITYSOURCE + ' the Security Core service.';
                                       end;
    FACILITY_USERMODE_LICENSING:       begin
                                         FacilityTag := 'FACILITY_USERMODE_LICENSING';
                                         FacilityDescr := FACILITYSOURCE + ' the Clip modern app and windows licensing service';
                                       end;
    FACILITY_SOS:                      begin
                                         FacilityTag := 'FACILITY_SOS';
                                         FacilityDescr := FACILITYSOURCE + ' the System Diagnostics service.';
                                       end;
    FACILITY_OCP_UPDATE_AGENT:         begin
                                         FacilityTag := 'FACILITY_OCP_UPDATE_AGENT';
                                         FacilityDescr := FACILITYSOURCE + ' the Update Agent service';
                                       end;
    FACILITY_DEBUGGERS:                begin
                                         FacilityTag := 'FACILITY_DEBUGGERS';
                                         FacilityDescr := FACILITYSOURCE + ' the debugger service.';
                                       end;
    FACILITY_RESTORE:                  begin
                                         FacilityTag := 'FACILITY_RESTORE';
                                         FacilityDescr := FACILITYSOURCE + ' the backup restore service.';
                                       end;
    FACILITY_DEPLOYMENT_SERVICES_SERVER:      begin
                                                FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_SERVER';
                                                FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Server service.';
                                              end;
    FACILITY_DEPLOYMENT_SERVICES_IMAGING:     begin
                                                FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_IMAGING';
                                                FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Disk Imaging service.';
                                              end;
    FACILITY_DEPLOYMENT_SERVICES_MANAGEMENT:  begin
                                                FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_MANAGEMENT';
                                                FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) MAnagement service.';
                                              end;
    FACILITY_DEPLOYMENT_SERVICES_UTIL:        begin
                                                FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_UTIL';
                                                FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Utility service.';
                                              end;
    FACILITY_DEPLOYMENT_SERVICES_BINLSVC:     begin
                                                FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_BINLSVC';
                                                FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Boot Information Negotiation Layer service.';
                                              end;
    FACILITY_DEPLOYMENT_SERVICES_PXE:         begin
                                                FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_PXE';
                                                FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) PXE (Preboot Execution Environment) service.';
                                              end;
    FACILITY_DEPLOYMENT_SERVICES_TFTP:        begin
                                                FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_TFTP';
                                                FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) TFTP service.';
                                              end;
    FACILITY_DEPLOYMENT_SERVICES_TRANSPORT_MANAGEMENT:  begin
                                                          FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_TRANSPORT_MANAGEMENT';
                                                          FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Transport Management service.';
                                                        end;
    FACILITY_DEPLOYMENT_SERVICES_DRIVER_PROVISIONING:   begin
                                                          FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_DRIVER_PROVISIONING';
                                                          FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Driver Provisioning service.';
                                                        end;
    FACILITY_DEPLOYMENT_SERVICES_MULTICAST_SERVER:      begin
                                                          FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_MULTICAST_SERVER';
                                                          FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Multicast Server service.';
                                                        end;
    FACILITY_DEPLOYMENT_SERVICES_MULTICAST_CLIENT:      begin
                                                          FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_MULTICAST_CLIENT';
                                                          FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Multicast Client service.';
                                                        end;
    FACILITY_DEPLOYMENT_SERVICES_CONTENT_PROVIDER:      begin
                                                          FacilityTag := 'FACILITY_DEPLOYMENT_SERVICES_CONTENT_PROVIDER';
                                                          FacilityDescr := FACILITYSOURCE + ' the Windows Deployment Services (WDS) Content Provider service';
                                                        end;
    FACILITY_HSP_SERVICES:                              begin
                                                          FacilityTag := 'FACILITY_HSP_SERVICES';
                                                          FacilityDescr := FACILITYSOURCE + ' the Hardware-enforced Stack Protection (HSP) service.';
                                                        end;
    FACILITY_HSP_SOFTWARE:                              begin
                                                          FacilityTag := 'FACILITY_HSP_SOFTWARE';
                                                          FacilityDescr := FACILITYSOURCE + ' the Software-enforced Stack Protection (HSP) service.';
                                                        end;
    FACILITY_LINGUISTIC_SERVICES:                       begin
                                                          FacilityTag := 'FACILITY_LINGUISTIC_SERVICES';
                                                          FacilityDescr := FACILITYSOURCE + ' the Extended Linguistic service.';
                                                        end;
    FACILITY_AUDIOSTREAMING:                            begin
                                                          FacilityTag := 'FACILITY_AUDIOSTREAMING';
                                                          FacilityDescr := FACILITYSOURCE + ' the Audio Streaming service';
                                                        end;
    FACILITY_TTD:                                       begin
                                                          FacilityTag := 'FACILITY_TTD';
                                                          FacilityDescr := FACILITYSOURCE + ' the Time Travel Debugging (TTD) service.';
                                                        end;
    FACILITY_ACCELERATOR:                               begin
                                                          FacilityTag := 'FACILITY_ACCELERATOR';
                                                          FacilityDescr := FACILITYSOURCE + ' the Keyboard Accelerator Functions service.';
                                                        end;
    FACILITY_WMAAECMA:             begin
                                     FacilityTag := 'FACILITY_WMAAECMA';
                                     FacilityDescr := FACILITYSOURCE + ' the Wmcodecdsp service.'
                                   end;
    FACILITY_DIRECTMUSIC:          begin
                                     FacilityTag := 'FACILITY_DIRECTMUSIC';
                                     FacilityDescr := FACILITYSOURCE + ' the Direct Music service.';
                                   end;
    FACILITY_DIRECT3D10:           begin
                                     FacilityTag := 'FACILITY_DIRECT3D10';
                                     FacilityDescr := FACILITYSOURCE + ' the Direct 3D10 service.';
                                   end;
    FACILITY_DXGI:                 begin
                                     FacilityTag := 'FACILITY_DXGI';
                                     FacilityDescr := FACILITYSOURCE + ' the DXGI service.';
                                   end;
    FACILITY_DXGI_DDI:             begin
                                     FacilityTag := 'FACILITY_DXGI_DDI';
                                     FacilityDescr := FACILITYSOURCE + ' the DXGI DDI service.';
                                   end;
    FACILITY_DIRECT3D11:           begin
                                     FacilityTag := 'FACILITY_DIRECT3D11';
                                     FacilityDescr := FACILITYSOURCE + ' the 3D11 service.';
                                   end;
    FACILITY_DIRECT3D11_DEBUG:     begin
                                     FacilityTag := 'FACILITY_DIRECT3D11_DEBUG';
                                     FacilityDescr := FACILITYSOURCE + ' the D3D11 Debug service.';
                                   end;
    FACILITY_DIRECT3D12:           begin
                                     FacilityTag := 'FACILITY_DIRECT3D12';
                                     FacilityDescr := FACILITYSOURCE + ' the D3D12 service.';
                                   end;
    FACILITY_DIRECT3D12_DEBUG:     begin
                                     FacilityTag := 'FACILITY_DIRECT3D12_DEBUG';
                                     FacilityDescr := FACILITYSOURCE + ' the D3D12 Debug service.';
                                   end;
    FACILITY_DXCORE:               begin
                                     FacilityTag := 'FACILITY_DXCORE';
                                     FacilityDescr := FACILITYSOURCE + ' the DirectX Core service.';
                                  end;
    FACILITY_PRESENTATION:         begin
                                     FacilityTag := 'FACILITY_PRESENTATION';
                                     FacilityDescr := FACILITYSOURCE + ' the Presentation service.';
                                   end;
    FACILITY_LEAP:                 begin
                                     FacilityTag := 'FACILITY_LEAP';
                                     FacilityDescr := FACILITYSOURCE + ' the Legacy Authentication Protocol service.';
                                   end;
    FACILITY_AUDCLNT:              begin
                                     FacilityTag := 'FACILITY_AUDCLNT';
                                     FacilityDescr := FACILITYSOURCE + ' the Audio Client (Audio stream between an audio application and the audio engine (for a shared-mode stream) or the hardware buffer of an audio endpoint device (for an exclusive-mode stream) service.)';
                                   end;
    FACILITY_WINCODEC_DWRITE_DWM:  begin
                                     FacilityTag := 'FACILITY_WINCODEC_DWRITE_DWM';
                                     FacilityDescr := FACILITYSOURCE + ' the DirectWrite Desktop Window Manager (DWM) service.';
                                   end;
    FACILITY_WINML:                begin
                                     FacilityTag := 'FACILITY_WINML';
                                     FacilityDescr := FACILITYSOURCE + ' the Windows Machine Learning Model Learning service.';
                                   end;
    FACILITY_DIRECT2D:             begin
                                     FacilityTag := 'FACILITY_DIRECT2D';
                                     FacilityDescr := FACILITYSOURCE + ' the Direct2D service';
                                   end;
    FACILITY_DEFRAG:               begin
                                     FacilityTag := 'FACILITY_DEFRAG';
                                     FacilityDescr := FACILITYSOURCE + ' the Defragging service.';
                                   end;
    FACILITY_USERMODE_SDBUS:       begin
                                     FacilityTag := 'FACILITY_USERMODE_SDBUS';
                                     FacilityDescr := FACILITYSOURCE + ' the user-mode service on Secure Digital Bus (SDBUS) service.';
                                   end;
    FACILITY_JSCRIPT:              begin
                                     FacilityTag := 'FACILITY_JSCRIPT';
                                     FacilityDescr := FACILITYSOURCE + ' the Javascript service.';
                                   end;
    FACILITY_PIDGENX:              begin
                                     FacilityTag := 'FACILITY_PIDGENX';
                                     FacilityDescr := FACILITYSOURCE + ' the Pid Generation service.';
                                   end;
    FACILITY_EAS:                  begin
                                     FacilityTag := 'FACILITY_EAS';
                                     FacilityDescr := FACILITYSOURCE + ' the Enterprise Agreement service.';
                                     end;
    FACILITY_WEB:                  begin
                                     FacilityTag := 'FACILITY_WEB';
                                     FacilityDescr := FACILITYSOURCE + ' the Windows Web servic.';
                                     end;
    FACILITY_WEB_SOCKET:           begin
                                     FacilityTag := 'FACILITY_WEB_SOCKET';
                                     FacilityDescr := FACILITYSOURCE + ' the Windows Web Socked service.';
                                     end;
    FACILITY_MOBILE:               begin
                                     FacilityTag := 'FACILITY_MOBILE';
                                     FacilityDescr := FACILITYSOURCE + ' the Windows Mobile service.';
                                     end;
    FACILITY_SQLITE:               begin
                                     FacilityTag := 'FACILITY_SQLITE';
                                     FacilityDescr := FACILITYSOURCE + ' the SQL Lite service.';
                                   end;
    FACILITY_SERVICE_FABRIC:       begin
                                     FacilityTag := 'FACILITY_SERVICE_FABRIC';
                                     FacilityDescr := FACILITYSOURCE + ' the Storage Area Network (SAN) fabric management service.';
                                   end;
    FACILITY_UTC:                  begin
                                     FacilityTag := 'FACILITY_UTC';
                                     FacilityDescr := FACILITYSOURCE + ' the Coordinated Universal Time (UTC) service.';
                                   end;
    FACILITY_WEP:                  begin
                                     FacilityTag := 'FACILITY_WEP';
                                     FacilityDescr := FACILITYSOURCE + ' the Network Location Awareness Service Provider (NLA) dervice';
                                   end;
    FACILITY_SYNCENGINE:           begin
                                     FacilityTag := 'FACILITY_SYNCENGINE';
                                     FacilityDescr := FACILITYSOURCE + ' the Sync Engine service.';
                                   end;
    FACILITY_XBOX:                 begin
                                     FacilityTag := 'FACILITY_XBOX';
                                     FacilityDescr := FACILITYSOURCE + ' the XBox service.';
                                   end;
    FACILITY_GAME:                 begin
                                     FacilityTag := 'FACILITY_GAME';
                                     FacilityDescr := FACILITYSOURCE + ' the DirectPlay service.r';
                                   end;
    FACILITY_PIX:                  begin
                                     FacilityTag := 'FACILITY_PIX';
                                     FacilityDescr := FACILITYSOURCE + ' the PIX for Windows tool service.';
                                   end
    else
      begin
        FacilityTag := 'UNKNOWN';
        FacilityDescr := FACILITYSOURCE + ' unknown.';
        hr := ERROR_NOT_FOUND;
      end;
  end;
  Result := hr;
end;

end.
