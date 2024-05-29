unit VideoMftClass;

interface

uses
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.WinError,
  System.Classes,
  System.SysUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfTransform,
  Profiles;

type
  TVideoProfile = (profile_H264,
                   profile_H265,
                   profile_MPEG2,
                   profile_WMV3);

  TVideoMft = class(TObject)
    private
      iSelectedMftInput, iSelectedMftOutput: Integer;
      pa_VideoProfile: TVideoProfile;
      pa_mftEnum: TMftEnumInfo;

    public

      constructor Create();
      destructor Destroy(); override;

      // Get video mft
      function GetMftInOutFormats(const aMft: CLSID; // like CLSID_CMSH264EncoderMFT
                                  mftEnumFlag: MFT_ENUM_FLAG // like MFT_ENUM_FLAG_ALL
                                  ): HResult;

      property VideoFormats: TMftEnumInfo read pa_mftEnum;
      property VideoProfiles: TVideoProfile read pa_VideoProfile;

  end;

var
  FVideoMft: TVideoMft;


implementation

uses
  dlgMft;

constructor TVideoMft.Create();
begin
  inherited Create();

end;


destructor TVideoMft.Destroy();
begin
  pa_mftEnum.Reset();
  CoTaskMemFree(@pa_mftEnum);
  inherited Destroy;
end;


function TVideoMft.GetMftInOutFormats(const aMft: CLSID;
                                      mftEnumFlag: MFT_ENUM_FLAG): HResult;
var
  hr: HResult;
  mftCodec: IMFTransform;

begin
  pa_mftEnum.Reset();

  // Get the encoder formats from the selected videoformat.
  hr := GetMftEnumInfo(aMft,
                       pa_mftEnum);

  if SUCCEEDED(hr) then
    begin
      // Show dialog Mft properties
      if Assigned(MftDlg) then
        begin
          // Show the videoformats dialog to select the output encoding parameters of the choosen format.
          if (MftDlg.ShowModal = 1 {mrOk}) then
            begin
              iSelectedMftInput := MftDlg.iSelectedInput;
              iSelectedMftOutput := MftDlg.iSelectedOutput;

              // Get the video encoder.
              hr := GetCodec(@pa_mftEnum.aInputTypes[iSelectedMftInput].RegisterTypeInfo,
                             @pa_mftEnum.aOutputTypes[iSelectedMftOutput].RegisterTypeInfo,
                             MFT_CATEGORY_VIDEO_ENCODER,
                             mftCodec,
                             mftEnumFlag,
                             iSelectedMftOutput);
            end
          else
            begin
              // User canceled device selection by user.
              Result := ERROR_CANCELLED;
              Exit;
           end;
        end
      else
        begin
          // Dialog not initialized.
          Result := E_POINTER;
          Exit;
       end;
    end;

  // Clone. Caller should dispose the record.
  //CopyMemory(@mftEnum,
  //           @pa_mftEnum,
  //           SizeOf(pa_mftEnum));

  Result := hr;
end;

end.
