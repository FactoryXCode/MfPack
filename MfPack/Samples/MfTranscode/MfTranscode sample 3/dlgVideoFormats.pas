unit dlgVideoFormats;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Grids;

type
  TVideoFormatDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Bevel2: TBevel;
    Panel1: TPanel;
    Label1: TLabel;
    cbxContainerFmt: TComboBox;
    Panel2: TPanel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Panel3: TPanel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    Label6: TLabel;
    stxtBitRate: TStaticText;
    stxtSampleRate: TStaticText;
    stxtBitsPerSample: TStaticText;
    stxtChannels: TStaticText;
    sgAudioFormats: TStringGrid;
    ComboBox3: TComboBox;
    Label4: TLabel;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    StaticText6: TStaticText;
  private
    { Private declarations }
    gVideoMediaFmt: TGuid;
    gContainerFormat: TGuid;

    procedure SetFormat(aVal: TGuid);

  public
    { Public declarations }
  end;

var
  VideoFormatDlg: TVideoFormatDlg;

implementation

{$R *.dfm}


procedure TVideoFormatDlg.SetFormat(aVal: TGuid);
begin
  // Set the outputformat

{end;
      case iSelectedContainerFmt of
      // Video /////////////////////////////////////////////////////////////
      1: begin
           // avi
           gVideoMediaFmt := MFVideoFormat_H264;
           gAudioMediaFmt := MFAudioFormat_AAC;
           gContainerFormat := MFTranscodeContainerType_AVI;
           // We keep it simpel; show user the target formats.
           lblContainer.Caption := 'Audio Video Interleave (AVI)';
           lblVideo.Caption := 'H.264 video encoder';
           lblAudio.Caption := 'Advanced Audio Coding (AAC)';
         end;
      2: begin
           // mp4 AAC
           gVideoMediaFmt := MFVideoFormat_H265;
           gAudioMediaFmt := MFAudioFormat_AAC;
           gContainerFormat := MFTranscodeContainerType_MPEG4;
           lblContainer.Caption := 'MPEG 4 (mp4)';
           lblVideo.Caption := 'H.265/HEVC video encoder';
           lblAudio.Caption := 'Advanced Audio Coding (AAC)';
         end;
      3: begin
           // MPEG-4 Video and Dolby AC-3 audio (Dolby Digital Audio Encoder)
           gVideoMediaFmt := MFVideoFormat_H264;
           gAudioMediaFmt := MFAudioFormat_Dolby_AC3;
           gContainerFormat := MFTranscodeContainerType_MPEG4;
           lblContainer.Caption := 'MPEG 4 (mp4)';
           lblVideo.Caption := 'H.264 video encoder';
           lblAudio.Caption := 'Dolby AC-3 audio';
         end;
      4: begin
           // wmv
           gVideoMediaFmt := MFVideoFormat_WMV3;
           gAudioMediaFmt := MFAudioFormat_WMAudioV9;
           gContainerFormat := MFTranscodeContainerType_ASF;
           lblContainer.Caption := 'Advanced Systems Format (ASF)';
           lblVideo.Caption := 'Windows Media Video (WMV)';
           lblAudio.Caption := 'Windows Media Audio (WMA)';
         end;
         }
end;

end.
