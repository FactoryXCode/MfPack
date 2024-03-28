// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
// Kind: Pascal Unit
// Release date: 24-02-2024
// Language: ENU
//
// Revision Version: 3.1.6
// Description: The main window.
//
// Company: FactoryX
// Intiator(s): Renate Schaaf.
// Contributor(s): Renate Schaaf, Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.Code Sinkwriter and Transcode Examples.
//         Bitmaps2Video for Media Foundation.
//         https://github.com/rmesch/Bitmaps2Video-for-Media-Foundation
//
// Copyright © 2003-2024 Renate Schaaf
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
unit frmMain;

interface

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  Winapi.ShellAPI,
  Winapi.ShlObj,
  {VCL}
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ImgList,
  VCL.ComCtrls,
  Vcl.CheckLst,
  Vcl.FileCtrl,
  Vcl.Samples.Spin,
  {Needed by TImage}
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage,
  Vcl.Imaging.GIFImg,
  {System}
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.Types,
  System.Diagnostics,
  System.IOUtils,
  System.Threading,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {MfPack}
  WinApi.MfPack.VideoStandardsCheat,
  {Application}
  Scale,
  Common,
  Tools,
  ImageRenderer,
  Transformer;

const
  MsgUpdate = WM_User + 1;

  {$IFDEF MSWINDOWS}
    MAXPATH = MAX_PATH - 12; { TFile.FCMinFileNameLen = 12. There is a problem in IOUtils and we cannot user Max_Path. }
  {$ELSE}
    MAXPATH = MAX_PATH;
  {$ENDIF}


type

  TfrmMain = class(TForm)
    fodSelectAudio: TFileOpenDialog;
    OD: TFileOpenDialog;
    FODPic: TFileOpenDialog;
    stbStatus: TStatusBar;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    cbxFileFormat: TComboBox;
    Label9: TLabel;
    cbxVideoCodec: TComboBox;
    Label14: TLabel;
    cbxResolution: TComboBox;
    Label15: TLabel;
    spedSetQuality: TSpinEdit;
    Label16: TLabel;
    cbxFrameRates: TComboBox;
    lblCodecInfo: TLabel;
    Label17: TLabel;
    Panel5: TPanel;
    butRenderSlideshow: TButton;
    Background: TCheckBox;
    CropLandscape: TCheckBox;
    ZoomInOut: TCheckBox;
    DebugTiming: TCheckBox;
    Bevel3: TBevel;
    StaticText1: TStaticText;
    StaticText3: TStaticText;
    edLocation: TEdit;
    ShowVideo: TButton;
    pnlSelectPics: TPanel;
    Panel1: TPanel;
    dlbDir: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    Splitter1: TSplitter;
    Panel2: TPanel;
    lblImageCount: TLabel;
    lbxFileBox: TCheckListBox;
    Panel3: TPanel;
    lblRenderingOrder: TLabel;
    lbxRenderingOrder: TListBox;
    Splitter2: TSplitter;
    Panel4: TPanel;
    Label18: TLabel;
    imgPreview: TImage;
    Splitter3: TSplitter;
    Bevel4: TBevel;
    Label22: TLabel;
    spedEffectDuration: TSpinEdit;
    Bevel5: TBevel;
    Bevel6: TBevel;
    cbxPickWinFolder: TComboBox;
    butRunPreview: TButton;
    Bevel7: TBevel;
    pbPreview: TProgressBar;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    Bevel2: TBevel;
    spedCompensation: TSpinEdit;
    Label3: TLabel;
    pnlInclAudio: TPanel;
    Label19: TLabel;
    cbxAudioCodec: TComboBox;
    Label21: TLabel;
    mmoAudioCodecDescr: TMemo;
    chbxAddAudio: TCheckBox;
    lblAudioSource: TLabel;
    AudioStartTime: TSpinEdit;
    Label12: TLabel;
    cbxSetPresentationDuration: TCheckBox;
    Label2: TLabel;
    spedImageDuration: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure butRenderSlideshowClick(Sender: TObject);
    procedure FileExtChange(Sender: TObject);
    procedure CodecsChange(Sender: TObject);
    procedure ShowVideoClick(Sender: TObject);
    procedure cbxFileFormatChange(Sender: TObject);
    procedure cbxVideoCodecChange(Sender: TObject);
    procedure cbxFrameRatesChange(Sender: TObject);
    procedure dlbDirChange(Sender: TObject);
    procedure lbxFileBoxClick(Sender: TObject);
    procedure lbxFileBoxClickCheck(Sender: TObject);
    procedure lbxFileBoxDblClick(Sender: TObject);
    procedure lbxRenderingOrderClick(Sender: TObject);
    procedure lbxRenderingOrderDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxRenderingOrderStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure lbxRenderingOrderDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DriveComboBox1Click(Sender: TObject);
    procedure chbxAddAudioClick(Sender: TObject);
    procedure cbxSetPresentationDurationClick(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure cbxResolutionChange(Sender: TObject);
    procedure cbxAudioCodecChange(Sender: TObject);
    procedure cbxPickWinFolderChange(Sender: TObject);
    procedure butRunPreviewClick(Sender: TObject);
    procedure edLocationClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    ImageRenderer: TImageRenderer;
    fFileList: TStringlist;
    fSelectedFilesList: TStringList;
    fImageFiles: TStringlist;

    myPicturesPath: string;
    fOutputFile: string;
    fCodecList: TCodecIdArray;
    bWriting: Boolean;
    fFramebm: TBitmap;
    iSourcePos: Integer;
    dAspectRatio: Double;
    iVideoWidth: Integer;
    iVideoHeight: Integer;
    dSelectedVideoFrameRate: Double;

    gAudioCodec: TGUID;
    iSelectedAudioFormat: Integer;
    fSelectedAudioFormat: TMFAudioFormat;
    sAudioCodecDescr: string;
    fAudioFileName: TFileName;
    llAudioDuration: LONGLONG;
    iPicturePresentationTime: LONGLONG;

    // Opens selected Windows folder.
    procedure OpenFolder(fldrindex: Integer);
    function GetDesktopFolder(): string;

    function GetOutputFileName: string;

    function CalculatePicturePresentationTime(): Int64;

    // Procedure showing the use of TBitmapEncoder
    procedure MakeSlideshow(aFiles: TStringlist;
                            const aWicImage: TWicImage;
                            const aBitmapImage: TBitmap;
                            var aDone: Boolean;
                            aThreaded: Boolean);


    function GetDoCrop: Boolean;
    function GetDoZoomInOut: Boolean;
    function GetAudioFile: string;
    function GetQuality: Integer;

    function GetAudioStart: Int64;
    function GetAudioDialog: Boolean;
    procedure GetResolutions();
    procedure SetResolution();
    procedure GetFramerates();
    procedure SetFrameRate();

    function PIDLToPath(IdList: PItemIDList): string;
    function PidlFree(var IdList: PItemIDList): Boolean;
    function GetRandomZoom(): TZoom;

    // Message handlers
    procedure DoUpdate(var msg: TMessage); message MsgUpdate;

  public
    // properties which read the input parameters for the bitmap-encoder
    // off the controls of the form
    property OutputFileName: string read GetOutputFileName;

    property VideoFrameRate: Double read dSelectedVideoFrameRate;
    property Quality: Integer read GetQuality;
    property DoCrop: Boolean read GetDoCrop;
    property DoZoomInOut: Boolean read GetDoZoomInOut;

    property CurrentAudioCodec: TGUID read gAudioCodec;
    property AudioFile: string read GetAudioFile;

    property AudioStart: Int64 read GetAudioStart;
    property AudioDialog: Boolean read GetAudioDialog;

  end;

var
  FfrmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  dlgAudioFormats;

function TfrmMain.PIDLToPath(IdList: PItemIDList): string;
begin
  SetLength(Result,
            MAX_PATH);
  if SHGetPathFromIdList(IdList,
                         PChar(Result)) then
    SetLength(Result,
              StrLen(PChar(Result)))
  else
    Result := '';
end;


function TfrmMain.PidlFree(var IdList: PItemIDList): Boolean;
var
  Malloc: IMalloc;

begin
  Result := False;
  if (IdList = nil) then
    Result := True
  else
    begin
      if Succeeded(SHGetMalloc(Malloc)) and (Malloc.DidAlloc(IdList) > 0) then
        begin
          Malloc.Free(IdList);
          IdList := nil;
          Result := True;
        end;
  end;
end;


function TfrmMain.GetDesktopFolder(): string;
var
  FolderPidl: PItemIDList;

begin
  if Succeeded(SHGetSpecialFolderLocation(0,
                                          $0000,
                                          FolderPidl)) then
    begin
      Result := PIDLToPath(FolderPidl);
      PidlFree(FolderPidl);
    end
  else
    Result := '';
end;


function TfrmMain.GetRandomZoom(): TZoom;
begin
  Result.xCenter := 0.5 + (Random - 0.5) * 0.7;
  Result.yCenter := 0.5 + (Random - 0.5) * 0.7;

  Result.Radius := Min(1 - Result.xCenter,
                       Result.xCenter);

  Result.Radius := Min(Result.Radius,
                       1 - Result.yCenter);

  Result.Radius := Min(Result.Radius,
                       Result.yCenter);

  {$IFDEF DEBUG}
  Assert(Result.Radius > 0);
  {$ENDIF}
  Result.Radius := 0.5 * Result.Radius;
end;


procedure TfrmMain.MakeSlideshow(aFiles: TStringlist;
                                 const aWicImage: TWicImage;
                                 const aBitmapImage: TBitmap;
                                 var aDone: Boolean;
                                 aThreaded: Boolean);
var
  i: Integer;
  bCrop: Boolean;
  sngDice: Single;
  // TZoom is a record (xcenter, ycenter, radius) defining a virtual zoom-rectangle
  // (xcenter-radius, ycenter-radius, xcenter+radius, ycenter+radius).
  // This rectangle should be a sub-rectangle of [0,1]x[0,1].
  // If multipied by the width/height of a target rectangle, it defines
  // an aspect-preserving sub-rectangle of the target.
  zZooms: TZoom;
  zZoom: TZoom;
  DoInOut: Boolean;

begin
  aWicImage.LoadFromFile(aFiles.Strings[0]);

  WicToBmp(aWicImage,
           aBitmapImage);

  bCrop := (aBitmapImage.Width > aBitmapImage.Height) and DoCrop;

  ImageRenderer.AddStillImage(aBitmapImage,
                              iPicturePresentationTime, // 4000 = default
                              bCrop);

  PostMessage(Handle,
              MsgUpdate,
              0,
              0);

  if not aThreaded then
    Application.ProcessMessages;

  for i := 1 to aFiles.Count - 1 do
    begin
      aWicImage.LoadFromFile(aFiles.Strings[i]);

      WicToBmp(aWicImage,
               aBitmapImage);

      bCrop := (aBitmapImage.Width > aBitmapImage.Height) and DoCrop;
      sngDice := Random;
      DoInOut := DoZoomInOut and (sngDice < (1 / 3));

      if not DoInOut then
        ImageRenderer.CrossFadeTo(aBitmapImage,
                                  spedEffectDuration.Value, // 2000 = default
                                  bCrop)

      else
        begin
          zZooms := GetRandomZoom();
          zZoom := GetRandomZoom();

          ImageRenderer.ZoomInOutTransitionTo(aBitmapImage,
                                              zZooms,
                                              zZoom,
                                              spedEffectDuration.Value + 500,  // 2500
                                              bCrop);
        end;

      ImageRenderer.AddStillImage(aBitmapImage,
                                  iPicturePresentationTime, // 4000 = default
                                  bCrop);

      PostMessage(Handle,
                  MsgUpdate,
                  i,
                  0);

      if not aThreaded then
        Application.ProcessMessages;

    end;

  aDone := True;
end;


procedure TfrmMain.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := False;
  // Make sure we selected all items.
  if (cbxFileFormat.ItemIndex < 0) or
     (cbxVideoCodec.ItemIndex < 0) or
     (cbxResolution.ItemIndex < 0) or
     (cbxFrameRates.ItemIndex < 0) then
    begin
      ShowMessage('You did not complete the output settings.' + #13 + 'Please check.');
      Exit;
    end;
  cbxPickWinFolder.ItemIndex := 0;
  cbxPickWinFolderChange(nil);
  AllowChange := True;
end;


procedure TfrmMain.ShowVideoClick(Sender: TObject);
begin
  if not bWriting then
    if FileExists(OutputFileName) then
      ShellExecute(Handle,
                   'open',
                   PWideChar(OutputFileName),
                   nil,
                   nil,
                   SW_SHOWNORMAL);
end;


function TfrmMain.CalculatePicturePresentationTime(): Int64;
var
  i: Integer;
  //slImageFiles: TStringlist;
  Latency: DWord;
begin
  fImageFiles := TStringlist.Create;

    for i := 0 to fSelectedFilesList.Count - 1 do
      fImageFiles.Add(fSelectedFilesList.Strings[i]);

    if (fImageFiles.Count = 0) then
      begin
        ShowMessage('No image files selected!');
        Result := 0;
        Exit;
      end;

  // The video decoder/encoder and the audio codec produces a latency depending on the in and output format.
  // So, we use an average latency of about 0.030 ms pultiplied by the number of images.
  Latency := spedCompensation.Value * fImageFiles.Count;
  Result := Trunc((llAudioDuration / fImageFiles.Count) / 10000) - Latency;

end;


procedure TfrmMain.butRenderSlideshowClick(Sender: TObject);
var
  hr: HResult;
  Bitmap: TBitmap;
  WicImage: TWicImage;
  StopWatch: TStopWatch;
  Task: ITask;
  bDone: Boolean;

begin

  if bWriting then
    begin
      ShowMessage('Rendering in progress, wait until finished.');
      Exit;
    end;

  if (chbxAddAudio.Checked and (cbxAudioCodec.ItemIndex = -1)) then
    begin
      ShowMessage('No Audio Codec selected.' + #13 + 'Please select an Audio Codec to continue.');
      Exit;
    end;

  bWriting := True;

  try
    // Set presentation of the video to duration of the audio.
      if cbxSetPresentationDuration.Checked then
        begin
          iPicturePresentationTime := CalculatePicturePresentationTime();
        end
      else  // Default
        iPicturePresentationTime := spedImageDuration.Value; // Default = 4 sec.

    // Create the renderer.
    ImageRenderer := TImageRenderer.Create();
    Bitmap := TBitmap.Create();
    WicImage := TWicImage.Create();
    StopWatch := TStopWatch.Create();

    try
      stbStatus.SimpleText := 'Preparing the renderer, please wait... ';
      StopWatch.Start;

      hr := ImageRenderer.Initialize(OutputFileName,
                                     Quality,
                                     fCodecList[cbxVideoCodec.ItemIndex],
                                     fSelectedAudioFormat,
                                     cfBicubic,
                                     iPicturePresentationTime,
                                     llAudioDuration,
                                     fAudioFileName,
                                     AudioStart);

      if FAILED(hr) then
        begin
          ShowMessage(Format('The format of the input file or the settings of bitrate or sample rate are not supported.' +
                             'HResult: %d' +
                             'Try again with different settings.', [hr]));
          ImageRenderer.Finalize();
          Exit;
        end;

      ImageRenderer.TimingDebug := DebugTiming.Checked;

      if Background.Checked then
        begin
          bDone := false;
          Task := TTask.Run(procedure
                              begin
                                MakeSlideshow(fImageFiles,
                                              WicImage,
                                              Bitmap,
                                              bDone,
                                              True);
                              end);
          while not bDone do
            begin
              HandleThreadMessages(GetCurrentThread());
            end;

          Task.Wait();
          Application.ProcessMessages;
        end
      else
        begin
          MakeSlideshow(fImageFiles,
                        WicImage,
                        Bitmap,
                        bDone,
                        False);
        end;

      StopWatch.Stop;

      ImageRenderer.Finalize();

      stbStatus.SimpleText := Format('Rendering finished. Writing speed including decoding of image files and computing transitions: %s fps',
                                     [FloatToStrF(1000 * ImageRenderer.FrameCount / StopWatch.ElapsedMilliseconds,
                                                  ffFixed,
                                                  5,
                                                  2)]);
      stbStatus.Repaint;

    finally
      WicImage.Free;
      Bitmap.Free;
      ImageRenderer.Free;
    end;
  finally
    FreeAndNil(fImageFiles);
    bWriting := False;
  end;
end;


// new
procedure TfrmMain.chbxAddAudioClick(Sender: TObject);
begin
  pnlInclAudio.Enabled := chbxAddAudio.Checked;
end;


procedure TfrmMain.OpenFolder(fldrindex: Integer);
var
  hr: HResult;
  path: PWideChar;
  folderID: TGUID;

const
  FOLDERID_Pictures: TGUID = '{33E28130-4E1E-4676-835A-98395C3BC3BB}';
  FOLDERID_PublicPictures: TGUID = '{B6EBFB86-6907-413C-9AF7-4FC2ABF07CC5}';
  FOLDERID_Desktop: TGUID = '{B4BFCC3A-DB2C-424C-B029-7FE99A87C641}';
  FOLDERID_Downloads: TGUID = '{374DE290-123F-4565-9164-39C4925E467B}';
  FOLDERID_Favorites: TGUID = '{1777F761-68AD-4D8A-87BD-30B759FA33DD}';

begin
  case cbxPickWinFolder.ItemIndex of
    0: folderID := FOLDERID_Pictures;         // %USERPROFILE%\My Documents\My Pictures
    1: folderID := FOLDERID_PublicPictures;   // %ALLUSERSPROFILE%\Documents\My Pictures
    2: folderID := FOLDERID_Desktop;          // %USERPROFILE%\Desktop
    3: folderID := FOLDERID_Downloads;        // %USERPROFILE%\Downloads
    4: folderID := FOLDERID_Favorites;        // %USERPROFILE%\Favorites
  else
    Exit;
  end;

  hr := SHGetKnownFolderPath(folderID,
                             KF_FLAG_DEFAULT,
                             0,
                             path);

  if FAILED(hr) then
    ShowMessage('Can''t find selected path.')
  else
    begin
      // re-init dir.
      mypicturespath := WideCharToString(path);
      SetCurrentDir(mypicturespath);
      dlbDir.Directory := mypicturespath;
      dlbDir.Refresh;
    end;
  path := nil;
end;


procedure TfrmMain.cbxSetPresentationDurationClick(Sender: TObject);
begin
  spedImageDuration.Enabled := not cbxSetPresentationDuration.Checked;
end;


procedure TfrmMain.cbxVideoCodecChange(Sender: TObject);
begin
  lblCodecInfo.Caption := CodecInfos[fCodecList[cbxVideoCodec.ItemIndex]];
  // We don't use a label here, so we can copy the link to clipboard.
  edLocation.Text := Format('The output will be saved to: %s',
                            [OutputFileName]);
end;


// Run preview
procedure TfrmMain.butRunPreviewClick(Sender: TObject);
var
  i: Integer;
  j: Integer;

begin
  if (lbxRenderingOrder.Count = 0) then
    begin
      ShowMessage('You have to select images first!');
      Exit;
    end;

  pbPreview.Max := lbxRenderingOrder.Count - 1;

  for i := 0 to lbxRenderingOrder.Count - 1 do
    begin

      for j := 0 to fFileList.Count - 1 do
        begin
          if EndsText(lbxRenderingOrder.Items[i],
                      fFileList.Strings[j]) then
            begin
              // Load the image.
              imgPreview.Picture.LoadFromFile(fFileList.Strings[j]);
              Application.ProcessMessages;
              pbPreview.Position := i + 1;
              Application.ProcessMessages;
              // Get the image presentation duration.
              if cbxSetPresentationDuration.Checked then
                Sleep(CalculatePicturePresentationTime())
              else
                Sleep(spedImageDuration.Value);
              // clear image (effect duration)
              imgPreview.Picture.Assign(nil);
              Application.ProcessMessages;
              Sleep(spedEffectDuration.Value);
              Break;
            end;
        end;
    end;
  pbPreview.Position := 0;
end;


procedure TfrmMain.cbxAudioCodecChange(Sender: TObject);
var
  i: Integer;

label
  done;
begin

  iSelectedAudioFormat := 0;

  // listed in control's property Items.
  // ===================================
  // Advanced Audio Coding (AAC)
  // Free Lossless Audio Codec (FLAC)
  // Dolby AC-3 (AC-3)

  case cbxAudioCodec.ItemIndex of
    1: gAudioCodec := MFAudioFormat_AAC;
    2: gAudioCodec := MFAudioFormat_FLAC;
    3: gAudioCodec := MFAudioFormat_Dolby_AC3;
    else
      goto done;
  end;
  AudioFormatDlg.GetAudioFormats(gAudioCodec);
  sAudioCodecDescr := cbxAudioCodec.Items[cbxAudioCodec.ItemIndex];
  // List the required audioformat capabilities.

  if (AudioFormatDlg.ShowModal = mrOk) then
    begin
      iSelectedAudioFormat := AudioFormatDlg.iSelectedFormat;
      fSelectedAudioFormat := AudioFormatDlg.aAudioFmts[iSelectedAudioFormat];
    end
  else
    begin
      // User did not select a valid audio format.
      iSelectedAudioFormat := 0;
   end;

  if (iSelectedAudioFormat > 0) then
    begin
      mmoAudioCodecDescr.Clear();
      for i := 0 to AudioFormatDlg.fAudioCodecDescription.Count - 1 do
        mmoAudioCodecDescr.Lines.Append(AudioFormatDlg.fAudioCodecDescription.Strings[i]);
      mmoAudioCodecDescr.SelStart := 0;
      mmoAudioCodecDescr.SelLength := 1;
    end;

done:
  if (iSelectedAudioFormat = 0) then
    begin
      ShowMessage('You did not select a valid audio format!');
      cbxAudioCodec.ItemIndex := 0;
    end
  else
    begin
      // Select an audiofile.
      if AudioDialog then
        begin
          fAudioFileName := GetAudioFile();
          if (fAudioFileName = 'No file selected.') then
            begin
              lblAudioSource.Caption := fAudioFileName;
              Exit;
            end
          else
            lblAudioSource.Caption := fAudioFileName;
          // Get the length of the audiofile.
          if FAILED(GetFileDuration(StrToPWideChar(fAudioFileName),
                                    llAudioDuration)) then
            begin
              ShowMessage('Could not retrieve the duration of the audio file.');
              llAudioDuration := 0;
              iPicturePresentationTime := spedImageDuration.Value; // default = 4 sec.
              cbxSetPresentationDuration.Checked := False;
            end;
        end
      else
        Exit;
    end;
end;


procedure TfrmMain.cbxFileFormatChange(Sender: TObject);
var
  i: Integer;

begin
  fCodecList := GetSupportedCodecs(cbxFileFormat.Items[cbxFileFormat.ItemIndex]);
  cbxVideoCodec.Clear();

  for i := 0 to Length(fCodecList) - 1 do
    cbxVideoCodec.Items.Add(CodecNames[fCodecList[i]]);

  cbxVideoCodec.ItemIndex := 0;
  cbxVideoCodecChange(nil);

end;


// new
procedure TfrmMain.cbxFrameRatesChange(Sender: TObject);
begin
  SetFrameRate();
end;


procedure TfrmMain.cbxPickWinFolderChange(Sender: TObject);
begin
  OpenFolder(cbxPickWinFolder.ItemIndex);
end;


procedure TfrmMain.cbxResolutionChange(Sender: TObject);
begin
  SetResolution();
end;


procedure TfrmMain.CodecsChange(Sender: TObject);
begin
  lblCodecInfo.Caption := CodecInfos[fCodecList[cbxVideoCodec.ItemIndex]];
end;


procedure TfrmMain.dlbDirChange(Sender: TObject);
var
  Path,
  SearchStr: string;
  MaskLen,
  MaskPos,
  SepPos: Integer;
  i: Integer;
{$IF COMPILERVERSION < 30.0}
  strings: TArray<string>;
  ClassicStrings: TStringDynArray;
{$ENDIF}


const
  mask = '*.bmp;*.jpg;*.png;*.gif';

begin
  fFileList.Clear;
  myPicturesPath := dlbDir.Directory;
  Path := IncludeTrailingBackSlash(myPicturesPath);
  MaskLen := Length(mask);
  MaskPos := 0;

  while (MaskPos >= 0) do
    begin
      SepPos := Pos(';',
                    mask,
                    MaskPos + 1) - 1;

      if (SepPos >= 0) then
        SearchStr := Copy(mask,
                          MaskPos + 1,
                          SepPos - MaskPos)
      else
        SearchStr := Copy(mask,
                          MaskPos + 1,
                          MaskLen);


    {$IF COMPILERVERSION > 34.0}  {Delphi 10.4 Sydney}
      fFileList.AddStrings(TDirectory.GetFiles(Path,
                                               SearchStr,
                                               TSearchOption.soTopDirectoryOnly));
    {$ELSE}
      ClassicStrings := TDirectory.GetFiles(Path,
                                            SearchStr,
                                            TSearchOption.soTopDirectoryOnly);

      if (Length(ClassicStrings) > 0) then
        begin
          SetLength(Strings,
                    Length(ClassicStrings));
          // Copy all fields to the new array
          for i := 0 to Length(ClassicStrings) -1 do
            Strings[i] := ClassicStrings[i];

          fFileList.AddStrings(Strings);
        end;

      SetLength(ClassicStrings,
                0);
      ClassicStrings := nil;
    {$ENDIF}

      if (SepPos >= 0) then
        begin
          inc(SepPos);
          if (SepPos >= MaskLen) then
            SepPos := -1;
        end;
      MaskPos := SepPos;
    end;

  // Natural sorting order, e.g. '7' '8' '9' '10'
  fFileList.CustomSort(LogicalCompare);

  lbxFileBox.Clear;
  for i := 0 to fFileList.Count - 1 do
    lbxFileBox.Items.Add(ExtractFileName(fFileList.Strings[i]));

  lbxFileBox.SelectAll();

end;



procedure TfrmMain.DoUpdate(var msg: TMessage);
begin
  stbStatus.SimpleText := Format('Image %d',
                                 [(msg.WParam + 1)]);
  stbStatus.Repaint;
end;


procedure TfrmMain.DriveComboBox1Click(Sender: TObject);
begin
  DriveComboBox1.Drive;
end;


procedure TfrmMain.edLocationClick(Sender: TObject);
begin
  //
  if not bWriting then
    if FileExists(OutputFileName) then
      ShellExecute(Application.Handle,
                   'open',
                   'explorer.exe',
                   PChar(Format('/select," %s "',
                                [OutputFileName])),
                   nil,
                   SW_NORMAL);
end;


procedure TfrmMain.FileExtChange(Sender: TObject);
var
  i: integer;

begin
  fCodecList := GetSupportedCodecs(cbxFileFormat.Items[cbxFileFormat.ItemIndex]);
  cbxVideoCodec.Clear;

  for i := 0 to Length(fCodecList) - 1 do
    cbxVideoCodec.Items.Add(CodecNames[fCodecList[i]]);

  cbxVideoCodec.ItemIndex := 0;
  cbxVideoCodecChange(nil);
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  if Assigned(fFileList) then
    FreeAndNil(fFileList);

  if Assigned(fSelectedFilesList) then
    FreeAndNil(fSelectedFilesList);

  if Assigned(fImageFiles) then
    FreeAndNil(fImageFiles);

  if Assigned(fFramebm) then
    fFramebm.Free;

  if Assigned(fVideoStandardsCheat) then
    FreeAndNil(fVideoStandardsCheat);
  CanClose := True;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin

  fFileList := TStringlist.Create;
  fSelectedFilesList := TStringlist.Create;
  fOutputFile := GetDesktopFolder + '\Example';
  fCodecList := GetSupportedCodecs('.mp4');

  for i := 0 to Length(fCodecList) - 1 do
    cbxVideoCodec.Items.Add(CodecNames[fCodecList[i]]);

  cbxVideoCodec.ItemIndex := 0;
  cbxVideoCodecChange(nil);
  cbxAudioCodec.ItemIndex := 0;

  // Load Video standards.
  fVideoStandardsCheat := TVideoStandardsCheat.Create();
  GetResolutions();
  GetFramerates();
  Randomize;
end;


function TfrmMain.GetAudioDialog: Boolean;
begin
  Result := chbxAddAudio.Checked;
end;


procedure TfrmMain.GetResolutions();
var
  i: Integer;

begin
  cbxResolution.Clear();
  // Get all resolutions.
  // This will be done automaticly when creating VideoStandardsCheat See private method GetResolutions();

  for i := 0 to Length(fVideoStandardsCheat.Resolutions) -1 do
    cbxResolution.Items.Append(Format('%s (%d x %d) Aspect Ratio: %s', [fVideoStandardsCheat.Resolutions[i].Resolution,
                                                                        fVideoStandardsCheat.Resolutions[i].iWidth,
                                                                        fVideoStandardsCheat.Resolutions[i].iHeight,
                                                                        fVideoStandardsCheat.Resolutions[i].StrAspectRatio]));
  // Default resolution and aspect ratio.
  cbxResolution.ItemIndex := 120; // 720p, 16:9
  SetResolution();
end;


procedure TfrmMain.SetResolution();
begin
  // Store current resolution.
  fVideoStandardsCheat.SetResolutionByIndex(cbxResolution.ItemIndex);
  iVideoWidth := fVideoStandardsCheat.SelectedResolution.iWidth;
  iVideoHeight := fVideoStandardsCheat.SelectedResolution.iHeight;
  dAspectRatio := fVideoStandardsCheat.SelectedResolution.AspectRatio;
end;


procedure TfrmMain.GetFramerates();
var
  i: Integer;
begin
  cbxFrameRates.Clear();

  for i := 0 to Length(fVideoStandardsCheat.FrameRates) -1 do
    cbxFrameRates.Items.Append(fVideoStandardsCheat.FrameRates[i].sFrameRate);
  cbxFrameRates.ItemIndex := 5; // 18 fps
  cbxFrameRatesChange(nil);
end;


procedure TfrmMain.SetFrameRate();
begin
  fVideoStandardsCheat.SetFrameRateByIndex(cbxFrameRates.ItemIndex);
  cbxFrameRates.Hint := fVideoStandardsCheat.SelectedFrameRate.sHint;
  dSelectedVideoFrameRate := fVideoStandardsCheat.SelectedFrameRate.FrameRate;
end;


function TfrmMain.GetAudioFile(): string;
begin
  Result := 'No audiofile selected.';
  fodSelectAudio.FileName := '';
  if not fodSelectAudio.Execute(Handle) then
    Exit;

  Result := fodSelectAudio.FileName;
end;


function TfrmMain.GetAudioStart: int64;
begin
  Result := AudioStartTime.Value;
end;


function TfrmMain.GetDoCrop: boolean;
begin
  Result := CropLandscape.Checked;
end;


function TfrmMain.GetDoZoomInOut: boolean;
begin
  Result := ZoomInOut.Checked;
end;


function TfrmMain.GetOutputFileName: string;
begin
  Result := fOutputFile + '_' + CodecShortNames[fCodecList[cbxVideoCodec.ItemIndex]] +
    cbxFileFormat.Text;
end;


function TfrmMain.GetQuality: integer;
begin
  Result := spedSetQuality.Value;
end;


procedure TfrmMain.lbxFileBoxClick(Sender: TObject);
begin
  // Show a preview of the selected imagefile.
  imgPreview.Picture.LoadFromFile(fFileList.Strings[lbxFileBox.ItemIndex]);
end;


procedure TfrmMain.lbxFileBoxClickCheck(Sender: TObject);
var
  i: Integer;
  n: Integer;
  selTxt: string;

begin
  n := 0;
  // Alternative for Selcount.
  for i := 0 to lbxFileBox.Items.Count -1 do
    if lbxFileBox.Checked[i] then
      Inc(n);

  if (n = 1)  then
    selTxt := 'image'
  else
    selTxt := 'images';

  lblImageCount.Caption := Format('Selected %d %s',
                                  [n, selTxt]);

  // Add or remove checked file to rendering order.
  if lbxFileBox.Checked[lbxFileBox.ItemIndex] then
    begin
      lbxRenderingOrder.Items.Append(lbxFileBox.Items[lbxFileBox.ItemIndex]);
      fSelectedFilesList.Append(fFileList.Strings[lbxFileBox.ItemIndex]);
    end
  else
    begin
      i := lbxRenderingOrder.Items.Count - 1;
      repeat
        if (lbxFileBox.Items[lbxFileBox.ItemIndex] = lbxRenderingOrder.Items[i]) then
          begin
            lbxRenderingOrder.Items.Delete(i);
            fSelectedFilesList.Delete(i);
          end;
        Dec(i);
      until (i < 0);
    end;
end;


procedure TfrmMain.lbxFileBoxDblClick(Sender: TObject);
begin
  // Add a duplicate
  if lbxFileBox.Checked[lbxFileBox.ItemIndex] then
    begin
      lbxRenderingOrder.Items.Append(lbxFileBox.Items[lbxFileBox.ItemIndex]);
      fSelectedFilesList.Append(fFileList.Strings[lbxFileBox.ItemIndex]);
    end
end;


procedure TfrmMain.lbxRenderingOrderClick(Sender: TObject);
var
  i: Integer;

begin
  // Show a preview of the selected imagefile.
  for i := 0 to fFileList.Count - 1 do
    begin
      if EndsText(lbxRenderingOrder.Items[lbxRenderingOrder.ItemIndex],
                  fFileList.Strings[i]) then
        begin
          imgPreview.Picture.LoadFromFile(fFileList.Strings[i]);
          Break;
        end;
    end;
end;


procedure TfrmMain.lbxRenderingOrderDragDrop(Sender, Source: TObject; X,
   Y: Integer);

var
  TargetPos: Integer;
  i: Integer;
  j: Integer;

begin

  TargetPos := lbxRenderingOrder.itemAtPos(Point(X, Y),
                                           False);

 if (TargetPos >= 0) and (TargetPos < lbxRenderingOrder.Count) then
   begin
     lbxRenderingOrder.Items.Move(iSourcePos,
                                  TargetPos);
     lbxRenderingOrder.ItemIndex := TargetPos;

     // Clear and update the fSelectedFilesList.
     fSelectedFilesList.Clear;
     for i := 0 to lbxRenderingOrder.Count - 1 do
       begin   //fFileList
         for j := 0 to fFileList.Count - 1 do
           begin
             if EndsText(lbxRenderingOrder.Items[i],
                         fFileList.Strings[j]) then
               fSelectedFilesList.Append(fFileList.Strings[j]);
           end;
       end;
   end;

end;


procedure TfrmMain.lbxRenderingOrderDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;


procedure TfrmMain.lbxRenderingOrderStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  iSourcePos := lbxRenderingOrder.ItemIndex;
end;


initialization
  // A gui app should always use COINIT_APARTMENTTHREADED in stead of COINIT_MULTITHREADED!
  CoInitializeEx(nil,
                COINIT_APARTMENTTHREADED);

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_FULL)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version ' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;

finalization

  MFShutdown();
  CoUnInitialize();

end.
