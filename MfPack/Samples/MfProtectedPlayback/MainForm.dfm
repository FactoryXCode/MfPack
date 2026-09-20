object ProtectedPlaybackForm: TProtectedPlaybackForm
  Left = 0
  Top = 0
  Caption = 'MF Protected Playback'
  ClientHeight = 600
  ClientWidth = 714
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object FLogMemo: TMemo
    Left = 0
    Top = 420
    Width = 714
    Height = 180
    Align = alBottom
    Color = 1515798
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object pnlVideo: TPanel
    Left = 0
    Top = 0
    Width = 714
    Height = 376
    Align = alClient
    BevelOuter = bvNone
    Color = 1515798
    ParentBackground = False
    TabOrder = 1
    OnResize = VideoResize
  end
  object pnlControls: TPanel
    Left = 0
    Top = 376
    Width = 714
    Height = 44
    Align = alBottom
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    object btnPlay: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Play'
      Enabled = False
      TabOrder = 0
      OnClick = PlayClick
    end
    object btnPause: TButton
      Left = 89
      Top = 9
      Width = 75
      Height = 25
      Caption = 'P&ause'
      Enabled = False
      TabOrder = 1
      OnClick = PauseClick
    end
    object btnStop: TButton
      Left = 170
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Stop'
      Enabled = False
      TabOrder = 2
      OnClick = StopClick
    end
  end
  object FOpenDialog: TOpenDialog
    Filter = 
      'Windows Media|*.wmv;*.wma;*.asf|MP3|*.mp3|AVI|*.avi|MP4|*.mp4;*.' +
      'm4v|MKV|*.mkv|WAV|*.wav|All files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 40
    Top = 32
  end
  object MainMenu1: TMainMenu
    Left = 88
    Top = 32
    object FileMenuItem: TMenuItem
      Caption = '&File'
      object FOpenFileItem: TMenuItem
        Caption = '&Open File...'
        ShortCut = 16463
        OnClick = OpenFileClick
      end
      object FOpenURLItem: TMenuItem
        Caption = 'Open &URL...'
        OnClick = OpenURLClick
      end
      object MenuSeparator: TMenuItem
        Caption = '-'
      end
      object ExitMenuItem: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitClick
      end
    end
  end
end
