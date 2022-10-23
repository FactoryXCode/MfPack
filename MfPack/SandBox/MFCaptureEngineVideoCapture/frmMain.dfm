object MainWindow: TMainWindow
  Left = 0
  Top = 0
  Caption = 'Capture Engine Application Sample'
  ClientHeight = 738
  ClientWidth = 1086
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    1086
    738)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 1086
    Height = 17
    Align = alTop
    ExplicitTop = -6
    ExplicitWidth = 1011
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 705
    Width = 1086
    Height = 33
    Panels = <>
    SimplePanel = True
    SimpleText = 'Please'
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 17
    Width = 1086
    Height = 688
    Align = alClient
    Color = 5197615
    ParentBackground = False
    ParentShowHint = False
    ShowCaption = False
    ShowHint = False
    TabOrder = 1
  end
  object pnlSnapShot: TPanel
    Left = 768
    Top = 23
    Width = 310
    Height = 234
    Anchors = [akTop, akRight]
    BevelOuter = bvLowered
    ShowCaption = False
    TabOrder = 2
    object pbCapture: TPaintBox
      Left = 1
      Top = 1
      Width = 308
      Height = 199
      Align = alClient
      Color = clBackground
      ParentColor = False
      ExplicitTop = 0
    end
    object Bevel2: TBevel
      Left = 1
      Top = 200
      Width = 308
      Height = 33
      Align = alBottom
      ExplicitTop = 206
    end
    object butSaveToFile: TButton
      Left = 91
      Top = 206
      Width = 73
      Height = 25
      Hint = 'Save photo to file'
      Caption = '&Save To File'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object butTakePhoto: TButton
      Left = 12
      Top = 206
      Width = 73
      Height = 25
      Hint = 'Save photo to file'
      Caption = '&Take Photo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = butTakePhotoClick
    end
  end
  object MainMenu: TMainMenu
    Left = 30
    Top = 36
    object Capture1: TMenuItem
      Caption = 'Capture'
      object mnuStartPreview: TMenuItem
        Caption = 'Start Preview'
        OnClick = mnuStartPreviewClick
      end
      object mnuChooseDevice: TMenuItem
        Caption = 'Choose Device'
        OnClick = mnuChooseDeviceClick
      end
      object mnuStartRecording: TMenuItem
        Caption = 'Start Recording'
        Enabled = False
        OnClick = mnuStartRecordingClick
      end
      object mnuTakePhoto: TMenuItem
        Caption = 'Take Photo'
        Enabled = False
        OnClick = mnuTakePhotoClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
      end
    end
  end
  object SaveFileDlg: TSaveDialog
    DefaultExt = '.mp4'
    FileName = 'MyPhoto.bmp'
    Filter = 
      'BMP image (*.bmp)|*.bmp|PNG image (*.png)|*.png|JPEG image (*.jp' +
      'g, *.jpeg)|*.jpg'
    Left = 96
    Top = 36
  end
end
