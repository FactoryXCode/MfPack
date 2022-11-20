object MainWindow: TMainWindow
  Left = 0
  Top = 0
  BorderWidth = 1
  Caption = 'Capture Engine Application Sample'
  ClientHeight = 736
  ClientWidth = 1084
  Color = clBtnFace
  DoubleBuffered = True
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
    1084
    736)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPreview: TPanel
    Left = 0
    Top = 0
    Width = 1084
    Height = 736
    Align = alClient
    AutoSize = True
    Color = 5197615
    UseDockManager = False
    ParentBackground = False
    ParentShowHint = False
    ShowCaption = False
    ShowHint = False
    TabOrder = 0
    ExplicitLeft = -1
    ExplicitWidth = 1086
    ExplicitHeight = 738
    object pnlInfo: TPanel
      Left = 1
      Top = 707
      Width = 1082
      Height = 28
      Align = alBottom
      Alignment = taLeftJustify
      AutoSize = True
      Caption = 'pnlInfo'
      ParentBackground = False
      TabOrder = 0
      ExplicitTop = 692
      ExplicitWidth = 1084
    end
  end
  object pnlSnapShot: TPanel
    Left = 773
    Top = 0
    Width = 310
    Height = 234
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    Color = clBackground
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    ExplicitLeft = 775
    object pbCapture: TPaintBox
      Left = 0
      Top = 0
      Width = 310
      Height = 206
      Align = alClient
      Color = clBackground
      ParentColor = False
      ExplicitLeft = 2
      ExplicitHeight = 201
    end
    object pnlControls: TPanel
      Left = 0
      Top = 206
      Width = 310
      Height = 28
      Align = alBottom
      Caption = 'pnlControls'
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      object butSaveToFile: TButton
        Left = 91
        Top = 1
        Width = 73
        Height = 25
        Hint = 'Save photo to file'
        Caption = '&Save To File'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = butSaveToFileClick
      end
      object butTakePhoto: TButton
        Left = 12
        Top = 1
        Width = 73
        Height = 25
        Hint = 'Save photo to file'
        Caption = '&Take Photo'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = butTakePhotoClick
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 32
    Top = 40
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
        OnClick = Exit1Click
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
    Top = 38
  end
end
