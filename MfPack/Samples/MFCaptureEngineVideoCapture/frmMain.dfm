object MainWindow: TMainWindow
  Left = 0
  Top = 0
  BorderWidth = 1
  Caption = 'Capture Engine Application Sample'
  ClientHeight = 654
  ClientWidth = 1091
  Color = 5197615
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    1091
    654)
  TextHeight = 13
  object pnlSnapShot: TPanel
    Left = 780
    Top = 0
    Width = 310
    Height = 234
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    Color = clBackground
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    ExplicitLeft = 776
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
      object chkNoPreview: TCheckBox
        Left = 176
        Top = 6
        Width = 97
        Height = 15
        Caption = 'Auto Save'
        TabOrder = 2
      end
    end
  end
  object pnlInfo: TPanel
    Left = -2
    Top = 627
    Width = 1099
    Height = 28
    Align = alCustom
    Anchors = [akLeft, akRight]
    AutoSize = True
    Caption = '-'
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    ExplicitTop = 626
    ExplicitWidth = 1095
  end
  object MainMenu: TMainMenu
    Left = 32
    Top = 38
    object Capture1: TMenuItem
      Caption = 'Capture'
      object mnuChooseDevice: TMenuItem
        Caption = 'Choose Device'
        OnClick = mnuChooseDeviceClick
      end
      object mnuStartPreview: TMenuItem
        Caption = 'Start Preview'
        OnClick = mnuStartPreviewClick
      end
      object mnuStartRecording: TMenuItem
        Caption = 'Start Recording'
        Enabled = False
        OnClick = mnuStartRecordingClick
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
