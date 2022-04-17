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
  OnDestroy = FormDestroy
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
    Left = 801
    Top = 23
    Width = 277
    Height = 193
    Anchors = [akTop, akRight]
    BevelOuter = bvLowered
    ShowCaption = False
    TabOrder = 2
    object pbCapture: TPaintBox
      Left = 1
      Top = 1
      Width = 275
      Height = 191
      Align = alClient
      Color = clBackground
      ParentColor = False
      ExplicitTop = 0
    end
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 32
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
      end
      object mnuTakePhoto: TMenuItem
        Caption = 'Take Photo'
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
  object SaveFileDialog: TSaveDialog
    DefaultExt = '.mp4'
    FileName = 'MyVideo.mp4'
    Filter = 
      'BMP image (*.bmp)|*.bmp|PNG image (*.png)|*.png|JPEG image (*.jp' +
      'g, *.jpeg)|*.jpg'
    Left = 96
    Top = 32
  end
  object ApplicationEvents: TApplicationEvents
    OnMessage = ApplicationEventsMessage
    Left = 176
    Top = 33
  end
end
