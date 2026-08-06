object MainWindow: TMainWindow
  Left = 0
  Top = 0
  BorderWidth = 1
  Caption = 'Capture Engine Application Sample'
  ClientHeight = 607
  ClientWidth = 1013
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    1013
    607)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSnapShot: TPanel
    Left = 724
    Top = 0
    Width = 288
    Height = 217
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    Color = clBackground
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object pbCapture: TPaintBox
      Left = 0
      Top = 0
      Width = 288
      Height = 191
      Align = alClient
      Color = clBackground
      ParentColor = False
    end
    object pnlControls: TPanel
      Left = 0
      Top = 191
      Width = 288
      Height = 26
      Align = alBottom
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      object butSaveToFile: TButton
        Left = 85
        Top = 1
        Width = 67
        Height = 23
        Hint = 'Save photo to file'
        Caption = '&Save To File'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = butSaveToFileClick
      end
      object butTakePhoto: TButton
        Left = 11
        Top = 1
        Width = 68
        Height = 23
        Hint = 'Save photo to file'
        Caption = '&Take Photo'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = butTakePhotoClick
      end
      object chkNoPreview: TCheckBox
        Left = 163
        Top = 6
        Width = 90
        Height = 14
        Caption = 'Auto Save'
        TabOrder = 2
      end
    end
  end
  object pnlInfo: TPanel
    Left = -2
    Top = 583
    Width = 1021
    Height = 26
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
  end
  object MainMenu: TMainMenu
    Left = 39
    Top = 29
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
    object Options1: TMenuItem
      Caption = 'Options'
      object mnuSetVideoOutputFormat: TMenuItem
        Caption = 'Set Video Output Format'
        OnClick = mnuSetVideoOutputFormatClick
      end
    end
  end
  object dlgSaveSnapShot: TSaveDialog
    DefaultExt = '.bmp'
    FileName = 'MyPhoto.bmp'
    Filter = 
      'BMP image (*.bmp)|*.bmp|PNG image (*.png)|*.png|JPEG image (*.jp' +
      'g, *.jpeg)|*.jpg'
    Left = 171
    Top = 29
  end
  object dlgSaveVideo: TSaveDialog
    DefaultExt = '.mp4'
    FileName = 'MyVideo.mp4'
    Filter = 
      'MPEG-4 (MP4)|.mp4|Windows Media Video (WMV)|.wmv|Audio Video Int' +
      'erleave (AVI)|.avi'
    Left = 317
    Top = 29
  end
end
