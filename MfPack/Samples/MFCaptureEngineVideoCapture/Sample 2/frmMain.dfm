object MainWindow: TMainWindow
  Left = 0
  Top = 0
  Margins.Left = 2
  Margins.Top = 2
  Margins.Right = 2
  Margins.Bottom = 2
  Anchors = [akTop]
  BorderWidth = 1
  Caption = 'Capture Engine Application Sample 2'
  ClientHeight = 674
  ClientWidth = 1093
  Color = clDarkslategray
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
    1093
    674)
  TextHeight = 13
  object pnlSnapShot: TPanel
    Left = 754
    Top = 0
    Width = 338
    Height = 234
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    Color = clBackground
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    ExplicitLeft = 748
    object pbCapture: TPaintBox
      Left = 0
      Top = 0
      Width = 338
      Height = 206
      Align = alClient
      Color = clBackground
      ParentColor = False
      ExplicitLeft = 60
      ExplicitWidth = 250
    end
    object pnlControls: TPanel
      Left = 0
      Top = 206
      Width = 338
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
        Left = 175
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
    Top = 648
    Width = 1101
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
    ExplicitTop = 631
    ExplicitWidth = 1095
  end
  object pnlCameraAndVideoControl: TPanel
    Left = 754
    Top = 235
    Width = 338
    Height = 222
    Align = alCustom
    Anchors = [akTop, akRight]
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    Visible = False
    ExplicitLeft = 748
    object Label1: TLabel
      Left = 14
      Top = 10
      Width = 95
      Height = 14
      AutoSize = False
      Caption = 'Camera Settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 14
      Top = 87
      Width = 95
      Height = 14
      AutoSize = False
      Caption = 'Video Settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 14
      Top = 32
      Width = 95
      Height = 13
      AutoSize = False
      Caption = 'Control Property'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 14
      Top = 107
      Width = 95
      Height = 19
      AutoSize = False
      Caption = 'Control Property'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 188
      Top = 32
      Width = 95
      Height = 13
      AutoSize = False
      Caption = 'Value'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 188
      Top = 115
      Width = 53
      Height = 14
      AutoSize = False
      Caption = 'Value'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 258
      Top = 32
      Width = 95
      Height = 13
      AutoSize = False
      Caption = 'Control'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 258
      Top = 115
      Width = 95
      Height = 16
      AutoSize = False
      Caption = 'Control'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblRotation: TLabel
      Left = 18
      Top = 159
      Width = 48
      Height = 13
      Caption = 'Rotation: '
    end
    object cbxCameraControlProperty: TComboBox
      Left = 14
      Top = 48
      Width = 163
      Height = 21
      ItemIndex = 0
      TabOrder = 0
      Text = 'Pan'
      OnSelect = cbxCameraControlPropertySelect
      Items.Strings = (
        'Pan'
        'Tilt'
        'Roll'
        'Zoom'
        'Exposure'
        'Iris'
        'Focus')
    end
    object cbxVideoControlProperty: TComboBox
      Left = 14
      Top = 128
      Width = 163
      Height = 21
      TabOrder = 1
      Text = 'Brightness '
      Items.Strings = (
        'Brightness '
        'Contrast'
        'Hue'
        'Saturation'
        'Sharpness'
        'Gamma'
        'ColorEnable'
        'WhiteBalance'
        'BacklightCompensation'
        'Gain')
    end
    object cbxCameraControlFlags: TComboBox
      Left = 258
      Top = 48
      Width = 73
      Height = 21
      TabOrder = 2
      Text = 'Manual'
      OnSelect = cbxCameraControlFlagsSelect
      Items.Strings = (
        'Manual'
        'Automatic'
        'Asynchronous'
        'Absolute'
        'Relative')
    end
    object cbxVideoControlFlags: TComboBox
      Left = 258
      Top = 129
      Width = 73
      Height = 21
      ItemIndex = 0
      TabOrder = 3
      Text = 'Manual'
      OnSelect = cbxVideoControlFlagsSelect
      Items.Strings = (
        'Manual'
        'Automatic')
    end
    object cboRotation: TComboBox
      Left = 16
      Top = 179
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = '0'
      Items.Strings = (
        '0'
        '90'
        '180'
        '270')
    end
    object cbxCameraValues: TComboBox
      Left = 188
      Top = 48
      Width = 59
      Height = 21
      ItemIndex = 0
      TabOrder = 5
      Text = '0'
      OnSelect = cbxCameraValuesSelect
      Items.Strings = (
        '0')
    end
    object cbxVideoValues: TComboBox
      Left = 188
      Top = 128
      Width = 59
      Height = 21
      ItemIndex = 0
      TabOrder = 6
      Text = '0'
      OnSelect = cbxVideoValuesSelect
      Items.Strings = (
        '0')
    end
  end
  object MainMenu: TMainMenu
    Left = 74
    Top = 37
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
      object mnuCameraVideoSettings: TMenuItem
        Caption = 'Camera And Video Settings'
        Enabled = False
        OnClick = mnuCameraVideoSettingsClick
      end
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
    Left = 72
    Top = 101
  end
  object dlgSaveVideo: TSaveDialog
    DefaultExt = '.mp4'
    FileName = 'MyVideo.mp4'
    Filter = 
      'MPEG-4 (MP4)|.mp4|Windows Media Video (WMV)|.wmv|Audio Video Int' +
      'erleave (AVI)|.avi'
    Left = 71
    Top = 165
  end
end
