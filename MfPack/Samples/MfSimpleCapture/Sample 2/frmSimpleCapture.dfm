object Frm_SimpleCapture: TFrm_SimpleCapture
  Left = 0
  Top = 0
  Caption = 'MfSimpleCapture Sample 2'
  ClientHeight = 410
  ClientWidth = 829
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnPaint = FormPaint
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 361
    Width = 829
    Height = 49
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    object Button1: TButton
      Left = 10
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 0
      OnClick = Button1Click
    end
    object butGetDevice: TButton
      Left = 183
      Top = 13
      Width = 108
      Height = 25
      Caption = 'Get Capture Device'
      TabOrder = 1
      OnClick = butGetDeviceClick
    end
    object butShowProperties: TButton
      Left = 297
      Top = 13
      Width = 108
      Height = 25
      Caption = 'Properties'
      Enabled = False
      TabOrder = 2
      OnClick = butShowPropertiesClick
    end
  end
  object pnlVideo: TPanel
    Left = 0
    Top = 0
    Width = 481
    Height = 361
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    Color = clNone
    Ctl3D = False
    DoubleBuffered = True
    ParentBackground = False
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 1
    OnResize = pnlVideoResize
  end
  object pnlCameraAndVideoControl: TPanel
    Left = 481
    Top = 0
    Width = 348
    Height = 361
    Align = alRight
    TabOrder = 2
    Visible = False
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
      OnSelect = cbxVideoControlPropertySelect
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
      OnChange = HandleRotationChanged
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
end
