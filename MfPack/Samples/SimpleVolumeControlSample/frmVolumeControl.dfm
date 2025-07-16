object VolumeControl: TVolumeControl
  Left = 0
  Top = 0
  Caption = 'Volume Control Sample'
  ClientHeight = 260
  ClientWidth = 517
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 15
    Top = 40
    Width = 107
    Height = 15
    Caption = 'SetMasterDbVolume'
  end
  object Label2: TLabel
    Left = -1
    Top = 88
    Width = 123
    Height = 15
    Caption = 'SetMasterScalarVolume'
  end
  object Edit1: TEdit
    Left = 133
    Top = 37
    Width = 91
    Height = 23
    NumbersOnly = True
    ReadOnly = True
    TabOrder = 0
    Text = '0.0'
  end
  object Edit2: TEdit
    Left = 133
    Top = 85
    Width = 91
    Height = 23
    NumbersOnly = True
    ReadOnly = True
    TabOrder = 1
    Text = '0.0'
  end
  object CheckBox1: TCheckBox
    Left = 93
    Top = 119
    Width = 55
    Height = 35
    Alignment = taLeftJustify
    Caption = 'Mute'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object tbDbVolume: TTrackBar
    Left = 230
    Top = 31
    Width = 253
    Height = 37
    PositionToolTip = ptLeft
    SelStart = -6
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = tbDbVolumeChange
  end
  object tbScVolume: TTrackBar
    Left = 230
    Top = 79
    Width = 253
    Height = 37
    DoubleBuffered = True
    ParentDoubleBuffered = False
    PositionToolTip = ptLeft
    SelEnd = 100
    TabOrder = 4
    TickMarks = tmBoth
    OnChange = tbScVolumeChange
  end
  object butClose: TButton
    Left = 15
    Top = 210
    Width = 119
    Height = 33
    Caption = 'Close'
    TabOrder = 5
    OnClick = butCloseClick
  end
  object MfAudioEndPoint1: TMfAudioEndPoint
    DeviceState = 'Active'
    MasterScalarVolume = 0.680000007152557400
    MasterDbVolume = -5.764730453491211000
    OnNotify = MfAudioEndPoint1Notify
    Left = 446
    Top = 142
  end
end
