object frmEqSettings: TfrmEqSettings
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsDialog
  Caption = 'EQ Settings'
  ClientHeight = 358
  ClientWidth = 725
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 144
  TextHeight = 23
  object grpLow: TGroupBox
    Left = 18
    Top = 18
    Width = 307
    Height = 115
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Low shelf'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lblLowFreqHz: TLabel
      Left = 29
      Top = 30
      Width = 133
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Freq (Hz)'
    end
    object lblLowSlope: TLabel
      Left = 29
      Top = 66
      Width = 133
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Slope S (0.1..4)'
    end
    object edtLowFreqHz: TEdit
      Left = 180
      Top = 27
      Width = 90
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      Text = '100'
    end
    object udLowFreqHz: TUpDown
      Left = 270
      Top = 33
      Width = 24
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = edtLowFreqHz
      Min = 20
      Max = 400
      Position = 100
      TabOrder = 1
    end
    object edtLowSlope: TEdit
      Left = 180
      Top = 63
      Width = 90
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 2
      Text = '100'
    end
    object udLowSlope: TUpDown
      Left = 270
      Top = 69
      Width = 24
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = edtLowSlope
      Min = 10
      Max = 400
      Position = 100
      TabOrder = 3
    end
  end
  object grpMid: TGroupBox
    Left = 342
    Top = 18
    Width = 360
    Height = 240
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Mid band'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lblMidFreqHz: TLabel
      Left = 34
      Top = 33
      Width = 115
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Freq (Hz)'
    end
    object lblMidQ: TLabel
      Left = 34
      Top = 69
      Width = 115
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Bandwidth Q'
    end
    object edtMidFreqHz: TEdit
      Left = 163
      Top = 30
      Width = 90
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      Text = '1.000'
    end
    object udMidFreqHz: TUpDown
      Left = 253
      Top = 30
      Width = 24
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = edtMidFreqHz
      Min = 200
      Max = 6000
      Position = 1000
      TabOrder = 1
    end
    object edtMidQ: TEdit
      Left = 163
      Top = 66
      Width = 90
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 2
      Text = '100'
    end
    object udMidQ: TUpDown
      Left = 253
      Top = 84
      Width = 24
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = edtMidQ
      Min = 30
      Max = 600
      Position = 100
      TabOrder = 3
    end
    object rgMidMode: TRadioGroup
      Left = 12
      Top = 115
      Width = 324
      Height = 104
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Mode'
      ItemIndex = 0
      Items.Strings = (
        'Peaking (Bell)'
        'Notch (Band-stop)')
      TabOrder = 4
    end
  end
  object grpHigh: TGroupBox
    Left = 18
    Top = 143
    Width = 307
    Height = 115
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'High shelf'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lblHighFreqHz: TLabel
      Left = 29
      Top = 30
      Width = 133
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      Caption = 'Freq (Hz)'
    end
    object lblHighSlope: TLabel
      Left = 29
      Top = 66
      Width = 133
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      Caption = 'Slope S (0.1..4)'
    end
    object edtHighFreqHz: TEdit
      Left = 180
      Top = 27
      Width = 90
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      Text = '10.000'
    end
    object udHighFreqHz: TUpDown
      Left = 270
      Top = 30
      Width = 24
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = edtHighFreqHz
      Min = 2000
      Max = 20000
      Position = 10000
      TabOrder = 1
    end
    object edtHighSlope: TEdit
      Left = 180
      Top = 63
      Width = 90
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 2
      Text = '100'
    end
    object udHighSlope: TUpDown
      Left = 270
      Top = 84
      Width = 24
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = edtHighSlope
      Min = 10
      Max = 400
      Position = 100
      TabOrder = 3
    end
  end
  object btnOK: TButton
    Left = 337
    Top = 291
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 463
    Top = 291
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnApply: TButton
    Left = 589
    Top = 291
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Apply'
    TabOrder = 5
    OnClick = btnApplyClick
  end
  object btnDefaults: TButton
    Left = 18
    Top = 291
    Width = 180
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Defaults'
    TabOrder = 6
    OnClick = btnDefaultsClick
  end
end
