object frmEqSettings: TfrmEqSettings
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsDialog
  Caption = 'EQ Settings'
  ClientHeight = 202
  ClientWidth = 410
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grpLow: TGroupBox
    Left = 10
    Top = 10
    Width = 174
    Height = 65
    Caption = 'Low shelf'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lblLowFreqHz: TLabel
      Left = 16
      Top = 17
      Width = 76
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Freq (Hz)'
    end
    object lblLowSlope: TLabel
      Left = 16
      Top = 37
      Width = 76
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Slope S (0.1..4)'
    end
    object edtLowFreqHz: TEdit
      Left = 102
      Top = 15
      Width = 51
      Height = 31
      TabOrder = 0
      Text = '100'
    end
    object udLowFreqHz: TUpDown
      Left = 153
      Top = 19
      Width = 13
      Height = 17
      Associate = edtLowFreqHz
      Min = 20
      Max = 400
      Position = 100
      TabOrder = 1
    end
    object edtLowSlope: TEdit
      Left = 102
      Top = 36
      Width = 51
      Height = 31
      TabOrder = 2
      Text = '100'
    end
    object udLowSlope: TUpDown
      Left = 153
      Top = 39
      Width = 13
      Height = 18
      Associate = edtLowSlope
      Min = 10
      Max = 400
      Position = 100
      TabOrder = 3
    end
  end
  object grpMid: TGroupBox
    Left = 193
    Top = 10
    Width = 204
    Height = 136
    Caption = 'Mid band'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lblMidFreqHz: TLabel
      Left = 19
      Top = 19
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Freq (Hz)'
    end
    object lblMidQ: TLabel
      Left = 19
      Top = 39
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Bandwidth Q'
    end
    object edtMidFreqHz: TEdit
      Left = 92
      Top = 17
      Width = 51
      Height = 31
      TabOrder = 0
      Text = '1000'
    end
    object udMidFreqHz: TUpDown
      Left = 143
      Top = 17
      Width = 14
      Height = 17
      Associate = edtMidFreqHz
      Min = 200
      Max = 6000
      Position = 1000
      TabOrder = 1
    end
    object edtMidQ: TEdit
      Left = 92
      Top = 37
      Width = 51
      Height = 31
      TabOrder = 2
      Text = '100'
    end
    object udMidQ: TUpDown
      Left = 143
      Top = 47
      Width = 14
      Height = 18
      Associate = edtMidQ
      Min = 30
      Max = 600
      Position = 100
      TabOrder = 3
    end
    object rgMidMode: TRadioGroup
      Left = 7
      Top = 65
      Width = 183
      Height = 59
      Caption = 'Mode'
      ItemIndex = 0
      Items.Strings = (
        'Peaking (Bell)'
        'Notch (Band-stop)')
      TabOrder = 4
    end
  end
  object grpHigh: TGroupBox
    Left = 10
    Top = 81
    Width = 174
    Height = 65
    Caption = 'High shelf'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lblHighFreqHz: TLabel
      Left = 53
      Top = 17
      Width = 39
      Height = 12
      Alignment = taRightJustify
      Caption = 'Freq (Hz)'
    end
    object lblHighSlope: TLabel
      Left = 31
      Top = 37
      Width = 61
      Height = 12
      Alignment = taRightJustify
      Caption = 'Slope S (0.1..4)'
    end
    object edtHighFreqHz: TEdit
      Left = 102
      Top = 15
      Width = 51
      Height = 31
      TabOrder = 0
      Text = '10000'
    end
    object udHighFreqHz: TUpDown
      Left = 153
      Top = 17
      Width = 13
      Height = 17
      Associate = edtHighFreqHz
      Min = 2000
      Max = 20000
      Position = 10000
      TabOrder = 1
    end
    object edtHighSlope: TEdit
      Left = 102
      Top = 36
      Width = 51
      Height = 31
      TabOrder = 2
      Text = '100'
    end
    object udHighSlope: TUpDown
      Left = 153
      Top = 47
      Width = 13
      Height = 18
      Associate = edtHighSlope
      Min = 10
      Max = 400
      Position = 100
      TabOrder = 3
    end
  end
  object btnOK: TButton
    Left = 190
    Top = 164
    Width = 64
    Height = 22
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 262
    Top = 164
    Width = 64
    Height = 22
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnApply: TButton
    Left = 333
    Top = 164
    Width = 64
    Height = 22
    Caption = 'Apply'
    TabOrder = 5
    OnClick = btnApplyClick
  end
  object btnDefaults: TButton
    Left = 10
    Top = 164
    Width = 102
    Height = 22
    Caption = 'Defaults'
    TabOrder = 6
    OnClick = btnDefaultsClick
  end
end
