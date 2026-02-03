object frmEqSettings: TfrmEqSettings
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsDialog
  Caption = 'EQ Settings'
  ClientHeight = 222
  ClientWidth = 452
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
  object pcMain: TPageControl
    Left = 1
    Top = 10
    Width = 449
    Height = 177
    ActivePage = tsDynamics
    TabOrder = 0
    object tsEQ: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'EQ'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grpLow: TGroupBox
        Left = 3
        Top = 3
        Width = 160
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
          Left = 2
          Top = 17
          Width = 75
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Freq (Hz)'
        end
        object lblLowSlope: TLabel
          Left = 2
          Top = 37
          Width = 75
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Slope S (0.1..4)'
        end
        object edtLowFreqHz: TEdit
          Left = 88
          Top = 15
          Width = 50
          Height = 31
          TabOrder = 0
          Text = '100'
        end
        object udLowFreqHz: TUpDown
          Left = 138
          Top = 15
          Width = 14
          Height = 18
          Associate = edtLowFreqHz
          Min = 20
          Max = 400
          Position = 100
          TabOrder = 1
        end
        object edtLowSlope: TEdit
          Left = 88
          Top = 36
          Width = 50
          Height = 31
          TabOrder = 2
          Text = '100'
        end
        object udLowSlope: TUpDown
          Left = 138
          Top = 36
          Width = 14
          Height = 17
          Associate = edtLowSlope
          Min = 10
          Max = 400
          Position = 100
          TabOrder = 3
        end
      end
      object grpHigh: TGroupBox
        Left = 3
        Top = 74
        Width = 160
        Height = 65
        Caption = 'High shelf'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object lblHighFreqHz: TLabel
          Left = 38
          Top = 17
          Width = 39
          Height = 12
          Alignment = taRightJustify
          Caption = 'Freq (Hz)'
        end
        object lblHighSlope: TLabel
          Left = 16
          Top = 37
          Width = 61
          Height = 12
          Alignment = taRightJustify
          Caption = 'Slope S (0.1..4)'
        end
        object edtHighFreqHz: TEdit
          Left = 88
          Top = 15
          Width = 50
          Height = 31
          TabOrder = 0
          Text = '10000'
        end
        object udHighFreqHz: TUpDown
          Left = 138
          Top = 15
          Width = 14
          Height = 18
          Associate = edtHighFreqHz
          Min = 2000
          Max = 20000
          Position = 10000
          TabOrder = 1
        end
        object edtHighSlope: TEdit
          Left = 88
          Top = 36
          Width = 50
          Height = 31
          TabOrder = 2
          Text = '100'
        end
        object udHighSlope: TUpDown
          Left = 138
          Top = 36
          Width = 14
          Height = 17
          Associate = edtHighSlope
          Min = 10
          Max = 400
          Position = 100
          TabOrder = 3
        end
      end
      object grpMid: TGroupBox
        Left = 166
        Top = 3
        Width = 148
        Height = 137
        Caption = 'Mid band'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        object lblMidFreqHz: TLabel
          Left = 3
          Top = 19
          Width = 65
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Freq (Hz)'
        end
        object lblMidQ: TLabel
          Left = 3
          Top = 39
          Width = 65
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Bandwidth Q'
        end
        object edtMidFreqHz: TEdit
          Left = 76
          Top = 17
          Width = 51
          Height = 31
          TabOrder = 0
          Text = '1000'
        end
        object udMidFreqHz: TUpDown
          Left = 127
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
          Left = 76
          Top = 37
          Width = 51
          Height = 31
          TabOrder = 2
          Text = '100'
        end
        object udMidQ: TUpDown
          Left = 127
          Top = 37
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
          Top = 60
          Width = 133
          Height = 59
          Caption = 'Mode'
          ItemIndex = 0
          Items.Strings = (
            'Peaking (Bell)'
            'Notch (Band-stop)')
          TabOrder = 4
        end
      end
      object GroupBox1: TGroupBox
        Left = 317
        Top = 3
        Width = 126
        Height = 137
        Caption = ' Interface '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        object Label1: TLabel
          Left = 6
          Top = 21
          Width = 73
          Height = 13
          Hint = 'Peakmeter frequency in milliseconds.'
          AutoSize = False
          Caption = 'Peakmeter Freq'
          ParentShowHint = False
          ShowHint = True
        end
        object edPeakMeterFreq: TEdit
          Left = 84
          Top = 20
          Width = 31
          Height = 31
          Hint = 'Peakmeter frequency in milliseconds.'
          NumbersOnly = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '100'
          OnExit = edPeakMeterFreqExit
        end
      end
    end
    object tsDynamics: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Dynamics'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grpComp: TGroupBox
        Left = 9
        Top = 3
        Width = 202
        Height = 143
        Caption = 'Compressor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object lblCompThreshold: TLabel
          Left = 10
          Top = 39
          Width = 76
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Threshold (dB)'
        end
        object lblCompRatio: TLabel
          Left = 10
          Top = 59
          Width = 76
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Ratio x10'
        end
        object lblCompAttack: TLabel
          Left = 10
          Top = 80
          Width = 76
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Attack (ms)'
        end
        object lblCompRelease: TLabel
          Left = 10
          Top = 100
          Width = 76
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Release (ms)'
        end
        object lblCompMakeup: TLabel
          Left = 10
          Top = 120
          Width = 76
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Makeup (dB)'
        end
        object chkCompEnabled: TCheckBox
          Left = 10
          Top = 17
          Width = 113
          Height = 15
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Enabled'
          TabOrder = 0
        end
        object edtCompThreshold: TEdit
          Left = 97
          Top = 37
          Width = 39
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 1
          Text = '-18'
        end
        object udCompThreshold: TUpDown
          Left = 136
          Top = 37
          Width = 14
          Height = 18
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtCompThreshold
          Min = -60
          Max = 0
          Position = -18
          TabOrder = 2
        end
        object edtCompRatio: TEdit
          Left = 97
          Top = 58
          Width = 39
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 3
          Text = '40'
        end
        object udCompRatio: TUpDown
          Left = 136
          Top = 58
          Width = 14
          Height = 17
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtCompRatio
          Min = 10
          Max = 200
          Position = 40
          TabOrder = 4
        end
        object edtCompAttack: TEdit
          Left = 97
          Top = 78
          Width = 39
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 5
          Text = '10'
        end
        object udCompAttack: TUpDown
          Left = 136
          Top = 78
          Width = 14
          Height = 18
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtCompAttack
          Min = 1
          Max = 200
          Position = 10
          TabOrder = 6
        end
        object edtCompRelease: TEdit
          Left = 97
          Top = 98
          Width = 39
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 7
          Text = '150'
        end
        object udCompRelease: TUpDown
          Left = 136
          Top = 98
          Width = 14
          Height = 18
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtCompRelease
          Min = 10
          Max = 2000
          Position = 150
          TabOrder = 8
        end
        object edtCompMakeup: TEdit
          Left = 97
          Top = 119
          Width = 39
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 9
          Text = '0'
        end
        object udCompMakeup: TUpDown
          Left = 136
          Top = 119
          Width = 14
          Height = 17
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtCompMakeup
          Min = -24
          Max = 24
          TabOrder = 10
        end
        object chkCompAutoMakeup: TCheckBox
          Left = 153
          Top = 119
          Width = 44
          Height = 16
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Auto'
          TabOrder = 11
        end
      end
      object grpLimiter: TGroupBox
        Left = 217
        Top = 3
        Width = 202
        Height = 143
        Caption = 'Limiter'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object lblLimCeiling: TLabel
          Left = 10
          Top = 39
          Width = 85
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Ceiling (dB)'
        end
        object lblLimRelease: TLabel
          Left = 10
          Top = 59
          Width = 85
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Release (ms)'
        end
        object lblLimLookahead: TLabel
          Left = 10
          Top = 80
          Width = 85
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Lookahead (ms)'
        end
        object lblLimKnee: TLabel
          Left = 10
          Top = 100
          Width = 85
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Knee (dB)'
        end
        object lblLimDetector: TLabel
          Left = 10
          Top = 120
          Width = 85
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Detector'
        end
        object lblLimRmsWindow: TLabel
          Left = 162
          Top = 120
          Width = 28
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'RMS'
        end
        object chkLimEnabled: TCheckBox
          Left = 10
          Top = 17
          Width = 68
          Height = 15
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Enabled'
          TabOrder = 0
        end
        object chkLimTruePeak: TCheckBox
          Left = 85
          Top = 17
          Width = 62
          Height = 15
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'TruePeak'
          TabOrder = 1
        end
        object cbLimOversample: TComboBox
          Left = 153
          Top = 15
          Width = 31
          Height = 20
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Style = csDropDownList
          TabOrder = 2
          Items.Strings = (
            '1x'
            '2x'
            '4x')
        end
        object edtLimCeiling: TEdit
          Left = 105
          Top = 37
          Width = 40
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 3
          Text = '-1'
        end
        object udLimCeiling: TUpDown
          Left = 145
          Top = 37
          Width = 13
          Height = 18
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtLimCeiling
          Min = -24
          Max = 0
          Position = -1
          TabOrder = 4
        end
        object edtLimRelease: TEdit
          Left = 105
          Top = 58
          Width = 40
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 5
          Text = '120'
        end
        object udLimRelease: TUpDown
          Left = 145
          Top = 58
          Width = 13
          Height = 17
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtLimRelease
          Min = 10
          Max = 2000
          Position = 120
          TabOrder = 6
        end
        object edtLimLookahead: TEdit
          Left = 105
          Top = 78
          Width = 40
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 7
          Text = '5'
        end
        object udLimLookahead: TUpDown
          Left = 145
          Top = 78
          Width = 13
          Height = 18
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtLimLookahead
          Max = 50
          Position = 5
          TabOrder = 8
        end
        object edtLimKnee: TEdit
          Left = 105
          Top = 98
          Width = 40
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 9
          Text = '0'
        end
        object udLimKnee: TUpDown
          Left = 145
          Top = 98
          Width = 13
          Height = 18
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtLimKnee
          Max = 24
          TabOrder = 10
        end
        object cbLimDetector: TComboBox
          Left = 105
          Top = 119
          Width = 53
          Height = 20
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Style = csDropDownList
          TabOrder = 11
          Items.Strings = (
            'Peak'
            'RMS')
        end
        object edtLimRmsWindow: TEdit
          Left = 162
          Top = 119
          Width = 25
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          TabOrder = 12
          Text = '50'
        end
        object udLimRmsWindow: TUpDown
          Left = 187
          Top = 119
          Width = 14
          Height = 17
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Associate = edtLimRmsWindow
          Min = 1
          Max = 200
          Position = 50
          TabOrder = 13
        end
      end
    end
  end
  object btnOK: TButton
    Left = 190
    Top = 194
    Width = 64
    Height = 22
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 262
    Top = 194
    Width = 64
    Height = 22
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnApply: TButton
    Left = 333
    Top = 194
    Width = 64
    Height = 22
    Caption = 'Apply'
    TabOrder = 4
    OnClick = btnApplyClick
  end
  object btnDefaults: TButton
    Left = 10
    Top = 194
    Width = 102
    Height = 22
    Caption = 'Defaults'
    TabOrder = 1
    OnClick = btnDefaultsClick
  end
end
