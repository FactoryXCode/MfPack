object frmEqSettings: TfrmEqSettings
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsDialog
  Caption = 'EQ Settings'
  ClientHeight = 392
  ClientWidth = 803
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
  object pcMain: TPageControl
    Left = 18
    Top = 18
    Width = 766
    Height = 313
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tsDynamics
    TabOrder = 0
    object tsEQ: TTabSheet
      Caption = 'EQ'
      object grpLow: TGroupBox
        Left = 6
        Top = 6
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
          Top = 27
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
          Top = 63
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
      object grpHigh: TGroupBox
        Left = 6
        Top = 131
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
        TabOrder = 1
        object lblHighFreqHz: TLabel
          Left = 88
          Top = 30
          Width = 74
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          Caption = 'Freq (Hz)'
        end
        object lblHighSlope: TLabel
          Left = 40
          Top = 66
          Width = 122
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
          Top = 27
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
          Top = 63
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
      object grpMid: TGroupBox
        Left = 332
        Top = 5
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
        TabOrder = 2
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
          Top = 66
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
    end
    object tsDynamics: TTabSheet
      Caption = 'Dynamics'
      ImageIndex = 1
      object grpComp: TGroupBox
        Left = 16
        Top = 6
        Width = 358
        Height = 253
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Compressor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object lblCompThreshold: TLabel
          Left = 18
          Top = 69
          Width = 135
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Threshold (dB)'
        end
        object lblCompRatio: TLabel
          Left = 18
          Top = 105
          Width = 135
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Ratio x10'
        end
        object lblCompAttack: TLabel
          Left = 18
          Top = 141
          Width = 135
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Attack (ms)'
        end
        object lblCompRelease: TLabel
          Left = 18
          Top = 177
          Width = 135
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Release (ms)'
        end
        object lblCompMakeup: TLabel
          Left = 18
          Top = 213
          Width = 135
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Makeup (dB)'
        end
        object chkCompEnabled: TCheckBox
          Left = 18
          Top = 30
          Width = 200
          Height = 27
          Caption = 'Enabled'
          TabOrder = 0
        end
        object edtCompThreshold: TEdit
          Left = 171
          Top = 66
          Width = 70
          Height = 31
          TabOrder = 1
          Text = '-18'
        end
        object udCompThreshold: TUpDown
          Left = 241
          Top = 66
          Width = 24
          Height = 31
          Associate = edtCompThreshold
          Min = -60
          Max = 0
          Position = -18
          TabOrder = 2
        end
        object edtCompRatio: TEdit
          Left = 171
          Top = 102
          Width = 70
          Height = 31
          TabOrder = 3
          Text = '40'
        end
        object udCompRatio: TUpDown
          Left = 241
          Top = 102
          Width = 24
          Height = 31
          Associate = edtCompRatio
          Min = 10
          Max = 200
          Position = 40
          TabOrder = 4
        end
        object edtCompAttack: TEdit
          Left = 171
          Top = 138
          Width = 70
          Height = 31
          TabOrder = 5
          Text = '10'
        end
        object udCompAttack: TUpDown
          Left = 241
          Top = 138
          Width = 24
          Height = 31
          Associate = edtCompAttack
          Min = 1
          Max = 200
          Position = 10
          TabOrder = 6
        end
        object edtCompRelease: TEdit
          Left = 171
          Top = 174
          Width = 70
          Height = 31
          TabOrder = 7
          Text = '150'
        end
        object udCompRelease: TUpDown
          Left = 241
          Top = 174
          Width = 24
          Height = 31
          Associate = edtCompRelease
          Min = 10
          Max = 2000
          Position = 150
          TabOrder = 8
        end
        object edtCompMakeup: TEdit
          Left = 171
          Top = 210
          Width = 70
          Height = 31
          TabOrder = 9
          Text = '0'
        end
        object udCompMakeup: TUpDown
          Left = 241
          Top = 210
          Width = 24
          Height = 31
          Associate = edtCompMakeup
          Min = -24
          Max = 24
          TabOrder = 10
        end
        object chkCompAutoMakeup: TCheckBox
          Left = 271
          Top = 211
          Width = 78
          Height = 27
          Caption = 'Auto'
          TabOrder = 11
        end
      end
      object grpLimiter: TGroupBox
        Left = 384
        Top = 6
        Width = 358
        Height = 253
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Limiter'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object lblLimCeiling: TLabel
          Left = 18
          Top = 69
          Width = 150
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Ceiling (dB)'
        end
        object lblLimRelease: TLabel
          Left = 18
          Top = 105
          Width = 150
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Release (ms)'
        end
        object lblLimLookahead: TLabel
          Left = 18
          Top = 141
          Width = 150
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Lookahead (ms)'
        end
        object lblLimKnee: TLabel
          Left = 18
          Top = 177
          Width = 150
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Knee (dB)'
        end
        object lblLimDetector: TLabel
          Left = 18
          Top = 213
          Width = 150
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Detector'
        end
        object lblLimRmsWindow: TLabel
          Left = 286
          Top = 213
          Width = 50
          Height = 23
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'RMS'
        end
        object chkLimEnabled: TCheckBox
          Left = 18
          Top = 30
          Width = 120
          Height = 27
          Caption = 'Enabled'
          TabOrder = 0
        end
        object chkLimTruePeak: TCheckBox
          Left = 150
          Top = 30
          Width = 110
          Height = 27
          Caption = 'TruePeak'
          TabOrder = 1
        end
        object cbLimOversample: TComboBox
          Left = 270
          Top = 27
          Width = 55
          Height = 31
          Style = csDropDownList
          TabOrder = 2
          Items.Strings = (
            '1x'
            '2x'
            '4x')
        end
        object edtLimCeiling: TEdit
          Left = 186
          Top = 66
          Width = 70
          Height = 31
          TabOrder = 3
          Text = '-1'
        end
        object udLimCeiling: TUpDown
          Left = 256
          Top = 66
          Width = 24
          Height = 31
          Associate = edtLimCeiling
          Min = -24
          Max = 0
          Position = -1
          TabOrder = 4
        end
        object edtLimRelease: TEdit
          Left = 186
          Top = 102
          Width = 70
          Height = 31
          TabOrder = 5
          Text = '120'
        end
        object udLimRelease: TUpDown
          Left = 256
          Top = 102
          Width = 24
          Height = 31
          Associate = edtLimRelease
          Min = 10
          Max = 2000
          Position = 120
          TabOrder = 6
        end
        object edtLimLookahead: TEdit
          Left = 186
          Top = 138
          Width = 70
          Height = 31
          TabOrder = 7
          Text = '5'
        end
        object udLimLookahead: TUpDown
          Left = 256
          Top = 138
          Width = 24
          Height = 31
          Associate = edtLimLookahead
          Max = 50
          Position = 5
          TabOrder = 8
        end
        object edtLimKnee: TEdit
          Left = 186
          Top = 174
          Width = 70
          Height = 31
          TabOrder = 9
          Text = '0'
        end
        object udLimKnee: TUpDown
          Left = 256
          Top = 174
          Width = 24
          Height = 31
          Associate = edtLimKnee
          Max = 24
          TabOrder = 10
        end
        object cbLimDetector: TComboBox
          Left = 186
          Top = 210
          Width = 94
          Height = 31
          Style = csDropDownList
          TabOrder = 11
          Items.Strings = (
            'Peak'
            'RMS')
        end
        object edtLimRmsWindow: TEdit
          Left = 286
          Top = 210
          Width = 45
          Height = 31
          TabOrder = 12
          Text = '50'
        end
        object udLimRmsWindow: TUpDown
          Left = 331
          Top = 210
          Width = 24
          Height = 31
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
    Left = 337
    Top = 344
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 463
    Top = 344
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnApply: TButton
    Left = 589
    Top = 344
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Apply'
    TabOrder = 4
    OnClick = btnApplyClick
  end
  object btnDefaults: TButton
    Left = 18
    Top = 344
    Width = 180
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Defaults'
    TabOrder = 1
    OnClick = btnDefaultsClick
  end
end
