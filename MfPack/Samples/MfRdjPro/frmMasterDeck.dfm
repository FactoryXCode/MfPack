object MasterDeckFrm: TMasterDeckFrm
  Left = 0
  Top = 0
  Margins.Left = 0
  Margins.Top = 0
  Margins.Right = 0
  Margins.Bottom = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Master'
  ClientHeight = 988
  ClientWidth = 322
  Color = clNone
  Constraints.MaxHeight = 1538
  Constraints.MaxWidth = 338
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  OldCreateOrder = True
  Position = poDesigned
  Visible = True
  StyleElements = [seFont, seClient]
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 483
    Width = 322
    Height = 505
    Align = alClient
    BevelOuter = bvNone
    Color = 4865081
    Ctl3D = True
    DoubleBuffered = True
    ParentBackground = False
    ParentCtl3D = False
    ParentDoubleBuffered = False
    TabOrder = 0
    ExplicitWidth = 332
    object avMixGraph: TRdjAudioMixVisualizer
      Left = 0
      Top = 0
      Width = 322
      Height = 211
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BackColor = 9216
      BarColor = 9891585
      PeakColor = 4227327
      PeakThreshold = 0.150000005960464500
      PeakCapFrac = 0.050000000745058060
      ShowMeterInfo = False
      DbTop = 6.000000000000000000
      DbMin = -45.000000000000000000
      DbLabelStep = 6
      InputTrimDb = 14.000000000000000000
      VolumeScaleMode = vsmDbPerceptual
      VolumeSmoothingMs = 30
      BarCount = 64
      FftSize = 2024
      AttackMs = 30
      ReleaseMs = 30
      DispatchEveryMs = 30
      ShowRms = False
      ShowPeakHold = False
      PeakHoldMs = 30
      ExplicitWidth = 323
    end
    object pnlRecIcecast: TPanel
      Left = 0
      Top = 204
      Width = 322
      Height = 301
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      Color = 4865081
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      ExplicitWidth = 332
      object pnlRecorder: TPanel
        Left = 0
        Top = 21
        Width = 322
        Height = 280
        Align = alClient
        AutoSize = True
        BevelOuter = bvNone
        Color = 5850948
        ParentBackground = False
        ShowCaption = False
        TabOrder = 0
        ExplicitWidth = 332
        object Bevel1: TBevel
          Left = 4
          Top = 14
          Width = 313
          Height = 265
        end
        object lblRecTime: TLabel
          Left = 11
          Top = 167
          Width = 298
          Height = 19
          AutoSize = False
          Caption = 'Recorded: 00:00:00.00'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clAqua
          Font.Height = -16
          Font.Name = 'Terminal'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label3: TLabel
          Left = 11
          Top = 114
          Width = 83
          Height = 17
          Hint = 'Enter a file name without extension.'
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'FileName:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
        end
        object lblRecordingDevice: TLabel
          Left = 11
          Top = 223
          Width = 292
          Height = 18
          AutoSize = False
          Caption = 'Recorder source: None.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMoneyGreen
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblRecorderStatus: TLabel
          Left = 11
          Top = 248
          Width = 292
          Height = 18
          AutoSize = False
          Caption = 'Status'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMoneyGreen
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblAudioRecorder: TLabel
          Left = 14
          Top = 4
          Width = 121
          Height = 17
          Alignment = taCenter
          AutoSize = False
          Caption = 'Audio recorder'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Transparent = False
        end
        object lblFileExt: TLabel
          Left = 273
          Top = 114
          Width = 27
          Height = 16
          Hint = 'Enter a file name without extension.'
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = '.wav'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
        end
        object btnRecord: TMPxpButton
          Left = 245
          Top = 40
          Width = 66
          Height = 41
          Hint = 'Start recording.'
          Alignment = taCenter
          Caption = 'Start'
          Color = clMaroon
          ColorWhenDown = clRed
          ColorWhenUp = clMaroon
          Behavior = bbPushButton
          Checked = False
          ImageIndexUnchecked = 0
          ImageIndexChecked = 0
          TabStop = True
          ShowFocusRect = False
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMoneyGreen
          Font.Height = -16
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          GlyphTransparentColor = clBlack
          GlyphTransparent = False
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -18
          HotTrackFont.Name = 'Segoe UI'
          HotTrackFont.Style = []
          LightColor = clRed
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShadowColor = clAqua
          ShowHint = True
          SlowDecease = True
          WordWrap = True
          OnClick = btnRecordClick
        end
        object chkPostFx: TMPxpButton
          Left = 167
          Top = 40
          Width = 66
          Height = 40
          Hint = 'Post-FX (Wet mix)'
          Alignment = taCenter
          Caption = 'Post-FX'
          Color = 6899524
          ColorWhenDown = 10045252
          ColorWhenUp = 6899524
          Behavior = bbCheckBox
          Checked = False
          GlyphUnchecked.Data = {
            C6040000424DC60400000000000036040000280000000C0000000C0000000100
            08000000000090000000230B0000230B00000001000000010000000000000000
            1900001900000019190019000000190019001919000060606000607E6000956B
            430000005C002F0000001D00000002000000020002000000130000002F000200
            2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
            7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
            A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
            00000000000000000010151717151000000000001116181A1A18161100000010
            16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
            170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
            1E1C1916100000001116181A1A18161100000000001015171715100000000000
            00000000000000000000}
          GlyphChecked.Data = {
            C6040000424DC60400000000000036040000280000000C0000000C0000000100
            08000000000090000000230B0000230B00000001000000010000000000000000
            80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
            A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
            94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
            D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
            00000000000000000010151717151000000000001116181A1A18161100000010
            16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
            170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
            1E1C1916100000001116181A1A18161100000000001015171715100000000000
            00000000000000000000}
          ImageIndexUnchecked = 0
          ImageIndexChecked = 0
          ShowFocusRect = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clAqua
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = []
          GlyphTransparentColor = clBlack
          GlyphTransparent = True
          HotTrackColor = clAqua
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -18
          HotTrackFont.Name = 'Segoe UI'
          HotTrackFont.Style = []
          Layout = blGlyphTop
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShadowColor = clSkyBlue
          ShowHint = True
          SlowDecease = True
          WordWrap = True
          OnClick = chkPostFxClick
        end
        object chkPreFx: TMPxpButton
          Left = 89
          Top = 40
          Width = 66
          Height = 40
          Hint = 'Pre-FX (Dry mix)'
          Alignment = taCenter
          Caption = 'Pre-FX'
          Color = 6899524
          ColorWhenDown = 10045252
          ColorWhenUp = 6899524
          Down = True
          Behavior = bbCheckBox
          Checked = True
          GlyphUnchecked.Data = {
            C6040000424DC60400000000000036040000280000000C0000000C0000000100
            08000000000090000000230B0000230B00000001000000010000000000000000
            1900001900000019190019000000190019001919000060606000607E6000956B
            430000005C002F0000001D00000002000000020002000000130000002F000200
            2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
            7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
            A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
            A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
            00000000000000000010151717151000000000001116181A1A18161100000010
            16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
            170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
            1E1C1916100000001116181A1A18161100000000001015171715100000000000
            00000000000000000000}
          GlyphChecked.Data = {
            C6040000424DC60400000000000036040000280000000C0000000C0000000100
            08000000000090000000230B0000230B00000001000000010000000000000000
            80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
            A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
            94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
            D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
            00000000000000000010151717151000000000001116181A1A18161100000010
            16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
            170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
            1E1C1916100000001116181A1A18161100000000001015171715100000000000
            00000000000000000000}
          ImageIndexUnchecked = 0
          ImageIndexChecked = 0
          ShowFocusRect = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clAqua
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = []
          GlyphTransparentColor = clBlack
          GlyphTransparent = True
          HotTrackColor = clAqua
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -18
          HotTrackFont.Name = 'Segoe UI'
          HotTrackFont.Style = []
          Layout = blGlyphTop
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShadowColor = clSkyBlue
          ShowHint = True
          SlowDecease = True
          WordWrap = True
          OnClick = chkPreFxClick
        end
        object edFileName: TEdit
          Left = 101
          Top = 114
          Width = 170
          Height = 20
          Hint = 'Enter a capture filename (No path)'
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taRightJustify
          AutoSize = False
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = 9216
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 30
          ParentFont = False
          TabOrder = 3
          Text = 'WAS-capture'
        end
        object btnSelectAudiDevice: TMPxpButton
          Left = 11
          Top = 39
          Width = 66
          Height = 40
          Hint = 'Choose an audio endpoint device'
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taCenter
          AllowAllUp = True
          Caption = 'Device'
          Color = 5914932
          ColorWhenDown = 11363625
          ColorWhenUp = 5914932
          Checked = False
          ShowFocusRect = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          GlyphTransparentColor = clFuchsia
          GlyphTransparent = True
          HotTrackColor = 5850948
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -18
          HotTrackFont.Name = 'Segoe UI'
          HotTrackFont.Style = []
          Layout = blGlyphRight
          LightColor = 12348265
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShadowColor = clSkyBlue
          ShowHint = True
          Style = bsModern
          OnClick = btnSelectAudiDeviceClick
        end
      end
      object pnlFXButtons: TPanel
        Left = 0
        Top = 0
        Width = 322
        Height = 21
        Align = alTop
        BevelOuter = bvNone
        Color = 4865081
        DoubleBuffered = True
        ParentBackground = False
        ParentDoubleBuffered = False
        ShowCaption = False
        TabOrder = 1
        ExplicitWidth = 332
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 37
    Width = 322
    Height = 446
    Align = alTop
    BevelOuter = bvNone
    Color = 5850948
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 1
    ExplicitWidth = 332
    object bvlPfl: TBevel
      Left = 201
      Top = 13
      Width = 116
      Height = 430
    end
    object bvlMaster: TBevel
      Left = 3
      Top = 13
      Width = 194
      Height = 430
    end
    object lblTitlePfl: TLabel
      Left = 213
      Top = 5
      Width = 36
      Height = 14
      Hint = 'Pre Fader Listening (PFL)'
      Alignment = taCenter
      AutoSize = False
      Caption = 'PFL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = False
    end
    object lblTitleMaster: TLabel
      Left = 11
      Top = 3
      Width = 63
      Height = 16
      Alignment = taCenter
      Caption = '  Volume  '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object lblBalMaster: TLabel
      Left = 72
      Top = 423
      Width = 55
      Height = 14
      Alignment = taCenter
      AutoSize = False
      Caption = 'Balance'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object lblMasterVolL: TLabel
      Left = 30
      Top = 49
      Width = 17
      Height = 13
      Alignment = taCenter
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblMasterVolR: TLabel
      Left = 151
      Top = 49
      Width = 17
      Height = 13
      Alignment = taCenter
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblPflVol: TLabel
      Left = 273
      Top = 51
      Width = 17
      Height = 13
      Alignment = taCenter
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object pmMasterL: TMfPeakMeterMmcs
      Left = 72
      Top = 76
      Width = 13
      Height = 293
      BevelStyle = bvLowered
      BevelWidth = 1
      GreenColorOn = 8453888
      GreenColorOff = 8417280
      GreenLeds = 10
      GreenMax = 50
      YellowColorOn = clYellow
      YellowColorOff = clOlive
      YellowLeds = 6
      YellowMax = 30
      RedColorOn = clRed
      RedColorOff = clMaroon
      RedLeds = 4
      RedMax = 20
      ShowSingleLed = False
      SeparatorWidth = 1
      SeparatorColor = clBlack
      Style = dsVertical
      Direction = ddLeftUp
      SampleChannel = mcLeft
      Enabled = True
      EngineMeterFloorDb = -40.000000000000000000
      EngineMeterCeilDb = -1.000000000000000000
      EngineMeterGamma = 0.899999976158142100
      EnginePeakWeight = 1.000000000000000000
      EngineRmsWeight = 1.100000023841858000
      EngineReleaseSec = 0.150000005960464500
      PeakHoldDuration = 0.500000000000000000
    end
    object pmMasterR: TMfPeakMeterMmcs
      Left = 112
      Top = 76
      Width = 13
      Height = 293
      BevelStyle = bvLowered
      BevelWidth = 1
      GreenColorOn = 8453888
      GreenColorOff = 8417280
      GreenLeds = 10
      GreenMax = 50
      YellowColorOn = clYellow
      YellowColorOff = clOlive
      YellowLeds = 6
      YellowMax = 30
      RedColorOn = clRed
      RedColorOff = clMaroon
      RedLeds = 4
      RedMax = 20
      ShowSingleLed = False
      SeparatorWidth = 1
      SeparatorColor = clBlack
      Style = dsVertical
      Direction = ddLeftUp
      SampleChannel = mcLeft
      Enabled = True
      EngineMeterFloorDb = -40.000000000000000000
      EngineMeterCeilDb = -1.000000000000000000
      EngineMeterGamma = 0.899999976158142100
      EnginePeakWeight = 1.000000000000000000
      EngineRmsWeight = 1.100000023841858000
      EngineReleaseSec = 0.150000005960464500
      PeakHoldDuration = 0.500000000000000000
    end
    object pmPflR: TMfPeakMeterMmcs
      Left = 233
      Top = 78
      Width = 14
      Height = 293
      BevelStyle = bvLowered
      BevelWidth = 1
      GreenColorOn = 8453888
      GreenColorOff = 8417280
      GreenLeds = 10
      GreenMax = 50
      YellowColorOn = clYellow
      YellowColorOff = clOlive
      YellowLeds = 6
      YellowMax = 30
      RedColorOn = clRed
      RedColorOff = clMaroon
      RedLeds = 4
      RedMax = 20
      ShowSingleLed = False
      SeparatorWidth = 1
      SeparatorColor = clBlack
      Style = dsVertical
      Direction = ddLeftUp
      SampleChannel = mcLeft
      Precision = 10
      Enabled = True
      EngineMeterFloorDb = -40.000000000000000000
      EngineMeterCeilDb = -1.000000000000000000
      EngineMeterGamma = 0.899999976158142100
      EnginePeakWeight = 1.000000000000000000
      EngineRmsWeight = 1.100000023841858000
      EngineReleaseSec = 0.150000005960464500
      PeakHoldDuration = 0.500000000000000000
    end
    object pmPflL: TMfPeakMeterMmcs
      Left = 213
      Top = 78
      Width = 14
      Height = 293
      BevelStyle = bvLowered
      BevelWidth = 1
      GreenColorOn = 8453888
      GreenColorOff = 8417280
      GreenLeds = 10
      GreenMax = 50
      YellowColorOn = clYellow
      YellowColorOff = clOlive
      YellowLeds = 6
      YellowMax = 30
      RedColorOn = clRed
      RedColorOff = clMaroon
      RedLeds = 4
      RedMax = 20
      ShowSingleLed = False
      SeparatorWidth = 1
      SeparatorColor = clBlack
      Style = dsVertical
      Direction = ddLeftUp
      SampleChannel = mcLeft
      Precision = 10
      Enabled = True
      EngineMeterFloorDb = -40.000000000000000000
      EngineMeterCeilDb = -1.000000000000000000
      EngineMeterGamma = 0.899999976158142100
      EnginePeakWeight = 1.000000000000000000
      EngineRmsWeight = 1.100000023841858000
      EngineReleaseSec = 0.150000005960464500
      PeakHoldDuration = 0.500000000000000000
    end
    object Label1: TLabel
      Left = 21
      Top = 417
      Width = 19
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Left'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 151
      Top = 417
      Width = 25
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Right'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Shape2: TShape
      Left = 19
      Top = 119
      Width = 40
      Height = 96
      Brush.Style = bsHorizontal
      Pen.Style = psClear
    end
    object Shape1: TShape
      Left = 19
      Top = 81
      Width = 40
      Height = 40
      Brush.Color = clRed
      Brush.Style = bsHorizontal
      Pen.Style = psClear
    end
    object Shape3: TShape
      Left = 141
      Top = 81
      Width = 40
      Height = 40
      Brush.Color = clRed
      Brush.Style = bsHorizontal
      Pen.Style = psClear
    end
    object Shape4: TShape
      Left = 141
      Top = 119
      Width = 40
      Height = 96
      Brush.Style = bsHorizontal
      Pen.Style = psClear
    end
    object Shape5: TShape
      Left = 262
      Top = 83
      Width = 40
      Height = 40
      Brush.Color = clRed
      Brush.Style = bsHorizontal
      Pen.Style = psClear
    end
    object Shape6: TShape
      Left = 262
      Top = 121
      Width = 40
      Height = 96
      Brush.Style = bsHorizontal
      Pen.Style = psClear
    end
    object tbBalance: TMfTrackBar
      Left = 4
      Top = 380
      Width = 189
      Height = 40
      TabOrder = 0
      Color = 5850948
      ParentColor = False
      Minimum = -100
      ThumbWidth = 60
      ThumbHeight = 50
      TransparentColor = clBlack
      ThumbPictureHorz.Data = {
        07544269746D617012010000424D120100000000000076000000280000001300
        00000D00000001000400000000009C000000330B0000330B0000100000001000
        000000000000959595009B9B9B00A2A2A200B9B9B900C4C4C400FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00055555554445555555000000531425454045452413500000531425454045
        4524135000005314254540454524135000005314254540454524135000005314
        2545404545241350000053142545404545241350000053142545404545241350
        0000531425454045452413500000531425454045452413500000531425454045
        452413500000531425454045452413500000055555554445555555000000}
      StretchBackground = False
      ShowTicks = True
      TickLabelMode = tlDb
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementH = tphBoth
      TickPlacementV = tpvBoth
      OnChange = tbMasterVolLChange
      OnDblClick = tbBalanceDblClick
    end
    object tbMasterVolL: TMfTrackBar
      Left = 19
      Top = 76
      Width = 40
      Height = 293
      TabOrder = 1
      Color = 5850948
      ParentColor = False
      Orientation = soVertical
      IncreaseToward = itUp
      ThumbWidth = 60
      ThumbHeight = 50
      TransparentColor = clBlack
      ThumbPictureVert.Data = {
        07544269746D61700E010000424D0E0100000000000076000000280000000D00
        000013000000010004000000000098000000330B0000330B0000100000001000
        000000000000959595009B9B9B00A2A2A200B9B9B900BFBFBF00C4C4C400C9C9
        C900CECECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00044444444444000048888888888840004777777777774000455555555555
        4000477777777777400045555555555540004777777777774000455555555555
        4000477777777777500050000000000050005444444444445000555555555555
        5000544444444444500055555555555550005222222222225000544444444444
        5000511111111111500053333333333350000555555555550000}
      ShowTicks = True
      TickCount = 21
      DbFloor = -60.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      LargeChange = 4
      OnChange = tbMasterVolLChange
      OnDblClick = tbMasterVolLDblClick
    end
    object tbMasterVolR: TMfTrackBar
      Left = 140
      Top = 76
      Width = 40
      Height = 293
      TabOrder = 2
      Color = 5850948
      ParentColor = False
      Orientation = soVertical
      IncreaseToward = itUp
      ThumbWidth = 60
      ThumbHeight = 50
      TransparentColor = clBlack
      ThumbPictureVert.Data = {
        07544269746D61700E010000424D0E0100000000000076000000280000000D00
        000013000000010004000000000098000000330B0000330B0000100000001000
        000000000000959595009B9B9B00A2A2A200B9B9B900BFBFBF00C4C4C400C9C9
        C900CECECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00044444444444000048888888888840004777777777774000455555555555
        4000477777777777400045555555555540004777777777774000455555555555
        4000477777777777500050000000000050005444444444445000555555555555
        5000544444444444500055555555555550005222222222225000544444444444
        5000511111111111500053333333333350000555555555550000}
      ShowTicks = True
      TickCount = 21
      DbFloor = -60.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      LargeChange = 4
      OnChange = tbMasterVolRChange
      OnDblClick = tbMasterVolRDblClick
    end
    object tbPflVol: TMfTrackBar
      Left = 262
      Top = 78
      Width = 40
      Height = 293
      TabOrder = 3
      Color = 5850948
      ParentColor = False
      Orientation = soVertical
      IncreaseToward = itUp
      ThumbWidth = 60
      ThumbHeight = 50
      TransparentColor = clBlack
      ThumbPictureVert.Data = {
        07544269746D61700E010000424D0E0100000000000076000000280000000D00
        000013000000010004000000000098000000330B0000330B0000100000001000
        000000000000959595009B9B9B00A2A2A200B9B9B900BFBFBF00C4C4C400C9C9
        C900CECECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00044444444444000048888888888840004777777777774000455555555555
        4000477777777777400045555555555540004777777777774000455555555555
        4000477777777777500050000000000050005444444444445000555555555555
        5000544444444444500055555555555550005222222222225000544444444444
        5000511111111111500053333333333350000555555555550000}
      ShowTicks = True
      TickCount = 21
      DbFloor = -60.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      LargeChange = 4
      OnChange = tbPflVolChange
      OnDblClick = tbPflVolDblClick
    end
    object chkLockMasterFaders: TMPxpButton
      Left = 66
      Top = 30
      Width = 67
      Height = 40
      Hint = 'Lock Faders'
      Alignment = taCenter
      Caption = 'Lock'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Down = True
      Behavior = bbCheckBox
      Checked = True
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clAqua
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShadowColor = clSkyBlue
      ShowHint = True
      SlowDecease = True
      WordWrap = True
    end
    object btnPflMute: TMPxpButton
      Left = 227
      Top = 386
      Width = 67
      Height = 40
      Alignment = taCenter
      Caption = 'Mute'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Behavior = bbCheckBox
      Checked = False
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clAqua
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      SlowDecease = True
      WordWrap = True
      OnClick = btnPflMuteClick
    end
  end
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 322
    Height = 37
    Align = alTop
    Alignment = taLeftJustify
    Color = 4865081
    ParentBackground = False
    TabOrder = 2
    ExplicitWidth = 332
    object lblCaption: TLabel
      Left = 8
      Top = 11
      Width = 46
      Height = 16
      Hint = 'Master deck'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Master'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
    end
    object shpRecordingCap: TShape
      Left = 238
      Top = 4
      Width = 87
      Height = 29
      Brush.Style = bsClear
      Pen.Color = 5668864
      Pen.Width = 3
    end
    object shpRecording: TShape
      Left = 242
      Top = 8
      Width = 79
      Height = 21
      Brush.Style = bsClear
      Pen.Color = 5668864
    end
    object lblRecording: TLabel
      Left = 242
      Top = 8
      Width = 83
      Height = 20
      Alignment = taCenter
      AutoSize = False
      Caption = 'REC OFF'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
  end
  object epPFL: TMfAudioEndPoint
    DeviceID = '{0.0.0.00000000}.{d7e13618-ca94-4b30-a916-0934986710a9}'
    DeviceState = 'Active'
    MasterScalarVolume = 1.000000000000000000
    Left = 244
    Top = 272
  end
  object epMaster: TMfAudioEndPoint
    DeviceID = '{0.0.0.00000000}.{ef4f5772-aeac-426a-8d69-a6bcf7153472}'
    DeviceIndex = 1
    DeviceState = 'Active'
    MasterScalarVolume = 0.300000011920929000
    MasterDbVolume = -17.823547363281250000
    Left = 98
    Top = 270
  end
  object tmrTime: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrTimeTimer
    Left = 68
    Top = 552
  end
end
