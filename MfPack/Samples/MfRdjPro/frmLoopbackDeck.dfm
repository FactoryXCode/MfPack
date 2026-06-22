object frmLoopbackDeck: TfrmLoopbackDeck
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'LoopbackDeckfrm'
  ClientHeight = 988
  ClientWidth = 261
  Color = 4865081
  Constraints.MaxHeight = 1538
  Constraints.MaxWidth = 395
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = True
  Visible = True
  StyleElements = [seFont, seClient]
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 37
    Width = 261
    Height = 578
    Align = alTop
    BevelOuter = bvNone
    Color = 5850948
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 249
    object Bevel7: TBevel
      Left = 0
      Top = 511
      Width = 255
      Height = 67
    end
    object Bevel5: TBevel
      Left = 129
      Top = 12
      Width = 66
      Height = 493
    end
    object Bevel3: TBevel
      Left = 69
      Top = 12
      Width = 66
      Height = 493
    end
    object lblVol: TLabel
      Left = 78
      Top = 487
      Width = 42
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Volume'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object lblVolumePerc: TLabel
      Left = 89
      Top = 15
      Width = 16
      Height = 15
      Alignment = taCenter
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblBalLeft: TLabel
      Left = 15
      Top = 557
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
    object lblBalRight: TLabel
      Left = 213
      Top = 557
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
    object vuInputL: TMfPeakMeterMmcs
      Left = 15
      Top = 55
      Width = 16
      Height = 256
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
      InputSource = isWasapiEngine
      EngineMeterFloorDb = -40.000000000000000000
      EngineMeterCeilDb = -1.000000000000000000
      EngineMeterGamma = 0.899999976158142100
      EnginePeakWeight = 1.000000000000000000
      EngineRmsWeight = 1.100000023841858000
      EngineReleaseSec = 0.150000005960464500
      PeakHoldDuration = 0.500000000000000000
    end
    object vuInputR: TMfPeakMeterMmcs
      Left = 38
      Top = 55
      Width = 16
      Height = 256
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
      InputSource = isWasapiEngine
      EngineMeterFloorDb = -40.000000000000000000
      EngineMeterCeilDb = -1.000000000000000000
      EngineMeterGamma = 0.899999976158142100
      EnginePeakWeight = 1.000000000000000000
      EngineRmsWeight = 1.100000023841858000
      EngineReleaseSec = 0.150000005960464500
      PeakHoldDuration = 0.500000000000000000
    end
    object lblPitch: TLabel
      Left = 149
      Top = 15
      Width = 16
      Height = 15
      Alignment = taCenter
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblP: TLabel
      Left = 144
      Top = 487
      Width = 28
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Pitch'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object lblBpm: TLabel
      Left = 0
      Top = 15
      Width = 61
      Height = 33
      Alignment = taCenter
      AutoSize = False
      Caption = '--.-- BPM'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object bldBeat: TMfBeatLed
      Left = 0
      Top = 0
      Width = 261
      Height = 11
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      LedShape = blsRectangle
      LedOffColor = 4864009
      LedOnColor = 15715181
      BorderColor = 5850948
      BorderWidth = 0
      HoldTimeMs = 10
      FadeTimeMs = 380
      Transparent = False
      ExplicitWidth = 249
    end
    object Bevel6: TBevel
      Left = 189
      Top = 12
      Width = 66
      Height = 493
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object Label3: TLabel
      Left = 207
      Top = 465
      Width = 21
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label4: TLabel
      Left = 207
      Top = 308
      Width = 22
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Freq'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label5: TLabel
      Left = 213
      Top = 155
      Width = 8
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Q'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object lblInputGainValue: TLabel
      Left = 15
      Top = 324
      Width = 32
      Height = 15
      Alignment = taCenter
      Caption = '0.0 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      WordWrap = True
    end
    object lblGain: TLabel
      Left = 20
      Top = 487
      Width = 26
      Height = 14
      Hint = 'Volume Gain'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
    end
    object Label1: TLabel
      Left = 205
      Top = 487
      Width = 21
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      Caption = 'PEQ'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object tbVolume: TMfTrackBar
      Left = 76
      Top = 36
      Width = 53
      Height = 442
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TabOrder = 0
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
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      LargeChange = 4
      OnChange = tbVolumeChange
      OnDblClick = tbVolumeDblClick
    end
    object tbBalance: TMfTrackBar
      Left = 9
      Top = 519
      Width = 234
      Height = 40
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TabOrder = 1
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
      ShowTicks = True
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementH = tphBoth
      TickPlacementV = tpvBoth
      OnChange = tbBalanceChange
      OnDblClick = tbBalanceDblClick
    end
    object tbPitch: TMfTrackBar
      Left = 137
      Top = 36
      Width = 51
      Height = 442
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TabOrder = 2
      Color = 5850948
      ParentColor = False
      Minimum = -100
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
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      OnChange = tbPitchChange
      OnDblClick = tbPitchDblClick
    end
    object tbEqQ: TMfTrackBar
      Left = 199
      Top = 21
      Width = 51
      Height = 134
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TabOrder = 3
      Color = 5850948
      ParentColor = False
      Minimum = 1
      Position = 10
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
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      OnChange = tbEqQChange
      OnDblClick = tbEqQDblClick
    end
    object tbEqCenterFreqHz: TMfTrackBar
      Left = 197
      Top = 175
      Width = 52
      Height = 134
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TabOrder = 4
      Color = 5850948
      ParentColor = False
      Minimum = 10
      Maximum = 22000
      Position = 1500
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
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      OnChange = tbEqCenterFreqHzChange
      OnDblClick = tbEqCenterFreqHzDblClick
    end
    object tbEqGainDb: TMfTrackBar
      Left = 197
      Top = 333
      Width = 52
      Height = 134
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TabOrder = 5
      Color = 5850948
      ParentColor = False
      Minimum = -18
      Maximum = 18
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
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      OnChange = tbEqGainDbChange
      OnDblClick = tbEqGainDbDblClick
    end
    object tbInputGain: TMfTrackBar
      Left = 11
      Top = 343
      Width = 45
      Height = 135
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TabOrder = 6
      Color = 5850948
      ParentColor = False
      Minimum = -120
      Maximum = 60
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
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickLabelsVisible = False
      TickPlacementV = tpvBoth
      OnChange = tbInputGainChange
      OnDblClick = tbInputGainDblClick
    end
  end
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 261
    Height = 37
    Align = alTop
    Color = 4865081
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 249
    object shpLiveCap: TShape
      Left = 160
      Top = 4
      Width = 87
      Height = 29
      Brush.Color = 5850948
      Pen.Color = 5668864
    end
    object lblCaption: TLabel
      Left = 8
      Top = 11
      Width = 59
      Height = 14
      Hint = 'Channel number'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'LoopBack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
    end
    object shpLive: TShape
      Left = 162
      Top = 6
      Width = 83
      Height = 25
      Brush.Color = 5668864
      Pen.Color = 5668864
    end
    object lblLive: TLabel
      Left = 166
      Top = 9
      Width = 75
      Height = 19
      Alignment = taCenter
      AutoSize = False
      Caption = 'LIVE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 924
    Width = 261
    Height = 64
    Align = alBottom
    BevelOuter = bvNone
    Color = 4865081
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    ExplicitWidth = 249
    object lblStatus: TLabel
      Left = 0
      Top = 38
      Width = 261
      Height = 26
      Align = alBottom
      AutoSize = False
      Caption = 'State: Stopped'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 249
    end
    object Bevel8: TBevel
      Left = 0
      Top = 5
      Width = 261
      Height = 33
      Align = alBottom
      ExplicitWidth = 249
    end
    object lblPlayed: TLabel
      Left = 5
      Top = 10
      Width = 93
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Played: 00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblAudioFormat: TLabel
      Left = 106
      Top = 10
      Width = 144
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Format: 44.1 kHz / 16-bit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
  end
  object pnlMid: TPanel
    Left = 0
    Top = 615
    Width = 261
    Height = 309
    Align = alClient
    BevelOuter = bvNone
    Color = 4865081
    ParentBackground = False
    TabOrder = 3
    ExplicitWidth = 249
    object Bevel1: TBevel
      Left = 4
      Top = 151
      Width = 251
      Height = 130
    end
    object lblProcess: TLabel
      Left = 13
      Top = 222
      Width = 48
      Height = 17
      Alignment = taRightJustify
      Caption = 'Process:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblProcessId: TLabel
      Left = 30
      Top = 251
      Width = 31
      Height = 20
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PID:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object edtProcessName: TEdit
      Left = 67
      Top = 220
      Width = 180
      Height = 25
      Color = 4865081
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Text = '-'
    end
    object btnSelectProcess: TMPxpButton
      Left = 12
      Top = 158
      Width = 107
      Height = 47
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = 'Select source'
      Color = 5914932
      ColorWhenDown = 11363625
      ColorWhenUp = 5914932
      Behavior = bbPushButton
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = clWhite
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      SlowDecease = True
      Style = bsModern
      OnClick = btnSelectProcessClick
    end
    object btnPlayStop: TMPxpButton
      Left = 81
      Top = 7
      Width = 93
      Height = 53
      Alignment = taCenter
      Caption = 'Play'
      Color = 5668864
      ColorWhenDown = 164
      ColorWhenUp = 5668864
      Checked = False
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
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
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      OnClick = btnPlayStopClick
    end
    object chkMute: TMPxpButton
      Left = 78
      Top = 87
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
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
      OnClick = chkMuteClick
    end
    object chkCrossFade: TMPxpButton
      Left = 189
      Top = 87
      Width = 67
      Height = 40
      Alignment = taCenter
      Caption = 'X Fade'
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
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
      OnClick = chkCrossFadeClick
    end
    object btnPFL: TMPxpButton
      Left = 5
      Top = 87
      Width = 66
      Height = 40
      Alignment = taCenter
      Caption = 'PFL'
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
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
      OnClick = btnPFLClick
    end
    object edtPID: TEdit
      Left = 67
      Top = 249
      Width = 180
      Height = 25
      Color = 4865081
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 6
      Text = '-'
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    Left = 38
    Top = 260
  end
end
