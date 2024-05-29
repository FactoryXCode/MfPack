object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ImagesToVideo Sample 3'
  ClientHeight = 600
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object stbStatus: TStatusBar
    Left = 0
    Top = 579
    Width = 944
    Height = 21
    Panels = <>
    SimplePanel = True
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 944
    Height = 579
    ActivePage = TabSheet1
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    HotTrack = True
    ParentFont = False
    Style = tsButtons
    TabOrder = 1
    OnChanging = PageControl1Changing
    object TabSheet1: TTabSheet
      Caption = 'Render Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ImageIndex = 1
      ParentFont = False
      DesignSize = (
        936
        547)
      object Bevel3: TBevel
        Left = 5
        Top = 20
        Width = 928
        Height = 157
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
      end
      object Label1: TLabel
        Left = 94
        Top = 38
        Width = 74
        Height = 15
        Alignment = taRightJustify
        Caption = 'Output format'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 99
        Top = 65
        Width = 70
        Height = 15
        Hint = 'Suppoerted codecs.'
        ParentCustomHint = False
        Alignment = taRightJustify
        Caption = 'Video Codec'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
      end
      object Label14: TLabel
        Left = 21
        Top = 92
        Width = 148
        Height = 15
        Alignment = taRightJustify
        Caption = 'Resolution and aspectratio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label15: TLabel
        Left = 78
        Top = 146
        Width = 90
        Height = 15
        Hint = '(recommended: >=60)'
        Alignment = taRightJustify
        Caption = 'Encoding quality'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
      end
      object Label16: TLabel
        Left = 108
        Top = 119
        Width = 60
        Height = 15
        Hint = 'Framerate in frames per second (fps)'
        Alignment = taRightJustify
        Caption = 'Frame rate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
      end
      object lblCodecInfo: TLabel
        Left = 529
        Top = 66
        Width = 381
        Height = 100
        Anchors = []
        AutoSize = False
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
        WordWrap = True
      end
      object Label17: TLabel
        Left = 418
        Top = 66
        Width = 101
        Height = 15
        Alignment = taRightJustify
        Caption = 'Codec Information'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object cbxFileFormat: TComboBox
        Left = 175
        Top = 35
        Width = 80
        Height = 23
        Hint = 'Currently MP4 only.'
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ItemIndex = 0
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '.mp4'
        OnChange = cbxFileFormatChange
        Items.Strings = (
          '.mp4')
      end
      object cbxVideoCodec: TComboBox
        Left = 175
        Top = 62
        Width = 193
        Height = 23
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnChange = cbxVideoCodecChange
      end
      object cbxResolution: TComboBox
        Left = 175
        Top = 89
        Width = 256
        Height = 23
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ItemIndex = 0
        ParentFont = False
        TabOrder = 2
        Text = 'SD    360p  (640 x 360)'
        OnChange = cbxResolutionChange
        Items.Strings = (
          'SD    360p  (640 x 360)'
          'SD    480p  (640 x 480)'
          'SD    480p  (854 x 480)'
          'HD    720p  (1280 x 720)'
          'FHD  1080p  (1920 x 1080)'
          '2K   1080p  (2048 x 1080)'
          'QHD  1440p  (2560 x 1440)'
          '4K   2160p  (3840 x 2160)')
      end
      object spedSetQuality: TSpinEdit
        Left = 175
        Top = 143
        Width = 49
        Height = 24
        Hint = 'Set the encoding quality (10 to 100, recommended: >=60)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        MaxValue = 100
        MinValue = 10
        ParentFont = False
        TabOrder = 3
        Value = 70
      end
      object cbxFrameRates: TComboBox
        Left = 175
        Top = 116
        Width = 74
        Height = 23
        Hint = 'Framerate in frames per second (fps)'
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnChange = cbxFrameRatesChange
      end
      object StaticText1: TStaticText
        Left = 18
        Top = 10
        Width = 106
        Height = 21
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Alignment = taCenter
        AutoSize = False
        BevelInner = bvNone
        BevelKind = bkFlat
        Caption = 'Video'
        TabOrder = 5
        Transparent = False
      end
      object pnlInclAudio: TPanel
        Left = 3
        Top = 199
        Width = 930
        Height = 335
        BevelOuter = bvLowered
        Enabled = False
        TabOrder = 6
        object Label19: TLabel
          Left = 94
          Top = 20
          Width = 70
          Height = 15
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          Caption = 'Audio Codec'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label21: TLabel
          Left = 355
          Top = 21
          Width = 102
          Height = 14
          Caption = 'Codec Information'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblAudioSource: TLabel
          Left = 170
          Top = 308
          Width = 730
          Height = 17
          AutoSize = False
          Caption = 'No file selected.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
          WordWrap = True
        end
        object Label4: TLabel
          Left = 64
          Top = 54
          Width = 101
          Height = 15
          Alignment = taRightJustify
          Caption = 'Codec Information'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 88
          Top = 309
          Width = 72
          Height = 15
          Alignment = taRightJustify
          Caption = 'Audio source'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object cbxAudioCodec: TComboBox
          Left = 174
          Top = 17
          Width = 299
          Height = 23
          Hint = 'Choose an audio codec.'
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Style = csDropDownList
          Font.Charset = ARABIC_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'Not Selected'
          OnChange = cbxAudioCodecChange
          Items.Strings = (
            'Not Selected'
            'Advanced Audio Coding (AAC)'
            'Free Lossless Audio Codec (FLAC)'
            'Dolby AC3 (AC-3)')
        end
        object mmoAudioCodecDescr: TMemo
          Left = 170
          Top = 50
          Width = 749
          Height = 250
          Hint = 
            'Codec information will be shown if an audio codec has been choos' +
            'en.'
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Color = clInfoBk
          Font.Charset = ARABIC_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Stencil'
          Font.Pitch = fpFixed
          Font.Style = []
          Lines.Strings = (
            'No audio codec selected in '
            'Create Slideshow page.')
          ParentFont = False
          ParentShowHint = False
          ReadOnly = True
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 1
          WantReturns = False
        end
      end
      object chbxAddAudio: TCheckBox
        Left = 15
        Top = 193
        Width = 95
        Height = 14
        Hint = 'Display dialog to add an audio file'
        Caption = 'Include Audio'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = chbxAddAudioClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Create SlideShow'
      ImageIndex = 1
      object Panel5: TPanel
        Left = 0
        Top = 294
        Width = 936
        Height = 253
        Hint = 'Audio start position in milliseconds.'
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object Bevel2: TBevel
          Left = 1
          Top = 15
          Width = 933
          Height = 196
        end
        object Bevel5: TBevel
          Left = 12
          Top = 41
          Width = 340
          Height = 163
        end
        object Label3: TLabel
          Left = 92
          Top = 174
          Width = 124
          Height = 15
          Hint = 'Encoder/decoder latency compensation per image  in ns'
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          Caption = 'Latency compensation'
        end
        object Label22: TLabel
          Left = 155
          Top = 89
          Width = 61
          Height = 15
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          Caption = 'Effect TIme'
        end
        object Bevel6: TBevel
          Left = 358
          Top = 41
          Width = 572
          Height = 163
        end
        object Label12: TLabel
          Left = 89
          Top = 61
          Width = 127
          Height = 16
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Audio Start Position'
        end
        object Label2: TLabel
          Left = 133
          Top = 140
          Width = 83
          Height = 15
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          Caption = 'Image duration'
        end
        object spedCompensation: TSpinEdit
          Left = 221
          Top = 171
          Width = 91
          Height = 24
          Hint = 'Encoder/decoder latency compensation per image  in ns'
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Increment = 10
          MaxValue = 0
          MinValue = 0
          TabOrder = 11
          Value = 300
        end
        object StaticText5: TStaticText
          Left = 21
          Top = 33
          Width = 133
          Height = 18
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          AutoSize = False
          BevelInner = bvNone
          BevelKind = bkFlat
          Caption = 'Timing'
          TabOrder = 10
          Transparent = False
        end
        object spedEffectDuration: TSpinEdit
          Left = 221
          Top = 86
          Width = 91
          Height = 24
          Hint = 'Effecttime in ms'
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Increment = 1000
          MaxValue = 0
          MinValue = 0
          TabOrder = 8
          Value = 2000
        end
        object StaticText3: TStaticText
          Left = 12
          Top = 7
          Width = 152
          Height = 18
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          AutoSize = False
          BevelInner = bvNone
          BevelKind = bkFlat
          Caption = 'SlideShow Settings'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 5
          Transparent = False
        end
        object butRenderSlideshow: TButton
          Left = 12
          Top = 217
          Width = 115
          Height = 28
          Caption = 'Render SlideShow'
          TabOrder = 0
          WordWrap = True
          OnClick = butRenderSlideshowClick
        end
        object Background: TCheckBox
          Left = 370
          Top = 61
          Width = 222
          Height = 14
          Caption = 'Run in background thread'
          TabOrder = 1
          WordWrap = True
        end
        object CropLandscape: TCheckBox
          Left = 370
          Top = 81
          Width = 221
          Height = 15
          Caption = 'Crop landscape images to video size'
          TabOrder = 2
        end
        object ZoomInOut: TCheckBox
          Left = 370
          Top = 103
          Width = 196
          Height = 15
          Hint = 'Include Zoom In and Out-transitions slows down rendering.'
          Caption = 'Zoom In and Out'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          WordWrap = True
        end
        object DebugTiming: TCheckBox
          Left = 370
          Top = 124
          Width = 254
          Height = 15
          Hint = 'Debug Timing (Displays encoded timestamp in seconds)'
          Caption = 'Display encoded timestamp in seconds'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          WordWrap = True
        end
        object edLocation: TEdit
          Left = 133
          Top = 222
          Width = 682
          Height = 20
          Cursor = crHandPoint
          Hint = 'Double Click to open the file.'
          AutoSize = False
          BorderStyle = bsNone
          Color = clInfoBk
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHotLight
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsUnderline]
          ParentFont = False
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 6
          OnClick = edLocationClick
        end
        object ShowVideo: TButton
          Left = 821
          Top = 217
          Width = 109
          Height = 28
          Caption = 'Play output video'
          TabOrder = 7
          OnClick = ShowVideoClick
        end
        object StaticText4: TStaticText
          Left = 368
          Top = 33
          Width = 133
          Height = 18
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          AutoSize = False
          BevelInner = bvNone
          BevelKind = bkFlat
          Caption = 'Rendering'
          TabOrder = 9
          Transparent = False
        end
        object AudioStartTime: TSpinEdit
          Left = 221
          Top = 58
          Width = 91
          Height = 24
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Increment = 1000
          MaxValue = 0
          MinValue = 0
          TabOrder = 12
          Value = 0
        end
        object cbxSetPresentationDuration: TCheckBox
          Left = 47
          Top = 117
          Width = 263
          Height = 18
          Hint = 'A dialog will be shown to add an audio file.'
          Margins.Left = 9
          Margins.Top = 9
          Margins.Right = 9
          Margins.Bottom = 9
          Alignment = taLeftJustify
          Caption = 'Set presentation length to audio file duration'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
          OnClick = cbxSetPresentationDurationClick
        end
        object spedImageDuration: TSpinEdit
          Left = 221
          Top = 137
          Width = 91
          Height = 24
          Hint = 'Imageduration in ms'
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Increment = 1000
          MaxValue = 0
          MinValue = 0
          TabOrder = 14
          Value = 4000
        end
      end
      object pnlSelectPics: TPanel
        Left = 0
        Top = 0
        Width = 936
        Height = 294
        Align = alClient
        TabOrder = 1
        object Splitter1: TSplitter
          Left = 238
          Top = 1
          Height = 292
          ExplicitHeight = 224
        end
        object Splitter2: TSplitter
          Left = 436
          Top = 1
          Height = 292
          ExplicitHeight = 224
        end
        object Splitter3: TSplitter
          Left = 632
          Top = 1
          Width = 4
          Height = 292
          ExplicitLeft = 641
          ExplicitTop = 8
          ExplicitHeight = 215
        end
        object Panel1: TPanel
          Left = 1
          Top = 1
          Width = 237
          Height = 292
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object Bevel4: TBevel
            Left = 0
            Top = 0
            Width = 237
            Height = 58
            Align = alTop
            Shape = bsBottomLine
          end
          object dlbDir: TDirectoryListBox
            Left = 0
            Top = 58
            Width = 237
            Height = 234
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentColor = True
            ParentFont = False
            TabOrder = 2
            OnChange = dlbDirChange
          end
          object DriveComboBox1: TDriveComboBox
            Left = 2
            Top = 35
            Width = 230
            Height = 20
            AutoComplete = False
            Color = cl3DLight
            DirList = dlbDir
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = DriveComboBox1Click
          end
          object cbxPickWinFolder: TComboBox
            Left = 2
            Top = 10
            Width = 230
            Height = 22
            Hint = 'Select a Windows folder.'
            Color = clHotLight
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clHighlightText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ItemIndex = 0
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            Text = 'User Pictures'
            OnChange = cbxPickWinFolderChange
            Items.Strings = (
              'User Pictures'
              'Public Pictures'
              'Desktop'
              'Downloads'
              'Favorites')
          end
        end
        object Panel2: TPanel
          Left = 241
          Top = 1
          Width = 195
          Height = 292
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 1
          object lblImageCount: TLabel
            AlignWithMargins = True
            Left = 5
            Top = 5
            Width = 185
            Height = 15
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            AutoSize = False
            Caption = 'Selected 0 images'
            Layout = tlCenter
            ExplicitTop = 2
          end
          object lbxFileBox: TCheckListBox
            Left = 0
            Top = 25
            Width = 195
            Height = 267
            Hint = 'DoublClick to add a duplicate to the Rendering Order.'
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            OnClickCheck = lbxFileBoxClickCheck
            Align = alClient
            Flat = False
            ItemHeight = 14
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = lbxFileBoxClick
            OnDblClick = lbxFileBoxDblClick
          end
        end
        object Panel3: TPanel
          Left = 439
          Top = 1
          Width = 193
          Height = 292
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 2
          object lblRenderingOrder: TLabel
            AlignWithMargins = True
            Left = 5
            Top = 5
            Width = 183
            Height = 15
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            AutoSize = False
            Caption = 'Rendering Order'
            Layout = tlCenter
          end
          object lbxRenderingOrder: TListBox
            Left = 0
            Top = 25
            Width = 193
            Height = 267
            Hint = 'Use drag and drop to change the rendering order.'
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            AutoCompleteDelay = 100
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 14
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = lbxRenderingOrderClick
            OnDragDrop = lbxRenderingOrderDragDrop
            OnDragOver = lbxRenderingOrderDragOver
            OnStartDrag = lbxRenderingOrderStartDrag
          end
        end
        object Panel4: TPanel
          Left = 636
          Top = 1
          Width = 299
          Height = 292
          Align = alClient
          BevelInner = bvLowered
          TabOrder = 3
          object Bevel7: TBevel
            Left = 2
            Top = 2
            Width = 295
            Height = 30
            Align = alTop
            ExplicitWidth = 306
          end
          object Label18: TLabel
            AlignWithMargins = True
            Left = 7
            Top = 8
            Width = 76
            Height = 15
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            AutoSize = False
            Caption = 'Preview'
            Layout = tlCenter
          end
          object imgPreview: TImage
            Left = 2
            Top = 47
            Width = 295
            Height = 243
            Align = alClient
            Center = True
            Proportional = True
            ExplicitTop = 48
          end
          object butRunPreview: TButton
            Left = 195
            Top = 5
            Width = 98
            Height = 23
            Hint = 'Timing Options: "Image Duration" sets the display time.'
            Caption = 'Run Preview'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = butRunPreviewClick
          end
          object pbPreview: TProgressBar
            Left = 2
            Top = 32
            Width = 295
            Height = 15
            Align = alTop
            DoubleBuffered = True
            ParentDoubleBuffered = False
            Smooth = True
            MarqueeInterval = 1
            Step = 1
            TabOrder = 1
          end
        end
      end
    end
  end
  object fodSelectAudio: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Audio files (*.wav;*.mp3;*.aac;*.wma)'
        FileMask = '*.wav;*.mp3;*.aac;*.wma'
      end
      item
        DisplayName = 'Audio- and video-files'
        FileMask = '*.wav;*.mp3;*.aac;*.wma;*.avi;*.mp4;*.mpg;*.mkv;*.vob;*.wmv'
      end
      item
        DisplayName = 'Any'
        FileMask = '*.*'
      end>
    FileTypeIndex = 2
    Options = []
    Title = 'Choose an audio file.'
    Left = 711
    Top = 8
  end
end
