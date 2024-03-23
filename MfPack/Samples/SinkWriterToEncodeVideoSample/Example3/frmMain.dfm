object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ImagesToVideo Sample 3'
  ClientHeight = 589
  ClientWidth = 955
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object stbStatus: TStatusBar
    Left = 0
    Top = 568
    Width = 955
    Height = 21
    Panels = <>
    SimplePanel = True
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 955
    Height = 568
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
      ImageIndex = 1
      DesignSize = (
        947
        536)
      object Bevel1: TBevel
        Left = 5
        Top = 200
        Width = 934
        Height = 332
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
      end
      object Bevel3: TBevel
        Left = 5
        Top = 20
        Width = 934
        Height = 157
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
      end
      object Label1: TLabel
        Left = 80
        Top = 38
        Width = 66
        Height = 14
        Alignment = taRightJustify
        Caption = 'Output format'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 85
        Top = 65
        Width = 62
        Height = 14
        Hint = 'Suppoerted codecs.'
        ParentCustomHint = False
        Alignment = taRightJustify
        Caption = 'Video Codec'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
      end
      object Label14: TLabel
        Left = 19
        Top = 92
        Width = 128
        Height = 14
        Alignment = taRightJustify
        Caption = 'Resolution and aspectratio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label15: TLabel
        Left = 68
        Top = 146
        Width = 78
        Height = 14
        Hint = '(recommended: >=60)'
        Alignment = taRightJustify
        Caption = 'Encoding quality'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
      end
      object Label16: TLabel
        Left = 94
        Top = 119
        Width = 52
        Height = 14
        Hint = 'Framerate in frames per second (fps)'
        Alignment = taRightJustify
        Caption = 'Frame rate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
      end
      object lblCodecInfo: TLabel
        Left = 482
        Top = 49
        Width = 431
        Height = 117
        Anchors = []
        AutoSize = False
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
        WordWrap = True
      end
      object Label17: TLabel
        Left = 482
        Top = 29
        Width = 102
        Height = 14
        Alignment = taRightJustify
        Caption = 'Codec Information'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label11: TLabel
        Left = 18
        Top = 505
        Width = 911
        Height = 25
        AutoSize = False
        Caption = 
          'Changing the sample rate from the one of the input-file might no' +
          't be supported prior to Windows10.'
        Layout = tlCenter
        WordWrap = True
      end
      object Label21: TLabel
        Left = 18
        Top = 216
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
      object cbxFileFormat: TComboBox
        Left = 153
        Top = 35
        Width = 80
        Height = 22
        Hint = 'Currently MP4 only.'
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
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
        Left = 153
        Top = 62
        Width = 193
        Height = 22
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnChange = cbxVideoCodecChange
      end
      object cbxResolution: TComboBox
        Left = 153
        Top = 89
        Width = 256
        Height = 22
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
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
        Left = 153
        Top = 143
        Width = 49
        Height = 23
        Hint = 'Set the encoding quality (10 to 100, recommended: >=60)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        MaxValue = 100
        MinValue = 10
        ParentFont = False
        TabOrder = 3
        Value = 70
      end
      object cbxFrameRates: TComboBox
        Left = 153
        Top = 116
        Width = 74
        Height = 22
        Hint = 'Framerate in frames per second (fps)'
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
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
      object StaticText2: TStaticText
        Left = 18
        Top = 190
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
        Caption = 'Audio'
        TabOrder = 6
        Transparent = False
      end
      object mmoAudioCodecDescr: TMemo
        Left = 17
        Top = 233
        Width = 915
        Height = 270
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
        Font.Height = -11
        Font.Name = 'Stencil'
        Font.Pitch = fpFixed
        Font.Style = []
        Lines.Strings = (
          'No audio codec selected in Create Slideshow page.')
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 7
        WantReturns = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Create Slideshow'
      ImageIndex = 1
      object Panel5: TPanel
        Left = 0
        Top = 302
        Width = 947
        Height = 234
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
          Top = 16
          Width = 935
          Height = 178
        end
        object Bevel5: TBevel
          Left = 330
          Top = 50
          Width = 303
          Height = 137
        end
        object Label22: TLabel
          Left = 364
          Top = 72
          Width = 97
          Height = 13
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Effect TIme'
        end
        object Label2: TLabel
          Left = 363
          Top = 101
          Width = 97
          Height = 17
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Image duration'
        end
        object Bevel6: TBevel
          Left = 639
          Top = 50
          Width = 291
          Height = 137
        end
        object pnlInclAudio: TPanel
          Left = 7
          Top = 51
          Width = 317
          Height = 137
          BevelOuter = bvLowered
          Enabled = False
          TabOrder = 13
          object Label12: TLabel
            Left = 6
            Top = 61
            Width = 111
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Audio Start Position'
          end
          object Label19: TLabel
            Left = 10
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
          object AudioStartTime: TSpinEdit
            Left = 10
            Top = 77
            Width = 142
            Height = 24
            Increment = 1000
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
          object cbxSetPresentationDuration: TCheckBox
            Left = 10
            Top = 112
            Width = 269
            Height = 15
            Hint = 'A dialog will be shown to add an audio file.'
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Set presentation length to audio file duration'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = cbxSetPresentationDurationClick
          end
          object cbxAudioCodec: TComboBox
            Left = 10
            Top = 38
            Width = 298
            Height = 22
            Hint = 'Choose an audio codec.'
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Style = csDropDownList
            Font.Charset = ARABIC_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ItemIndex = 0
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            Text = 'Not Selected'
            OnChange = cbxAudioCodecChange
            Items.Strings = (
              'Not Selected'
              'Advanced Audio Coding (AAC)'
              'Free Lossless Audio Codec (FLAC)')
          end
        end
        object StaticText3: TStaticText
          Left = 21
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
          TabOrder = 5
          Transparent = False
        end
        object WriteSlideshow: TButton
          Left = 21
          Top = 200
          Width = 115
          Height = 29
          Caption = 'Create SlideShow'
          TabOrder = 0
          WordWrap = True
          OnClick = WriteSlideshowClick
        end
        object Background: TCheckBox
          Left = 651
          Top = 69
          Width = 222
          Height = 14
          Caption = 'Run in background thread'
          TabOrder = 1
          WordWrap = True
        end
        object CropLandscape: TCheckBox
          Left = 651
          Top = 89
          Width = 221
          Height = 15
          Caption = 'Crop landscape images to video size'
          TabOrder = 2
        end
        object ZoomInOut: TCheckBox
          Left = 651
          Top = 111
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
          Left = 651
          Top = 132
          Width = 254
          Height = 15
          Hint = 'Debug Timing (Displays encoded timestamp in seconds)'
          Caption = 'Display encoded timestamp in seconds'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          WordWrap = True
        end
        object chbxAddAudio: TCheckBox
          Left = 17
          Top = 45
          Width = 95
          Height = 14
          Hint = 'Display dialog to add an audio file'
          Caption = 'Include Audio'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = chbxAddAudioClick
        end
        object edLocation: TEdit
          Left = 142
          Top = 205
          Width = 673
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
          TabOrder = 7
          OnClick = edLocationClick
        end
        object ShowVideo: TButton
          Left = 821
          Top = 204
          Width = 109
          Height = 22
          Caption = 'Play output video'
          TabOrder = 8
          OnClick = ShowVideoClick
        end
        object spedEffectDuration: TSpinEdit
          Left = 466
          Top = 69
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
          TabOrder = 9
          Value = 2000
        end
        object spedImageDuration: TSpinEdit
          Left = 466
          Top = 98
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
          TabOrder = 10
          Value = 4000
        end
        object StaticText4: TStaticText
          Left = 651
          Top = 42
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
          Caption = 'Rendering Settings'
          TabOrder = 11
          Transparent = False
        end
        object StaticText5: TStaticText
          Left = 344
          Top = 43
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
          Caption = 'Timing Settings'
          TabOrder = 12
          Transparent = False
        end
      end
      object pnlSelectPics: TPanel
        Left = 0
        Top = 0
        Width = 947
        Height = 302
        Align = alClient
        TabOrder = 1
        object Splitter1: TSplitter
          Left = 238
          Top = 1
          Height = 300
          ExplicitHeight = 224
        end
        object Splitter2: TSplitter
          Left = 436
          Top = 1
          Height = 300
          ExplicitHeight = 224
        end
        object Splitter3: TSplitter
          Left = 632
          Top = 1
          Width = 4
          Height = 300
          ExplicitLeft = 641
          ExplicitTop = 8
          ExplicitHeight = 215
        end
        object Panel1: TPanel
          Left = 1
          Top = 1
          Width = 237
          Height = 300
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
            Height = 242
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentColor = True
            ParentFont = False
            TabOrder = 0
            OnChange = dlbDirChange
          end
          object DriveComboBox1: TDriveComboBox
            Left = 1
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
            StyleElements = []
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
            TabOrder = 2
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
          Height = 300
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 1
          object ImageCount: TLabel
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
            Caption = 'ImageCount'
            Layout = tlCenter
            ExplicitTop = 2
          end
          object lbxFileBox: TCheckListBox
            Left = 0
            Top = 25
            Width = 195
            Height = 275
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
          Height = 300
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
            Height = 275
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
          Width = 310
          Height = 300
          Align = alClient
          BevelInner = bvLowered
          TabOrder = 3
          object Bevel7: TBevel
            Left = 2
            Top = 2
            Width = 306
            Height = 30
            Align = alTop
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
            Top = 48
            Width = 306
            Height = 250
            Align = alClient
            Center = True
            Proportional = True
            ExplicitTop = 75
            ExplicitHeight = 222
          end
          object butRunPreview: TButton
            Left = 204
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
            Width = 306
            Height = 16
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
    Left = 366
    Top = 466
  end
  object OD: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 497
    Top = 465
  end
  object FODPic: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'All supported'
        FileMask = '*.bmp;*.jpg;*.png;*.gif'
      end
      item
        DisplayName = 'All'
        FileMask = '*.*'
      end>
    Options = []
    Left = 434
    Top = 465
  end
end
