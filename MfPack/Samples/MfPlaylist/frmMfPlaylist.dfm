object MfPlaylistForm: TMfPlaylistForm
  Left = 0
  Top = 0
  Caption = 'MfPack - Sequencer Source Playlist'
  ClientHeight = 780
  ClientWidth = 1120
  Color = clBtnFace
  Constraints.MinHeight = 640
  Constraints.MinWidth = 980
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 17
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1120
    Height = 80
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 10
      Width = 281
      Height = 31
      Caption = 'Media Foundation playlist'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -23
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblIntroduction: TLabel
      Left = 17
      Top = 46
      Width = 1000
      Height = 20
      AutoSize = False
      Caption = 
        'One Media Session, one Sequencer Source, multiple topologies. Ad' +
        'd files, build the sequence, then play.'
    end
  end
  object pnlTransport: TPanel
    Left = 0
    Top = 452
    Width = 1120
    Height = 56
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblState: TLabel
      Left = 700
      Top = 20
      Width = 260
      Height = 20
      AutoSize = False
      Caption = 'State: Empty'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnBuild: TButton
      Left = 16
      Top = 12
      Width = 124
      Height = 32
      Caption = 'Build playlist'
      TabOrder = 0
      OnClick = BuildClick
    end
    object btnPlay: TButton
      Left = 154
      Top = 12
      Width = 84
      Height = 32
      Caption = 'Play'
      TabOrder = 1
      OnClick = PlayClick
    end
    object btnPause: TButton
      Left = 246
      Top = 12
      Width = 84
      Height = 32
      Caption = 'Pause'
      TabOrder = 2
      OnClick = PauseClick
    end
    object btnStop: TButton
      Left = 338
      Top = 12
      Width = 84
      Height = 32
      Caption = 'Stop'
      TabOrder = 3
      OnClick = StopClick
    end
    object btnPlaySelected: TButton
      Left = 438
      Top = 12
      Width = 126
      Height = 32
      Caption = 'Play selected'
      TabOrder = 4
      OnClick = PlaySelectedClick
    end
    object btnRelease: TButton
      Left = 580
      Top = 12
      Width = 90
      Height = 32
      Hint = 'Close the sequence and session so the playlist can be edited.'
      Caption = 'Release'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = ReleaseClick
    end
  end
  object pnlTime: TPanel
    Left = 0
    Top = 508
    Width = 1120
    Height = 72
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      1120
      72)
    object lblCurrent: TLabel
      Left = 16
      Top = 2
      Width = 1060
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'No segment is playing'
    end
    object lblTime: TLabel
      Left = 16
      Top = 24
      Width = 1060
      Height = 19
      AutoSize = False
      Caption = 'Segment 00:00:00 / 00:00:00     Presentation clock 00:00:00'
    end
    object prbSegment: TProgressBar
      Left = 16
      Top = 49
      Width = 1088
      Height = 13
      Cursor = crHandPoint
      Hint = 
        'Click to play from this position in the current segment, or righ' +
        't-click for the menu.'
      Anchors = [akLeft, akTop, akRight]
      Max = 1000
      ParentShowHint = False
      PopupMenu = pmProgress
      ShowHint = True
      TabOrder = 0
      OnContextPopup = ProgressContextPopup
      OnMouseDown = ProgressMouseDown
    end
  end
  object pnlLog: TPanel
    Left = 0
    Top = 580
    Width = 1120
    Height = 200
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Right = 16
    Padding.Bottom = 12
    TabOrder = 4
    object lblLog: TLabel
      Left = 16
      Top = 0
      Width = 1088
      Height = 25
      Align = alTop
      AutoSize = False
      Caption = 'Session events and learning trace'
    end
    object memLog: TMemo
      Left = 16
      Top = 25
      Width = 1088
      Height = 163
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
  end
  object pnlContent: TPanel
    Left = 0
    Top = 80
    Width = 1120
    Height = 372
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Right = 16
    TabOrder = 1
    object Splitter: TSplitter
      Left = 366
      Top = 0
      Width = 10
      Height = 372
      MinSize = 340
    end
    object pnlPlaylist: TPanel
      Left = 16
      Top = 0
      Width = 350
      Height = 372
      Align = alLeft
      BevelOuter = bvNone
      Constraints.MinWidth = 340
      TabOrder = 0
      object lblPlaylist: TLabel
        Left = 0
        Top = 0
        Width = 350
        Height = 28
        Align = alTop
        AutoSize = False
        Caption = 'Playlist order'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pnlEdit: TPanel
        Left = 0
        Top = 332
        Width = 350
        Height = 40
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btnAdd: TButton
          Left = 0
          Top = 8
          Width = 80
          Height = 28
          Caption = 'Add files...'
          TabOrder = 0
          OnClick = AddClick
        end
        object btnRemove: TButton
          Left = 86
          Top = 8
          Width = 68
          Height = 28
          Caption = 'Remove'
          TabOrder = 1
          OnClick = RemoveClick
        end
        object btnClear: TButton
          Left = 160
          Top = 8
          Width = 54
          Height = 28
          Caption = 'Clear'
          TabOrder = 2
          OnClick = ClearClick
        end
        object btnUp: TButton
          Left = 220
          Top = 8
          Width = 52
          Height = 28
          Caption = 'Up'
          TabOrder = 3
          OnClick = UpClick
        end
        object btnDown: TButton
          Left = 278
          Top = 8
          Width = 68
          Height = 28
          Caption = 'Down'
          TabOrder = 4
          OnClick = DownClick
        end
      end
      object lstPlaylist: TListBox
        Left = 0
        Top = 28
        Width = 350
        Height = 304
        Align = alClient
        ItemHeight = 17
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = SelectionChanged
        OnDblClick = PlaySelectedClick
      end
    end
    object pnlPreview: TPanel
      Left = 376
      Top = 0
      Width = 728
      Height = 372
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object lblPreview: TLabel
        Left = 0
        Top = 0
        Width = 728
        Height = 28
        Align = alTop
        AutoSize = False
        Caption = 'Video preview / audio artwork'
      end
      object pnlVideo: TPanel
        Left = 0
        Top = 28
        Width = 728
        Height = 344
        Align = alClient
        BevelOuter = bvNone
        Color = clBlack
        ParentBackground = False
        TabOrder = 0
      end
      object pnlArtwork: TPanel
        Left = 0
        Top = 28
        Width = 728
        Height = 344
        Align = alClient
        BevelOuter = bvNone
        Color = clBlack
        ParentBackground = False
        TabOrder = 1
        Visible = False
        object imgArtwork: TImage
          Left = 0
          Top = 0
          Width = 728
          Height = 344
          Align = alClient
          Center = True
          Proportional = True
          Stretch = True
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'mp4'
    Filter = 
      'Media files|*.mp4;*.m4v;*.wmv;*.wma;*.avi;*.mp3;*.wav;*.m4a;*.aa' +
      'c|All files|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Title = 'Add files to the sequence'
    Left = 840
    Top = 16
  end
  object PositionTimer: TTimer
    Enabled = False
    Interval = 250
    OnTimer = PositionTimerTimer
    Left = 888
    Top = 16
  end
  object pmProgress: TPopupMenu
    AutoPopup = False
    Left = 936
    Top = 16
    object mnuPlayFromHere: TMenuItem
      Caption = 'Play from this position'
      OnClick = PlayFromHereClick
    end
  end
end
