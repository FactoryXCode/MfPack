object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'LoopBackCapture Sample 2'
  ClientHeight = 232
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 113
    Width = 405
    Height = 83
  end
  object Label1: TLabel
    Left = 29
    Top = 127
    Width = 79
    Height = 13
    Hint = 'Enter a file name without extension.'
    AutoSize = False
    Caption = 'FileName'
    ParentShowHint = False
    ShowHint = True
  end
  object lblFileExt: TLabel
    Left = 294
    Top = 142
    Width = 27
    Height = 16
    Hint = 'Enter a file name without extension.'
    Caption = '.wav'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Transparent = False
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 15
    Top = 39
    Width = 92
    Height = 17
    Hint = 
      'Obtain the process ID for the process tree you wish to capture o' +
      'r exclude from capture.'#10'You can use Task Manager or the tlist pr' +
      'ogram to get this ID. Run the sample with the process ID, the'#10'de' +
      'sired capture mode (including the process tree or excluding it),' +
      ' and the output WAV file.'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Process ID (PID):'
  end
  object sbMsg: TStatusBar
    Left = 0
    Top = 208
    Width = 424
    Height = 24
    DoubleBuffered = True
    Panels = <>
    ParentDoubleBuffered = False
    SimplePanel = True
    SimpleText = 'Start Capture'
  end
  object butStart: TButton
    Left = 72
    Top = 169
    Width = 85
    Height = 27
    Caption = 'Start Capture'
    TabOrder = 1
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 163
    Top = 169
    Width = 85
    Height = 27
    Caption = 'Stop Capture'
    Enabled = False
    TabOrder = 2
    OnClick = butStopClick
  end
  object edFileName: TEdit
    Left = 29
    Top = 142
    Width = 265
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = 'loopback-capture'
    OnKeyUp = edFileNameKeyUp
  end
  object butPlayData: TButton
    Left = 254
    Top = 169
    Width = 80
    Height = 27
    Hint = 'Play recorded data.'
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = butPlayDataClick
  end
  object cbxDontOverWrite: TCheckBox
    Left = 26
    Top = 16
    Width = 253
    Height = 15
    Hint = 'Do not overwrite files with the same name.'
    Caption = 'Don'#39't overwrite excisting  files.'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 5
  end
  object edPID: TEdit
    Left = 113
    Top = 37
    Width = 182
    Height = 21
    Hint = 'Enter a numeric value!'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    Text = '1234'
  end
  object rb2: TRadioButton
    Left = 26
    Top = 85
    Width = 291
    Height = 17
    Caption = 'Capture audio from process ID (PID)  and its children'
    TabOrder = 7
  end
  object rb1: TRadioButton
    Left = 26
    Top = 62
    Width = 365
    Height = 17
    Caption = 
      'Capture audio from all process except process ID (PID) and its c' +
      'hildren'
    Checked = True
    TabOrder = 8
    TabStop = True
  end
  object butGetPID: TButton
    Left = 306
    Top = 35
    Width = 75
    Height = 23
    Hint = 'Get the Program ID of this application.'
    Caption = 'Get PID'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    OnClick = butGetPIDClick
  end
end
