object frmTranscoder: TfrmTranscoder
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'MfTranscoder Sample'
  ClientHeight = 201
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object lblChooseSourceFile: TLabel
    Left = 0
    Top = 16
    Width = 59
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Sourcefile'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    OnClick = mnuOpenSourceFileClick
    OnMouseMove = lblChooseSourceFileMouseMove
  end
  object lblChooseTargetfile: TLabel
    Left = 0
    Top = 43
    Width = 59
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Targetfile'
    Color = clBtnFace
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    OnClick = mnuTargetfileClick
    OnMouseMove = lblChooseTargetfileMouseMove
  end
  object lblProgress: TLabel
    Left = 9
    Top = 76
    Width = 85
    Height = 13
    AutoSize = False
    Caption = 'Progress 0%'
  end
  object lblEstFinish: TLabel
    Left = 108
    Top = 76
    Width = 99
    Height = 13
    Caption = 'Estimated finish at: -'
  end
  object stxtSourceFile: TEdit
    Left = 65
    Top = 13
    Width = 455
    Height = 21
    ReadOnly = True
    TabOrder = 0
    Text = '-'
  end
  object stxtTargetFile: TEdit
    Left = 65
    Top = 40
    Width = 455
    Height = 21
    ReadOnly = True
    TabOrder = 1
    Text = '-'
  end
  object Panel1: TPanel
    Left = 8
    Top = 126
    Width = 513
    Height = 39
    BevelOuter = bvLowered
    TabOrder = 2
    object butExecute: TButton
      Left = 6
      Top = 9
      Width = 81
      Height = 23
      Caption = '&Transcode'
      Enabled = False
      TabOrder = 0
      OnClick = butExecuteClick
    end
    object butStop: TButton
      Left = 93
      Top = 9
      Width = 81
      Height = 23
      Caption = '&Abort'
      Enabled = False
      TabOrder = 1
      OnClick = butStopClick
    end
    object butPlay: TButton
      Left = 180
      Top = 9
      Width = 81
      Height = 23
      Caption = '&Play'
      Enabled = False
      TabOrder = 2
      OnClick = butPlayClick
    end
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 94
    Width = 512
    Height = 21
    Smooth = True
    Step = 1
    TabOrder = 3
  end
  object sbMsg: TStatusBar
    Left = 0
    Top = 176
    Width = 528
    Height = 25
    Panels = <>
    SimplePanel = True
    SimpleText = 'Please select a sourcefile.'
  end
  object MainMenu: TMainMenu
    Left = 468
    Top = 5
    object File1: TMenuItem
      Caption = 'File'
      object mnuOpenSourceFile: TMenuItem
        Caption = 'Open Source File'
        OnClick = mnuOpenSourceFileClick
      end
      object mnuTargetfile: TMenuItem
        Caption = 'Set Target file'
        Enabled = False
        OnClick = mnuTargetfileClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnuExitClick
      end
    end
  end
  object dlgSource: TOpenDialog
    DefaultExt = '.avi'
    Left = 468
    Top = 60
  end
  object dlgTarget: TSaveDialog
    DefaultExt = '.wav'
    Left = 470
    Top = 119
  end
end
