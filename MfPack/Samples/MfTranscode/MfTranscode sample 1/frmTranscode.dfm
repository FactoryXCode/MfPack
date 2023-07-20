object frmTranscoder: TfrmTranscoder
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'MfTranscoder'
  ClientHeight = 151
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lblSourceFile: TLabel
    Left = 0
    Top = 16
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Sourcefile'
  end
  object Label2: TLabel
    Left = 0
    Top = 43
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Targetfile'
  end
  object lblProgress: TLabel
    Left = 9
    Top = 67
    Width = 85
    Height = 13
    AutoSize = False
    Caption = 'Progress 100%'
  end
  object lblEstFinish: TLabel
    Left = 108
    Top = 67
    Width = 99
    Height = 13
    Caption = 'Estimated finish at: -'
  end
  object edSourceFile: TEdit
    Left = 62
    Top = 13
    Width = 459
    Height = 21
    TabOrder = 0
    Text = 'none'
  end
  object edTargetFile: TEdit
    Left = 62
    Top = 40
    Width = 459
    Height = 21
    TabOrder = 1
    Text = 'none'
  end
  object Panel1: TPanel
    Left = 0
    Top = 112
    Width = 528
    Height = 39
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 2
    object butExecute: TButton
      Left = 6
      Top = 9
      Width = 81
      Height = 23
      Caption = 'Transcode'
      Enabled = False
      TabOrder = 0
      OnClick = butExecuteClick
    end
    object butStop: TButton
      Left = 93
      Top = 9
      Width = 81
      Height = 23
      Caption = 'Abort'
      Enabled = False
      TabOrder = 1
      OnClick = butStopClick
    end
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 85
    Width = 512
    Height = 21
    Smooth = True
    Step = 1
    TabOrder = 3
  end
  object MainMenu1: TMainMenu
    Left = 466
    Top = 12
    object File1: TMenuItem
      Caption = 'File'
      object mnuOpenSourceFile: TMenuItem
        Caption = 'Open Source File'
        OnClick = mnuOpenSourceFileClick
      end
      object mnuTargetfile: TMenuItem
        Caption = 'Set Target file'
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
  object OpenDialog1: TOpenDialog
    Filter = 'All files|*.*'
    Left = 466
    Top = 59
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.wmf'
    Filter = 'Windows Movie (wmv)|.wmv|Windows Audio (wma)|.wma'
    Left = 466
    Top = 106
  end
end
