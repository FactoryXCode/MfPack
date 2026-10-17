// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Module: dlgFxServePassword.pas
// Kind: Pascal Unit
//
//==============================================================================
unit dlgFxServePassword;

interface

uses
  {System}
  System.Classes,
  {Vcl}
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  TFxServePasswordDlg = class(TForm)
    lblPrompt: TLabel;
    edtPassword: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
  end;

implementation

{$R *.dfm}

end.
