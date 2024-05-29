unit dlgMft;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.ExtCtrls,
  VideoMftClass;

type
  TMftDlg = class(TForm)
    lblMftSupportedInput: TLabel;
    lblSupportedOutput: TLabel;
    butOk: TButton;
    Button2: TButton;
    sgMftInput: TStringGrid;
    stxtBitRate: TStaticText;
    StaticText1: TStaticText;
    edInputMajorType: TEdit;
    edInputSubType: TEdit;
    sgMftOutput: TStringGrid;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    edOutputMajorType: TEdit;
    edOutPutSubType: TEdit;
    Bevel1: TBevel;
    procedure FormShow(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure sgMftInputClick(Sender: TObject);
    procedure sgMftOutputClick(Sender: TObject);
  private
    { Private declarations }
    procedure Populate();

  public
    { Public declarations }
    iSelectedInput, iSelectedOutput: Integer;

  end;

var
  MftDlg: TMftDlg;

implementation

{$R *.dfm}


procedure TMftDlg.butOkClick(Sender: TObject);
begin
  iSelectedInput := StrToInt(sgMftInput.Cells[2, sgMftInput.Row]);
  iSelectedOutput := StrToInt(sgMftOutput.Cells[2, sgMftOutput.Row]);
  ModalResult := mrOk;
end;


procedure TMftDlg.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;


procedure TMftDlg.FormShow(Sender: TObject);
begin
  sgMftInput.Row := 0;
  iSelectedInput := 0;
  iSelectedOutput := 0;
  Caption := FVideoMft.VideoFormats.pwcStr;
  Populate();
end;


procedure TMftDlg.Populate();
var
  i: Integer;

begin
  // Clear the grid
  for i := 0 to sgMftInput.ColCount - 1 do
    sgMftInput.Cols[i].Clear;
  sgMftInput.RowCount := 1;

  // We need the following arrayvalues to show in the gridcells.

  // initialize the grid
  sgMftInput.ColCount := 3;
  sgMftInput.RowCount := 1;

  sgMftInput.ColWidths[0] := 260; // Major type
  sgMftInput.ColWidths[1] := 260; // Sub type
  sgMftInput.ColWidths[2] := -1;  // Hide last column

  // List.
  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgMftInput.BeginUpdate();
    {$IFEND}
  {$ENDIF}

  sgMftInput.RowCount := Length(FVideoMft.VideoFormats.aInputTypes);

  for i := 0 to Length(FVideoMft.VideoFormats.aInputTypes) -1 do
    begin
      // show readable types.
      sgMftInput.Cells[0, i] := WideCharToString(FVideoMft.VideoFormats.aInputTypes[i].majorTypeDescr);
      sgMftInput.Cells[1, i] := WideCharToString(FVideoMft.VideoFormats.aInputTypes[i].GuidName);
      // invisible
      sgMftInput.Cells[2, i] := IntToStr(i); // index!
    end;

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgMftInput.EndUpdate();
       sgMftInput.BeginUpdate();
    {$IFEND}
  {$ENDIF}

  sgMftOutput.RowCount := Length(FVideoMft.VideoFormats.aOutputTypes);

  for i := 0 to Length(FVideoMft.VideoFormats.aOutputTypes) -1 do
    begin
      // show readable types.
      sgMftOutput.Cells[0, i] := WideCharToString(FVideoMft.VideoFormats.aOutputTypes[i].majorTypeDescr);
      sgMftOutput.Cells[1, i] := WideCharToString(FVideoMft.VideoFormats.aOutputTypes[i].GuidName);
      // invisible
      sgMftOutput.Cells[2, i] := IntToStr(i); // index!
    end;

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgMftInput.EndUpdate();
    {$IFEND}
  {$ENDIF}
end;


procedure TMftDlg.sgMftInputClick(Sender: TObject);
begin
  edInputMajorType.Text := sgMftInput.Cells[0, sgMftInput.Row];
  edInputSubType.Text := sgMftInput.Cells[1, sgMftInput.Row];
end;


procedure TMftDlg.sgMftOutputClick(Sender: TObject);
begin
  edOutputMajorType.Text := sgMftOutput.Cells[0, sgMftOutput.Row];
  edOutputSubType.Text := sgMftOutput.Cells[1, sgMftOutput.Row];
end;

end.
