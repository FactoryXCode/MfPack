unit dlgTimedTextLanguages;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  {Vcl}
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  {Project}
  MfPlayerClassX,
  TimedTextClass,
  FloatingFrm,
  LangTags,
  MfPCXConstants;

type
  TDlgTimedTextLanguages = class(TForm)
    butOk: TButton;
    butCancel: TButton;
    lvTTxtLang: TListView;
    procedure FormShow(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure lvTTxtLangMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  protected
    procedure WMLButtonDown(var Msg: TWMLBUTTONDOWN); message WM_LBUTTONDOWN;

  private
    { Private declarations }

  public
    { Public declarations }

  end;

var
  dlgTimedTextLang: TDlgTimedTextLanguages;

implementation

{$R *.dfm}

uses frmMfPlayer;


procedure TDlgTimedTextLanguages.WMLButtonDown(var Msg: TWMLBUTTONDOWN);
begin
  if lvTTxtLang.GetItemAt(msg.XPos, msg.YPos) <> Nil then
    inherited;
end;


procedure TDlgTimedTextLanguages.butOkClick(Sender: TObject);
begin
  dlgTimedTextLang.Close();
end;


procedure TDlgTimedTextLanguages.FormShow(Sender: TObject);
var
  I: Integer;
  lItem: TListItem;

begin

  lvTTxtLang.Items.Clear;

  if (MfPlayerX.m_hwndFloatingForm = 0) then
    Exit;
  // Read the tag again, because a user may have added or deleted a timedtextfile.
  SetLength(pc_LanguageTags.TimedTxtPropsArray, 0);
  pc_LanguageTags.TimedTxtPropsArray := pc_LanguageTags.ReadFileTags(MfPlayerX.MediaFileName,
                                                                     FloatingForm.PreferredLanguage,
                                                                     0,
                                                                     EXTSUBRIP);

  if Length(pc_LanguageTags.TimedTxtPropsArray) = 0 then
    Exit;

  for I := 0 to Length(pc_LanguageTags.TimedTxtPropsArray) - 1 do
   begin
     // Add a new line
     lItem := lvTTxtLang.Items.Add;
     lItem.Checked := pc_LanguageTags.TimedTxtPropsArray[I].bActiveFile;
     lItem.Caption := pc_LanguageTags.TimedTxtPropsArray[I].sLanguageTag;
     lItem.SubItems.Add(pc_LanguageTags.TimedTxtPropsArray[i].sFriendlyLanguageName);
   end;
end;


procedure TDlgTimedTextLanguages.lvTTxtLangMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ht: THitTests;
  Item: TListItem;
  I: Integer;

begin
  ht := (Sender as TCustomListView).GetHitTestInfoAt(X, Y);

  if (htOnStateIcon in ht) then
    begin
      Item := (Sender as TCustomListView).GetItemAt(X, Y);

      if Assigned(Item) then
        begin
          for I := 0 to lvTTxtLang.Items.Count - 1 do
            if lvTTxtLang.Items[i].Checked and (Item.Index <> I) then
              lvTTxtLang.Items[i].Checked := false;

          // Set the new preffered language
          pc_LanguageTags.TimedTxtPropsArray[Item.Index].bActiveFile := true;
          MfPlayerX.SubtitleLanguage := pc_LanguageTags.TimedTxtPropsArray[Item.Index].sLanguageTag;
          if (MfPlayerX.m_hwndFloatingForm > 0) then
            begin
              fTimedText.TimedTextFile := pc_LanguageTags.TimedTxtPropsArray[Item.Index].sFile;
              FloatingForm.PreferredLanguage := pc_LanguageTags.TimedTxtPropsArray[Item.Index].sLanguageTag;
            end;
        end;
    end;
end;

end.
