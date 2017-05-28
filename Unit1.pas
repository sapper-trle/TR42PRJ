unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdActns, System.Actions,
  Vcl.ActnList, Vcl.Menus, Vcl.ComCtrls, unit2, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    FileSaveAs1: TFileSaveAs;
    SaveAs1: TMenuItem;
    Image1: TImage;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FileSaveAs1Update(Sender: TObject);
    procedure FileSaveAs1BeforeExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    l: TTRLevel;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.UITypes, unit3;

procedure TForm1.FileOpen1Accept(Sender: TObject);
var
  r: uint8;
begin
  if LowerCase(ExtractFileExt(FileOpen1.Dialog.FileName)) <> '.tr4' then
  begin
    MessageDlg('Not a TR4 file!', mtError, [mbOK], 0);
    Exit;
  end;

  r := l.Load(FileOpen1.Dialog.FileName);
  case r of
    1:
      begin
        MessageDlg('File does not exist!', mtError, [mbOK], 0);
        Exit;
      end;
    2:
      begin
        MessageDlg('Not a TR4 file!', mtError, [mbOK], 0);
        Exit;
      end;
    3:
      begin
        MessageDlg('Encrypted TR4 file!', mtError, [mbOK], 0);
        Exit;
      end;
  end; //case
  Image1.Picture.Bitmap:=l.bmp;
end;

procedure TForm1.FileSaveAs1Accept(Sender: TObject);
var
  p: TTRProject;
begin
  p := l.ConvertToPRJ(FileSaveAs1.Dialog.FileName);
  p.Save(FileSaveAs1.Dialog.FileName);
  p.Free;
  // Halt(0);
end;

procedure TForm1.FileSaveAs1BeforeExecute(Sender: TObject);
var
  s : string;
begin
  s := ExtractFileName(FileOpen1.Dialog.FileName);
  s := ChangeFileExt(s,'');
  FileSaveAs1.Dialog.FileName := s;
end;

procedure TForm1.FileSaveAs1Update(Sender: TObject);
begin
  FileSaveAs1.Enabled := l.file_version > 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  l := TTRLevel.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  l.Free;
end;

end.

