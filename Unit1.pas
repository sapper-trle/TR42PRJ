unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdActns, System.Actions,
  Vcl.ActnList, Vcl.Menus, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Samples.Gauges, Vcl.ExtDlgs,
  unit2, // TTRLevel type
  unit3; // TTRProject type

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
    FileOpen2: TFileOpen;
    Button1: TButton;
    Panel1: TPanel;
    Gauge1: TGauge;
    SaveDialog1: TSaveDialog;
    PopupMenu1: TPopupMenu;
    SaveTGA1: TMenuItem;
    Cancel1: TMenuItem;
    Button2: TButton;
    UnloadPRJ: TAction;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FileSaveAs1Update(Sender: TObject);
    procedure FileSaveAs1BeforeExecute(Sender: TObject);
    procedure FileOpen2Accept(Sender: TObject);
    procedure HandleDrops(var Msg: tagMSG; var Handled: Boolean);
    procedure FileOpen2Update(Sender: TObject);
    procedure SaveTGA1Click(Sender: TObject);
    procedure Cancel1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure UnloadPRJUpdate(Sender: TObject);
    procedure UnloadPRJExecute(Sender: TObject);
  private
    { Private declarations }
    const program_version = 0.7;
  public
    { Public declarations }
    l: TTRLevel;
    aktrekker:TAktrekkerPRJ;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.UITypes,
  Winapi.ShellAPI, // routines to handle files dropped on form
  Imaging, ImagingTypes, ImagingComponents; // Vampyre Imaging Library for .TGA support

procedure TForm1.FileOpen1Accept(Sender: TObject);
var
  r: uint8;
begin
  Image1.Canvas.Brush.Color:=clRed;
  Image1.Canvas.Brush.Style:=bsDiagCross;
  if LowerCase(ExtractFileExt(FileOpen1.Dialog.FileName)) <> '.tr4' then
  begin
    MessageDlg('Not a TR4 file!', mtError, [mbOK], 0);
    Exit;
  end;
  try
    if Assigned(l) then FreeAndNil(l);
    if Assigned(aktrekker) then FreeAndNil(aktrekker); //unload TR2PRJ project
    l := TTRLevel.Create;
    r := l.Load(FileOpen1.Dialog.FileName,Gauge1);
  except
    on EReadError do r:=4;
  end;
  Gauge1.Progress:=0;
  case r of
    0:;
    1:
      begin
        MessageDlg('File does not exist!', mtError, [mbOK], 0);
        FreeAndNil(l);
        Image1.Picture.Bitmap.SetSize(256,256);
        Image1.Canvas.FillRect(Image1.ClientRect);
        Exit;
      end;
    2:
      begin
        MessageDlg('TR4 signature not found!', mtError, [mbOK], 0);
        Image1.Picture.Bitmap.SetSize(256,256);
        Image1.Canvas.FillRect(Image1.ClientRect);
        FreeAndNil(l);
        Exit;
      end;
    3:
      begin
        MessageDlg('Encrypted TR4 file!', mtError, [mbOK], 0);
        FreeAndNil(l);
        Image1.Picture.Bitmap.SetSize(256,256);
        Image1.Canvas.FillRect(Image1.ClientRect);
        Exit;
      end;
    4:
      begin
        MessageDlg('Error reading TR4!', mtError, [mbOK], 0);
        FreeAndNil(l);
        Image1.Picture.Bitmap.SetSize(256,256);
        Image1.Canvas.FillRect(Image1.ClientRect);
        Exit;
      end;
  end; //case
  Image1.Picture.Bitmap:=l.bmp;
end;

procedure TForm1.FileOpen2Accept(Sender: TObject);
var
  r:UInt8;
  p:TTRProject;
begin
  if LowerCase(ExtractFileExt(FileOpen2.Dialog.FileName))<>'.prj' then
  begin
    MessageDlg('Not a PRJ file!',mtError, [mbOK],0);
    Exit;
  end;
  try
    if Assigned(aktrekker) then FreeAndNil(aktrekker);
    aktrekker:=TAktrekkerPRJ.Create(0,300);
    r:= aktrekker.Load(FileOpen2.Dialog.FileName);
  except
    on EReadError do r:=2;
  end;
  if r=0 then
  begin
    p := l.ConvertToPRJ('',False);
    if not p.isCompatible(aktrekker) then r:=3;
    p.Free;
  end;
  case r of
  0:;
  1:
    begin
      MessageDlg('PRJ signature not found!',mtError, [mbOK],0);
      FreeAndNil(aktrekker);
      Exit;
    end;
  2:
    begin
      MessageDlg('Error reading PRJ!',mtError, [mbOK],0);
      FreeAndNil(aktrekker);
      Exit;
    end;
  3:
    begin
      MessageDlg('Incompatible PRJ!',mtError, [mbOK],0);
      FreeAndNil(aktrekker);
      Exit;
    end;
  end;
end;

procedure TForm1.FileOpen2Update(Sender: TObject);
begin
  Button1.Enabled := Assigned(l);
end;

procedure TForm1.FileSaveAs1Accept(Sender: TObject);
var
  p: TTRProject;
begin
  p := l.ConvertToPRJ(FileSaveAs1.Dialog.FileName);
  if Assigned(aktrekker) and p.isCompatible(aktrekker) then
  begin
    if CheckBox1.Checked then p.CopyDoorsFromPRJ(aktrekker);
    if CheckBox2.Checked then p.CopyTexFromPRJ(aktrekker);
    if CheckBox3.Checked then p.CopyLightsFromPRJ(aktrekker);
  end;
  p.Save(FileSaveAs1.Dialog.FileName);
  p.Free;
//  Halt(0);
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
  FileSaveAs1.Enabled := Assigned(l);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:=True;
  FileOpen1.Dialog.FileName:='mix1w';
{$ENDIF}
  Image1.Canvas.Brush.Color:=clRed;
  Image1.Canvas.Brush.Style:=bsDiagCross;
  Image1.Canvas.FillRect(Image1.ClientRect);
  DragAcceptFiles(Self.Handle,True);
  Application.OnMessage:=HandleDrops;
  Caption := Format('TR4 to PRJ  v%.1f',[program_version]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  l.Free;
  aktrekker.Free;
  DragAcceptFiles(Self.Handle, False);
end;

procedure TForm1.HandleDrops(var Msg: tagMSG; var Handled: Boolean);
var
  filename : string;
  droppedFileCount : Integer;
  FileNameLength : Integer;

begin
  Handled := Msg.Message=WM_DROPFILES;
  if not Handled then Exit;
  try
    DroppedFileCount := DragQueryFile(Msg.wParam, $FFFFFFFF, nil, 0);
    FileNameLength := DragQueryFile(Msg.wParam, 0, nil, 0);
    SetLength(FileName, FileNameLength);
    DragQueryFile(Msg.Wparam,0,PChar(filename),FileNameLength+1);
    if LowerCase(ExtractFileExt(FileName)) = '.tr4' then
    begin
      FileOpen1.Dialog.FileName := filename;
      FileOpen1Accept(nil);
    end
    else if (LowerCase(ExtractFileExt(FileName)) = '.prj') and (Assigned(l))then
    begin
      FileOpen2.Dialog.FileName := filename;
      FileOpen2Accept(nil);
    end
    else if (LowerCase(ExtractFileExt(FileName)) = '.prj') and (not Assigned(l))then
    begin
      MessageDlg('TR4 file not loaded.',mtError,[mbOK],0);
    end
    else
    begin
      MessageDlg('Unknown filetype.',mtError,[mbOK],0);
    end;
  finally
    DragFinish (Msg.WParam);
  end;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  PopupMenu1.Items[0].Enabled:= Assigned(l);
end;

procedure TForm1.SaveTGA1Click(Sender: TObject);
var
  img:TImageData;
  s:string;
begin
  if not Assigned(l) then Exit;
  if SaveDialog1.Execute then
  begin
    s:=SaveDialog1.FileName;
    InitImage(img);
    img.Format := ifR8G8B8;
    ConvertBitmapToData(l.bmp,img); // make sure bitmap's pixelformat was set to 24!!
    SaveImageToFile(s,img);
    FreeImage(img);
  end;
end;

procedure TForm1.UnloadPRJExecute(Sender: TObject);
begin
  if Assigned(aktrekker) then FreeAndNil(aktrekker);
end;

procedure TForm1.UnloadPRJUpdate(Sender: TObject);
begin
  Button2.Enabled := Assigned(aktrekker);
  CheckBox1.Enabled := Assigned(aktrekker);
  CheckBox2.Enabled := Assigned(aktrekker);
  CheckBox3.Enabled := Assigned(aktrekker);
end;

procedure TForm1.Cancel1Click(Sender: TObject);
begin
  PopupMenu1.CloseMenu;
end;

end.

