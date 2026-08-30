{
        File: frmNewChapter.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmNewChapter;

{$mode delphi}

interface

uses
  Classes, Forms, StdCtrls, Buttons, DefaultTranslator, ExtCtrls;

type

  TNewChapterResult = (ncrDownload, ncrQueue, ncrCancel);

  { TNewChapter }

  TNewChapter = class(TForm)
    btDownload: TBitBtn;
    btCancel: TBitBtn;
    btQueue: TBitBtn;
    lbNotification: TLabel;
    mmMemo  : TMemo;
    pnBottom: TPanel;
    procedure btCancelClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btQueueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    FFormResult: TNewChapterResult;
  private
    { private declarations }
  public
    { public declarations }
    property   FormResult: TNewChapterResult read FFormResult;
  end;

implementation

{$R *.lfm}

{ TNewChapter }

procedure TNewChapter.FormCreate(Sender: TObject);
begin
  FFormResult := ncrCancel;
end;

procedure TNewChapter.btDownloadClick(Sender: TObject);
begin
  FFormResult:= ncrDownload;
end;

procedure TNewChapter.btQueueClick(Sender: TObject);
begin
  FFormResult:= ncrQueue;
end;

procedure TNewChapter.btCancelClick(Sender: TObject);
begin
  FFormResult:= ncrCancel;
end;

end.

