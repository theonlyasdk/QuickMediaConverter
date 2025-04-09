unit FormAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, lclintf;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BtnClose: TButton;
    Image1: TImage;
    LblGithub: TLabel;
    LblTitle: TLabel;
    LicenseBox: TMemo;
    Panel1: TPanel;
    procedure BtnCloseClick(Sender: TObject);
    procedure LblGithubClick(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.LblGithubClick(Sender: TObject);
begin
  OpenURL('https://github.com/theonlyasdk/QuickMediaConverter');
end;

end.

