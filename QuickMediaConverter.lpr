program QuickMediaConverter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, FormAbout, FormTools,
  uDarkStyleParams,
  uMetaDarkStyle,
  uDarkStyleSchemes;

{$R *.res}

begin
  PreferredAppMode := pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TToolsForm, ToolsForm);
  Application.Run;
end.

