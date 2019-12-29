program Exptest;

uses
  Forms,
  Expmain in 'EXPMAIN.PAS' {FMain};

{$R *.RES}

begin
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
