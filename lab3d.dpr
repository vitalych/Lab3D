program lab3d;

uses
  Forms,
  mainfrm in 'mainfrm.pas' {Form1},
  about in 'about.pas' {AboutBox},
  unite_3d in 'unite_3d.pas',
  objets3d in 'objets3d.pas',
  matrix in 'matrix.pas',
  fichiers in 'fichiers.pas',
  clipping in 'clipping.pas',
  objinsp in 'objinsp.pas' {frmObjInspector},
  texture in 'texture.pas',
  physique in 'physique.pas',
  vector in 'vector.pas',
  forcesfrm in 'forcesfrm.pas' {frmForces},
  EditExp in 'EditExp\EDITEXP.PAS';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Lab3D';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TfrmObjInspector, frmObjInspector);
  Application.CreateForm(TfrmForces, frmForces);
  Application.Run;
end.
