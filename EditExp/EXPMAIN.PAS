unit Expmain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, EditExp, ExtCtrls, Grids;

type
  TFMain = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    StringGrid1: TStringGrid;
    Edit3: TEdit;
    Label4: TLabel;
    EditExp1: TEditExp;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

{$R *.DFM}

procedure TFMain.Button1Click(Sender: TObject);
begin
     EditExp1.UseVar:=CheckBox1.checked;

     Edit1.Text:=EditExp1.SValue;

     Edit2.Text:=EditExp1.GetError;

     Edit3.Text:=EditExp1.Exp;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
     with StringGrid1 do
     begin
          Cells[1,0]:='VAR';
          Cells[2,0]:='VALUE';
     end;
end;

end.
