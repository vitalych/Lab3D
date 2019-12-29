// Copyright (c) 2002-2019 Vitaly Chipounov
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

unit forcesfrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, EditExp,
  vector, unite_3d, objets3d;

type

D3D_ForceInfo=record
    X, Y, Z:int64; phi, theta:double;
    Objet:integer;
 end;
  TfrmForces = class(TForm)
    lstForceList: TListView;
    GroupBox1: TGroupBox;
    edtX: TEdit;
    edtY: TEdit;
    edtZ: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    lblNorme: TLabel;
    GroupBox2: TGroupBox;
    edtPhi: TEditExp;
    edtTheta: TEditExp;
    Label4: TLabel;
    Label5: TLabel;
    groupbox3: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    lstObjects: TComboBox;
    Button1: TButton;
    procedure lstForceListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure edtXChange(Sender: TObject);
    procedure edtThetaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtPhiKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lstObjectsChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { D�clarations priv�es}
  public
    { D�clarations publiques}
    force_struct:array[0..11] of D3D_ForceInfo;
    force_sapply:array[0..11] of boolean;
    procedure UpdateObjectsList;
    procedure FillForces;
    function force_apply(num:integer; silent:bool):boolean;
    function force_checkvalid(num:integer):integer;
    function force_execute(num:integer):integer;
  end;


var
  frmForces: TfrmForces;


implementation

uses physique, mainfrm;

{$R *.DFM}

procedure TfrmForces.lstForceListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
 idx:integer;
begin
  edtx.Text := item.SubItems.Strings[0];
  edty.Text := item.SubItems.Strings[1];
  edtz.Text := item.SubItems.Strings[2];
  lblNorme.caption := item.SubItems.Strings[3];

  edtPhi.text := item.SubItems.Strings[4];
  edtTheta.text := item.SubItems.Strings[5];

  if item.SubItems.Strings[6]='' then exit;
  idx := lstObjects.Items.IndexOf(item.SubItems.Strings[6]);

  if idx <> -1 then
  lstObjects.ItemIndex := idx
  else
   MessageDlg('L''objet "'+item.SubItems.Strings[6]+'"'+
   ' n''existe pas dans l''environnement actuel.'#13#10'Vous ne pouvez pas par consequent'
   +' lui appliquer de force.'#13#10'Veuillez en choisir un autre dans la liste.',
   mtWarning, [mbOK], 0);
end;

procedure TfrmForces.FormCreate(Sender: TObject);
var
 i, j:integer;
 item:TListItem;
begin
 for i:=0 to 11 do
 begin
  item:=lstForceList.Items.Add;
  item.Caption := 'F'+inttostr(i+1);
   for j:=0 to 6 do
     item.SubItems.Add('');

 force_sapply[i] := false;
 end;

 fillchar(force_struct, sizeof(force_struct), 0);

end;

procedure TfrmForces.edtXChange(Sender: TObject);
begin
 if sender is TEdit then
 begin
    with sender as TEdit do
    begin
        if text = '' then exit;

        if Name = 'edtX' then begin
         try
            strtoint(edtx.text);
            lstForceList.Selected.SubItems[0] := edtx.Text;
            force_struct[lstForceList.Selected.Index].X := strtoint(edtx.Text);
         except
            MessageDlg('Vous devez entrer une valeur entiere'+
            ' pour X', mtError, [mbOK],0);
            exit;
         end;
        end;

         if Name = 'edtY' then begin
         try
            strtoint(edtx.text);
            lstForceList.Selected.SubItems[1] := edty.Text;
            force_struct[lstForceList.Selected.Index].Y := strtoint(edty.Text);
         except
            MessageDlg('Vous devez entrer une valeur entiere'+
            ' pour Y', mtError, [mbOK],0);
            exit;
         end;
         end;

         if Name = 'edtZ' then begin
         try
            strtoint(edtx.text);
            lstForceList.Selected.SubItems[2] := edtz.Text;
            force_struct[lstForceList.Selected.Index].z := strtoint(edtz.Text);
         except
            MessageDlg('Vous devez entrer une valeur entiere'+
            ' pour Z', mtError, [mbOK],0);
            exit;
         end;
        end;

        try
         lblNorme.caption := floattostr(D3D_VecNorme(P3D(strtoint(edtx.text),
         strtoint(edty.text), strtoint(edtz.text))));
         lstForceList.Selected.SubItems[3] := lblNorme.caption;
        except
           exit;
        end;
        end;
    end;


end;

procedure TfrmForces.edtThetaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 tmp:string;
begin
 if key = ord(#13) then
 begin
   try
          tmp := TEditExp(sender).svalue;
          //if tmp<>TEditExp(sender).Text then
          TEditExp(sender).Text := tmp;
          TEditExp(sender).Tag := 0;
          lstForceList.Selected.SubItems[5] := tmp;
          force_struct[lstForceList.Selected.Index].theta := strtofloat(tmp);
       except
          MessageDlg('Vous devez entrer une expression valide.',
          mtError, [mbOK, mbHelp], 0);
          TEditExp(Sender).SetFocus;
              Exit;
       end;
 end;

end;

procedure TfrmForces.edtPhiKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

var
 tmp:string;
begin
 if key = ord(#13) then
 begin
   try
          tmp := TEditExp(sender).svalue;
          //if tmp<>TEditExp(sender).Text then
          TEditExp(sender).Text := tmp;
          TEditExp(sender).Tag := 0;
          lstForceList.Selected.SubItems[4] := tmp;
          force_struct[lstForceList.Selected.Index].phi := strtofloat(tmp);
       except
          MessageDlg('Vous devez entrer une expression valide.',
          mtError, [mbOK, mbHelp], 0);
          TEditExp(Sender).SetFocus;
              Exit;
       end;
 end;


end;


procedure TfrmForces.UpdateObjectsList;
var
 i:integer;
begin
    lstObjects.Clear;

    for i:=0 to v3d_world.object_count-1 do
      if v3d_world.objects[i].object_class = [objSphere] then
        lstObjects.Items.add(v3d_world.objects[i].object_name);
end;

procedure TfrmForces.lstObjectsChange(Sender: TObject);
begin
    lstForceList.Selected.SubItems[6] := lstObjects.Items.Strings[lstobjects.itemindex];
    force_struct[lstForceList.Selected.Index].Objet := W3D_ObjectIndex(lstForceList.Selected.SubItems[6]);
end;

procedure TfrmForces.FillForces;
var
 i:integer;
begin
 for i:=0 to 11 do
 begin
   lstForceList.items[i].SubItems[0] := inttostr(force_struct[i].X);
   lstForceList.items[i].SubItems[1] := inttostr(force_struct[i].Y);
   lstForceList.items[i].SubItems[2] := inttostr(force_struct[i].Z);
   lstForceList.items[i].SubItems[3] := floattostr(D3D_VecNorme(P3D(force_struct[i].X,
   force_struct[i].Y, force_struct[i].Z)));
   lstForceList.items[i].SubItems[4] := floattostr(force_struct[i].phi);
   lstForceList.items[i].SubItems[5] := floattostr(force_struct[i].theta);
   lstForceList.items[i].SubItems[6] := v3d_world.objects[force_struct[i].objet].object_name;
 end;


end;

function TfrmForces.force_checkvalid(num:integer):integer;
begin
  if (force_struct[num].X=0) and (force_struct[num].y=0) and (force_struct[num].z=0) then
  begin
     result := 1;
     exit;
  end;

  try
  if (v3d_world.objects[force_struct[num].Objet] = nil) then
  begin
     result := 2;
     exit;
  end;

  if (v3d_world.objects[force_struct[num].Objet].object_class <> [objSphere]) then
  begin
     result := 3;
  end;

  except
     result :=2;
     exit;
  end;


end;

function TfrmForces.force_execute(num:integer):integer;
var
 tmpvect:T3DPoint;
begin
   // while (form1.isdrawing) do;
    tmpvect := P3D(0,0,0);
    if (force_struct[num].phi = -1) and (force_struct[num].theta = -1) then
       D3D_VecAdd(v3d_world.objects[force_struct[num].Objet].physparam.s_force,
       P3D(force_struct[num].x, force_struct[num].y, force_struct[num].z))
    else begin
       D3D_VecAdd(v3d_world.objects[force_struct[num].Objet].physparam.s_force,
       P3D(force_struct[num].x, force_struct[num].y, force_struct[num].z));

         D3D_VecMult( P3D(force_struct[num].x, force_struct[num].y, force_struct[num].z),
         phys_get_force_pt_sphere(v3d_world.objects[force_struct[num].Objet]^,
         force_struct[num].phi, force_struct[num].theta), tmpvect);
         D3D_VecAdd(v3d_world.objects[force_struct[num].Objet].physparam.moment_torsion, tmpvect);

    end;
end;

function TfrmForces.force_apply(num:integer; silent:bool):boolean;
var
 i:integer;
begin
   i := force_checkvalid(num);
   result := false;
   if silent=false then
   begin
   case i of
     1: MessageDlg('Les trois composantes X, Y et Z de la force '+inttostr(num+1)+' sont nulles.'#13#10+
     'Allez dans l''applicateur de forces pour en definir une.', mtWarning, [mbOK],0);


     2: MessageDlg('L''objet sur lequel la force '+inttostr(num+1)+' doit etre appliquee n''existe pas.'#13#10+
     'Allez dans l''applicateur de forces pour en definir un.', mtWarning, [mbOK],0);

     3: MessageDlg('L''objet sur lequel la force '+inttostr(num+1)+' s''applique n''est pas une sphere.'#13#10+
     'Allez dans l''applicateur de forces pour corriger cela.', mtWarning, [mbOK],0);
   else
      // force_execute(num);
       result := true;
   end;
   end
   else
   begin
      if (i<>1) and (i<>2) and (i<>3) then begin
        force_execute(num);
        result := true;
      end;
   end;
end;


procedure TfrmForces.Button1Click(Sender: TObject);
begin
    if lstForceList.Selected <> nil then begin

      force_struct[lstForceList.Selected.Index].theta := -1;
      force_struct[lstForceList.Selected.Index].phi := -1;
      lstForceList.Selected.subitems[4] := '-1';
      lstForceList.Selected.subitems[5] := '-1';

    end;
end;

end.
