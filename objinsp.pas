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

unit objinsp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, unite_3d, Grids,
 ImgList,  ComCtrls,
  editexp, vector, matrix;

type
  TfrmObjInspector = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnDeleteObj: TSpeedButton;

    ImageList1: TImageList;

    ScrollBox1: TScrollBox;
    btnSetCond: TSpeedButton;
    btnReturnToCond: TSpeedButton;
    cbList: TComboBox;
    procedure cbListChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditsOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditsOnExit(Sender:TObject);
    procedure btnDeleteObjClick(Sender: TObject);
    procedure btnSetCondClick(Sender: TObject);
    procedure btnReturnToCondClick(Sender: TObject);
  private
    { D�clarations priv�es}
    first_time:integer;

    //propertygrid1:TStringList;
  public
    { D�clarations publiques}
    obj_count:integer;
    propertygrid1:TStringList;
    procedure objinsp_reset;
    procedure AddObject(obj:TObject_t);
    procedure UpdateInspector(index:integer; obj:TObject_t);

    procedure UpdateObjectProperties(obj:integer);
    procedure DestroyProps;
    procedure CreateProps;
    procedure CompactWorld(num:integer);
  end;

var
  frmObjInspector: TfrmObjInspector;
  Labels:array[0..15] of TLabel;
  Edits:array[0..15] of TEditExp;
  props_exist:boolean=false;

implementation

uses mainfrm, forcesfrm;

{$R *.DFM}



procedure TfrmObjInspector.objinsp_reset;
var
 i:integer;
begin
 first_time := 1;
 obj_count := 0;
 cbList.Items.Clear;
 cbList.Items.Add('Aucun objet disponible=1');
 cbList.itemindex:=0;
 cbList.update;
 DestroyProps;

end;

procedure TfrmObjInspector.AddObject(obj:TObject_t);
var
 i:integer;
begin
    if props_exist = false then begin
      cbList.items.Clear;
      cbList.Update;
      CreateProps;
    end;

    i:=cbList.items.add(obj.object_name+'=0');
    cbList.ItemIndex := i;
    cbList.Update;

    UpdateInspector(i, obj);

    inc(obj_count);
    frmForces.UpdateObjectsList;
end;

procedure TfrmObjInspector.UpdateInspector(index:integer; obj:TObject_t);
begin

     with propertygrid1 do
     begin
         if obj.object_class = [objCube] then
             edits[propertygrid1.IndexOf('Type')].text := 'Cube';
         if obj.object_class = [objplane] then
             edits[propertygrid1.IndexOf('Type')].text := 'Surface';
         if obj.object_class = [objsphere] then
             edits[propertygrid1.IndexOf('Type')].text := 'Sphere';

         edits[propertygrid1.IndexOf('Type')].ReadOnly := true;
         edits[propertygrid1.IndexOf('Type')].color := clLime;

         edits[propertygrid1.IndexOf('Nom')].text := Obj.object_name;
         edits[propertygrid1.IndexOf('Nom')].Tag := 0;
         edits[propertygrid1.IndexOf('Echelle_X')].text := floattostrf(obj.Scaling.x, ffFixed, 4, 0);
         edits[propertygrid1.IndexOf('Echelle_Y')].text := floattostr(obj.Scaling.y);
         edits[propertygrid1.IndexOf('Echelle_Z')].text := floattostr(obj.Scaling.z);

         edits[propertygrid1.IndexOf('Position_X')].text := floattostr(obj.Position.x);
         edits[propertygrid1.IndexOf('Position_Y')].text := floattostr(obj.Position.y);
         edits[propertygrid1.IndexOf('Position_Z')].text := floattostr(obj.Position.z);

         edits[propertygrid1.IndexOf('Angle_A')].text := floattostr(obj.Angle.x);
         edits[propertygrid1.IndexOf('Angle_B')].text := floattostr(obj.Angle.y);
         edits[propertygrid1.IndexOf('Angle_G')].text := floattostr(obj.Angle.z);
         edits[propertygrid1.IndexOf('Frottements')].text := floattostr(obj.physparam.frottements);
         edits[propertygrid1.IndexOf('Masse')].text := floattostr(obj.physparam.masse);
    end;
end;



procedure TfrmObjInspector.cbListChange(Sender: TObject);
begin
 if obj_count > 0 then
    UpdateInspector(cbList.ItemIndex, v3d_world.objects[cbList.ItemIndex]^);
end;


procedure TfrmObjInspector.UpdateObjectProperties(obj:integer);
begin
  with v3d_world.objects[obj]^ do
  begin
       //try
        Scaling.x := strtofloat(edits[PropertyGrid1.IndexOf('Echelle_X')].Text);
        Scaling.y := strtofloat(edits[PropertyGrid1.IndexOf('Echelle_Y')].Text);
        Scaling.z := strtofloat(edits[PropertyGrid1.IndexOf('Echelle_Z')].Text);

        Position.x := strtofloat(edits[PropertyGrid1.IndexOf('Position_X')].Text);
        Position.y := strtofloat(edits[PropertyGrid1.IndexOf('Position_Y')].Text);
        Position.z := strtofloat(edits[PropertyGrid1.IndexOf('Position_Z')].Text);

        Angle.x := strtofloat(edits[PropertyGrid1.IndexOf('Angle_A')].Text);
        Angle.y := strtofloat(edits[PropertyGrid1.IndexOf('Angle_B')].Text);
        Angle.z := strtofloat(edits[PropertyGrid1.IndexOf('Angle_G')].Text);


        physparam.acceleration_angulaire := P3D(0,0,0);
        physparam.vitesse_angulaire := P3D(0,0,0);


        physparam.s_force := P3D(0,0,0);
        physparam.vitesse := P3D(0,0,0);
        physparam.acceleration := P3D(0,0,0);

        physparam.frottements := strtofloat(edits[PropertyGrid1.IndexOf('Frottements')].Text);
         physparam.masse := strtofloat(edits[PropertyGrid1.IndexOf('Masse')].Text);
       //except
       //   MessageDlg('Vous devez entrer un nombre entier ou decimal valide.'#13#10+
       //   'Utilisez la virgule a la place du point!', mtError, [mbOK, mbHelp], 10);
       //end;
  end;
end;

procedure TfrmObjInspector.FormCreate(Sender: TObject);
begin
   objinsp_reset;
end;

procedure TfrmObjInspector.DestroyProps;
var
 i:integer;
begin
 if props_exist = false then exit;

 for i:=0 to propertygrid1.Count-1 do
 begin
   edits[i].free;
   edits[i] := nil;
   labels[i].free;
   labels[i] := nil;
 end;

 propertygrid1.Free;
 props_exist := false;

end;


procedure TfrmObjInspector.CreateProps;
var
 i:integer;
begin

 if obj_count > 0 then objinsp_reset;

 propertygrid1 := TStringList.Create;
 PropertyGrid1.Add('Nom');

 PropertyGrid1.Add('Type');
 PropertyGrid1.Add('Echelle_X');
 PropertyGrid1.Add('Echelle_Y');
 PropertyGrid1.Add('Echelle_Z');
 PropertyGrid1.Add('Position_X');
 PropertyGrid1.Add('Position_Y');
 PropertyGrid1.Add('Position_Z');
 PropertyGrid1.Add('Angle_A');
 PropertyGrid1.Add('Angle_B');
 PropertyGrid1.Add('Angle_G');
 PropertyGrid1.Add('Frottements');
 PropertyGrid1.Add('Masse');


 for i:=0 to propertygrid1.Count-1 do
 begin
      labels[i] := TLabel.Create(ScrollBox1);
      edits[i] := TEditExp.Create(ScrollBox1);


      ScrollBox1.InsertControl(labels[i]);
      ScrollBox1.InsertControl(edits[i]);


      labels[i].Caption := propertygrid1.Strings[i]+':';
      labels[i].Top := i*20+5;
      labels[i].Transparent := true;
      labels[i].font.Color := clLime;

      edits[i].Top := i*20;
      edits[i].Left := 60;
      edits[i].Name := propertygrid1.Strings[i];
      edits[i].Text := '';
      edits[i].OnKeyDown := EditsOnKeyDown;
      edits[i].OnExit := EditsOnExit;
      edits[i].width := 80;
      edits[i].Tag := 0;


 end;
 props_exist := true;
end;

procedure TfrmObjInspector.EditsOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
 i:integer;
 tmp:String;
begin
   if sender is TEditExp then begin
    if Key <> ord(#13) then TEditExp(Sender).Tag := 1;

    if Key=ord(#13) then
    begin
     if pos('Equation',TEditExp(sender).Name) = 0 then
     begin
       try
          tmp := TEditExp(sender).svalue;
          //if tmp<>TEditExp(sender).Text then
          TEditExp(sender).Text := tmp;
          TEditExp(sender).Tag := 0;
       except
             MessageDlg('Vous devez entrer une expression valide.',
          mtError, [mbOK, mbHelp], 0);
          TEditExp(Sender).SetFocus;
              Exit;
       end;
     end;
     //MessageDlg(TEdit(Sender).Name, mtInformation, [mbOK], 0);
     if (TEditExp(Sender).Name='Nom') and (TEditExp(sender).tag=1) then
     begin
        for i:=0 to cbList.Items.count-1 do
        begin
           if TEditExp(Sender).Text = cbList.Items.Names[cbList.ItemIndex] then
           begin
              MessageDlg('Un objet avec ce nom existe deja.'#13#10+
              'Vous devez choisir un nom unique!', mtError, [mbOK], 0);
              TEditExp(Sender).SetFocus;
              Exit;
           end;
        end;
     end else
      begin
        try
           UpdateObjectProperties(cbList.ItemIndex);
           TEditExp(sender).Tag := 0;
        except
             MessageDlg('Vous devez entrer un nombre entier ou decimal valide.'#13#10+
             'Utilisez la virgule a la place du point!', mtError, [mbOK, mbHelp], 10);
             TEditExp(Sender).SetFocus;
             exit;
        end;
      end;

    end;
   end;
end;

procedure TfrmObjInspector.EditsOnExit(Sender:TObject);
var
 key:word;
begin
     if sender is TEditExp then
       if TEditExp(sender).tag<>0 then begin
          key := ord(#13);
          EditsOnKeyDown(sender, key, [ssShift]);
       end;
end;

procedure TfrmObjInspector.CompactWorld(num:integer);
var
 i:integer;
begin
 for i:=num to v3d_world.object_count-2 do
 begin
   v3d_world.objects[i] := v3d_world.objects[i+1];
 end;
   v3d_world.objects[v3d_world.object_count-1] := nil;
   dec(v3d_world.object_count);
end;

procedure TfrmObjInspector.btnDeleteObjClick(Sender: TObject);
var
 i, j:integer;
begin
     if v3d_world.object_count = 0 then
      begin
        MessageDlg('Aucun objet a supprimer !', mtError,
        [mbOK],0);
        exit;
      end;
     i := cbList.itemindex;

     form1.DXTimer1.Enabled := false;
     while form1.isdrawing = true do ;
     for j:=0 to v3d_world.objects[i].PolygonCount-1 do
      begin
           fillchar(v3d_world.objects[i].Polygon[j].Vertex^,
          v3d_world.objects[i].polygon[j].Count*4,0);
          freemem(v3d_world.objects[i].Polygon[j].Vertex,
          v3d_world.objects[i].polygon[j].Count*4);
      end;

      freemem(v3d_world.objects[i].Vertex,  sizeof(TVertex)*v3d_world.objects[i].VertexCount);

      fillchar(v3d_world.objects[i]^, sizeof(TObject_t), 0);
      freemem(v3d_world.objects[i], sizeof(TObject_t));

      CompactWorld(i);

      cbList.Items.Delete(i);
      if cbList.Items.Count = 0 then
      begin
         DestroyProps;
         objinsp_reset;
      end;
      cbList.itemindex := 0;
      cbList.Update;

      frmForces.UpdateObjectsList;
      form1.DXTimer1.enabled := true;
end;

procedure TfrmObjInspector.btnSetCondClick(Sender: TObject);
var
 i:integer;
begin
 for i:=0 to v3d_world.object_count-1 do
 begin
  with v3d_world.objects[i]^ do
  begin
     initial_cond.physparams := physparam;
     initial_cond.Angle := angle;
     initial_cond.Position := Position;
  end;
  end;
end;

procedure TfrmObjInspector.btnReturnToCondClick(Sender: TObject);
var
 i:integer;
begin
 for i:=0 to v3d_world.object_count-1 do
 begin
 with v3d_world.objects[i]^ do
  begin
     physparam := initial_cond.physparams;
     angle := initial_cond.Angle;
     D3D_IdentityMatrix(angle_mat);
     position := initial_cond.Position;
  end;
  end;

  form1.SetFocus;
end;

end.
