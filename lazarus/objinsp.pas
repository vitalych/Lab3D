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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, l3d_world, Math;

type

  { TObjectInspector }
  TPFloat = ^float;

  TUpdater = class(TObject)
  private
    FData: TPFloat;
    FObject: T3DWorldObject;
  public
    constructor Create(Obj: T3DWorldObject; Data: TPFloat);
    function Update(Value: string): boolean;
  end;

  TObjectInspector = class(TForm)
    ComboBox1: TComboBox;
    SpeedButton1: TSpeedButton;
    StringGrid1: TStringGrid;
    procedure ComboBox1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
  private
    FWorld: T3DWorld;

    procedure OnWorldObjectChanged(Obj: T3DWorldObject);
  public
    procedure SetWorld(World: T3DWorld);
    property World: T3DWorld read FWorld write SetWorld;
  end;

var
  ObjectInspector: TObjectInspector;

implementation

{$R *.lfm}

constructor TUpdater.Create(Obj: T3DWorldObject; Data: TPFloat);
begin
  FData := Data;
  FObject := Obj;
end;

function TUpdater.Update(Value: string): boolean;
begin
  try
    FData^ := StrToFloat(Value);
    FObject.Dirty := True;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TObjectInspector.SetWorld(World: T3DWorld);
begin
  FWorld := World;
  FWorld.OnWorldObjectsChanged.Add(@OnWorldObjectChanged);
end;

procedure TObjectInspector.ComboBox1Change(Sender: TObject);
var
  box: TComboBox;
  obj: T3DWorldObject;
begin
  box := Sender as TComboBox;
  if box.ItemIndex < 0 then
    Exit();

  obj := box.Items.Objects[box.ItemIndex] as T3DWorldObject;

  StringGrid1.Clear;
  StringGrid1.RowCount := 10;
  StringGrid1.InsertRowWithValues(1, ['X', Format('%f', [obj.PositionVec.x])]);
  StringGrid1.InsertRowWithValues(2, ['Y', Format('%f', [obj.PositionVec.y])]);
  StringGrid1.InsertRowWithValues(3, ['Z', Format('%f', [obj.PositionVec.z])]);
  StringGrid1.Objects[1, 1] := TUpdater.Create(obj, @obj.PositionVec.x);
  StringGrid1.Objects[1, 2] := TUpdater.Create(obj, @obj.PositionVec.y);
  StringGrid1.Objects[1, 3] := TUpdater.Create(obj, @obj.PositionVec.z);

  StringGrid1.InsertRowWithValues(4, ['Scale X', Format('%f', [obj.ScaleVec.x])]);
  StringGrid1.InsertRowWithValues(5, ['Scale Y', Format('%f', [obj.ScaleVec.y])]);
  StringGrid1.InsertRowWithValues(6, ['Scale Z', Format('%f', [obj.ScaleVec.z])]);
  StringGrid1.Objects[1, 4] := TUpdater.Create(obj, @obj.ScaleVec.x);
  StringGrid1.Objects[1, 5] := TUpdater.Create(obj, @obj.ScaleVec.y);
  StringGrid1.Objects[1, 6] := TUpdater.Create(obj, @obj.ScaleVec.z);

  StringGrid1.InsertRowWithValues(7, ['Angle X', Format('%f', [obj.RotationVec.x])]);
  StringGrid1.InsertRowWithValues(8, ['Angle Y', Format('%f', [obj.RotationVec.y])]);
  StringGrid1.InsertRowWithValues(9, ['Angle Z', Format('%f', [obj.RotationVec.z])]);
  StringGrid1.Objects[1, 7] := TUpdater.Create(obj, @obj.RotationVec.x);
  StringGrid1.Objects[1, 8] := TUpdater.Create(obj, @obj.RotationVec.y);
  StringGrid1.Objects[1, 9] := TUpdater.Create(obj, @obj.RotationVec.z);
end;

procedure TObjectInspector.SpeedButton1Click(Sender: TObject);
var
  obj: T3DWorldObject;
begin
  if ComboBox1.ItemIndex >= 0 then
  begin
    obj := ComboBox1.Items.Objects[ComboBox1.ItemIndex] as T3DWorldObject;
    FWorld.RemoveWorldObject(obj);
    StringGrid1.Clear;
    if ComboBox1.Items.Count > 0 then
    begin
      ComboBox1.ItemIndex := 0;
      ComboBox1.OnChange(ComboBox1);
    end;
  end;
end;

procedure TObjectInspector.StringGrid1EditingDone(Sender: TObject);
var
  obj: TUpdater;
begin
  if (StringGrid1.Col < 0) or (StringGrid1.Row < 0) then
    Exit();

  if (StringGrid1.Col >= StringGrid1.ColCount) or
    (StringGrid1.Row >= StringGrid1.RowCount) then
    Exit();

  obj := StringGrid1.Objects[StringGrid1.Col, StringGrid1.Row] as TUpdater;
  obj.Update(StringGrid1.Cells[StringGrid1.Col, StringGrid1.Row]);
  FWorld.Dirty := True;
end;

procedure TObjectInspector.OnWorldObjectChanged(Obj: T3DWorldObject);
var
  i: integer;
  wrlObj: T3DWorldObject;
  modelName: string;
  oldIndex: integer;
begin
  oldIndex := ComboBox1.ItemIndex;
  ComboBox1.Clear;
  StringGrid1.Clear;
  for i := 0 to FWorld.WorldObjectsCount - 1 do
  begin
    wrlObj := FWorld.WorldObjects[i];
    modelName := FWorld.Objects[wrlObj.ModelIndex].Name;
    ComboBox1.AddItem(Format('Object %d [%s]', [i, modelName]), wrlObj);
  end;

  if (ComboBox1.Items.Count > 0) and (ComboBox1.ItemIndex < ComboBox1.Items.Count) then
    ComboBox1.ItemIndex := oldIndex;

  if (ComboBox1.Items.Count > 0) and (ComboBox1.ItemIndex < 0) then
  begin
    ComboBox1.ItemIndex := 0;
  end;
  ComboBox1.OnChange(ComboBox1);
end;

end.
