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

unit l3d_world;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, l3d_objects, matrix, Math, vector, fastbmp, l3d_clipping,
  events;

type
  T3DWorldObject = class
  private
    FModelIndex: integer;
    FComposition: T4DMatrix;
    FDirty: boolean;

    FScaleVec: T3DVec;
    FPositionVec: T3DVec;
    FRotationVec: T3DVec;

    function Compose(): T4DMatrix;
  public
    constructor Create(index: integer);
    procedure Scale(sx, sy, sz: float);
    procedure Rotate(ax, ay, az: float);
    procedure Translate(x, y, z: float);
    property Composition: T4DMatrix read Compose;
    property ModelIndex: integer read FModelIndex;
    property PositionVec: T3DVec read FPositionVec write FPositionVec;
    property ScaleVec: T3DVec read FScaleVec write FScaleVec;
    property RotationVec: T3DVec read FRotationVec write FRotationVec;
    property Dirty: boolean read FDirty write FDirty;
  end;

  T3DWorld = class
    type
    TOnWorldObjectsChangedEvent = specialize TEventSource<T3DWorldObject>;
  private
    FObjects: array of T3DObject;
    FWorldObjects: array of T3DWorldObject;

    FViewMatrix: T4DMatrix;
    FProjectionMatrix: T4DMatrix;
    FComposition: T4DMatrix;
    FScreenWidth, FScreenHeight: integer;
    FDirty: boolean;

    FProjectedVertices: array of array of T4DVec;

    FOnWorldObjectsChanged: TOnWorldObjectsChangedEvent;

    function Compose(): T4DMatrix;
    function ToScreenCooordinates(vec: T3DVec): T2DVec;

    function GetObject(i: integer): T3DObject;
    function GetWorldObject(i: integer): T3DWorldObject;
    function GetWorldObjectsCount(): integer;
    procedure SetDirty(Value: boolean);

  public
    constructor Create();
    function AddWorldObject(objectIndex: integer): T3DWorldObject;
    function RemoveWorldObject(obj: T3DWorldObject): boolean;
    procedure Clear();

    procedure SetProjectionMatrix(Width, Height: integer;
      fov, nearPlane, farPlane: float);
    procedure SetViewMatrix(mat: T4DMatrix);
    procedure Project();
    procedure DrawWireFrame(surface: TFastBitmap);

    property Composition: T4DMatrix read Compose;
    property Dirty: boolean read FDirty write SetDirty;

    property Objects[i: integer]: T3DObject read GetObject;
    property WorldObjects[i: integer]: T3DWorldObject read GetWorldObject;
    property WorldObjectsCount: integer read GetWorldObjectsCount;

    property OnWorldObjectsChanged: TOnWorldObjectsChangedEvent
      read FOnWorldObjectsChanged;
  end;

function IsVertexVisible(point: T4DVec): boolean;
function IsTriangleVisible(points: array of T4DVec): boolean;

implementation

function IsVertexVisible(point: T4DVec): boolean;
var
  s: T3DVec;
begin
  s := D3D_VecScale(point);
  if not ((-1 < s.x) and (s.x < 1)) then
    Exit(False);
  if not ((-1 < s.y) and (s.y < 1)) then
    Exit(False);
  if not ((-1 < s.z) and (s.z < 1)) then
    Exit(False);
  Result := True;
end;

function IsTriangleVisible(points: array of T4DVec): boolean;
var
  i: integer;
begin
  for i := 0 to Length(points) - 1 do
  begin
    if IsVertexVisible(points[i]) then
      Exit(True);
  end;
  Result := False;
end;

constructor T3DWorldObject.Create(index: integer);
begin
  FModelIndex := index;
  FComposition := D3D_IdentityMatrix();
  FScaleVec := T3DVec.Create(1, 1, 1);
  FPositionVec := T3DVec.Create(0, 0, 0);
  FRotationVec := T3DVec.Create(0, 0, 0);
  FDirty := False;
end;

function T3DWorldObject.Compose(): T4DMatrix;
var
  scalev, position, rotation: T4DMatrix;
begin
  if not FDirty then
    Exit(FComposition);

  scalev := D3D_ScaleMatrix(FScaleVec.x, FScaleVec.y, FScaleVec.z);
  rotation := D3D_RotationMatrix(FRotationVec.x, FRotationVec.y, FRotationVec.z);
  position := D3D_TranslationMatrix(FPositionVec.x, FPositionVec.y, FPositionVec.z);

  FComposition := D3D_MatrixMult(position, D3D_MatrixMult(scalev, rotation));
  FDirty := False;
  Result := FComposition;
end;

procedure T3DWorldObject.Scale(sx, sy, sz: float);
begin
  FScaleVec := T3DVec.Create(sx, sy, sz);
  FDirty := True;
end;

procedure T3DWorldObject.Rotate(ax, ay, az: float);
begin
  FRotationVec := T3DVec.Create(ax, ay, az);
  FDirty := True;
end;

procedure T3DWorldObject.Translate(x, y, z: float);
begin
  FPositionVec := T3DVec.Create(x, y, z);
  FDirty := True;
end;

constructor T3DWorld.Create();
begin
  FObjects := nil;
  SetLength(FObjects, 3);
  FObjects[OBJ_CUBE] := T3DCube.Create();
  FObjects[OBJ_BOARD] := T3DBoard.Create();
  FObjects[OBJ_SpHERE] := T3DSphere.Create();

  FProjectionMatrix := D3D_IdentityMatrix();
  FViewMatrix := D3D_IdentityMatrix();
  FComposition := D3D_IdentityMatrix();
  FScreenWidth := 0;
  FScreenHeight := 0;
  FDirty := False;

  FOnWorldObjectsChanged := TOnWorldObjectsChangedEvent.Create();
end;

function T3DWorld.AddWorldObject(objectIndex: integer): T3DWorldObject;
var
  len: integer;
begin
  if objectIndex >= Length(FObjects) then
    Exit(nil);

  len := Length(FWorldObjects);
  SetLength(FWorldObjects, len + 1);
  FWorldObjects[len] := T3DWorldObject.Create(objectIndex);
  FDirty := True;
  OnWorldObjectsChanged.Invoke(FWorldObjects[len]);
  Result := FWorldObjects[len];
end;

function T3DWorld.RemoveWorldObject(obj: T3DWorldObject): boolean;
var
  i, index: integer;
begin
  if obj = nil then
    Exit(False);

  index := -1;
  for i := 0 to Length(FWorldObjects) - 1 do
  begin
    if FWorldObjects[i] = obj then
    begin
      index := i;
      break;
    end;
  end;

  if index = -1 then
    Exit(False);

  FWorldObjects[index] := nil;
  for i := index to Length(FWorldObjects) - 2 do
  begin
    FWorldObjects[i] := FWorldObjects[i + 1];
  end;
  SetLength(FWorldObjects, Length(FWorldObjects) - 1);
  FDirty := True;
  OnWorldObjectsChanged.Invoke(nil);
  Result := True;
end;

procedure T3DWorld.Clear(); begin
  SetLength(FWorldObjects, 0);
  FDirty := True;
  OnWorldObjectsChanged.Invoke(nil);
end;

function T3DWorld.GetWorldObject(i: integer): T3DWorldObject;
begin
  if i >= Length(FWorldObjects) then
    Exit(nil);
  Result := FWorldObjects[i];
end;

function T3DWorld.GetWorldObjectsCount(): integer;
begin
  Result := Length(FWorldObjects);
end;

function T3DWorld.GetObject(i: integer): T3DObject;
begin
  if i >= Length(FObjects) then
    Exit(nil);
  Result := FObjects[i];
end;

procedure T3DWorld.SetDirty(Value: boolean);
begin
  FDirty := Value;
  if FDirty then
    OnWorldObjectsChanged.Invoke(nil);
end;

procedure T3DWorld.SetProjectionMatrix(Width, Height: integer;
  fov, nearPlane, farPlane: float);
begin
  FProjectionMatrix := D3D_Projection(fov, nearPlane, farPlane, Width / Height);
  FScreenWidth := Width;
  FScreenHeight := Height;
  FDirty := True;
end;

procedure T3DWorld.SetViewMatrix(mat: T4DMatrix);
begin
  FViewMatrix := mat;
  Dirty := True;
end;

function T3DWorld.Compose(): T4DMatrix;
begin
  if not Dirty then
    Exit(FComposition);

  FComposition := D3D_MatrixMult(FProjectionMatrix, FViewMatrix);
  Dirty := False;
  Result := FComposition;
end;

procedure T3DWorld.Project();
var
  objIndex, vertexIndex: integer;
  modelObj: T3DObject;
  worldObj: T3DWorldObject;
  finalMatrix: T4DMatrix;
  objProjVert: array of T4DVec;
begin
  if not Dirty then
    Exit();

  SetLength(FProjectedVertices, Length(FWorldObjects));
  for objIndex := 0 to Length(FWorldObjects) - 1 do
  begin
    worldObj := FWorldObjects[objIndex];
    finalMatrix := D3D_MatrixMult(Compose(), worldObj.Compose());

    modelObj := FObjects[worldObj.ModelIndex];
    SetLength(FProjectedVertices[objIndex], Length(modelObj.Vertices));
    objProjVert := FProjectedVertices[objIndex];

    for vertexIndex := 0 to Length(modelObj.Vertices) - 1 do
    begin
      objProjVert[vertexIndex] :=
        D3D_VecMultMatrix(finalMatrix, T4DVec.Create(modelObj.Vertices[vertexIndex]));
    end;
  end;
end;

function T3DWorld.ToScreenCooordinates(vec: T3DVec): T2DVec;
var
  ret: T2DVec;
begin
  ret.x := trunc((vec.x + 1) * FScreenWidth / 2);
  ret.y := trunc((vec.y + 1) * FScreenHeight / 2);
  Result := ret;
end;

procedure T3DWorld.DrawWireFrame(surface: TFastBitmap);
var
  objId, objIndex, triangleIndex: integer;
  objectVertices: array of T4DVec;
  p0, p1, p2: T4DVec;
  s0, s1, s2: T2DVec;
  point1, point2: T2DVec;
  triangle: T3DTriangle;
  screen, clipped: TCoordinates;
begin
  screen := TCoordinates.Create(T2DVec.Create(0, 0),
    T2DVec.Create(FScreenWidth - 1, FScreenHeight - 1));
  clipped := screen;

  for objIndex := 0 to Length(FProjectedVertices) - 1 do
  begin
    objectVertices := FProjectedVertices[objIndex];
    objId := FWorldObjects[objIndex].ModelIndex;

    // Draw the connecting lines between all triangles
    for triangleIndex := 0 to Length(FObjects[objId].Triangles) - 1 do
    begin
      triangle := FObjects[objId].Triangles[triangleIndex];

      p0 := objectVertices[triangle[0]];
      p1 := objectVertices[triangle[1]];
      p2 := objectVertices[triangle[2]];
      if not IsTriangleVisible([p0, p1, p2]) then
        continue;

      s0 := ToScreenCooordinates(D3D_VecScale(p0));
      s1 := ToScreenCooordinates(D3D_VecScale(p1));
      s2 := ToScreenCooordinates(D3D_VecScale(p2));

      point1 := s0;
      point2 := s1;
      if ClipLine(clipped, TCoordinates.Create(point1, point2), screen) then
        surface.DrawLine(clipped.P1.x, clipped.P1.y, clipped.P2.x,
          clipped.P2.y, $ff00);

      point1 := s1;
      point2 := s2;
      if ClipLine(clipped, TCoordinates.Create(point1, point2), screen) then
        surface.DrawLine(clipped.P1.x, clipped.P1.y, clipped.P2.x,
          clipped.P2.y, $ff00);

      point1 := s2;
      point2 := s0;
      if ClipLine(clipped, TCoordinates.Create(point1, point2), screen) then
        surface.DrawLine(clipped.P1.x, clipped.P1.y, clipped.P2.x,
          clipped.P2.y, $ff00);
    end;
  end;
end;

end.
