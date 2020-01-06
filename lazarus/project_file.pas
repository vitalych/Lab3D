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

unit project_file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, l3d_world, vector, Math, l3d_camera;

type
  TProjectFileCamera = record
    Position: T3DVec;
    AngleX, AngleY: float;
  end;

  TProjectFile = record
    Magic: UInt32;
    Version: UInt32;
    WorldObjectsCount: UInt32;
    Camera: TProjectFileCamera;
  end;

  TProjectFileWorldObject = record
    ModelIndex: UInt32;
    ScaleVec: T3DVec;
    PositionVec: T3DVec;
    RotationVec: T3DVec;
  end;

const
  PROJECT_FILE_MAGIC = $deaddead;
  PROJECT_FILE_VERSION = 1;

procedure SaveWorldToFile(world: T3DWorld; camera: TCameraPosition; filePath: string);
procedure LoadWorldFromFile(var world: T3DWorld; var camera: TCameraPosition;
  filePath: string);

implementation

procedure SaveWorldToFile(world: T3DWorld; camera: TCameraPosition; filePath: string);
var
  header: TProjectFile;
  obj: TProjectFileWorldObject;
  fsOut: TFileStream;
  i: integer;
begin
  header.Magic := PROJECT_FILE_MAGIC;
  header.Version := PROJECT_FILE_VERSION;
  header.WorldObjectsCount := world.WorldObjectsCount;
  header.Camera.AngleX := camera.AngleX;
  header.Camera.AngleY := camera.AngleY;
  header.Camera.Position := camera.Position;

  fsOut := TFileStream.Create(filePath, fmCreate);
  fsOut.Write(header, sizeof(header));

  for i := 0 to header.WorldObjectsCount - 1 do
  begin
    obj.ModelIndex := world.WorldObjects[i].ModelIndex;
    obj.ScaleVec := world.WorldObjects[i].ScaleVec;
    obj.PositionVec := world.WorldObjects[i].PositionVec;
    obj.RotationVec := world.WorldObjects[i].RotationVec;
    fsOut.Write(obj, sizeof(obj));
  end;

  fsOut.Free;
end;

procedure LoadWorldFromFile(var world: T3DWorld; var camera: TCameraPosition;
  filePath: string);
var
  header: TProjectFile;
  obj: TProjectFileWorldObject;
  stream: TFileStream;
  i: integer;
  wobj: T3DWorldObject;
begin
  world := nil;
  camera := nil;

  try
    stream := TFileStream.Create(filePath, fmOpenRead);
    stream.ReadBuffer(header, Sizeof(header));

    if (header.Magic <> PROJECT_FILE_MAGIC) or (header.Version <>
      PROJECT_FILE_VERSION) then
      Exit();

    camera := TCameraPosition.Create(T3DVec.Create(0, 0, 0), 0);
    camera.Position := header.Camera.Position;
    camera.AngleX := header.Camera.AngleX;
    camera.AngleY := header.Camera.AngleY;

    world := T3DWorld.Create();
    for i := 0 to header.WorldObjectsCount - 1 do
    begin
      stream.ReadBuffer(obj, sizeof(obj));

      wobj := world.AddWorldObject(obj.ModelIndex);
      if wobj = nil then
        Exit();

      wobj.PositionVec := obj.PositionVec;
      wobj.ScaleVec := obj.ScaleVec;
      wobj.RotationVec := obj.RotationVec;
      wobj.Dirty := True;
    end;
    world.Dirty := True;
  finally
    stream.Free;
  end;
end;

end.
