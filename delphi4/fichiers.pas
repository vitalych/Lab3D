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

{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U+,V+,W-,X+,Y+,Z1}

{$MINSTACKSIZE $00004000}

{$MAXSTACKSIZE $00100000}

{$IMAGEBASE $00400000}

{$APPTYPE GUI}

unit fichiers;

interface

uses sysutils, classes, unite_3d, dialogs, registry, windows, forcesfrm;

function LAB3D_SaveToFile(filename:string):integer;
function LAB3D_LoadFromFile(filename:string):integer;
procedure LAB3D_SaveSettings;
function LAB3D_LoadSettings:boolean;

implementation

function LAB3D_SaveToFile(filename:string):integer;
var
  fichier:TStream;
  i, j, k:integer;
  ver:double;
begin
  fichier := TFileStream.Create(filename, FMOPENREADWRITE or FMCREATE);

  ver := 0.1;
  fichier.WriteBuffer('LAB3D', 5);
  fichier.WriteBuffer(ver, sizeof(double));

  fichier.WriteBuffer(v3d_world, sizeof(TWorld));

  for i:=0 to v3d_world.object_count - 1 do
  begin
     fichier.WriteBuffer(v3d_world.objects[i]^, sizeof(TObject_t));
     for j:=0 to v3d_world.objects[i].VertexCount - 1 do
     begin
       fichier.WriteBuffer(v3d_world.objects[i].vertex[j], sizeof(TVertex));
     end;

     for j:=0 to v3d_world.objects[i].PolygonCount - 1 do
     begin
       fichier.WriteBuffer(v3d_world.objects[i].polygon[j], sizeof(TPolygon));
       for k:=0 to v3d_world.objects[i].polygon[j].Count - 1 do
           fichier.WriteBuffer(v3d_world.objects[i].polygon[j].vertex[k],
           sizeof(integer));
     end;
  end;

  fichier.WriteBuffer(frmForces.force_struct, sizeof(frmForces.force_struct));
  fichier.Free;
result := 1;
end;

function LAB3D_LoadFromFile(filename:string):integer;
var
  fichier:TStream;
  i, j, k:integer;
  ver:double;
  signature:array[0..5] of char;
begin
  fichier := TFileStream.Create(filename, FMOPENREAD);

  fichier.ReadBuffer(signature, 5);
  fichier.ReadBuffer(ver, sizeof(double));
 // MessageDlg(signature + ' ' +floattostr(ver), mtInformation, [mbOK], 0);

  fichier.ReadBuffer(v3d_world, sizeof(v3d_world));

  getmem(v3d_world.objects, sizeof(PObject));

  for i:=0 to v3d_world.object_count - 1 do
  begin
  getmem(v3d_world.objects[i], sizeof(TObject_t));
     fichier.Read(v3d_world.objects[i]^, sizeof(TObject_t));
      getmem(v3d_world.objects[i].Vertex, sizeof(TVertex)*v3d_world.objects[i].VertexCount);

     for j:=0 to v3d_world.objects[i].VertexCount - 1 do
     begin
       fichier.ReadBuffer(v3d_world.objects[i].vertex[j], sizeof(TVertex));
     end;

     getmem(v3d_world.objects[i].Polygon, sizeof(TPolygon)*v3d_world.objects[i].PolygonCount);
     for j:=0 to v3d_world.objects[i].PolygonCount - 1 do
     begin


       fichier.ReadBuffer(v3d_world.objects[i].polygon[j], sizeof(TPolygon));
       getmem(v3d_world.objects[i].Polygon[j].Vertex,
        sizeof(integer)*v3d_world.objects[i].Polygon[j].Count);

       for k:=0 to v3d_world.objects[i].polygon[j].Count - 1 do begin
           fichier.ReadBuffer(v3d_world.objects[i].polygon[j].vertex[k],
           sizeof(integer));
       end;
     end;
  end;

  try
   fichier.ReadBuffer(frmForces.force_struct, sizeof(frmForces.force_struct));
  except

  end;
fichier.free;
result := 1;
end;


procedure LAB3D_SaveSettings;
var
  reg:TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := HKEY_LOCAL_MACHINE;

  reg.OpenKey('SOFTWARE\ChipounovSoftware\Lab3D',True);
  reg.WriteBinaryData('PlayerPosAngle', v3d_view, sizeof(v3d_view));
  reg.free;
end;

function LAB3D_LoadSettings:boolean;
var
  reg:TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := HKEY_LOCAL_MACHINE;

  reg.OpenKey('SOFTWARE\ChipounovSoftware\Lab3D',true);

  try
   reg.ReadBinaryData('PlayerPosAngle', v3d_view, sizeof(v3d_view));
  except
     result := false;
     reg.free;
     exit;
  end;
  reg.free;
  result := true;
end;


end.
