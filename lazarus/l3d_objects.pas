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

unit l3d_objects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vector;

type

  T3DTriangle = array[0..2] of integer;

  T3DObject = class
  protected
    function GetName(): string; virtual; abstract;
  public
    Vertices: array of T3DVec;
    Triangles: array of T3DTriangle;
    constructor Create;
    property Name: string read GetName;
  end;

  T3DCube = class(T3DObject)
  protected
    function GetName(): string; override;
  public
    constructor Create;
  end;

  T3DBoard = class(T3DObject)
  protected
    function GetName(): string; override;
  public
    constructor Create;
  end;

  T3DSphere = class(T3DObject)
  protected
    function GetName(): string; override;
  public
    constructor Create;
  end;

  T3DTriangleObj = class(T3DObject)
  protected
    function GetName(): string; override;
  public
    constructor Create;
  end;

const
  OBJ_CUBE = 0;
  OBJ_BOARD = 1;
  OBJ_SPHERE = 2;

implementation

constructor T3DObject.Create();
begin
  SetLength(Vertices, 0);
  SetLength(Triangles, 0);
end;

constructor T3DBoard.Create();
var
  size: integer;
  tr_per_row: integer;
  i, x, y: integer;
begin
  size := 11;
  SetLength(Vertices, size * size);
  i := 0;
  for y := 0 to size - 1 do
  begin
    for x := 0 to size - 1 do
    begin
      Vertices[i] := T3DVec.Create(x - size / 2, 0, y - size / 2);
      i += 1;
    end;
  end;

  tr_per_row := size - 1;
  SetLength(Triangles, (2 * (tr_per_row) * (tr_per_row)));
  i := 0;
  for y := 0 to tr_per_row - 1 do
  begin
    for x := 0 to tr_per_row - 1 do
    begin
      Triangles[i][0] := y * size + x;
      Triangles[i][1] := y * size + x + 1;
      Triangles[i][2] := (y + 1) * size + x;

      Triangles[i + 1][0] := y * size + x + 1;
      Triangles[i + 1][1] := (y + 1) * size + x + 1;
      Triangles[i + 1][2] := (y + 1) * size + x;
      i := i + 2;
    end;
  end;
end;

function T3DBoard.GetName(): string;
begin
     Result := 'Board';
end;

constructor T3DSphere.Create();
var
  phi, theta: double;
  density, numVertices, numTriangles: integer;
  i, j, k, l: integer;
  vi, hi: integer;
  point: T3DVec;
begin
  density := 16;

  numVertices := density * (density - 1) + 2;
  SetLength(Vertices, numVertices);

  numTriangles := (density * 2) + 2 * density * (density - 2);
  SetLength(Triangles, numTriangles);

  Vertices[0] := T3DVec.Create(0.0, 0.0, 1.0);
  Vertices[numVertices - 1] := T3DVec.Create(0.0, 0.0, -1.0);

  for vi := 1 to density - 1 do
  begin
    for hi := 0 to density - 1 do
    begin
      theta := pi * vi / density;
      phi := 2 * pi * hi / density;
      point.x := sin(theta) * cos(phi);
      point.y := sin(theta) * sin(phi);
      point.z := cos(theta);
      Vertices[1 + (vi - 1) * density + hi] := point;
    end;
  end;

  for i := 0 to density - 1 do
  begin
    Triangles[i][0] := 0;
    Triangles[i][1] := ((i + 1) mod density) + 1;
    Triangles[i][2] := (i mod density) + 1;

    Triangles[numTriangles - 1 - i][0] := numVertices - 1;
    Triangles[numTriangles - 1 - i][1] :=
      numVertices - 1 - density + ((i + 1) mod density);
    Triangles[numTriangles - 1 - i][2] := numVertices - 1 - density + (i mod density);
  end;

  l := 1; // Index of first vertex
  for i := 0 to density - 3 do
  begin
    for j := 0 to density - 1 do
    begin
      k := density + 2 * i * density;

      Triangles[k + j][0] := l + j;
      Triangles[k + j][1] := l + (j + 1) mod density;
      Triangles[k + j][2] := l + j + density;

      Triangles[k + density + j][0] := l + (j + 1) mod density;
      Triangles[k + density + j][1] := l + ((j + 1) mod density) + density;
      Triangles[k + density + j][2] := l + j + density;
    end;
    l += density;
  end;
end;

function T3DSphere.GetName(): string;
begin
     Result := 'Sphere';
end;

constructor T3DTriangleObj.Create();
begin
  SetLength(Vertices, 8);
  Vertices[0] := T3DVec.Create(-1, -1, -1);
  Vertices[1] := T3DVec.Create(1, -1, -1);
  Vertices[2] := T3DVec.Create(1, 1, -1);

  SetLength(Triangles, 1);
  Triangles[0][0] := 0;
  Triangles[0][1] := 1;
  Triangles[0][2] := 2;
end;

function T3DTriangleObj.GetName(): string;
begin
     Result := 'Triangle';
end;

constructor T3DCube.Create();
begin
  SetLength(Vertices, 8);
  Vertices[0] := T3DVec.Create(-1, -1, -1);
  Vertices[1] := T3DVec.Create(1, -1, -1);
  Vertices[2] := T3DVec.Create(1, 1, -1);
  Vertices[3] := T3DVec.Create(-1, 1, -1);
  Vertices[4] := T3DVec.Create(-1, -1, 1);
  Vertices[5] := T3DVec.Create(1, -1, 1);
  Vertices[6] := T3DVec.Create(1, 1, 1);
  Vertices[7] := T3DVec.Create(-1, 1, 1);

  SetLength(Triangles, 12);
  Triangles[0][0] := 4;
  Triangles[0][1] := 5;
  Triangles[0][2] := 6;

  Triangles[1][0] := 4;
  Triangles[1][1] := 6;
  Triangles[1][2] := 7;

  Triangles[2][0] := 5;
  Triangles[2][1] := 1;
  Triangles[2][2] := 2;

  Triangles[3][0] := 5;
  Triangles[3][1] := 2;
  Triangles[3][2] := 6;

  Triangles[4][0] := 4;
  Triangles[4][1] := 0;
  Triangles[4][2] := 3;

  Triangles[5][0] := 4;
  Triangles[5][1] := 3;
  Triangles[5][2] := 7;

  Triangles[6][0] := 4;
  Triangles[6][1] := 5;
  Triangles[6][2] := 1;

  Triangles[7][0] := 4;
  Triangles[7][1] := 1;
  Triangles[7][2] := 0;

  Triangles[8][0] := 7;
  Triangles[8][1] := 6;
  Triangles[8][2] := 2;

  Triangles[9][0] := 7;
  Triangles[9][1] := 2;
  Triangles[9][2] := 3;

  Triangles[10][0] := 1;
  Triangles[10][1] := 3;
  Triangles[10][2] := 0;

  Triangles[11][0] := 1;
  Triangles[11][1] := 2;
  Triangles[11][2] := 3;
end;

function T3DCube.GetName(): string;
begin
     Result := 'Cube';
end;

end.
