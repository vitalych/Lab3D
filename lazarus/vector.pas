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

{$modeSwitch advancedRecords}

unit vector;

interface

uses Math;

type
  T3DVec = record
    x, y, z: float;
    constructor Create(_x, _y, _z: float);
  end;

  T4DVec = record
    x, y, z, w: float;
    constructor Create(_x, _y, _z, _w: float);
    constructor Create(v: T3DVec);
  end;

  T3DPolarVec = record
    r, theta, phi: float;
    constructor Create(_r, _t, _p: float);
  end;

  T2DVec = record
    x, y: integer;
    constructor Create(_x, _y: integer);
    constructor Create(v: T3DVec);
  end;

function D3D_MkVec(x, y, z: float): T3DVec;
function D3D_MkVecFromPoints(A, B: T3DVec): T3DVec;

function D3D_VecAdd(var vec1: T3DVec; vec2: T3DVec): T3DVec;
function D3D_VecDifference(Vector1, vector2: T3DVec): T3DVec;
function D3D_VecScale(vec: T3DVec; k: float): T3DVec;
function D3D_VecMult(vec1: T3DVec; vec2: T3DVec): T3DVec;
function D3D_DotProduct(vec1: T3DVec; vec2: T3DVec): float;

function D3D_VecLength(vec: T3DVec): float;
function D3D_SetVecLength(vec: T3DVec; length: float): T3DVec;

function D3D_VecToPolar(vec: T3DVec): T3DPolarVec;

function D3D_VecScale(vec: T4DVec): T3DVec;

implementation

function D3D_VecScale(vec: T4DVec): T3DVec;
var
  ret: T3DVec;
begin
  ret.x := vec.x / vec.w;
  ret.y := vec.y / vec.w;
  ret.z := vec.z / vec.w;
  Result := ret;
end;

constructor T4DVec.Create(_x, _y, _z, _w: float);
begin
  x := _x;
  y := _y;
  z := _z;
  w := _w;
end;

constructor T4DVec.Create(v: T3DVec);
begin
  x := v.x;
  y := v.y;
  z := v.z;
  w := 1;
end;

constructor T3DPolarVec.Create(_r, _t, _p: float);
begin
  r := _r;
  theta := _t;
  phi := _p;
end;

function D3D_VecToPolar(vec: T3DVec): T3DPolarVec;
var
  ret: T3DPolarVec;
begin
  ret.r := D3D_VecLength(vec);
  ret.theta := arctan2(vec.y, vec.x);
  ret.phi := arccos(vec.z / ret.r);
  Result := ret;
end;

constructor T3DVec.Create(_x, _y, _z: float);
begin
  x := _x;
  y := _y;
  z := _z;
end;

constructor T2DVec.Create(_x, _y: integer);
begin
  x := _x;
  y := _y;
end;

constructor T2DVec.Create(v: T3DVec);
begin
  x := round(v.x);
  y := round(v.y);
end;

function D3D_MkVec(x, y, z: float): T3DVec;
var
  ret: T3DVec;
begin
  ret.x := x;
  ret.y := y;
  ret.z := z;
  Result := ret;
end;

function D3D_MkVecFromPoints(A, B: T3DVec): T3DVec;
var
  ret: T3DVec;
begin
  ret.x := B.x - A.x;
  ret.y := B.y - A.y;
  ret.z := B.z - A.z;
  Result := ret;
end;

function D3D_VecAdd(var vec1: T3DVec; vec2: T3DVec): T3DVec;
var
  ret: T3DVec;
begin
  ret.x := vec1.x + vec2.x;
  ret.y := vec1.y + vec2.y;
  ret.z := vec1.z + vec2.z;
  Result := ret;
end;

function D3D_VecDifference(Vector1, vector2: T3DVec): T3DVec;
begin
  Result := D3D_MkVec(Vector1.x - Vector2.x, Vector1.y - Vector2.y,
    Vector1.z - Vector2.z);
end;

function D3D_VecScale(vec: T3DVec; k: float): T3DVec;
var
  ret: T3DVec;
begin
  ret.x := vec.x * k;
  ret.y := vec.y * k;
  ret.z := vec.z * k;
  Result := ret;
end;

function D3D_VecMult(vec1: T3DVec; vec2: T3DVec): T3DVec;
var
  dest: T3DVec;
begin
  dest.x := vec1.y * vec2.z - vec1.z * vec2.y;
  dest.y := vec1.z * vec2.x - vec1.x * vec2.z;
  dest.z := vec1.x * vec2.y - vec1.y * vec2.x;
  Result := dest;
end;

function D3D_DotProduct(vec1: T3DVec; vec2: T3DVec): float;
begin
  Result := vec1.x * vec2.x + vec1.y * vec2.y + vec1.z * vec2.z;
end;

function D3D_VecLength(vec: T3DVec): float;
begin
  Result := sqrt(power(vec.x, 2) + power(vec.y, 2) + power(vec.z, 2));
end;

function D3D_SetVecLength(vec: T3DVec; length: float): T3DVec;
var
  ol: float;
  ret: T3DVec;
begin
  ol := D3D_VecLength(vec);
  if ol = 0 then
    exit;

  ret.x := vec.x * (length / ol);
  ret.y := vec.y * (length / ol);
  ret.z := vec.z * (length / ol);
  Result := ret;
end;

end.
