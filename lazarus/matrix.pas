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

unit matrix;

interface

uses vector, Math;

type
  T4DMatrix = array [0..3] of array [0..3] of float;

function D3D_MatrixMult(mat1: T4DMatrix; mat2: T4DMatrix): T4DMatrix;
function D3D_VecMultMatrix(matrix: T4DMatrix; vec: T3DVec): T3DVec;
function D3D_VecMultMatrix(matrix: T4DMatrix; vec: T4DVec): T4DVec;
function D3D_ZeroMatrix(): T4DMatrix;
function D3D_IdentityMatrix(): T4DMatrix;
function D3D_TranslationMatrix(tx, ty, tz: float): T4DMatrix;
function D3D_ScaleMatrix(sx, sy, sz: float): T4DMatrix;
function D3D_InverseMatrix(m: T4DMatrix): T4DMatrix;

function D3D_Translate(var matrix: T4DMatrix; tx, ty, tz: float): T4DMatrix;
function D3D_Translate(var matrix: T4DMatrix; vec: T3DVec): T4DMatrix;
function D3D_Scale(var matrix: T4DMatrix; sx, sy, sz: float): T4DMatrix;
function D3D_RotationMatrix(ax, ay, az: float): T4DMatrix;
function D3D_RotationMatrixX(ax: float): T4DMatrix;
function D3D_RotationMatrixY(ay: float): T4DMatrix;
function D3D_RotationMatrixZ(az: float): T4DMatrix;

function D3D_Projection(angleOfView, nearField, farField, aspect: float): T4DMatrix;

implementation

// TODO: rename to T4DMatrix
function D3D_MatrixMult(mat1: T4DMatrix; mat2: T4DMatrix): T4DMatrix;
var
  dest: T4DMatrix;
  i, j, k: integer;
  acc: float;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      acc := 0;
      for k := 0 to 3 do
        acc += mat1[i][k] * mat2[k][j];
      dest[i][j] := acc;
    end;

  Result := dest;
end;

function D3D_InverseMatrix(m: T4DMatrix): T4DMatrix;
var
  det: float;
  ret: T4DMatrix;
  A2323: float;
  A1323: float;
  A1223: float;
  A0323: float;
  A0223: float;
  A0123: float;
  A2313: float;
  A1313: float;
  A1213: float;
  A2312: float;
  A1312: float;
  A1212: float;
  A0313: float;
  A0213: float;
  A0312: float;
  A0212: float;
  A0113: float;
  A0112: float;
begin
  A2323 := m[2][2] * m[3][3] - m[2][3] * m[3][2];
  A1323 := m[2][1] * m[3][3] - m[2][3] * m[3][1];
  A1223 := m[2][1] * m[3][2] - m[2][2] * m[3][1];
  A0323 := m[2][0] * m[3][3] - m[2][3] * m[3][0];
  A0223 := m[2][0] * m[3][2] - m[2][2] * m[3][0];
  A0123 := m[2][0] * m[3][1] - m[2][1] * m[3][0];
  A2313 := m[1][2] * m[3][3] - m[1][3] * m[3][2];
  A1313 := m[1][1] * m[3][3] - m[1][3] * m[3][1];
  A1213 := m[1][1] * m[3][2] - m[1][2] * m[3][1];
  A2312 := m[1][2] * m[2][3] - m[1][3] * m[2][2];
  A1312 := m[1][1] * m[2][3] - m[1][3] * m[2][1];
  A1212 := m[1][1] * m[2][2] - m[1][2] * m[2][1];
  A0313 := m[1][0] * m[3][3] - m[1][3] * m[3][0];
  A0213 := m[1][0] * m[3][2] - m[1][2] * m[3][0];
  A0312 := m[1][0] * m[2][3] - m[1][3] * m[2][0];
  A0212 := m[1][0] * m[2][2] - m[1][2] * m[2][0];
  A0113 := m[1][0] * m[3][1] - m[1][1] * m[3][0];
  A0112 := m[1][0] * m[2][1] - m[1][1] * m[2][0];

  det := m[0][0] * (m[1][1] * A2323 - m[1][2] * A1323 + m[1][3] * A1223) -
    m[0][1] * (m[1][0] * A2323 - m[1][2] * A0323 + m[1][3] * A0223) +
    m[0][2] * (m[1][0] * A1323 - m[1][1] * A0323 + m[1][3] * A0123) -
    m[0][3] * (m[1][0] * A1223 - m[1][1] * A0223 + m[1][2] * A0123);
  det := 1 / det;

  ret[0][0] := det * (m[1][1] * A2323 - m[1][2] * A1323 + m[1][3] * A1223);
  ret[0][1] := det * -(m[0][1] * A2323 - m[0][2] * A1323 + m[0][3] * A1223);
  ret[0][2] := det * (m[0][1] * A2313 - m[0][2] * A1313 + m[0][3] * A1213);
  ret[0][3] := det * -(m[0][1] * A2312 - m[0][2] * A1312 + m[0][3] * A1212);
  ret[1][0] := det * -(m[1][0] * A2323 - m[1][2] * A0323 + m[1][3] * A0223);
  ret[1][1] := det * (m[0][0] * A2323 - m[0][2] * A0323 + m[0][3] * A0223);
  ret[1][2] := det * -(m[0][0] * A2313 - m[0][2] * A0313 + m[0][3] * A0213);
  ret[1][3] := det * (m[0][0] * A2312 - m[0][2] * A0312 + m[0][3] * A0212);
  ret[2][0] := det * (m[1][0] * A1323 - m[1][1] * A0323 + m[1][3] * A0123);
  ret[2][1] := det * -(m[0][0] * A1323 - m[0][1] * A0323 + m[0][3] * A0123);
  ret[2][2] := det * (m[0][0] * A1313 - m[0][1] * A0313 + m[0][3] * A0113);
  ret[2][3] := det * -(m[0][0] * A1312 - m[0][1] * A0312 + m[0][3] * A0112);
  ret[3][0] := det * -(m[1][0] * A1223 - m[1][1] * A0223 + m[1][2] * A0123);
  ret[3][1] := det * (m[0][0] * A1223 - m[0][1] * A0223 + m[0][2] * A0123);
  ret[3][2] := det * -(m[0][0] * A1213 - m[0][1] * A0213 + m[0][2] * A0113);
  ret[3][3] := det * (m[0][0] * A1212 - m[0][1] * A0212 + m[0][2] * A0112);
  Result := ret;
end;

function D3D_VecMultMatrix(matrix: T4DMatrix; vec: T4DVec): T4DVec;
var
  ret: T4DVec;
begin
  ret.x := matrix[0][0] * vec.x + matrix[0][1] * vec.y + matrix[0][2] *
    vec.z + matrix[0][3] * vec.w;
  ret.y := matrix[1][0] * vec.x + matrix[1][1] * vec.y + matrix[1][2] *
    vec.z + matrix[1][3] * vec.w;
  ret.z := matrix[2][0] * vec.x + matrix[2][1] * vec.y + matrix[2][2] *
    vec.z + matrix[2][3] * vec.w;
  ret.w := matrix[3][0] * vec.x + matrix[3][1] * vec.y + matrix[3][2] *
    vec.z + matrix[3][3] * vec.w;
  Result := ret;
end;

function D3D_VecMultMatrix(matrix: T4DMatrix; vec: T3DVec): T3DVec;
var
  ret: T3DVec;
  res: T4DVec;
begin
  res := D3D_VecMultMatrix(matrix, T4DVec.Create(vec));
  if res.w <> 1.0 then
  begin
    ret.x := res.x / res.w;
    ret.y := res.y / res.w;
    ret.z := res.z / res.w;
  end
  else
  begin
    ret.x := res.x;
    ret.y := res.y;
    ret.z := res.z;
  end;

  Result := ret;
end;

function D3D_IdentityMatrix(): T4DMatrix;
var
  mat: T4DMatrix;
begin
  mat[0][0] := 1;
  mat[0][1] := 0;
  mat[0][2] := 0;
  mat[0][3] := 0;
  mat[1][0] := 0;
  mat[1][1] := 1;
  mat[1][2] := 0;
  mat[1][3] := 0;
  mat[2][0] := 0;
  mat[2][1] := 0;
  mat[2][2] := 1;
  mat[2][3] := 0;
  mat[3][0] := 0;
  mat[3][1] := 0;
  mat[3][2] := 0;
  mat[3][3] := 1;
  Result := mat;
end;

function D3D_ZeroMatrix(): T4DMatrix;
var
  mat: T4DMatrix;
begin
  mat[0][0] := 0;
  mat[0][1] := 0;
  mat[0][2] := 0;
  mat[0][3] := 0;
  mat[1][0] := 0;
  mat[1][1] := 0;
  mat[1][2] := 0;
  mat[1][3] := 0;
  mat[2][0] := 0;
  mat[2][1] := 0;
  mat[2][2] := 0;
  mat[2][3] := 0;
  mat[3][0] := 0;
  mat[3][1] := 0;
  mat[3][2] := 0;
  mat[3][3] := 0;
  Result := mat;
end;

function D3D_TranslationMatrix(tx, ty, tz: float): T4DMatrix;
var
  tmat: T4DMatrix;
begin
  tmat[0][0] := 1;
  tmat[0][1] := 0;
  tmat[0][2] := 0;
  tmat[0][3] := tx;
  tmat[1][0] := 0;
  tmat[1][1] := 1;
  tmat[1][2] := 0;
  tmat[1][3] := ty;
  tmat[2][0] := 0;
  tmat[2][1] := 0;
  tmat[2][2] := 1;
  tmat[2][3] := tz;
  tmat[3][0] := 0;
  tmat[3][1] := 0;
  tmat[3][2] := 0;
  tmat[3][3] := 1;
  Result := tmat;
end;

function D3D_Translate(var matrix: T4DMatrix; tx, ty, tz: float): T4DMatrix;
var
  tmat: T4DMatrix;
begin
  tmat := D3D_TranslationMatrix(tx, ty, tz);
  Result := D3D_MatrixMult(matrix, tmat);
end;

function D3D_Translate(var matrix: T4DMatrix; vec: T3DVec): T4DMatrix;
var
  tmat: T4DMatrix;
begin
  tmat := D3D_TranslationMatrix(vec.x, vec.y, vec.z);
  Result := D3D_MatrixMult(matrix, tmat);
end;

function D3D_ScaleMatrix(sx, sy, sz: float): T4DMatrix;
var
  smat: T4DMatrix;
begin
  smat[0][0] := sx;
  smat[0][1] := 0;
  smat[0][2] := 0;
  smat[0][3] := 0;

  smat[1][0] := 0;
  smat[1][1] := sy;
  smat[1][2] := 0;
  smat[1][3] := 0;

  smat[2][0] := 0;
  smat[2][1] := 0;
  smat[2][2] := sz;
  smat[2][3] := 0;

  smat[3][0] := 0;
  smat[3][1] := 0;
  smat[3][2] := 0;
  smat[3][3] := 1;
  Result := smat;
end;

function D3D_Scale(var matrix: T4DMatrix; sx, sy, sz: float): T4DMatrix;
var
  smat: T4DMatrix;
begin
  smat := D3D_ScaleMatrix(sx, sy, sz);
  Result := D3D_MatrixMult(matrix, smat);
end;

function D3D_RotationMatrixX(ax: float): T4DMatrix;
var
  xmat: T4DMatrix;
begin
  xmat[0][0] := 1;
  xmat[0][1] := 0;
  xmat[0][2] := 0;
  xmat[0][3] := 0;
  xmat[1][0] := 0;
  xmat[1][1] := COS(ax);
  xmat[1][2] := -SIN(ax);
  xmat[1][3] := 0;
  xmat[2][0] := 0;
  xmat[2][1] := SIN(ax);
  xmat[2][2] := COS(ax);
  xmat[2][3] := 0;
  xmat[3][0] := 0;
  xmat[3][1] := 0;
  xmat[3][2] := 0;
  xmat[3][3] := 1;
  Result := xmat;
end;

function D3D_RotationMatrixY(ay: float): T4DMatrix;
var
  ymat: T4DMatrix;
begin
  ymat[0][0] := COS(ay);
  ymat[0][1] := 0;
  ymat[0][2] := SIN(ay);
  ymat[0][3] := 0;
  ymat[1][0] := 0;
  ymat[1][1] := 1;
  ymat[1][2] := 0;
  ymat[1][3] := 0;
  ymat[2][0] := -SIN(ay);
  ymat[2][1] := 0;
  ymat[2][2] := COS(ay);
  ymat[2][3] := 0;
  ymat[3][0] := 0;
  ymat[3][1] := 0;
  ymat[3][2] := 0;
  ymat[3][3] := 1;
  Result := ymat;
end;

function D3D_RotationMatrixZ(az: float): T4DMatrix;
var
  zmat: T4DMatrix;
begin
  zmat[0][0] := COS(az);
  zmat[0][1] := -SIN(az);
  zmat[0][2] := 0;
  zmat[0][3] := 0;
  zmat[1][0] := SIN(az);
  zmat[1][1] := COS(az);
  zmat[1][2] := 0;
  zmat[1][3] := 0;
  zmat[2][0] := 0;
  zmat[2][1] := 0;
  zmat[2][2] := 1;
  zmat[2][3] := 0;
  zmat[3][0] := 0;
  zmat[3][1] := 0;
  zmat[3][2] := 0;
  zmat[3][3] := 1;
  Result := zmat;
end;

function D3D_RotationMatrix(ax, ay, az: float): T4DMatrix;
var
  matrix, xmat, ymat, zmat, mat1, mat2: T4DMatrix;
begin
  xmat := D3D_RotationMatrixX(ax);
  ymat := D3D_RotationMatrixX(ay);
  zmat := D3D_RotationMatrixX(az);

  matrix := D3D_IdentityMatrix();
  mat1 := D3D_MatrixMult(matrix, ymat);
  mat2 := D3D_MatrixMult(mat1, xmat);
  Result := D3D_MatrixMult(mat2, zmat);
end;

// http://vernier.frederic.free.fr/Teaching/IGSD/Rendu3D_2_Transformations.pdf
function D3D_Projection(angleOfView, nearField, farField, aspect: float): T4DMatrix;
var
  M: T4DMatrix;
  r, l, b, t: float;
begin
  // set the basic projection matrix
  t := nearField * tan(angleOfView * 0.5 * pi / 180);
  r := t * aspect;
  l := -r;
  b := -t;

  M := D3D_ZeroMatrix();
  M[0][0] := (2 * nearField) / (r - l);
  M[0][2] := (r + l) / (r - l);
  M[1][1] := (2 * nearField) / (t - b);
  M[1][2] := (t + b) / (t - b);
  M[2][2] := -(farField + nearField) / (farField - nearField);
  M[2][3] := -(2 * farField * nearField) / (farField - nearField);
  M[3][2] := -1;
  Result := M;
end;

end.
