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

uses Math, vector;

type
 T3DMatrix = array [0..3] of array [0..3] of Double;

procedure D3D_MatrixCopy(source:T3DMatrix; var dest:T3DMatrix);
procedure D3D_MatrixMult(mat1, mat2:T3DMatrix; var dest:T3DMatrix);
procedure D3D_VecMultMatrix(source:T3DPoint; matrix:T3DMatrix; var dest:T3DPoint);

procedure D3D_IdentityMatrix(var mat:T3DMatrix);
procedure D3D_Translate(var matrix:T3DMatrix; tx, ty, tz:double);
procedure TR_Scale(var matrix:T3DMatrix; sx, sy, sz:double);
procedure D3D_MRotate(var matrix:T3DMatrix; ax, ay, az:double);



var
   mat1, mat2:T3DMatrix;
implementation

uses unite_3d;



procedure D3D_MatrixCopy(source:T3DMatrix; var dest:T3DMatrix);
var
 i, j:integer;
begin
 for i:=0 to 3 do
   for j:=0 to 3 do
     dest[i][j] := source[i][j];
end;

procedure D3D_MatrixMult(mat1, mat2:T3DMatrix; var dest:T3DMatrix);
var
 i, j:integer;
begin
 for i:=0 to 3 do
   for j:=0 to 3 do
     dest[i][j] := mat1[i][0] * mat2[0][j] +
                   mat1[i][1] * mat2[1][j] +
                   mat1[i][2] * mat2[2][j] +
                   mat1[i][3] * mat2[3][j];
end;

procedure D3D_VecMultMatrix(source:T3DPoint; matrix:T3DMatrix; var dest:T3DPoint);
begin
    Dest.x:= Source.x*matrix[0][0]+
            Source.y*matrix[1][0]+
            Source.z*matrix[2][0]+
                     matrix[3][0];
    Dest.y:= Source.x*matrix[0][1]+
            Source.y*matrix[1][1]+
            Source.z*matrix[2][1]+
                     matrix[3][1];
    Dest.z:= Source.x*matrix[0][2]+
            Source.y*matrix[1][2]+
            Source.z*matrix[2][2]+
                     matrix[3][2];

end;



procedure D3D_IdentityMatrix(var mat:T3DMatrix);
begin
    mat[0][0]:=1; mat[0][1]:=0; mat[0][2]:=0; mat[0][3]:=0;
    mat[1][0]:=0; mat[1][1]:=1; mat[1][2]:=0; mat[1][3]:=0;
    mat[2][0]:=0; mat[2][1]:=0; mat[2][2]:=1; mat[2][3]:=0;
    mat[3][0]:=0; mat[3][1]:=0; mat[3][2]:=0; mat[3][3]:=1;
end;

procedure D3D_Translate(var matrix:T3DMatrix; tx, ty, tz:double);
var
 tmat:T3DMatrix;
begin
   tmat[0][0]:=1;  tmat[0][1]:=0;  tmat[0][2]:=0;  tmat[0][3]:=0;
   tmat[1][0]:=0;  tmat[1][1]:=1;  tmat[1][2]:=0;  tmat[1][3]:=0;
   tmat[2][0]:=0;  tmat[2][1]:=0;  tmat[2][2]:=1;  tmat[2][3]:=0;
   tmat[3][0]:=tx; tmat[3][1]:=ty; tmat[3][2]:=tz; tmat[3][3]:=1;
   D3D_MatrixMult(matrix,tmat,mat1);
   D3D_MatrixCopy(mat1,matrix);
end;

procedure TR_Scale(var matrix:T3DMatrix; sx, sy, sz:double);
var
   smat:T3DMatrix;
begin
   smat[0][0]:=sx; smat[0][1]:=0;  smat[0][2]:=0;  smat[0][3]:=0;
   smat[1][0]:=0;  smat[1][1]:=sy; smat[1][2]:=0;  smat[1][3]:=0;
   smat[2][0]:=0;  smat[2][1]:=0;  smat[2][2]:=sz; smat[2][3]:=0;
   smat[3][0]:=0;  smat[3][1]:=0;  smat[3][2]:=0;  smat[3][3]:=1;
   D3D_MatrixMult(matrix,smat,mat1);
   D3D_MatrixCopy(mat1,matrix);
end;

procedure D3D_MRotate(var matrix:T3DMatrix; ax, ay, az:double);
var
   xmat, ymat, zmat:T3DMatrix;
begin
   xmat[0][0]:=1;        xmat[0][1]:=0;        xmat[0][2]:=0;        xmat[0][3]:=0;
   xmat[1][0]:=0;        xmat[1][1]:=COS(ax);  xmat[1][2]:=SIN(ax);  xmat[1][3]:=0;
   xmat[2][0]:=0;        xmat[2][1]:=-SIN(ax); xmat[2][2]:=COS(ax);  xmat[2][3]:=0;
   xmat[3][0]:=0;        xmat[3][1]:=0;        xmat[3][2]:=0;        xmat[3][3]:=1;

   ymat[0][0]:=COS(ay);  ymat[0][1]:=0;        ymat[0][2]:=-SIN(ay); ymat[0][3]:=0;
   ymat[1][0]:=0;        ymat[1][1]:=1;        ymat[1][2]:=0;        ymat[1][3]:=0;
   ymat[2][0]:=SIN(ay);  ymat[2][1]:=0;        ymat[2][2]:=COS(ay);  ymat[2][3]:=0;
   ymat[3][0]:=0;        ymat[3][1]:=0;        ymat[3][2]:=0;        ymat[3][3]:=1;

   zmat[0][0]:=COS(az);  zmat[0][1]:=SIN(az);  zmat[0][2]:=0;        zmat[0][3]:=0;
   zmat[1][0]:=-SIN(az); zmat[1][1]:=COS(az);  zmat[1][2]:=0;        zmat[1][3]:=0;
   zmat[2][0]:=0;        zmat[2][1]:=0;        zmat[2][2]:=1;        zmat[2][3]:=0;
   zmat[3][0]:=0;        zmat[3][1]:=0;        zmat[3][2]:=0;        zmat[3][3]:=1;

   D3D_MatrixMult(matrix,ymat,mat1);
   D3D_MatrixMult(mat1,xmat,mat2);
   D3D_MatrixMult(mat2,zmat,matrix);
end;

end.
