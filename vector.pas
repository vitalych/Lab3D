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

unit vector;

interface

uses math;

type
 //Coordonnees d'un vecteur 3D
  T3DVec = record
     x, y, z:extended;
  end;

  //Coordonnees d'un point 3D
  T3DPoint = record
     x, y, z:double;
  end;

  //Coordonnees d'un point 2D
  T2DPoint = record
     x, y:integer;
  end;


procedure D3D_VecMult(vec1:T3DPoint; vec2:T3DPoint; var dest:T3DPoint);
function D3D_DotProduct(vec1:T3DPoint; vec2:T3DPoint):double;
function D3D_MkVect(A, B:T3DPoint):T3DVec;
function D3D_VecNorme(vec:T3DPoint):double;
function D3D_VecProd(vec:T3DPoint; k:double):T3DPoint;
procedure D3D_VecAdd(var vec1:T3DPoint; vec2:T3DPoint);
function P3D(x, y, z:double):T3DPoint;
function D3D_VecDifference(Vector1, vector2:T3DPoint):T3DPoint;
procedure D3D_SetVecLength(var vec:T3DPoint; length:double);

implementation


procedure D3D_VecMult(vec1:T3DPoint; vec2:T3DPoint; var dest:T3DPoint);
begin
  dest.x := vec1.y * vec2.z - vec1.z * vec2.y;
  dest.y := vec1.z * vec2.x - vec1.x * vec2.z;
  dest.z := vec1.x * vec2.y - vec1.y * vec2.x;
end;

function D3D_DotProduct(vec1:T3DPoint; vec2:T3DPoint):double;
begin
    result := vec1.x*vec2.x + vec1.y*vec2.y + vec1.z*vec2.z;
end;

//On cree un vecteur a partir de deux points
function D3D_MkVect(A, B:T3DPoint):T3DVec;
begin
    result.x := B.x - A.x;
    result.y := B.y - A.y;
    result.z := B.z - A.z;
end;

//Calcule la norme d'un vecteur dans l'espace
function D3D_VecNorme(vec:T3DPoint):double;
begin
   result := sqrt(power(vec.x, 2)+ power(vec.y,2)+ power(vec.z,2));
end;

function D3D_VecProd(vec:T3DPoint; k:double):T3DPoint;
begin
    result.x := vec.x*k;
    result.y := vec.y*k;
    result.z := vec.z*k;
end;

procedure D3D_VecAdd(var vec1:T3DPoint; vec2:T3DPoint);
begin
    vec1.x := vec1.x + vec2.x;
    vec1.y := vec1.y + vec2.y;
    vec1.z := vec1.z + vec2.z;
end;

function D3D_VecDifference(Vector1, vector2:T3DPoint):T3DPoint;
begin
  result := P3D(Vector1.x-Vector2.x,Vector1.y-Vector2.y,Vector1.z-Vector2.z);
end;

function P3D(x, y, z:double):T3DPoint;
begin
   result.x := x;
   result.y := y;
   result.z := z;
end;

procedure D3D_SetVecLength(var vec:T3DPoint; length:double);
var
 ol:double;
begin
 ol := D3D_VecNorme(vec);
 if ol=0 then exit;
 vec.x := vec.x * (length/ol);
 vec.y := vec.y * (length/ol);
 vec.z := vec.z * (length/ol);
end;

end.
