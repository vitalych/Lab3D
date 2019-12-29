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

unit clipping;

interface

uses unite_3d, math, vector;

var
 TmpPoly:TPolygon2D;
 ZMin:integer=20;


procedure CLIP_Left(var Polygon:TPolygon2D; V1,V2:T2DPoint);
procedure CLIP_Right(var Polygon:TPolygon2D; V1,V2:T2DPoint);
procedure CLIP_Top(var Polygon:TPolygon2D; V1,V2:T2DPoint);
procedure CLIP_Bottom(var Polygon:TPolygon2D; V1,V2:T2DPoint);
procedure CLIP_Polygon(var Polygon:TPolygon2D;var Clipped:TPolygon2D);

procedure CLIP_Front(var Polygon:TCPolygon; V1, V2:T3DPoint);
procedure CLIP_Z(var Polygon:TPolygon; var Obj:TObject_t; var ZClipped:TCPolygon);

procedure M3D_Project(var Polygon:TCPolygon; var Clipped:TPolygon2D;focaldistance:integer);

implementation

procedure CLIP_Left(var Polygon:TPolygon2D; V1,V2:T2DPoint);
var
  dx,dy, m:double;
begin

   m:=1;

   dx:=V2.x-V1.x;  dy:=V2.y-V1.y;
   if (dx<>0) then m:=dy/dx;



   // ************OK************
   if ((V1.x>=TopLeft.x) and (V2.x>=TopLeft.x) ) then begin
      Polygon.Points[Polygon.PointsCount]:=V2;
       inc(Polygon.PointsCount);
   end;
   // *********LEAVING**********
   if ( (V1.x>=TopLeft.x) and (V2.x<TopLeft.x) ) then
   begin
      Polygon.Points[Polygon.PointsCount].x:=TopLeft.x;
      Polygon.Points[Polygon.PointsCount].y:=round(V1.y+m*(TopLeft.x-V1.x));
      inc(Polygon.PointsCount);
   end;
   // ********ENTERING*********
   if ( (V1.x<TopLeft.x) and (V2.x>=TopLeft.x) )  then
   begin
      Polygon.Points[Polygon.PointsCount].x:=TopLeft.x;
      Polygon.Points[Polygon.PointsCount].y:=round(V1.y+m*(TopLeft.x-V1.x));
      inc(Polygon.PointsCount);
      Polygon.Points[Polygon.PointsCount]:=V2;
      inc(Polygon.PointsCount);
   end;
end;

procedure CLIP_Right(var Polygon:TPolygon2D; V1,V2:T2DPoint);
var
  dx,dy, m:double;
begin

   m:=1;
    dx:=V2.x-V1.x;  dy:=V2.y-V1.y;
   if (dx<>0) then m:=dy/dx;

   // ************OK************
   if ( (V1.x<=DownRight.x) and (V2.x<=DownRight.x) ) then begin

      Polygon.Points[Polygon.PointsCount]:=V2;   inc(Polygon.PointsCount);
   end;
   // *********LEAVING**********
   if ( (V1.x<=DownRight.x) and (V2.x>DownRight.x) ) then
   begin
      Polygon.Points[Polygon.PointsCount].x:=DownRight.x;
      Polygon.Points[Polygon.PointsCount].y:=round(V1.y+m*(DownRight.x-V1.x));
     inc(Polygon.PointsCount);
   end;
   // ********ENTERING*********
   if ( (V1.x>DownRight.x) and (V2.x<=DownRight.x) ) then
   begin
      Polygon.Points[Polygon.PointsCount].x:=DownRight.x;
      Polygon.Points[Polygon.PointsCount].y:=round(V1.y+m*(DownRight.x-V1.x));
      inc(Polygon.PointsCount);
      Polygon.Points[Polygon.PointsCount]:=V2;
      inc(Polygon.PointsCount);
   end;
end;

procedure CLIP_Top(var Polygon:TPolygon2D; V1,V2:T2DPoint);
var
  dx,dy, m:double;
begin

   m:=1;

    dx:=V2.x-V1.x;  dy:=V2.y-V1.y;
   if (dx<>0) then m:=dy/dx;

   // ************OK************
   if ( (V1.y>=TopLeft.y) and (V2.y>=TopLeft.y) ) then begin

      Polygon.Points[Polygon.PointsCount]:=V2;   inc(Polygon.PointsCount);
   end;
   // *********LEAVING**********
   if ( (V1.y>=TopLeft.y) and (V2.y<TopLeft.y) )  then
   begin
      if(dx<>0) then
         Polygon.Points[Polygon.PointsCount].x:=round(V1.x+(TopLeft.y-V1.y)/m)
      else
         Polygon.Points[Polygon.PointsCount].x:=V1.x;

      Polygon.Points[Polygon.PointsCount].y:=TopLeft.y;
      inc(Polygon.PointsCount);
   end;
   // ********ENTERING*********
   if ( (V1.y<TopLeft.y) and (V2.y>=TopLeft.y) ) then
   begin
      if(dx<>0) then
         Polygon.Points[Polygon.PointsCount].x:=round(V1.x+(TopLeft.y-V1.y)/m)
      else
         Polygon.Points[Polygon.PointsCount].x:=V1.x;

      Polygon.Points[Polygon.PointsCount].y:=TopLeft.y;
      inc(Polygon.PointsCount);
      Polygon.Points[Polygon.PointsCount]:=V2;
      inc(Polygon.PointsCount);
   end;
end;

procedure CLIP_Bottom(var Polygon:TPolygon2D; V1,V2:T2DPoint);
var
  dx,dy, m:double;
begin

   m:=1;

   dx:=V2.x-V1.x;  dy:=V2.y-V1.y;
   if (dx<>0) then m:=dy/dx;

   // ************OK************
   if ( (V1.y<=DownRight.y) and (V2.y<=DownRight.y) )   then begin

      Polygon.Points[Polygon.PointsCount]:=V2;   inc(Polygon.PointsCount);
   end;
   // *********LEAVING**********
   if ( (V1.y<=DownRight.y) and (V2.y>DownRight.y) ) then
   begin
      if(dx<>0) then
         Polygon.Points[Polygon.PointsCount].x:=round(V1.x+(DownRight.y-V1.y)/m)
      else
         Polygon.Points[Polygon.PointsCount].x:=V1.x;

      Polygon.Points[Polygon.PointsCount].y:=DownRight.y;
      inc(Polygon.PointsCount);
   end;
   // ********ENTERING*********
   if ( (V1.y>DownRight.y) and (V2.y<=DownRight.y) ) then
   begin
      if(dx<>0)  then
         Polygon.Points[Polygon.PointsCount].x:=round(V1.x+(DownRight.y-V1.y)/m)
      else
         Polygon.Points[Polygon.PointsCount].x:=V1.x;

      Polygon.Points[Polygon.PointsCount].y:=DownRight.y;
      inc(Polygon.PointsCount);
      Polygon.Points[Polygon.PointsCount]:=V2;
      inc(Polygon.PointsCount);
   end;
end;

//And now we must do the entire polygon clipping function,

procedure CLIP_Polygon(var Polygon:TPolygon2D;var Clipped:TPolygon2D);
var
   v,d:integer;
begin
   Clipped.PointsCount:=0;
   TmpPoly.PointsCount:=0;

   for v:=0 to Polygon.PointsCount-1 do
   begin
      d:=v+1;
      if (d=Polygon.PointsCount) then d:=0;
      CLIP_Left(TmpPoly, Polygon.Points[v],Polygon.Points[d]);
   end;
   for v:=0 to TmpPoly.PointsCount-1 do
   begin
      d:=v+1;
      if (d=TmpPoly.PointsCount) then d:=0;
      CLIP_Right(Clipped, TmpPoly.Points[v],TmpPoly.Points[d]);
   end;
   TmpPoly.PointsCount:=0;
   for v:=0 to  Clipped.PointsCount -1 do
   begin
      d:=v+1;
      if(d=Clipped.PointsCount) then d:=0;
      CLIP_Top(TmpPoly, Clipped.Points[v],Clipped.Points[d]);
   end;
   Clipped.PointsCount:=0;
   for v:=0 to TmpPoly.PointsCount-1 do
   begin
      d:=v+1;
      if (d=TmpPoly.PointsCount) then d:=0;
      CLIP_Bottom(Clipped, TmpPoly.Points[v],TmpPoly.Points[d]);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure CLIP_Front(var Polygon:TCPolygon; V1, V2:T3DPoint);
var
 dold,dnew, m:double;
begin
    m:=1;
  // ZMin := round(v3d_view.position.z);
   dold:=V2.z-V1.z;  dnew:=ZMin-V1.z;
   if(dold<>0) then m:=dnew/dold;

   if ( (V1.z>=ZMin) and (V2.z>=ZMin) ) then begin
      Polygon.Vertex[Polygon.Count]:=V2;
      inc(Polygon.Count);
   end;


   if ( (V1.z>=ZMin) and (V2.z<ZMin) ) then
   begin
      Polygon.Vertex[Polygon.Count   ].x:=V1.x + (V2.x-V1.x)*m;
      Polygon.Vertex[Polygon.Count   ].y:=V1.y + (V2.y-V1.y)*m;
      Polygon.Vertex[Polygon.Count   ].z:=ZMin;
      inc(Polygon.Count);
   end;

   if ( (V1.z<ZMin) and (V2.z>=ZMin) ) then
   begin
      Polygon.Vertex[Polygon.Count   ].x:=V1.x + (V2.x-V1.x)*m;
      Polygon.Vertex[Polygon.Count   ].y:=V1.y + (V2.y-V1.y)*m;
      Polygon.Vertex[Polygon.Count ].z:=ZMin;
            inc(Polygon.Count);
      Polygon.Vertex[Polygon.Count ]:=V2;
            inc(Polygon.Count);
   end;


end;


procedure CLIP_Z(var Polygon:TPolygon; var Obj:TObject_t; var ZClipped:TCPolygon);
var
 d, v:integer;
begin

   ZClipped.Count:=0;
   for v:=0 to Polygon.Count-1 do
   begin
      d:=v+1;
      if (d=Polygon.Count) then d:=0;
      CLIP_Front(ZClipped, Obj.Vertex[Polygon.Vertex[v]].Aligned,Obj.Vertex[Polygon.Vertex[d]].Aligned);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure M3D_Project(var Polygon:TCPolygon; var Clipped:TPolygon2D;focaldistance:integer);
var
 v:integer;
begin

    for v:=0 to Polygon.Count-1 do
   begin
      if (Polygon.Vertex[v].z=0) then Polygon.Vertex[v].z:=1;
         Clipped.Points[v].x:=round(Polygon.Vertex[v].x*focaldistance/
            Polygon.Vertex[v].z+v3d_origine.x);
         Clipped.Points[v].y:=round(Polygon.Vertex[v].y*focaldistance/
            Polygon.Vertex[v].z+v3d_origine.y);
   end;
   Clipped.PointsCount:=Polygon.Count;
end;



end.
