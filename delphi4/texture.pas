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

unit texture;

interface

uses windows, unite_3d, graphics, mainfrm,  sysutils, dialogs, vector;



var
 vO, vH, vV:T3DPoint;
 P, N, M:T3DPoint;
 A, B, C, D:integer;

 v3d_texture:array[0..10] of TBitmap;
 v3d_texarray:array[0..10] of ^PInteger;
 v3d_texcount:integer=0;
 v3d_bitcount:integer;
  _offset:integer;
procedure TEX_HLine(x1,x2,y,col:TColor; usetexture:boolean; texindex:integer);
procedure TEX_Setup(var polygon:TPolygon; var obj:TObject_t);
procedure ENG3D_SetPlane(Polygon:TPolygon);

procedure TEX_AddTexture(filename:string);
procedure TEX_AddTextureFromRessource(resname:string);

implementation

procedure TEX_AddTexture(filename:string);
var
 x, y:integer;
 idx:integer;
begin
   if (fileexists(filename)=false) then exit;

   v3d_texture[v3d_texcount] := TBitmap.Create;
   v3d_texture[v3d_texcount].LoadFromFile(filename);
   getmem(v3d_texarray[v3d_texcount], v3d_texture[v3d_texcount].Height*v3d_texture[v3d_texcount].Width*sizeof(integer));
   fillchar(v3d_texarray[v3d_texcount], v3d_texture[v3d_texcount].Height*v3d_texture[v3d_texcount].Width*sizeof(integer), 0);
   for y:=0 to v3d_texture[v3d_texcount].height-1 do
     for x:=0 to v3d_texture[v3d_texcount].width-1 do
         v3d_texarray[v3d_texcount][y*v3d_texture[v3d_texcount].height+x] := v3d_texture[v3d_texcount].Canvas.Pixels[x, y];
   inc(v3d_texcount);
end;

procedure TEX_AddTextureFromRessource(resname:string);
var
 x, y:integer;
 idx:integer;
 r, v, b:byte;

begin
   v3d_texture[v3d_texcount] := TBitmap.Create;
   v3d_texture[v3d_texcount].LoadFromResourceName(HInstance, resname);

     getmem(v3d_texarray[v3d_texcount], v3d_texture[v3d_texcount].Height*v3d_texture[v3d_texcount].Width*sizeof(TColor));
   for y:=0 to v3d_texture[v3d_texcount].height-1 do
     for x:=0 to v3d_texture[v3d_texcount].width-1 do
     begin
         idx := colortorgb(v3d_texture[v3d_texcount].Canvas.Pixels[x, y]);

         r := (idx and $FF0000) shr 16;
         v := (idx and $00FF00) shr 08;
         b := (idx and $FF);

                  idx := rgb(r, v, b);
                  v3d_texarray[v3d_texcount][y*v3d_texture[v3d_texcount].height+x] := idx;


     end;
   inc(v3d_texcount);
end;

procedure ENG3D_SetPlane(Polygon:TPolygon);
begin

   //Initialize texture vectors
   P:=Polygon.P.Aligned;
   M:=D3D_VecDifference(Polygon.M.Aligned,Polygon.P.Aligned);
   N:=D3D_VecDifference(Polygon.N.Aligned,Polygon.P.Aligned);

   P.x:=P.x*v3d_Focal_Distance;
   P.y:=P.y*v3d_Focal_Distance;

   M.x:=M.x*v3d_Focal_Distance;
   M.y:=M.y*v3d_Focal_Distance;

   N.x:=N.x*v3d_Focal_Distance;
   N.y:=N.y*v3d_Focal_Distance;

   vH.x:=(N.y*P.z-N.z*P.y)*(1/1640);
   vV.x:=(N.z*P.x-N.x*P.z)*(1/1640);
   vO.x:=(N.x*P.y-N.y*P.x)*(1/1640);

   vH.z:=(M.z*N.y-M.y*N.z)*(1/1640);
   vV.z:=(M.x*N.z-M.z*N.x)*(1/1640);
   vO.z:=(M.y*N.x-M.x*N.y)*(1/1640);

   vH.y:=(M.y*P.z-M.z*P.y)*(1/1640);
   vV.y:=(M.z*P.x-M.x*P.z)*(1/1640);
   vO.y:=(M.x*P.y-M.y*P.x)*(1/1640);
end;


procedure TEX_HLine(x1,x2,y,col:TColor; usetexture:boolean; texindex:integer);
var

  color:TColor;
  i, j, u, v:integer;
  va, vb, vc:integer;
  Hx, Hy, Hz:integer;
  z_, dz_:double;
  len:integer;
  z, dz:double;
begin

   if (x1<0) then x1 := 0;
   if (y<0)  then y := 0;
   if (x2<0) then x2 := 0;
   if (x1>=DownRight.x) then x1 := DownRight.x-1;
   if (x2>=DownRight.x) then x2 := DownRight.x-1;
   if (y>=DownRight.y)  then y := DownRight.y-1;
   if (usetexture = true) and (v3d_texture[texindex]=nil) then exit;


   //Initialize screen offset
   _offset:=y*DownRight.x+x1;
//   if _offset >= v3d_zbufsize then exit;


   //Screen Coordinates
   i:=x1-v3d_origine.x;
   j:=y-v3d_origine.y;

   //Texture Mapping Coordinates
   u:=0;
   v:=0;

   len:=x2-x1;
  // if(len=0) then exit;

   //Initialize Magic Coordinates
   va:=round(-(vO.x+(vV.x*j)+(vH.x*i))*v3d_texwidth);
   vb:=round((vO.y+(vV.y*j)+(vH.y*i))*v3d_texheight);
   vc:=round((vO.z+(vV.z*j)+(vH.z*i)));

   //Initialize 1/z value (z: 1/z)
   if D=0 then D:=1;
   dz_:=(((A / v3d_focal_distance) / -D));
   z_:=(((dz_*i)+( (B*j/v3d_Focal_Distance) + C) /-D));
   dz:=(dz_*(1 shl 20));

   z:=(z_*(1 shl 20));

   Hx:=round(vH.x)*-v3d_texwidth;
   Hy:=round(vH.y)*v3d_texheight;
   Hz:=round(vH.z);

   for i:=0 to len do
   begin
     if _offset>=v3d_zbufsize then exit;
     if((z)<v3d_zbuffer[_offset]) then
       begin
            if vc=0 then vc := 1;
            u:=round(va / vc);
            v:=round(vb / vc);

            color := col;

            if (usetexture = true) and (v3d_usetextures=true) then begin
            //  if ((u>64) or  (u<-64)) or ((v<-64) or (v>64)) then exit;
              color:=v3d_texarray[texindex][(64-(v mod 64))*64+(u mod 64)];
            end;

               if v3d_bitcount = 32 then
               begin
                   ptrSurface[y*(SurfaceStruct.dwLinearSize)+x1*4+i*4+0] := color and $FF;
                   ptrSurface[y*(SurfaceStruct.dwLinearSize)+x1*4+i*4+1] := (color shr 8)  and $FF;
                   ptrSurface[y*(SurfaceStruct.dwLinearSize)+x1*4+i*4+2] := (color shr 16) and $FF;
                   ptrSurface[y*(SurfaceStruct.dwLinearSize)+x1*4+i*4+3] := 0;
               end;

               if v3d_bitcount = 16 then
               begin
                   ptrSurface[y*(SurfaceStruct.dwLinearSize)+x1*2+i*2] := ((color shr 3) and $1F) + ((color shr 5) and $E0);
                   ptrSurface[y*(SurfaceStruct.dwLinearSize)+x1*2+i*2+1] := ((color shr 13)and $7) +((color shr 16) and $F8);
               end;

             v3d_zbuffer[_offset] := round(z);
     end;

     va:=Hx+va;
     vb:=Hy+vb;
     vc:=Hz+vc;

     //if dz_ = 0 then dz_:=0.001;
     z:=z+dz;
     inc(_offset);

   end;  //fin du for
end;

procedure TEX_Setup(var Polygon:Tpolygon; var Obj:TObject_t);
begin
   Polygon.P.Local:=P3D(obj.Vertex[Polygon.Vertex[1]].Local.x,
                        obj.Vertex[Polygon.Vertex[1]].local.y,
                        Obj.Vertex[Polygon.Vertex[1]].Local.z);
   Polygon.M.Local:=P3D(Obj.Vertex[Polygon.Vertex[0]].Local.x,
                        Obj.Vertex[Polygon.Vertex[0]].Local.y,
                        Obj.Vertex[Polygon.Vertex[0]].Local.z);
   Polygon.N.Local:=P3D(Obj.Vertex[Polygon.Vertex[2]].Local.x,
                        Obj.Vertex[Polygon.Vertex[2]].Local.y,
                        Obj.Vertex[Polygon.Vertex[2]].Local.z);
end;

end.
