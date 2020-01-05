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


{*******************************************************************************
  Unite de gestion de la 3D
  Remarque: toutes les valeurs d'angle doivent être exprimees en RADIANS !
*******************************************************************************}

unit unite_3d;

interface

uses Math, sysutils, graphics, matrix, vector;

type


  TVertex = record
     local:T3DPoint;
     world:T3DPoint;
     aligned:T3DPoint;
     screen:T2Dpoint;
  end;

  TView = record
     position:T3DPoint;
     angle:T3DPoint;
  end;

  TPolygon2D = record
    PointsCount:integer;
    color:TColor;
    texture:integer;
    usetexture:boolean;
    Points:array [0..20] of T2DPoint;
  end;

  PVertex = array [0..MaxInt DIV sizeof(TVertex)-1] of TVertex;
  PInteger = array [0..MaxInt DIV sizeof(Integer)-1] of Integer;
  PByte = array [0..MaxInt DIV sizeof(Integer)-1] of byte;

  TPolygon = record
     Count:integer;
     Vertex:^PInteger;
     Texture:integer;
     color:TColor;
     usetexture:boolean;
     P,M,N:TVertex;
  end;

  TCPolygon = record
    Count:integer;
    Vertex:array[0..20] of T3DPoint;
  end;



  PPolygon = array [0..400] of TPolygon;

  TObjectClassType = (objCube, objSphere, objPlane);
  TObjectClass = set of TObjectClassType;

  TPhysParams=record
     moment_inertie:double;

     acceleration:T3DPoint;
     vitesse:T3DPoint;   //Vecteur vitesse
     masse:double;
     s_force:T3DPoint;   //Somme des forces appilquees sur le centre d'inertie

     forces_torsion:T3DPoint;
     moment_torsion:T3DPoint;
     acceleration_angulaire:T3DPoint;
     vitesse_angulaire:T3DPoint;
     frottements:double;
   end;

  TObject_initcond = record
    physparams:TPhysparams;
    Position:T3DPoint;
    Angle:T3DPoint;
    Angle_mat:T3DMatrix;
  end;

  TObject_t = record
   VertexCount:integer;
   PolygonCount:integer;
   Vertex:^PVertex;
   Polygon:^PPolygon;
   Scaling:T3DPoint;
   Position:T3DPoint;
   Angle:T3DPoint;
   Angle_mat:T3DMatrix;
   object_name:string[20];
   object_class:TObjectClass;
   initial_cond:TObject_initcond;

   physparam:TPhysParams;
   rayon:double;                //Rayon d'une sphere
  end;

  PObject = array [0..400] of ^TObject_t;
  PColor = array[0..sizeof(TColor)*10000] of TColor;

  TWorld = record
     object_count:integer;
     objects:^PObject;
  end;

  //Modes de projection disponibles
  TProjectMode = (pmParallele, pmIsometrique, pmPerspective);
  TProjectModeVar = set of TProjectMode;

var
  //Variables globales de l'unite.
  //Doivent être initialisees avant tout appel de fonctions 3D.

  v3d_origine:T2DPoint;                     //Origine de l'ecran
  v3d_focal_distance:double;
  v3d_view:TView;
  v3d_world:TWorld;

  v3d_delta_t:double;

  v3d_zbuffer:^PInteger;
  v3d_zbufsize:integer;

  v3d_texheight:integer;
  v3d_texwidth:integer;

  TopLeft:T2DPoint;
  DownRight:T2DPoint;

  v3d_usezbuf:boolean=false;
  v3d_usetextures:boolean=false;
  v3d_fildefer:boolean=false;
  v3d_simulation:boolean=false;




//**********************************************************************************************/
procedure D3D_InitEngine;

procedure D3D_Project(var vertex:TVertex);

procedure W3D_SetVert(var Polygon:TPolygon; Num:integer;  params:array of integer);
procedure W3D_InitPolygon(var Polygon:TPolygon; NumVertices:integer);
procedure W3D_InitObject(var Obj:TObject_t; NumVertices, NumPolygons:integer);
function W3D_CreateObject:integer;
procedure W3D_Draw;

procedure D3D_DestroyEngine;



//**********************************************************************************************/

implementation

uses mainfrm,  clipping, texture, objinsp, physique;


procedure D3D_Project(var vertex:TVertex);
begin
  if(Vertex.Aligned.z = 0) then
    Vertex.Aligned.z:=1;
   Vertex.Screen.x := round(v3d_focal_distance * Vertex.Aligned.x / Vertex.Aligned.z + v3d_origine.x);
   Vertex.Screen.y := round(v3d_focal_distance * Vertex.Aligned.y / Vertex.Aligned.z + v3d_origine.y);
end;

///////////////////////////////////////////////////////
//Cree un objet
function W3D_CreateObject:integer;
begin
     getmem(v3d_world.objects[v3d_world.object_count], sizeof(TObject_t));
     inc(v3d_world.object_count);
     result := v3d_world.object_count-1;
end;


procedure W3D_SetVert(var Polygon:TPolygon; Num:integer;  params:array of integer);
var
 v:integer;
begin
   W3D_InitPolygon(polygon, 4);
   for v:=0 to Num-1 do
       Polygon.Vertex[v]:=params[v];
end;

procedure W3D_InitPolygon(var Polygon:TPolygon; NumVertices:integer);
begin
    Polygon.Count:=NumVertices;
   GetMem(Polygon.Vertex, sizeof(integer)*Polygon.Count);
end;

procedure W3D_InitObject(var Obj:TObject_t; NumVertices, NumPolygons:integer);
begin
   Obj.VertexCount:=NumVertices;
   getmem(Obj.Vertex, sizeof(TVertex)*Obj.VertexCount);

   Obj.PolygonCount:=NumPolygons;
   getmem(Obj.Polygon,sizeof(TPolygon)*Obj.PolygonCount);
end;



////////////////////////////////////////////////////////////////////////////////
function MinPoint(a, b: T2DPoint):T2DPoint;
begin
    if a.y < b.y then
      result := a
    else result := b;
end;

function MaxPoint(a, b: T2DPoint):T2DPoint;
begin
    if a.y > b.y then
      result := a
    else result := b;
end;

function MinPoint3(a, b, c:T2DPoint):T2DPoint;
begin
    result := MinPoint(MinPoint(a,b), MinPoint(b,c));
end;

function MidPoint3(a, b, c:T2DPoint):T2DPoint;
begin
    result := MaxPoint(MinPoint(a,b), MinPoint(a,c));
end;

function MaxPoint3(a, b, c:T2DPoint):T2DPoint;
begin
    result := MaxPoint(MaxPoint(a,b), MaxPoint(b,c));
end;
////////////////////////////////////////////////////////////////////////////////

procedure W3D_PolyTriangle(p1, p2, p3:T2DPoint; c:TColor; usetexture:boolean; texindex:integer);
var
   p1d,p2d,p3d:T2DPoint;
   xd1,yd1,xd2,yd2:integer;
   Lx,Rx:integer;
   i:integer;

begin
   form1.dxdraw.surface.Canvas.Pen.Color := c;

   p1d := MinPoint3(p1,p2,p3);
   p2d := MidPoint3(p2,p3,p1);
   p3d := MaxPoint3(p3,p1,p2);

   if (p2.y < p1.y) then begin
      p1d:=MinPoint3(p2,p1,p3);
      p2d:=MidPoint3(p1,p3,p2);
   end;

   xd1:=((p2d.x-p1d.x));
   yd1:=(p2d.y-p1d.y);
   xd2:=(p3d.x-p1d.x);
   yd2:=(p3d.y-p1d.y);

   if yd1<>0 then
   begin
      for i:=(p1d.y) to (p2d.y) do
      begin
       Lx := ((p1d.x) + ((i - (p1d.y)) * xd1) div yd1);
          Rx := ((p1d.x) + ((i - (p1d.y)) * xd2) div yd2);

          if (Lx<>Rx) then
          begin
             if v3d_usezbuf = true then
               TEX_HLine(MIN(Lx,Rx),MAX(Lx,Rx),i,c, usetexture, texindex)
             else begin
              form1.DXDraw.surface.canvas.MoveTo((MIN(Lx,Rx)),i);
              form1.DXDraw.surface.canvas.LineTo((MAX(Lx,Rx)),i);
             end;
          end;

      end;
   end;

   xd1:=(p3d.x-p2d.x);
   yd1:=(p3d.y-p2d.y);

   if (yd1<>0) then
   begin
      for i:=(p2d.y) to (p3d.y) do
      begin
         Lx :=   (p1d.x   + ((i   - p1d.y)   * xd2) div yd2);
         Rx :=   (p2d.x   + ((i   - p2d.y)   * xd1) div yd1);
         if(Lx<>Rx) then
         begin

            if v3d_usezbuf=true then
               Tex_HLine(MIN(Lx,Rx),MAX(Lx,Rx),i,c, usetexture, texindex)
            else begin
                 form1.DXDraw.surface.canvas.MoveTo((MIN(Lx,Rx)),i);
                 form1.DXDraw.surface.canvas.LineTo((MAX(Lx,Rx)),i);
            end;
         end;
      end;

   end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure W3D_DrawPolygonEX(polygon:TPolygon2D);
var
 i:integer;
 P1, P2, P3:T2DPoint;

begin
  P1 := Polygon.Points[0];

  for i:=1 to polygon.PointsCount-2 do
  begin
       P2:=Polygon.Points[i];
       P3:=Polygon.Points[i+1];


       if v3d_fildefer = true then
       begin
            form1.DXDraw.surface.canvas.brush.Color := clLime;
            form1.DXDraw.surface.canvas.MoveTo(p1.x, p1.y);
            form1.DXDraw.surface.canvas.LineTo(p2.x, p2.y);
            form1.DXDraw.surface.canvas.LineTo(p3.x, p3.y);
            form1.DXDraw.surface.canvas.LineTo(p1.x, p1.y);
      end else
       W3D_PolyTriangle(P1,P2,P3,Polygon.color, polygon.usetexture, polygon.texture);
  end;

end;

////////////////////////////////////////////////////////////////////////////////
procedure W3D_Draw_Object(objnum:integer);
var
  matrix, tmpmat:T3DMatrix;
  v3d_vitang_mat:T3DMatrix;
  o, p,q, r:integer;
  tmp, tmp1:T3DPoint;

  x1, x2, x3, y1, y2, y3, z1, z2, z3:double;

  poly2D:TPolygon2D;
  poly2D_clipped:TPolygon2D;
  p3D_clipped:TCPolygon;

begin

 //Creation de la matrice d'identite
 D3D_IdentityMatrix(matrix);

 //Mise a l'echelle de la matrice globale avec l'echelle de l'objet
 TR_Scale(matrix, v3d_world.objects[objnum].Scaling.x,
 v3d_world.objects[objnum].Scaling.y,
 v3d_world.objects[objnum].Scaling.z);

 //Rotation de la matrice suivant l'angle de rotation de l'objet
 //Si c'est un objet fixe, on le fait tourner normalement, sinon
 //on utilise son vecteur vitesse angulaire

 if (not (v3d_world.objects[objnum].object_class = [objPlane]))
 and (not (v3d_world.objects[objnum].object_class = [objCube]))
 and (v3d_simulation)then begin

  D3D_MatrixMult(v3d_world.objects[objnum].Angle_mat, matrix, matrix);
 end
 else begin
      D3D_MRotate(matrix, v3d_world.objects[objnum].Angle.x,
   v3d_world.objects[objnum].Angle.y,v3d_world.objects[objnum].Angle.z);
 end;

 //Translation de la matrice dans l'espace suivant les coordonnees du centre de l'objet
 D3D_Translate(matrix, v3d_world.objects[objnum].position.x,
 v3d_world.objects[objnum].position.y,
 v3d_world.objects[objnum].position.z);

 //On multiplie les coordonnees locales de l'objet par cette matrice pour
 //obtenir ses coordonnees par rapport a l'origine du monde
 for o:=0 to v3d_world.objects[objnum].VertexCount - 1 do begin
   D3D_VecMultMatrix(v3d_world.objects[objnum].Vertex[o].local, matrix, tmp);
   v3d_world.objects[objnum].Vertex[o].world := tmp;
 end;

     //Il faut faire de même pour les coordonnees des textures
     for o:=0 to v3d_world.objects[objnum].PolygonCount-1 do
     begin
         D3D_VecMultMatrix(v3d_world.objects[objnum].Polygon[o].P.Local,matrix,tmp);
         v3d_world.objects[objnum].Polygon[o].P.World := tmp;
         D3D_VecMultMatrix(v3d_world.objects[objnum].Polygon[o].M.Local,matrix,tmp);
         v3d_world.objects[objnum].Polygon[o].M.World := tmp;
         D3D_VecMultMatrix(v3d_world.objects[objnum].Polygon[o].N.Local,matrix,tmp);
         v3d_world.objects[objnum].Polygon[o].N.World := tmp;
     end;

 //On projette le tout sur l'ecran, pour cela il faut:

 //Recreer un matrice d'identite
 D3D_IdentityMatrix(matrix);

 //Translater par rapport a la position du spectateur
 D3D_Translate(matrix, v3d_view.position.x, v3d_view.position.y, v3d_view.position.z);
 D3D_MRotate(matrix, v3d_view.angle.x, v3d_view.angle.y, v3d_view.angle.z);


 //Multiplier les coordonnees mondiales de l'objet par cette matrice pour
 //obtenir les coordonnees par rapport a l'origine de l'observateur
 for o:=0 to v3d_world.objects[objnum].VertexCount - 1 do begin
   D3D_VecMultMatrix(v3d_world.objects[objnum].Vertex[o].world, matrix, tmp);
   v3d_world.objects[objnum].Vertex[o].aligned := tmp;
 end;

      //Il faut faire de même pour les coordonnees des textures
     for o:=0 to v3d_world.objects[objnum].PolygonCount-1 do
     begin
        D3D_VecMultMatrix(v3d_world.objects[objnum].Polygon[o].P.World,matrix,tmp);
         v3d_world.objects[objnum].Polygon[o].P.Aligned := tmp;
         D3D_VecMultMatrix(v3d_world.objects[objnum].Polygon[o].M.World,matrix,tmp);
         v3d_world.objects[objnum].Polygon[o].M.Aligned := tmp;
         D3D_VecMultMatrix(v3d_world.objects[objnum].Polygon[o].N.World,matrix,tmp);
         v3d_world.objects[objnum].Polygon[o].N.Aligned := tmp;
     end;


  //On peut maintenant dessiner les polygones
  for p:=0 to v3d_world.objects[objnum].PolygonCount-1 do
  begin
      r := objnum;
      ENG3D_SetPlane(v3d_world.objects[objnum].Polygon[p]);
      try
       p3d_clipped.Count := v3d_world.objects[objnum].Polygon[p].Count;
       for o:=0 to v3d_world.objects[objnum].Polygon[p].Count - 1 do begin

           //if (objnum>v3d_world.object_count-1) then exit;
           p3d_clipped.Vertex[o] := v3d_world.objects[r].vertex[v3d_world.objects[r].Polygon[p].vertex[o]].aligned;
           if p3D_clipped.vertex[o].z > 0 then
            exit;
       end;

     except
         exit;
     end;
       //CLIP_Z(v3d_world.objects[objnum].polygon[p], v3d_world.objects[objnum]^, p3D_clipped);
      // p3d_clipped.Count := v3d_world.objects[objnum].Polygon[p].Count;
       x1 := p3D_clipped.vertex[0].x;
       x2 := p3D_clipped.vertex[1].x;
       x3 := p3D_clipped.vertex[2].x;
       y1 := p3D_clipped.vertex[0].y;
       y2 := p3D_clipped.vertex[1].y;
       y3 := p3D_clipped.vertex[2].y;
       z1 := p3D_clipped.vertex[0].z;
       z2 := p3D_clipped.vertex[1].z;
       z3 := p3D_clipped.vertex[2].z;

        A:=round(y1*(z2-z3)+y2*(z3-z1)+y3*(z1-z2));
        B:=round(z1*(x2-x3)+z2*(x3-x1)+z3*(x1-x2));
        C:=round(x1*(y2-y3)+x2*(y3-y1)+x3*(y1-y2));
        D:=round(-x1*(y2*z3-y3*z2)-x2*(y3*z1-y1*z3)-x3*(y1*z2-y2*z1));

       for o:=0 to 20 do begin
         poly2D.Points[o].x := 0;
         poly2D.Points[o].y := 0;
       end;

       if D>=0 then begin

          //Si la face est visible, on la projette a l'ecran
          M3D_Project(p3D_clipped, poly2d, round(v3d_focal_distance));

          poly2d.PointsCount := v3d_world.objects[objnum].Polygon[p].Count;
          poly2d.color := v3d_world.objects[objnum].Polygon[p].color;
          poly2d.usetexture := v3d_world.objects[objnum].Polygon[p].usetexture;
          poly2d.texture := v3d_world.objects[objnum].Polygon[p].Texture;
          poly2d_clipped := poly2d;

          //On realise le clipping du polygone 2D - Il ne faut pas dessiner
          //a l'exterieur de l'ecran ! (perte considerable de vitesse)
          CLIP_Polygon(poly2d, poly2D_clipped);

          if poly2D_clipped.PointsCount <> 0 then
             W3D_DrawPolygonEX(poly2D_clipped)
          else
             W3D_DrawPolygonEX(poly2D);

       end;
  end;


end;


procedure W3D_Draw;
var
 i:integer;
begin

 for i:=0 to v3d_world.object_count-1 do
 begin
     if v3d_world.objects[i] <> nil then
     begin

      if (v3d_world.objects[i].object_class <> [objPlane]) and
      (v3d_world.objects[i].object_class <> [objCube]) then begin

      if v3d_simulation then
         phys_docomputepos(i, v3d_delta_t);
      end;
      try
       W3D_Draw_Object(i);
      except
         exit;
      end;
     end;
 end;

end;
//*****************************************************************************/
//Initialisation du moteur 3D
procedure D3D_InitEngine;
begin
     v3d_focal_distance := 500;

     getmem(v3d_world.objects, sizeof(pointer)*400);
     fillchar(v3d_world.objects^, sizeof(pointer)*400-1, 0);

     TopLeft.x := 0;
     TopLeft.y := 0;
     form1.DXDraw.Initialize;
     DownRight.x := form1.dxdraw.surfacewidth;
     DownRight.y := form1.DXDraw.surfaceHeight;

     //Allocation de la memoire pour le ZBuffer
     v3d_texheight := 64;//Form1.Image1.Picture.Bitmap.Height;
    v3d_texwidth := 64;//Form1.Image1.Picture.Bitmap.Width;
     v3d_usezbuf := true;
end;


procedure D3D_DestroyEngine;
var
 i, j:integer;
begin
   Form1.DXDraw.Finalize;
   while (form1.isdrawing = true) do ;

   for i:=0 to v3d_world.object_count-1 do
   begin
      for j:=0 to v3d_world.objects[i].PolygonCount-1 do
      begin
           fillchar(v3d_world.objects[i].Polygon[j].Vertex^,
          v3d_world.objects[i].polygon[j].Count*4,0);
          freemem(v3d_world.objects[i].Polygon[j].Vertex,
          v3d_world.objects[i].polygon[j].Count*4);
      end;

      freemem(v3d_world.objects[i].Vertex,  sizeof(TVertex)*v3d_world.objects[i].VertexCount);

      fillchar(v3d_world.objects[i]^, sizeof(TObject_t), 0);
      freemem(v3d_world.objects[i], sizeof(TObject_t));
   end;

   freemem(v3d_world.objects, sizeof(pointer)*400);
   fillchar(v3d_world, sizeof(TWorld),0);
   frmObjInspector.DestroyProps;
end;




end.

