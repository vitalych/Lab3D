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

unit objets3d;

interface

uses Math, DXClass, DXDraws, Graphics, windows, unite_3d, sysutils, vector, matrix;

var
   v3d_dxdraw: TDXDraw;                    //Surface DirectDraw

procedure D3D_ObjectsInit(dxdraw:TDXDraw); //Initialise la gestion des objets 3D


procedure InitSphere(var sphere:TObject_t; rayon:double);
function W3D_AddCube:integer;
function W3D_AddPlane:integer;
function W3D_AddSphere:integer;

function W3D_GetUniqueName(name:string):string;
function W3D_ObjectIndex(name:string):integer;

implementation

uses texture;

procedure D3D_ObjectsInit(dxdraw:TDXDraw);
begin
  v3d_dxdraw := dxdraw;
end;

procedure InitSphere(var sphere:TObject_t; rayon:double);
var
  phi, teta:double;
  i:integer;
begin
  //sphere.VertexCount := 121;
 // getmem(sphere.Vertex, sizeof(TVertex) * 121);

  phi := -pi/2;
  teta := 0;
  i:=0;
  while teta < 2*pi do
  begin
       while phi < PI/2 do
       begin
         sphere.Vertex[i].local.x := rayon * cos(phi) * cos (teta) + 00;
         sphere.Vertex[i].local.y := rayon * cos(phi) * sin (teta) + 00;
         sphere.Vertex[i].local.z := rayon * sin(phi) + 00;
         phi := phi + (PI/10);
         inc(i);
       end;
       //dec(i);
       phi := -pi/2;
       teta := teta + (2*PI/10);
       //MessageDlg(inttostr(i), mtInformation, [mbOk], 0);
  end;

  sphere.Position.x:=0;  sphere.position.y:=0;  sphere.position.z:=0;
  sphere.Angle.x := 0;   sphere.Angle.y := 0;  sphere.Angle.z := 0;
  sphere.Scaling.x := 1;  sphere.Scaling.y := 1;   sphere.Scaling.z := 1;


   sphere.physparam.vitesse_angulaire.x := 0;
   sphere.physparam.vitesse_angulaire.y := 0;
   sphere.physparam.vitesse_angulaire.z := 0;

   sphere.physparam.vitesse.x := 0;
   sphere.physparam.vitesse.y := 0;
   sphere.physparam.vitesse.z := 0;

   sphere.physparam.s_force.x := 0;
   sphere.physparam.s_force.y := 0;
   sphere.physparam.s_force.z := 0;

   sphere.physparam.acceleration.x := 0;
   sphere.physparam.acceleration.y := 0;
   sphere.physparam.acceleration.z := 0;

   sphere.physparam.forces_torsion.x := 0;
   sphere.physparam.forces_torsion.y := 0;
   sphere.physparam.forces_torsion.z := 0;

   sphere.physparam.moment_torsion := P3D(0,0,0);

   sphere.physparam.masse := 1;
   sphere.rayon := rayon;




end;


function W3D_AddSphere:integer;
var
 p:integer;
 objnum:integer;
 //polygon:TPolygon;
 name:string;
begin
   name := W3D_GetUniqueName('Sphere');
   objnum := W3D_CreateObject;
   W3D_InitObject(v3d_world.objects[objnum]^, 121, 121);
   InitSphere(v3d_world.objects[objnum]^, 50);

   for p:=0 to v3d_world.objects[objnum].PolygonCount-1 do
   begin
      W3D_InitPolygon(v3d_world.objects[objnum].Polygon[p],4);
   end;

   for p:=0 to 120 do
   begin
      v3d_world.objects[objnum].Polygon[p].usetexture := true;
      v3d_world.objects[objnum].Polygon[p].Texture := p mod 5;

      v3d_world.objects[objnum].Polygon[p].color := RGB(0,p*2,0);
      v3d_world.objects[objnum].Polygon[p].Vertex[2] := (p+12) mod 121;
      v3d_world.objects[objnum].Polygon[p].Vertex[1] := (p+11) mod 121;
      v3d_world.objects[objnum].Polygon[p].Vertex[0] := p; //mod 121;
      v3d_world.objects[objnum].Polygon[p].Vertex[3] := (p+1) mod 121;

      TEX_Setup(v3d_world.objects[objnum].Polygon[p],v3d_world.objects[objnum]^);
   end;



    v3d_world.objects[objnum].object_name := name;
    v3d_world.objects[objnum].object_class := [objSphere];

    D3D_IdentityMatrix(v3d_world.objects[objnum].Angle_mat);
   result := objnum;
end;

function W3D_AddCube:integer;
var
 p:integer;
 objnum:integer;
 //polygon:TPolygon;
 name:string;
begin
   name := W3D_GetUniqueName('Cube');
   objnum := W3D_CreateObject;
   W3D_InitObject(v3d_world.objects[objnum]^, 8, 6);


   for p:=0 to v3d_world.objects[objnum].PolygonCount-1 do
   begin
      W3D_InitPolygon(v3d_world.objects[objnum].Polygon[p],4);
   end;

   v3d_world.objects[objnum].Vertex[0].Local:=P3D(-10,-10,-10);
   v3d_world.objects[objnum].Vertex[1].Local:=P3D( 10,-10,-10);
   v3d_world.objects[objnum].Vertex[2].Local:=P3D( 10, 10,-10);
   v3d_world.objects[objnum].Vertex[3].Local:=P3D(-10, 10,-10);
   v3d_world.objects[objnum].Vertex[4].Local:=P3D(-10,-10, 10);
   v3d_world.objects[objnum].Vertex[5].Local:=P3D( 10,-10, 10);
   v3d_world.objects[objnum].Vertex[6].Local:=P3D( 10, 10, 10);
   v3d_world.objects[objnum].Vertex[7].Local:=P3D(-10, 10, 10);

   W3D_SetVert(v3d_world.objects[objnum].Polygon[0], 4, [0,1,5,4]);
   v3d_world.objects[objnum].Polygon[0].color := clRed;
   v3d_world.objects[objnum].Polygon[0].usetexture := true;
   v3d_world.objects[objnum].Polygon[0].texture := 0;
   TEX_Setup(v3d_world.objects[objnum].Polygon[0],v3d_world.objects[objnum]^);


   W3D_SetVert(v3d_world.objects[objnum].Polygon[1], 4, [5,6,7,4]);
   v3d_world.objects[objnum].Polygon[1].color := clWhite;
   v3d_world.objects[objnum].Polygon[1].usetexture := true;
   v3d_world.objects[objnum].Polygon[1].texture := 1;
   TEX_Setup(v3d_world.objects[objnum].Polygon[1],v3d_world.objects[objnum]^);

   W3D_SetVert(v3d_world.objects[objnum].Polygon[2], 4, [6,2,3,7]);
   v3d_world.objects[objnum].Polygon[2].color := clGreen;
   v3d_world.objects[objnum].Polygon[2].usetexture := true;
   v3d_world.objects[objnum].Polygon[2].texture := 2;
   TEX_Setup(v3d_world.objects[objnum].Polygon[2],v3d_world.objects[objnum]^);

   W3D_SetVert(v3d_world.objects[objnum].Polygon[3], 4, [2,1,0,3]);
   v3d_world.objects[objnum].Polygon[3].color := clYellow;
   v3d_world.objects[objnum].Polygon[3].usetexture := true;
   v3d_world.objects[objnum].Polygon[3].texture := 3;
   TEX_Setup(v3d_world.objects[objnum].Polygon[3],v3d_world.objects[objnum]^);

   W3D_SetVert(v3d_world.objects[objnum].Polygon[4], 4, [2,6,5,1]);
   v3d_world.objects[objnum].Polygon[4].color := clBlack;
   v3d_world.objects[objnum].Polygon[4].usetexture := true;
   v3d_world.objects[objnum].Polygon[4].texture := 4;
   TEX_Setup(v3d_world.objects[objnum].Polygon[4],v3d_world.objects[objnum]^);

   W3D_SetVert(v3d_world.objects[objnum].Polygon[5], 4, [4,7,3,0]);
   v3d_world.objects[objnum].Polygon[5].color := clGray;
   v3d_world.objects[objnum].Polygon[5].usetexture := true;
   v3d_world.objects[objnum].Polygon[5].texture := 0;
   TEX_Setup(v3d_world.objects[objnum].Polygon[5],v3d_world.objects[objnum]^);


   v3d_world.objects[objnum].Scaling:=P3D(1,1,1);
   v3d_world.objects[objnum].Angle:=P3D(0,0,0);
   v3d_world.objects[objnum].Position:=P3D(0,0,0);
   v3d_world.objects[objnum].object_name := name;
   v3d_world.objects[objnum].object_class := [objCube];


   v3d_world.objects[objnum].physparam.vitesse_angulaire.x := 0;
   v3d_world.objects[objnum].physparam.vitesse_angulaire.y := 0;
   v3d_world.objects[objnum].physparam.vitesse_angulaire.z := 0;
   D3D_IdentityMatrix(v3d_world.objects[objnum].Angle_mat);
   result :=objnum;
end;

function W3D_AddPlane:integer;
var
 p:integer;
 objnum:integer;
 //polygon:TPolygon;
 name:string;
begin
    name := W3D_GetUniqueName('Surface');
   objnum := W3D_CreateObject;
   W3D_InitObject(v3d_world.objects[objnum]^, 4, 2);


   for p:=0 to v3d_world.objects[objnum].PolygonCount-1 do
   begin
      W3D_InitPolygon(v3d_world.objects[objnum].Polygon[p],4);
   end;

    v3d_world.objects[objnum].Vertex[0].Local:=P3D(-10,0,10);
   v3d_world.objects[objnum].Vertex[1].Local:=P3D( 10,0,10);
   v3d_world.objects[objnum].Vertex[2].Local:=P3D( 10, 0,-10);
   v3d_world.objects[objnum].Vertex[3].Local:=P3D(-10, 0,-10);


   W3D_SetVert(v3d_world.objects[objnum].Polygon[0], 4, [0,1,2,3]);
   v3d_world.objects[objnum].Polygon[0].color := clGray;
   v3d_world.objects[objnum].Polygon[0].usetexture := false;
   v3d_world.objects[objnum].Polygon[0].Texture := 0;
   TEX_Setup(v3d_world.objects[objnum].Polygon[0],v3d_world.objects[objnum]^);

   W3D_SetVert(v3d_world.objects[objnum].Polygon[1], 4, [3,2,1,0]);
   v3d_world.objects[objnum].Polygon[1].color := clGreen;
   v3d_world.objects[objnum].Polygon[1].usetexture := false;
   v3d_world.objects[objnum].Polygon[1].Texture := 0;
   TEX_Setup(v3d_world.objects[objnum].Polygon[1],v3d_world.objects[objnum]^);



   v3d_world.objects[objnum].Scaling:=P3D(40,1,40);
   v3d_world.objects[objnum].Angle:=P3D(0,0,0);
   v3d_world.objects[objnum].Position:=P3D(0,-50,0);
   v3d_world.objects[objnum].object_name := name;
   v3d_world.objects[objnum].object_class := [objPlane];

   v3d_world.objects[objnum].physparam.vitesse_angulaire.x := 0;
   v3d_world.objects[objnum].physparam.vitesse_angulaire.y := 0;
   v3d_world.objects[objnum].physparam.vitesse_angulaire.z := 0;

   v3d_world.objects[objnum].physparam.vitesse.x := 0;
   v3d_world.objects[objnum].physparam.vitesse.y := 0;
   v3d_world.objects[objnum].physparam.vitesse.z := 0;

   v3d_world.objects[objnum].physparam.s_force.x := 0;
   v3d_world.objects[objnum].physparam.s_force.y := 0;
   v3d_world.objects[objnum].physparam.s_force.z := 0;

   v3d_world.objects[objnum].physparam.acceleration.x := 0;
   v3d_world.objects[objnum].physparam.acceleration.y := 0;
   v3d_world.objects[objnum].physparam.acceleration.z := 0;

   v3d_world.objects[objnum].physparam.masse := 1;
   v3d_world.objects[objnum].physparam.frottements := 0;
   D3D_IdentityMatrix(v3d_world.objects[objnum].Angle_mat);


   result :=objnum;
end;


function W3D_GetUniqueName(name:string):string;
var
 i:integer;
 loop:integer;
begin
 loop:=0;

 for i:=0 to v3d_world.object_count - 1 do
 begin
    if v3d_world.objects[i].object_name = name + inttostr(loop) then
       inc(loop);
 end;
result := name+inttostr(loop);
end;

function W3D_ObjectIndex(name:string):integer;
var
 i:integer;
begin
 for i:=0 to v3d_world.object_count -1 do
   if v3d_world.objects[i].object_name = name then
     begin result := i; exit; end;
result := -1;
end;

end.
