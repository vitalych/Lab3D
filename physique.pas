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

unit physique;

interface

uses varsfrm, unite_3d, mainfrm, dialogs, windows, mmsystem, math, matrix, sysutils,
graphics, vector;

var
  phys_gravite : double = 9.81;
  phys_echelle : double = 100;

  v3d_rotmatrix:T3DMatrix;

  _cobj:integer = 0;



function phys_docollide(var obj:TObject_t; objnum:integer; var position:T3DPoint):integer;
procedure phys_docomputepos(objnum:integer; delta_t:double);

function phys_maj_angle(vitesse_angulaire:T3DPoint):T3DMatrix;
function phys_get_force_pt_sphere(obj:TObject_t; phi, theta:double):T3DPoint;

implementation


//Met a jour l'angle de l'objet en fonction de sa vitesse angulaire
function phys_maj_angle(vitesse_angulaire:T3DPoint):T3DMatrix;
var
   th, a, b, g:double;
   mat:T3DMatrix;
   omega:T3DPoint;
   i, j:integer;
begin
   th := (D3D_VecNorme(vitesse_angulaire));
   omega :=vitesse_angulaire;
   D3D_SetVecLength(omega, 1);
   a := omega.x;
   b := omega.y;
   g := omega.z;

   for i:=0 to 3 do
     for j:=0 to 3 do
      mat[i][j] := 0;

   mat[0][0] := a*a*(1-cos(th))+cos(th);
   mat[0][1] := a*b*(1-cos(th))-g*sin(th);
   mat[0][2] := a*g*(1-cos(th))+b*sin(th);

   mat[1][0] := a*b*(1-cos(th))+g*sin(th);
   mat[1][1] := b*b*(1-cos(th))+cos(th);
   mat[1][2] := b*g*(1-cos(th))-a*sin(th);

   mat[2][0] := a*g*(1-cos(th))-b*sin(th);
   mat[2][1] := b*g*(1-cos(th))+a*sin(th);
   mat[2][2] := g*g*(1-cos(th))+cos(th);

   mat[3][3] := 1;

   result  := mat;

end;


procedure phys_computeplaneeq(var a, b, c, d:double; var normale:T3DPoint; var obj:TObject_t);
var
  x1, x2, x3, y1, y2, y3, z1, z2, z3:double;
begin
       x1 := obj.vertex[0].world.x;
       x2 := obj.vertex[1].world.x;
       x3 := obj.vertex[2].world.x;
       y1 := obj.vertex[0].world.y;
       y2 := obj.vertex[1].world.y;
       y3 := obj.vertex[2].world.y;
       z1 := obj.vertex[0].world.z;
       z2 := obj.vertex[1].world.z;
       z3 := obj.vertex[2].world.z;


     D3D_VecMult(P3D(x3-x1,y3-y1,z3-z1),P3D(x2-x1,y2-y1,z2-z1),normale);


        D3D_SetVecLength(normale,-1);


      a := normale.x;
      b := normale.y;
      c := normale.z;

      d := D3D_DotProduct(normale, obj.Position);


end;


function phys_cantouch(obj1, obj2:TObject_t):boolean;
begin
    if (((obj1.Position.x > min(obj2.vertex[0].world.x, obj2.vertex[1].world.x)) and
       (obj1.Position.x < max(obj2.vertex[0].world.x, obj2.vertex[1].world.x))) or
       ((obj1.Position.x > min(obj2.vertex[2].world.x, obj2.vertex[3].world.x)) and
       (obj1.Position.x < max(obj2.vertex[2].world.x, obj2.vertex[3].world.x)))) and

       (((obj1.Position.z > min(obj2.vertex[0].world.z, obj2.vertex[3].world.z)) and
       (obj1.Position.z < max(obj2.vertex[1].world.z, obj2.vertex[2].world.z))) or
       ((obj1.Position.z > min(obj2.vertex[1].world.z, obj2.vertex[2].world.z)) and
       (obj1.Position.z < max(obj2.vertex[0].world.z, obj2.vertex[3].world.z))))
       then
        result := true
   else result := false;

end;


function phys_get_force_pt_sphere(obj:TObject_t; phi, theta:double):T3DPoint;
begin
   result.x := obj.rayon*obj.scaling.x * cos (phi) * cos (theta);
   result.y := obj.rayon*obj.scaling.y * cos (phi) * sin (theta);
   result.z := obj.rayon*obj.scaling.z * sin (phi);
end;

function D3D_ProduitVect(vec1,vec2:T3DPoint):double;
begin
      result := vec1.x * vec2.y - vec1.y * vec2.x;
end;




//renvoie -1 si a et b sont de signes differents, 1 sinon
function math_sign(a,b:double):integer;
begin
    if (a<0) and (b>0) then result := -1
    else if (a>0) and (b<0) then result := -1
    else result := 1;
end;

//Gestion des collisions
//0: pas de collision, -1: penetration, 1 collision resolue
function phys_docollide(var obj:TObject_t; objnum:integer; var position:T3DPoint):integer;
var
    i, s:integer;
    norme, dp:double;
    normale, norm2, vit2, pos2:T3DPoint;
    mat:T3DMatrix;
    a, b, c, d, eq, eq1, alpha:double;
    length:double;
begin
    for i:=_cobj to v3d_world.object_count-1 do
    begin
       if i=objnum then continue;
       if v3d_world.objects[i] = nil then continue;
       norme := 0;

       //On verifie s'il peut y avoir une collision avec l'objet en cours et une surface
       if v3d_world.objects[i].object_class = [objPlane] then
       if phys_cantouch(v3d_world.objects[objnum]^, v3d_world.objects[i]^) then
       begin
          phys_computeplaneeq(a, b, c, d, normale, v3d_world.objects[i]^);
          norm2 := normale;
          D3D_SetVecLength(norm2,v3d_world.objects[objnum].rayon*v3d_world.objects[objnum].Scaling.y);

          eq := D3D_DotProduct(D3D_VecProd(normale,-1), Position)+d;
          pos2 := Position;
          D3D_VecAdd(pos2, D3D_VecProd(norm2,-1))  ;
          eq1 := D3D_DotProduct(D3D_VecProd(normale, -1), pos2)+d;



           if  (math_sign(eq, eq1) = 1) then
            begin
               result := 0;
               continue;
            end;


          if (round(abs(eq1))<v3d_world.objects[objnum].rayon*v3d_world.objects[objnum].Scaling.y) and (math_sign(eq, eq1)=-1) then
          begin
             if D3D_DotProduct(normale,v3d_world.objects[objnum].physparam.acceleration)>0 then
              continue;
    {      form1.DXDraw.surface.Canvas.Font.Color := clYellow;
          form1.DXDraw.surface.Canvas.TextOut(0,120, 'Collision '+ v3d_world.objects[i].object_name);
          form1.DXDraw.surface.Canvas.Font.Color := clLime;
          form1.DXDraw.surface.Canvas.TextOut(0,140, 'eq='+floattostr(round(eq*1000)/1000));
          form1.DXDraw.surface.Canvas.TextOut(0,160, 'eq1='+floattostr(round(eq1*1000)/1000));
     }
               D3D_VecAdd(v3d_world.objects[objnum].position, D3D_VecProd(normale, eq1));

               norm2 := normale;
               D3D_VecAdd(v3d_world.objects[objnum].physparam.s_force, D3D_VecProd(normale, phys_gravite*v3d_world.objects[objnum].physparam.masse));

               vit2 := v3d_world.objects[objnum].physparam.vitesse;

               alpha := D3D_DotProduct(normale, vit2)/(D3D_VecNorme(vit2)*round(D3D_VecNorme(normale)*1000)/1000);


               alpha := arccos(alpha)-pi/2;
               D3D_SetVecLength(norm2, round(D3D_VecNorme(vit2)*1000)/1000*sin(alpha));
               D3D_VecAdd(norm2, vit2);

               v3d_world.objects[objnum].physparam.vitesse := norm2;

               //On determine la direction et le sens des forces de frottement
               norm2 := D3D_VecProd(norm2, -1);
               D3D_SetVecLength(norm2, v3d_world.objects[i].physparam.frottements);
               D3D_VecAdd(v3d_world.objects[objnum].physparam.s_force, norm2);

               //On calcule le moment de torsion
               D3D_VecMult(D3D_VecProd(normale, -v3d_world.objects[objnum].rayon*v3d_world.objects[objnum].Scaling.y),
               D3D_VecProd(v3d_world.objects[objnum].physparam.acceleration,2/5*
               v3d_world.objects[objnum].physparam.masse),vit2);

               D3D_VecAdd(v3d_world.objects[objnum].physparam.moment_torsion,vit2);

               result := 1;
               //exit;
          end else
           begin
                   result := -1;
               _cobj := i;
               exit;

            end ;



       end;



    end;
    result := 1;
end;


procedure phys_docomputepos(objnum:integer; delta_t:double);
var
    i, j:integer;
    tmpforce:T3DPoint;
    tmpangle:T3DPoint;
    tmppos:T3DPoint;
    tempobj:TObject_t;
    dt2, dtx, dty, dtz:double;
begin

   // tempobj := v3d_world.objects[objnum]^;
    dt2 := delta_t;
    with v3d_world.objects[objnum]^ do
    begin
         //a. On determine la moment d'inertie
         physparam.moment_inertie := (2/5)*physparam.masse*power(rayon*Scaling.x, 2);


         //0. On calcule la somme des forces

         tmpforce :=  physparam.s_force;
         D3D_VecAdd(tmpforce, D3D_VecProd(P3D(0,1,0), -phys_gravite*v3d_world.objects[objnum].physparam.masse));
         D3D_VecAdd(tmpforce, physparam.forces_torsion);

         //tmpforce := D3D_VecProd(tmpforce, phys_echelle);

         //1. On determine le vecteur acceleration
         physparam.acceleration := D3D_VecProd(tmpforce, 1/(v3d_world.objects[objnum].physparam.masse)*phys_echelle);

         //2. Avec ca, on calcule le vecteur vitesse
         D3D_VecAdd(physparam.vitesse, D3D_VecProd(physparam.acceleration, delta_t));

         //3. Maintenant, on a notre position sur l'ecran avec la formule suivante
          physparam.s_force := P3D(0,0,0);


        tmppos := Position;
        phys_docollide(v3d_world.objects[objnum]^, objnum, tmppos);
     //  position := tmppos;

        D3D_VecAdd(position, D3D_VecProd(physparam.vitesse,delta_t));



         //4. Acceleration angulaire
         v3d_world.objects[objnum].physparam.acceleration_angulaire :=
         D3D_VecProd(v3d_world.objects[objnum].physparam.moment_torsion,
         1/(v3d_world.objects[objnum].physparam.moment_inertie));

         //5. Vitesse angulaire
         D3D_VecAdd(v3d_world.objects[objnum].physparam.vitesse_angulaire,
         D3D_VecProd(v3d_world.objects[objnum].physparam.acceleration_angulaire, delta_t));

         //6. Angle de l'objet
         D3D_MatrixMult(phys_maj_angle(D3D_VecProd(v3d_world.objects[objnum].physparam.vitesse_angulaire,
         delta_t)), v3d_world.objects[objnum].Angle_mat, v3d_world.objects[objnum].angle_mat);

          physparam.forces_torsion := P3D(0,0,0);
         physparam.moment_torsion := P3D(0,0,0);

        end;
    //v3d_world.objects[objnum]^ := tempobj;
end;

end.
