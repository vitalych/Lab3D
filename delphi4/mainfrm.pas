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

unit mainfrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DXClass, DXInput, ComCtrls, Menus, math,
  DXDraws, Buttons, unite_3d, objets3d, ImgList, fichiers,
  Grids, DXSprite, jpeg, ExtDlgs,
  DXSounds, MPlayer, mmsystem, directx, Wave;

type
  TForm1 = class(TDXForm)
    DXDraw: TDXDraw;
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    Fichier1: TMenuItem;
    Quitter1: TMenuItem;
    Voir1: TMenuItem;
    Inspecteurdobjets1: TMenuItem;
    Aide1: TMenuItem;
    Aproposde1: TMenuItem;
    Rubriquedaide1: TMenuItem;
    DXTimer1: TDXTimer;
    Button2: TButton;
    Button3: TButton;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    N1: TMenuItem;
    Ouvrir1: TMenuItem;
    Enregistrersous1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;

    DXImageList1: TDXImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    chkZBUF: TCheckBox;
    chkTextures: TCheckBox;
    DXInput1: TDXInput;
    Nouveau1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    ImageList1: TImageList;
    Button4: TButton;
    btnTrigMove: TSpeedButton;
    btnPlay: TSpeedButton;
    btnPause: TSpeedButton;
    Applicateurdeforces1: TMenuItem;
    N4: TMenuItem;
    SimulationONOFF1: TMenuItem;
    N5: TMenuItem;
    Insrer1: TMenuItem;
    Sphre1: TMenuItem;
    Surface1: TMenuItem;
    Cube1: TMenuItem;
    N6: TMenuItem;
    DplacementsONOFF1: TMenuItem;
    chkFilFer: TCheckBox;

    procedure DXDrawFinalize(Sender: TObject);
    procedure DXDrawInitializing(Sender: TObject);
    procedure DXTimer1Timer(Sender: TObject; LagCount: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Quitter1Click(Sender: TObject);
    procedure Aproposde1Click(Sender: TObject);
    procedure DXDrawResize(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure Inspecteurdobjets1Click(Sender: TObject);
    procedure Nouveau1Click(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkZBUFClick(Sender: TObject);
    procedure chkTexturesClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure DXDrawInitializeSurface(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Variables1Click(Sender: TObject);
    procedure btnTrigMoveClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure DisplayStatus;
    procedure Applicateurdeforces1Click(Sender: TObject);
    procedure SimulationONOFF1Click(Sender: TObject);
    procedure chkFilFerClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button3Click(Sender: TObject);
    procedure DisplayHelp;
  private
    { D�clarations priv�es}

  public
    { D�clarations publiques}

     tps:double;

    cibleX:integer;
    cibleY:integer;
    dessiner_cible:boolean;
    temps_ms:double;         //Variable t des fonctions
    orig_dist:double;
    cube_cote:integer;

    isdrawing:boolean;
    decz:boolean;
    s:integer;
    //Variables de rotation de la camera
    sd_x, sd_y:integer;     //Debut de l'operation glisser/deplacer
    is_draging:boolean;

    //Position en cours du curseur sur la surface
    surface_curx, surface_cury:integer;

    total_frame:integer;

    moves:boolean;
    current_fps:integer;
    equations_var:TStringList;
    function  AjouterObjet(obj:TObjectClass):integer;

  end;

var
  Form1: TForm1;
  FSurface:TDirectDrawSurface;
  vidptr:pointer;
  ptrSurface:^PByte;
   SurfaceStruct:TDDSurfaceDesc;
   src:HRSRC;
  src_ad:HGLOBAL;
  helpidx:integer=0;
procedure DessinerCible(x, y:integer);


implementation

uses about, objinsp, texture, varsfrm, forcesfrm;

{$R *.DFM}


function  Tform1.AjouterObjet(obj:TObjectClass):integer;
var
 idx:integer;
begin

 if objCube in obj then
   idx := W3D_AddCube
 else if objPlane in obj then
   idx := W3D_AddPlane
 else if objSphere in obj then
   idx := W3D_AddSphere;


 frmObjInspector.AddObject(v3d_world.objects[idx]^);

 result := idx;
end;

procedure DessinerCible(x, y:integer);
begin
    if form1.dessiner_cible = true then
    begin
         Form1.DXDraw.Surface.Canvas.MoveTo(0, y);
         Form1.DXDraw.Surface.Canvas.LineTo(form1.dxdraw.surfacewidth, y);
         Form1.DXDraw.Surface.Canvas.MoveTo(x, 0);
         Form1.DXDraw.Surface.Canvas.LineTo(x, form1.DXDraw.height);
    end;

end;


procedure TForm1.DXDrawFinalize(Sender: TObject);
begin
     DXTimer1.Enabled := False;

end;

procedure TForm1.DXDrawInitializing(Sender: TObject);
begin
     DXTimer1.Enabled := True;
end;

procedure TForm1.DisplayHelp;
begin
       DXDraw.Surface.Canvas.Font.Size := 10;
   DXDraw.Surface.Canvas.Font.Name := 'Arial';

     case helpidx of
       0:   DXDraw.Surface.Canvas.TextOut(0 , DXDraw.SurfaceHeight-50,
   'Pour demarrer rapidement, cliquez sur Fichier/Ouvrir et choisissez un environnement');

       1:  DXDraw.Surface.Canvas.TextOut(0 , DXDraw.SurfaceHeight-50,
   'Pour apprendre a manipuler le logiciel et decouvrir son fonctionnement, '+
   'appelez l''aide');
     end;

end;

procedure TForm1.DisplayStatus;
var
 rect:TRect;
begin
   DXDraw.Surface.canvas.brush.Color := clNavy;
   rect.left := 0;
   rect.Top := DXDraw.SurfaceHeight-50;
   rect.Right := DXDraw.Surfacewidth;
   rect.Bottom := DXDraw.SurfaceHeight;
   DXDraw.Surface.canvas.fillrect(rect);


   DXDraw.Surface.Canvas.Font.Color := clRed;
   DXDraw.Surface.Canvas.Font.Name := 'Digital SF';
   DXDraw.Surface.Canvas.Font.Size := 12;
    DXDraw.surface.canvas.Font.Style := [fsBold];
   DXDraw.Surface.Canvas.TextOut(0, DXDraw.SurfaceHeight-20, inttostr(DXTimer1.FrameRate)+' FPS');
   DXDraw.Surface.Canvas.Font.Color := clLime;



   DXDraw.Surface.Canvas.Font.Size := 12;
   DXDraw.Surface.Canvas.Font.Name := 'Digital SF';
   DXDraw.surface.canvas.textout(50, DXDraw.SurfaceHeight-20,
  ' Pos_X='+inttostr(round(v3d_view.position.x))+
  ' Pos_Y='+inttostr(round(v3d_view.position.y))+
  ' Pos_Z='+inttostr(round(v3d_view.position.z)));

  DXDraw.surface.canvas.textout(DXDraw.surface.canvas.TextWidth(' Pos_X='+inttostr(round(v3d_view.position.x))+
  ' Pos_Y='+inttostr(round(v3d_view.position.y))+
  ' Pos_Z='+inttostr(round(v3d_view.position.z)))+80, DXDraw.SurfaceHeight-20,
  ' A='+inttostr(round(radtodeg(v3d_view.angle.x)) mod 360)+'�'+
  ' B='+inttostr(round(radtodeg(v3d_view.angle.y)) mod 360)+'�'+
  ' G='+inttostr(round(radtodeg(v3d_view.angle.z)) mod 360)+'�');


 DisplayHelp;

  DXDraw.Surface.canvas.brush.Color := clBlue;
end;


////////////////////////////////////////////////////////////////////////
///PROCEDURE CENTRALE DU PROJET                                      ///
///S'OCCUPPE DU RAFRAICHISSEMENT DE L'ECRAN LES PLUS DE FOIS POSSIBLE///
///PAR SECONDE                                                       ///
////////////////////////////////////////////////////////////////////////
procedure TForm1.DXTimer1Timer(Sender: TObject; LagCount: Integer);
var
 i:integer;
begin
  if isdrawing then exit;
  isdrawing := true;


  if v3d_simulation = true then
  begin
     for i:=0 to 11 do
       if frmForces.force_sapply[i] = true then
          frmForces.force_execute(i);
  end;


if moves then
begin

  DXInput1.Update;

  inc(total_frame);



  if isButton1 in DXInput1.States then
     i:=4
  else i:=1;

  if isLeft in DXInput1.States then
     v3d_view.angle.y := v3d_view.angle.y + i*0.02;

   if isRight in DXInput1.States then
     v3d_view.angle.y := v3d_view.angle.y - i*0.02;

  if isDown in DXInput1.States then begin
     v3d_view.position.z := v3d_view.position.z - i*10*cos(v3d_view.angle.y);
     v3d_view.position.x := v3d_view.position.x + i*10*sin(v3d_view.angle.y);
  end;

   if isUp in DXInput1.States then begin
     v3d_view.position.z := v3d_view.position.z + i*10*cos(v3d_view.angle.y);
     v3d_view.position.x := v3d_view.position.x - i*10*sin(v3d_view.angle.y);
  end;
end;

   v3d_view.angle.x := 0.2;

  DXDraw.surface.canvas.brush.Color := clBlue;

  //On efface la surface avec la couleur bleue
  DXDraw.Surface.Fill(rgb(255, 0, 0));
    DXDraw.Surface.Lock(dxdraw.Surface.ClientRect, SurfaceStruct);

//   v3d_zbuffer[i] := -32767;

  //On remet a zero le ZBuffer
       asm
         push edi
         mov ecx, v3d_zbufsize
         mov eax, 65536
         mov edi, v3d_zbuffer
         cld
         rep stosd
         pop edi
       end;

    current_fps := DXTimer1.FrameRate;
    if current_fps = 0 then current_fps := 1;
    temps_ms := temps_ms + (total_frame / current_fps);

    v3d_delta_t := 1 / current_fps;



  isDrawing := true;

  ptrSurface := SurfaceStruct.lpsurface;
    try
     W3D_draw;
    except

    end;

    DisplayStatus;
     DXDraw.Surface.UnLock;
     DXDraw.surface.canvas.Release;
     DXDraw.Flip;
      isdrawing := false;

end;
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
     //Initialisation du moteur 3D
     D3D_InitEngine;
     D3D_ObjectsInit(DXDraw);

     //On fixe l'origine au centre de l'ecran
     v3d_origine.x := dxdraw.surfacewidth div 2;
     v3d_origine.y := DXDraw.Height div 2;

     randomize;

  src := FindResource(hInstance, 'DING1', RT_RCDATA);
  src_ad := LoadResource(hInstance, src);


     v3d_view.angle.x := 11;
     v3d_view.angle.y := 0;
     v3d_view.angle.z := 0;

     v3d_view.position.z := -2550;
     v3d_view.position.y := 0;
     v3d_view.position.x := 0;


  equations_var := TStringList.Create;
  equations_var.Add('t=0');
  DecimalSeparator := '.';

   DXDraw.surface.canvas.Font.Name := 'Digital SF';
   DXDraw.surface.canvas.Font.Size := 12;
   DXDraw.surface.canvas.Font.Color := clLime;
   DXDraw.surface.canvas.Font.Style := [fsBold];

   opendialog1.InitialDir := ExtractFileDir(Application.ExeName);
   savedialog1.InitialDir := ExtractFileDir(Application.ExeName);
end;

procedure TForm1.Quitter1Click(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TForm1.Aproposde1Click(Sender: TObject);
begin
 aboutbox.showmodal;
end;

procedure TForm1.DXDrawResize(Sender: TObject);
begin
 dxtimer1.Enabled := false;
 v3d_origine.x := dxdraw.surfacewidth div 2;
 v3d_origine.y := DXDraw.surfaceHeight div  2;

 DownRight.x := form1.dxdraw.surfacewidth;
 DownRight.y := form1.DXDraw.surfaceHeight;


 VirtualFree(v3d_zbuffer, v3d_zbufsize*4, MEM_FREE);

  //Allocation de la memoire pour le ZBuffer
     v3d_zbufsize := dxdraw.surfacewidth*DXDraw.surfaceHeight;

      v3d_zbuffer := VirtualAlloc( pointer(0), v3d_zbufsize*4, MEM_COMMIT, PAGE_READWRITE);
      if v3d_zbuffer = nil then
      MessageDlg(inttostr(GetLastError), mtError, [mbOK], 0);

       asm
         push edi
         mov ecx, v3d_zbufsize
         mov eax, 65536
         mov edi, v3d_zbuffer
         cld
         rep stosd
         pop edi
       end;



 dxtimer1.Enabled := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 AjouterObjet([objCube]);
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
 if SaveDialog1.Execute then
 begin
        LAB3D_SaveToFile(savedialog1.filename);
 end;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
var
 i:integer;
begin
 if OpenDialog1.Execute then begin
   LAB3D_LoadFromFile(opendialog1.filename);
 frmObjInspector.objinsp_reset;
 try
 frmForces.FillForces;
 except

 end;
  for i:=0 to v3d_world.object_count-1 do
    frmObjInspector.AddObject(v3d_world.objects[i]^);
 helpidx := 1;
 end;
end;

procedure TForm1.Inspecteurdobjets1Click(Sender: TObject);
begin
 frmObjInspector.show;
end;

procedure TForm1.Nouveau1Click(Sender: TObject);
var
 i:integer;
begin
 frmObjInspector.objinsp_reset;
 D3D_DestroyEngine;
 D3D_InitEngine;

end;

procedure TForm1.btnStopClick(Sender: TObject);
begin

 v3d_simulation := false;

 SimulationONOFF1.checked := false;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

 if (Key>=VK_F1) and (key<=VK_F12) then
   if frmforces.force_apply((key and $0f), false)=true then
      frmForces.force_sapply[(key and $0f)] :=true;


 if Key=VK_ESCAPE then
    Close;

  {  Screen mode change  }
  if (ssAlt in Shift) and (Key=VK_RETURN) then
  begin
    DXDraw.Finalize;

    if doFullScreen in DXDraw.Options then
    begin
      RestoreWindow;

      DXDraw.Cursor := crDefault;
      BorderStyle := bsSizeable;
      DXDraw.Options := DXDraw.Options - [doFullScreen];

    end else
    begin
      StoreWindow;

      //DXDraw.Cursor := crNone;
      BorderStyle := bsNone;
      DXDraw.Options := DXDraw.Options + [doFullScreen];

    end;

    DXDraw.Initialize;
  end;

  if moves then
     DXDraw.SetFocus;
end;

procedure TForm1.chkZBUFClick(Sender: TObject);
begin
 if chkZBUF.checked then
   v3d_usezbuf:= true
 else v3d_usezbuf := false;
end;

procedure TForm1.chkTexturesClick(Sender: TObject);
begin
 if chkTextures.checked then
   v3d_usetextures := true
 else
   v3d_usetextures := false;
end;

procedure TForm1.btnPlayClick(Sender: TObject);
var
  res:TFileStream;
  src:HRSRC;
  src_ad:HGLOBAL;
begin
  orig_dist := 100;

  v3d_simulation := true;
  SimulationONOFF1.checked := true;
 // src := FindResource(hInstance, 'rep', RT_RCDATA);
 // src_ad := LoadResource(hInstance, src);


  //PlaySound(pointer(src_ad), hInstance, SND_MEMORY or SND_ASYNC);
/// timer1.enabled := true;
end;

procedure TForm1.DXDrawInitializeSurface(Sender: TObject);
begin

 v3d_bitcount := DXDraw.Display.Mode.BitCount;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
 helpidx := 0;

 TEX_AddTextureFromRessource('LAB3D_TEX1');
 TEX_AddTextureFromRessource('LAB3D_TEX2');
 TEX_AddTextureFromRessource('LAB3D_TEX3');
 TEX_AddTextureFromRessource('LAB3D_TEX4');
 TEX_AddTextureFromRessource('LAB3D_TEX5');
end;

procedure TForm1.Variables1Click(Sender: TObject);
begin
 frmVariables.show;
end;

procedure TForm1.btnTrigMoveClick(Sender: TObject);
begin
 if moves = false then begin
 moves:=true;
 btnTrigMove.down := true;
 DplacementsONOFF1.Checked := true;
 end
 else begin
 moves := false;
 btnTrigMove.Down := false;
 DplacementsONOFF1.Checked := false;
 end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
     AjouterObjet([objPlane]);
end;

procedure TForm1.btnPauseClick(Sender: TObject);
begin
 DXTimer1.OnTimer(sender, 0);
end;

procedure TForm1.Applicateurdeforces1Click(Sender: TObject);
begin
 frmForces.show;
end;

procedure TForm1.SimulationONOFF1Click(Sender: TObject);
begin
 if SimulationONOFF1.checked = true then begin
  btnPauseClick(sender);
  btnPause.Down := true;
  SimulationONOFF1.checked := false;
  end
 else begin
  btnPlayClick(sender);
  btnPlay.Down := true;
  SimulationONOFF1.checked := true;
 end;
end;

procedure TForm1.chkFilFerClick(Sender: TObject);
begin
 v3d_fildefer := chkFilFer.Checked;
 chkZBUF.enabled := not chkFilFer.Checked;
 chkTextures.Enabled := not chkFilFer.Checked;

end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (Key>=VK_F1) and (key<=VK_F12) then
  frmForces.force_sapply[(key and $0f)] :=false;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 AjouterObjet([objSphere]);
end;

end.


