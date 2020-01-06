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

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, LCLType, OpenGLContext, gl,
  fastbmp, glsurface, idletimer, vector, l3d_world,
  l3d_objects, l3d_camera, Types, objinsp, project_file, about;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnuHelpAbout: TMenuItem;
    N2: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuSaveAs: TMenuItem;
    N1: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuInsertBoard: TMenuItem;
    mnuInsertSphere: TMenuItem;
    mnuInsertCube: TMenuItem;
    mnuObjectInspector: TMenuItem;
    mnuFileExit: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    ObjInsp: TObjectInspector;
    AboutBox: TAboutBox;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure mnuFileNewClick(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuInsertBoardClick(Sender: TObject);
    procedure mnuInsertCubeClick(Sender: TObject);
    procedure mnuInsertSphereClick(Sender: TObject);
    procedure mnuObjectInspectorClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject; LagCount: integer);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    FWorld: T3DWorld;
    FColor: TColor;


    FSurface: TOpenGLSurface;
    FTimer: TIdleTimer;

    FCamera: TCameraPosition;

    // This is the state of the arrow keys
    FLeft, FRight, FUp, FDown: boolean;

    // State of the mouse buttons and cursor
    FMLeft: boolean;
    FMx, FMy: integer;
    FMouseDelta: T2DVec;

    FFileName: string;

    procedure UpdateCameraPosition();
    procedure UpdateProjectionMatrix();
    function NeedRedraw(): boolean;
    procedure EnableRedrawIfNeeded();
    procedure OnWorldChanged(obj: T3DWorldObject);
    procedure OnCameraChange(camera: TCameraPosition);
    procedure UpdateTitle();
  public

  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

{ TMainForm }



procedure TMainForm.mnuFileExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
var
  ctrl: TControl;
begin
  ctrl := Sender as TControl;
  FSurface.Width := ctrl.Width;
  FSurface.Height := ctrl.Height;
  UpdateProjectionMatrix();
end;

function TMainForm.NeedRedraw(): boolean;
begin
  Result := FWorld.Dirty or FLeft or FRight or FUp or FDown or FMLeft;
end;

procedure TMainForm.EnableRedrawIfNeeded();
begin
  if NeedRedraw() and not FTimer.Enabled then
    FTimer.Enabled := True;
end;

procedure TMainForm.UpdateProjectionMatrix();
begin
  // A field of view that is too large will cause stretching of objects
  // when they get to the side of the view port.
  FWorld.SetProjectionMatrix(FSurface.Width, FSurface.Height, 45, 0.01, 1000);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  obj: T3DWorldObject;
begin
  FSurface := TOpenGLSurface.Create(Panel1);
  FSurface.Name := 'Surface1';
  FSurface.Parent := Panel1;
  FSurface.OnMouseDown := @FormMouseDown;
  FSurface.OnMouseUp := @FormMouseUp;
  FSurface.OnMouseMove := @FormMouseMove;

  FTimer := TIdleTimer.Create(Self);
  FTimer.Name := 'Timer1';
  FTimer.OnTimer := @Timer1Timer;
  FTimer.Interval := 10;
  FTimer.Enabled := True;

  AboutBox := TAboutBox.Create(MainForm);

  FMx := 0;
  FMy := 0;
  FMouseDelta := T2DVec.Create(0, 0);

  FCamera := TCameraPosition.Create(T3DVec.Create(0, 0, 0), pi);
  FCamera.OnCameraChange.Add(@OnCameraChange);

  FWorld := T3DWorld.Create();
  FWorld.OnWorldObjectsChanged.Add(@OnWorldChanged);

  ObjInsp := TObjectInspector.Create(self);
  ObjInsp.SetWorld(FWorld);

  obj := FWorld.AddWorldObject(OBJ_SPHERE);
  obj.Translate(0, 0, 10);
  obj := FWorld.AddWorldObject(OBJ_CUBE);
  obj.Translate(-5, 0, 10);
  obj := FWorld.AddWorldObject(OBJ_CUBE);
  obj.Translate(5, 0, 10);
  obj := FWorld.AddWorldObject(OBJ_BOARD);
  obj.Translate(0, 1, 10);

  OnCameraChange(FCamera);
  UpdateProjectionMatrix();
  UpdateTitle();
end;

procedure TMainForm.OnWorldChanged(obj: T3DWorldObject);
begin
  UpdateProjectionMatrix();
  FTimer.Enabled := True;
end;

procedure TMainForm.OnCameraChange(camera: TCameraPosition);
begin
  FWorld.SetViewMatrix(FCamera.Matrix);
  FTimer.Enabled := True;

  StatusBar1.Panels.Items[1].Text :=
    Format('x: %f y: %f z: %f  ax=%f° ay=%f°', [FCamera.Position.x,
    FCamera.Position.y, FCamera.Position.z, FCamera.AngleX * 180 /
    pi, FCamera.AngleY * 180 / pi]);
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  FTimer.Enabled := False;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  FTimer.Enabled := True;
end;


procedure TMainForm.Timer1Timer(Sender: TObject; LagCount: integer);
begin
  StatusBar1.Panels.Items[0].Text := Format('%d fps', [FTimer.FrameRate]);
  UpdateCameraPosition();

  if FWorld.Dirty then
  begin
    FWorld.Project();
    FSurface.Surface.Clear(FColor);
    FWorld.DrawWireFrame(FSurface.Surface);
    FSurface.Draw;
  end;

  // FColor += 1;
  // FAngle += 0.01;
  if not NeedRedraw() then
    FTimer.Enabled := False;
end;

procedure TMainForm.UpdateCameraPosition();
begin
  FCamera.Update(FUp, FDown, FLeft, FRight);
  if FMLeft then
  begin
    FCamera.Update(FMouseDelta);
    FMouseDelta := T2DVec.Create(0, 0);
  end;
end;


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_UP: FUp := True;
    VK_DOWN: FDown := True;
    VK_LEFT: FLeft := True;
    VK_RIGHT: FRight := True;
  end;

  EnableRedrawIfNeeded();
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_UP: FUp := False;
    VK_DOWN: FDown := False;
    VK_LEFT: FLeft := False;
    VK_RIGHT: FRight := False;
  end;

  EnableRedrawIfNeeded();
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = TMouseButton.mbLeft then
    FMLeft := True;

  EnableRedrawIfNeeded();
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  FMouseDelta := T2DVec.Create(X - FMx, Y - FMy);
  FMx := X;
  FMy := Y;

  EnableRedrawIfNeeded();
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = TMouseButton.mbLeft then
    FMLeft := False;

  EnableRedrawIfNeeded();
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  FCamera.UpdateMouseWheel(WheelDelta);
  FWorld.SetViewMatrix(FCamera.Matrix);
  EnableRedrawIfNeeded();
end;

procedure TMainForm.UpdateTitle();
begin
  if FFileName = '' then
  begin
    Caption := 'Lab3D [untitled1]';
  end
  else
  begin
    Caption := Format('Lab3D [%s]', [FFileName]);
  end;
end;

procedure TMainForm.mnuFileNewClick(Sender: TObject);
begin
  FWorld.Clear();
  FFileName := '';
  UpdateTitle();
end;

procedure TMainForm.mnuFileOpenClick(Sender: TObject);
var
  world: T3DWorld;
  camera: TCameraPosition;
begin
  world := nil;
  camera := nil;

  if OpenDialog1.Execute then
  begin
    try
      LoadWorldFromFile(world, camera, OpenDialog1.FileName);
      if (world = nil) or (camera = nil) then
      begin
        raise Exception.Create('Invalid project or camera');
      end
      else
      begin
        FFileName := OpenDialog1.FileName;
        UpdateTitle();

        FCamera := camera;
        FCamera.OnCameraChange.Add(@OnCameraChange);
        FWorld := world;
        FWorld.OnWorldObjectsChanged.Add(@OnWorldChanged);

        ObjInsp.SetWorld(FWorld);
        FWorld.Dirty := True;
        OnCameraChange(FCamera);
      end;
    except
      MessageDlg('Error', 'Could not open project file', mtError, [mbOK], '');
    end;
  end;
end;

procedure TMainForm.mnuFileSaveClick(Sender: TObject);
begin
  if FFileName <> '' then
  begin
    try
      SaveWorldToFile(FWorld, FCamera, FFileName);
    except
      MessageDlg('Error', 'Could not save project file', mtError, [mbOK], '');
    end;
  end
  else
  begin
    mnuSaveAsClick(Sender);
  end;
end;

procedure TMainForm.mnuHelpAboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.mnuSaveAsClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    try
      SaveWorldToFile(FWorld, FCamera, SaveDialog1.FileName);
      FFileName := SaveDialog1.FileName;
      UpdateTitle();
    except
      MessageDlg('Error', 'Could not save project file', mtError, [mbOK], '');
    end;
  end;
end;

procedure TMainForm.mnuInsertBoardClick(Sender: TObject);
begin
  FWorld.AddWorldObject(OBJ_BOARD);
end;

procedure TMainForm.mnuInsertCubeClick(Sender: TObject);
begin
  FWorld.AddWorldObject(OBJ_CUBE);
end;

procedure TMainForm.mnuInsertSphereClick(Sender: TObject);
begin
  FWorld.AddWorldObject(OBJ_SPHERE);
end;

procedure TMainForm.mnuObjectInspectorClick(Sender: TObject);
begin
  ObjInsp.Show;
end;

end.
