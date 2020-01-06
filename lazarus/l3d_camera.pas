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

unit l3d_camera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vector, Math, matrix, events;

type
  TCameraPosition = class
    type
    TOnCameraChange = specialize TEventSource<TCameraPosition>;
  private
    FPosition: T3DVec;
    FAngleX, FAngleY: float;

    FOnCameraChange: TOnCameraChange;

    function GetMatrix(): T4DMatrix;
    procedure SetAngleX(x: float);
    procedure SetAngleY(y: float);
    procedure SetPosition(p: T3DVec);
  public
    constructor Create(pos: T3DVec; angle: float);
    procedure Update(up, down, left, right: boolean);
    procedure Update(mouseDelta: T2DVec);
    procedure UpdateMouseWheel(wheelDelta: integer);
    property Position: T3DVec read FPosition write SetPosition;
    property AngleX: float read FAngleX write SetAngleX;
    property AngleY: float read FAngleY write SetAngleY;
    property Matrix: T4DMatrix read GetMatrix;
    property OnCameraChange: TOnCameraChange read FOnCameraChange;
  end;

function KeepAngleInBounds(angle: float): float;

implementation

constructor TCameraPosition.Create(pos: T3DVec; angle: float);
begin
  FAngleY := angle;
  FAngleX := 0;
  FPosition := pos;
  FOnCameraChange := TOnCameraChange.Create();
end;

function TCameraPosition.GetMatrix(): T4DMatrix;
var
  translation, xrot, yrot: T4DMatrix;
begin
  yrot := D3D_RotationMatrixY(FAngleY);
  xrot := D3D_RotationMatrixX(FAngleX);
  translation := D3D_TranslationMatrix(FPosition.x, FPosition.y, FPosition.z);
  Result := D3D_InverseMatrix(D3D_MatrixMult(translation, D3D_MatrixMult(yrot, xrot)));
end;

function KeepAngleInBounds(angle: float): float;
begin
  if angle < 0 then
  begin
    angle := angle + 2 * pi;
  end;

  if angle >= 2 * pi then
  begin
    angle := angle - 2 * pi;
  end;
  Result := angle;
end;

procedure TCameraPosition.Update(mouseDelta: T2DVec);
begin
  FAngleY += mouseDelta.x * 0.2 * pi / 180;
  FAngleX += mouseDelta.y * 0.2 * pi / 180;

  FAngleX := KeepAngleInBounds(FAngleX);
  FAngleY := KeepAngleInBounds(FAngleY);

  FOnCameraChange.Invoke(Self);
end;

procedure TCameraPosition.Update(up, down, left, right: boolean);
var
  dirty: boolean;
begin
  dirty := up or down or right or left;

  if Up then
  begin
    FPosition.z := FPosition.z - 0.1 * cos(-FAngleY);
    FPosition.x := FPosition.x + 0.1 * sin(-FAngleY);
  end;

  if Down then
  begin
    FPosition.z := FPosition.z + 0.1 * cos(-FAngleY);
    FPosition.x := FPosition.x - 0.1 * sin(-FAngleY);
  end;

  if Right then
  begin
    FAngleY := (FAngleY - 0.01);
    dirty := True;
  end;

  if Left then
  begin
    FAngleY := (FAngleY + 0.01);
    dirty := True;
  end;

  FAngleY := KeepAngleInBounds(FAngleY);

  if dirty then
    FOnCameraChange.Invoke(Self);
end;

procedure TCameraPosition.UpdateMouseWheel(wheelDelta: integer);
begin
  if wheelDelta > 0 then
  begin
    FPosition.z := FPosition.z - wheelDelta * 0.01 * cos(-FAngleY);
    FPosition.x := FPosition.x + wheelDelta * 0.01 * sin(-FAngleY);
  end
  else
  begin
    FPosition.z := FPosition.z + abs(wheelDelta) * 0.01 * cos(-FAngleY);
    FPosition.x := FPosition.x - abs(wheelDelta) * 0.01 * sin(-FAngleY);
  end;
  FOnCameraChange.Invoke(Self);
end;

procedure TCameraPosition.SetAngleX(x: float);
begin
  FAngleX := x;
  FOnCameraChange.Invoke(Self);
end;

procedure TCameraPosition.SetAngleY(y: float);
begin
  FAngleY := y;
  FOnCameraChange.Invoke(Self);
end;

procedure TCameraPosition.SetPosition(p: T3DVec);
begin
  FPosition := p;
  FOnCameraChange.Invoke(Self);
end;

end.

