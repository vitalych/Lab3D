// Copyright (c) 2019 Vitaly Chipounov
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

unit glsurface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Windows, Dialogs,
  Controls, OpenGLContext, gl, fastbmp;

type

  // This type provides a 2D OpenGL surface where it is possible
  // to modify individual pixels.
  TOpenGLSurface = class(TOpenGLControl)
  private
    FBitmap: TFastBitmap;
    FInited: boolean;

    procedure InitGl;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw;
    procedure Paint; override;
    property Surface: TFastBitmap read FBitmap;
  published

  end;

procedure Register;

implementation

constructor TOpenGLSurface.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TFastBitmap.Create();
  FInited := True;
end;

destructor TOpenGLSurface.Destroy;
begin
  inherited;
end;

procedure TOpenGLSurface.Resize;
var
  size: TPoint;
begin
  inherited;
  if not FInited then
    exit;

  size.X := Width;
  size.Y := Height;
  FBitmap.Size := size;
  InitGl;
end;

procedure TOpenGLSurface.Draw();
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glDrawPixels(Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, Surface.PixelsData);
  SwapBuffers;
end;

procedure TOpenGLSurface.InitGL;
begin
  MakeCurrent;

  glViewport(0, 0, Width, Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, Width, 0, Height, 0.1, 1);
  glPixelZoom(1, -1);
  glRasterPos3f(0, Height - 1, -0.3);
end;

procedure TOpenGLSurface.Paint();
begin
  Draw;
end;


procedure Register;
begin
  RegisterComponents('OpenGL', [TOpenGLSurface]);
end;

end.
