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

unit fastbmp;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TFastBitmapPixel = integer;
  PFastBitmapPixel = ^TFastBitmapPixel;

  TFastBitmap = class
  private
    FPixelsData: PByte;
    FSize: TPoint;
    FBufferSize: integer;
    function GetPixel(X, Y: integer): TFastBitmapPixel; inline;
    procedure SetPixel(X, Y: integer; const AValue: TFastBitmapPixel); inline;
    procedure SetSize(const AValue: TPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear(color: TFastBitmapPixel);
    procedure DrawLine(x1, y1, x2, y2: integer; color: TFastBitmapPixel);
    property Size: TPoint read FSize write SetSize;
    property Height: integer read FSize.Y;
    property Width: integer read FSize.X;
    property BufferSize: integer read FBufferSize;
    property Pixels[X, Y: integer]: TFastBitmapPixel read GetPixel write SetPixel;
    property PixelsData: PByte read FPixelsData;
  end;

implementation

// https://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#Delphi
procedure TFastBitmap.DrawLine(x1, y1, x2, y2: integer; color: TFastBitmapPixel);
var
  // displacements in x and y
  a, b: integer;

  // decision variable
  d: integer;

  // d's increment for diagonal steps
  diag_inc: integer;

  // diagonal x step for next pixel
  dx_diag: integer;

  // nondiagonal x step for next pixel
  dx_nondiag: integer;

  // diagonal y step for next pixel
  dy_diag: integer;

  // nondiagonal y step for next pixel
  dy_nondiag: integer;

  i: integer;

  // d's increment for nondiagonal steps
  nondiag_inc: integer;

  // temporary variable for swap
  swap: integer;

  // current x and y coordinates
  x, y: integer;
begin
  x := x1;
  y := y1;
  // Determine drawing direction and step to the next pixel.
  a := x2 - x1;
  b := y2 - y1;

  // Determine whether end point lies to right or left of start point.
  if a < 0 then
  begin
    a := -a;
    dx_diag := -1;
  end
  else
    dx_diag := 1;

  // Determine whether end point lies above or below start point.
  if b < 0 then
  begin
    b := -b;
    dy_diag := -1;
  end
  else
    dy_diag := 1;

  // Identify octant containing end point.
  if a < b then
  begin
    swap := a;
    a := b;
    b := swap;
    dx_nondiag := 0;
    dy_nondiag := dy_diag;
  end
  else
  begin
    dx_nondiag := dx_diag;
    dy_nondiag := 0;
  end;

  d := b + b - a;           // initial value for d is 2*b - a
  nondiag_inc := b + b;     // set initial d increment values
  diag_inc := b + b - a - a;

  for i := 0 to a do
  begin
    Pixels[x, y] := color;
    if d < 0 then
    begin
      x := x + dx_nondiag;
      y := y + dy_nondiag;
      d := d + nondiag_inc;
    end
    else
    begin
      x := x + dx_diag;
      y := y + dy_diag;
      d := d + diag_inc;
    end;
  end;
end;

procedure TFastBitmap.Clear(color: TFastBitmapPixel);
begin
  if FPixelsData <> nil then
    FillDWord(FPixelsData^, Width * Height, color);
end;

function TFastBitmap.GetPixel(X, Y: integer): TFastBitmapPixel;
begin
  Result := PFastBitmapPixel(FPixelsData + (Y * FSize.X + X) *
    SizeOf(TFastBitmapPixel))^;
end;

procedure TFastBitmap.SetPixel(X, Y: integer; const AValue: TFastBitmapPixel);
begin
  PFastBitmapPixel(FPixelsData + (Y * FSize.X + X) * SizeOf(TFastBitmapPixel))^ :=
    AValue;
end;

procedure TFastBitmap.SetSize(const AValue: TPoint);
begin
  if (FSize.X = AValue.X) and (FSize.Y = AValue.X) then
    Exit;
  FSize := AValue;
  FBufferSize := FSize.X * FSize.Y * SizeOf(TFastBitmapPixel);
  FPixelsData := ReAllocMem(FPixelsData, FBufferSize);
  Clear(0);
end;

constructor TFastBitmap.Create;
begin
  Size := Point(0, 0);
  FBufferSize := 0;
  FPixelsData := nil;
end;

destructor TFastBitmap.Destroy;
begin
  FreeMem(FPixelsData);
  inherited Destroy;
end;

end.
