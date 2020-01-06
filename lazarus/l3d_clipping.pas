// Copyright (c) 2020 Vitaly Chipounov
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

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

unit l3d_clipping;

interface

uses
  vector, Math;

type
  TCoordinates = record
    P1: T2DVec;
    P2: T2DVec;
    constructor Create(_p1, _p2: T2DVec);
  end;

const
  INSIDE: integer = 0;
  LEFT: integer = 1;
  RIGHT: integer = 2;
  BOTTOM: integer = 4;
  TOP: integer = 8;

function ClipGetRegion(Point: T2DVec; Box: TCoordinates): integer;
function ClipLine(var Clipped: TCoordinates; Line, Box: TCoordinates): boolean;

implementation

constructor TCoordinates.Create(_p1, _p2: T2DVec);
begin
  P1 := _p1;
  P2 := _p2;
end;

function ClipGetRegion(Point: T2DVec; Box: TCoordinates): integer;
var
  region: integer;
begin
  region := 0;
  if Point.x < Box.P1.x then
    region := region or LEFT;
  if Point.x > Box.P2.x then
    region := region or RIGHT;
  if Point.y < Box.P1.y then
    region := region or TOP;
  if Point.y > Box.P2.y then
    region := region or BOTTOM;
  Result := region;
end;

// Cohen-Sutherland algorithm
function ClipLine(var Clipped: TCoordinates; Line, Box: TCoordinates): boolean;
var
  r1, r2, r: integer;
  accept: boolean;
  x, y: float;
  x0, y0, x1, y1: float;
  xmin, ymin, xmax, ymax: float;
begin
  x0 := Line.P1.x;
  y0 := Line.P1.y;
  x1 := Line.P2.x;
  y1 := Line.P2.y;

  xmin := Box.P1.x;
  ymin := Box.P1.y;
  xmax := Box.P2.x;
  ymax := Box.P2.y;

  r1 := ClipGetRegion(Line.P1, Box);
  r2 := ClipGetRegion(Line.P2, Box);

  accept := False;

  while True do
  begin
    if (r1 or r2) = 0 then
    begin
      // Optimal case, when the line falls inside the box
      accept := True;
      break;
    end
    else if (r1 and r2) <> 0 then
    begin
      // Line is completely outside of the box
      break;
    end
    else
    begin
      // At least one endpoint is outside the clip rectangle; pick it.
      if r1 <> 0 then
        r := r1
      else
        r := r2;

      if (r and TOP) <> 0 then
      begin
        x := x0 + (x1 - x0) * (ymin - y0) / (y1 - y0);
        y := ymin;
      end
      else if (r and BOTTOM) <> 0 then
      begin
        x := x0 + (x1 - x0) * (ymax - y0) / (y1 - y0);
        y := ymax;
      end
      else if (r and RIGHT) <> 0 then
      begin
        y := y0 + (y1 - y0) * (xmax - x0) / (x1 - x0);
        x := xmax;
      end
      else if (r and LEFT) <> 0 then
      begin
        y := y0 + (y1 - y0) * (xmin - x0) / (x1 - x0);
        x := xmin;
      end;

      if r = r1 then
      begin
        x0 := x;
        y0 := y;
        r1 := ClipGetRegion(T2DVec.Create(round(x0), round(y0)), Box);
      end
      else
      begin
        x1 := x;
        y1 := y;
        r2 := ClipGetRegion(T2DVec.Create(round(x1), round(y1)), Box);
      end;
    end; // while true

  end;

  if accept then
  begin
    Clipped.P1 := T2DVec.Create(round(x0), round(y0));
    Clipped.P2 := T2DVec.Create(round(x1), round(y1));
  end;

  Result := accept;
end;

end.
