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

unit events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  generic TEventCallback<T1> = procedure(arg1: T1) of object;

  generic TEventSource<T1> = class
  type
      TCb = specialize TEventCallback<T1>;
  private

    FCallbacks: array of TCb;
  public
    constructor Create();
    procedure Add(cb: TCb);
    procedure Invoke(arg1: T1);
  end;

implementation

constructor TEventSource.Create();
begin
  SetLength(FCallbacks, 0);
end;

procedure TEventSource.Add(cb: TCb);
begin
  SetLength(FCallbacks, Length(FCallbacks) + 1);
  FCallbacks[Length(FCallbacks) - 1] := cb;
end;

procedure TEventSource.Invoke(arg1: T1);
var
  i: integer;
begin
  for i := 0 to Length(FCallbacks) - 1 do
    FCallbacks[i](arg1);
end;

end.

