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

unit IdleTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Controls, LCLType, Forms, MMSystem;

type

  TIdleTimerEvent = procedure(Sender: TObject; LagCount: integer) of object;

  // This class implements a timer that is triggered when the application
  // is idle. It relies on Application.OnIdle signal for that.
  TIdleTimer = class(TComponent)
  private
    FEnabled: boolean;
    FFrameRate: integer;
    FInitialized: boolean;
    FInterval: cardinal;
    FInterval2: cardinal;
    FNowFrameRate: integer;
    FOldTime: DWORD;
    FOldTime2: DWORD;
    FOnTimer: TIdleTimerEvent;
    procedure AppIdle(Sender: TObject; var Done: boolean);
    procedure Finalize;
    procedure Initialize;
    procedure Resume;
    procedure SetEnabled(Value: boolean);
    procedure SetInterval(Value: cardinal);
    procedure Suspend;
  protected
    procedure DoTimer(LagCount: integer); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write SetEnabled;
    property FrameRate: integer read FFrameRate;
    property Interval: cardinal read FInterval write SetInterval;
    property OnTimer: TIdleTimerEvent read FOnTimer write FOnTimer;
  end;

{  TDXTimer  }

procedure Register;

implementation

constructor TIdleTimer.Create(AOwner: TComponent);
begin
  inherited;
  Interval := 1000;
  Enabled := True;
end;

destructor TIdleTimer.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

procedure TIdleTimer.AppIdle(Sender: TObject; var Done: boolean);
var
  t, t2: DWORD;
  LagCount, i: integer;
begin
  Done := False;

  t := TimeGetTime;
  t2 := t - FOldTime;
  if t2 >= FInterval then
  begin
    FOldTime := t;

    LagCount := t2 div FInterval2;
    if LagCount < 1 then
      LagCount := 1;

    Inc(FNowFrameRate);

    i := Max(t - FOldTime2, 1);
    if i >= 1000 then
    begin
      FFrameRate := Round(FNowFrameRate * 1000 / i);
      FNowFrameRate := 0;
      FOldTime2 := t;
    end;

    DoTimer(LagCount);
  end;
end;

procedure TIdleTimer.DoTimer(LagCount: integer);
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self, LagCount);
end;

procedure TIdleTimer.Finalize;
begin
  if FInitialized then
  begin
    Suspend;
    FInitialized := False;
  end;
end;

procedure TIdleTimer.Initialize;
begin
  Finalize;
  Resume;
  FInitialized := True;
end;

procedure TIdleTimer.Loaded;
begin
  inherited Loaded;
  if (not (csDesigning in ComponentState)) and FEnabled then
    Initialize;
end;

procedure TIdleTimer.Resume;
begin
  FOldTime := TimeGetTime;
  FOldTime2 := TimeGetTime;
  Application.OnIdle := @AppIdle;
end;

procedure TIdleTimer.Suspend;
begin
  Application.OnIdle := nil;
end;

procedure TIdleTimer.SetEnabled(Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if ComponentState * [csReading, csLoading] = [] then
      if FEnabled then
        Initialize
      else
        Finalize;
  end;
end;

procedure TIdleTimer.SetInterval(Value: cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Max(Value, 0);
    FInterval2 := Max(Value, 1);
  end;
end;

procedure Register;
begin
  RegisterComponents('System', [TIdleTimer]);
end;

end.
