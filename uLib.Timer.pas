unit uLib.Timer;

interface

uses
  WinApi.Windows
  ,System.Classes
  ,System.SyncObjs
  ,System.Diagnostics
  ;

type
  TConsoleTimer = Class(TThread)
  private
    FCancelFlag: TSimpleEvent;
    FTimerEnabledFlag: TSimpleEvent;
    FTimerProc: TNotifyEvent; // method to call
    FInterval: integer;
    procedure SetEnabled(doEnable: boolean);
    function GetEnabled: boolean;
    procedure SetInterval(interval: integer);
    procedure SwapToMainThread;
  protected
    procedure Execute; override;
  public
    Constructor Create;
    Destructor Destroy; override;
    property Enabled : boolean read GetEnabled write SetEnabled;
    property Interval: integer read FInterval write SetInterval;
    // Note: OnTimerEvent is executed in TConsoleTimer thread
    property OnTimerEvent: TNotifyEvent read FTimerProc write FTimerProc;
  end;

implementation

function KeyPressed:Boolean;
var
  lpNumberOfEvents     : DWORD;
  lpBuffer             : TInputRecord;
  lpNumberOfEventsRead : DWORD;
  nStdHandle           : THandle;
begin
  Result:=false;
  //get the console handle
  nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
  lpNumberOfEvents:=0;
  //get the number of events
  GetNumberOfConsoleInputEvents(nStdHandle,lpNumberOfEvents);
  if lpNumberOfEvents<> 0 then
  begin
    //retrieve the event
    PeekConsoleInput(nStdHandle,lpBuffer,1,lpNumberOfEventsRead);
    if lpNumberOfEventsRead <> 0 then
    begin
      if lpBuffer.EventType = KEY_EVENT then //is a Keyboard event?
      begin
        if lpBuffer.Event.KeyEvent.bKeyDown then //the key was pressed?
          Result:=true
        else
          FlushConsoleInputBuffer(nStdHandle); //flush the buffer
      end
      else
      FlushConsoleInputBuffer(nStdHandle);//flush the buffer
    end;
  end;
end;

constructor TConsoleTimer.Create;
begin
  inherited Create(false);
  FTimerEnabledFlag := TSimpleEvent.Create;
  FCancelFlag := TSimpleEvent.Create;
  FTimerProc := nil;
  FInterval := 1000;
  Self.FreeOnTerminate := false; // Main thread controls for thread destruction
end;

destructor TConsoleTimer.Destroy; // Call TConsoleTimer.Free to cancel the thread
begin
  Terminate;
  FTimerEnabledFlag.ResetEvent; // Stop timer event
  FCancelFlag.SetEvent; // Set cancel flag
  Waitfor; // Synchronize
  FCancelFlag.Free;
  FTimerEnabledFlag.Free;
  inherited;
end;

procedure TConsoleTimer.SetEnabled(doEnable: boolean);
begin
  if doEnable then
    FTimerEnabledFlag.SetEvent
  else
    FTimerEnabledFlag.ResetEvent;
end;

procedure TConsoleTimer.SetInterval(interval: integer);
begin
  FInterval := interval;
end;

procedure TConsoleTimer.Execute;
var
  waitList: array [0 .. 1] of THandle;
  waitInterval,
  lastProcTime: Int64;
  sw: TStopWatch;
begin
  sw.Create;
  waitList[0] := FTimerEnabledFlag.Handle;
  waitList[1] := FCancelFlag.Handle;
  lastProcTime := 0;
  while not Terminated do
  begin
    if (WaitForMultipleObjects(2, @waitList[0], false, INFINITE) <>
      WAIT_OBJECT_0) then
      break; // Terminate thread when FCancelFlag is signaled
    if Assigned(FTimerProc) then
    begin
      waitInterval := FInterval - lastProcTime;
      if (waitInterval < 0) then
        waitInterval := 0;
      if WaitForSingleObject(FCancelFlag.Handle,waitInterval) <> WAIT_TIMEOUT then
        break;

      if WaitForSingleObject(FTimerEnabledFlag.Handle, 0) = WAIT_OBJECT_0 then
      begin
        sw.Start;
        //FTimerProc(Self);
        Synchronize(SwapToMainThread);
        sw.Stop;
        // Interval adjusted for FTimerProc execution time
        lastProcTime := sw.ElapsedMilliSeconds;
      end;
    end;
  end;
end;

procedure TConsoleTimer.SwapToMainThread;
begin
  FTimerProc(Self);
end;

function TConsoleTimer.GetEnabled: boolean;
begin
  Result := (FTimerEnabledFlag.Waitfor(0) = wrSignaled);
end;

end.
