unit BaseThread;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type

  { TBaseThread }

  TBaseThread = class(TThread)
  private
    FOnCustomTerminate: TNotifyEvent;
    function GetTerminated: Boolean;
  protected
    procedure CallOnCustomTerminate; inline;
    {$if FPC_FULLVERSION >= 30202}
    procedure TerminatedSet; override;
    {$else}
  public
    procedure Terminate;
    {$endif}
  public
    constructor Create(CreateSuspended: Boolean = True);
    destructor Destroy; override;
    property IsTerminated: Boolean read GetTerminated;
    property OnCustomTerminate: TNotifyEvent read FOnCustomTerminate write FOnCustomTerminate;
  end;

implementation

{ TBaseThread }

function TBaseThread.GetTerminated: Boolean;
begin
  Result := Self.Terminated;
end;

procedure TBaseThread.CallOnCustomTerminate;
begin
  FOnCustomTerminate(Self);
end;

{$if FPC_FULLVERSION >= 30202}
procedure TBaseThread.TerminatedSet;
begin
{$else}
procedure TBaseThread.Terminate;
begin
  inherited Terminate;
{$endif}
  if Assigned(FOnCustomTerminate) then
    FOnCustomTerminate(Self);
end;

constructor TBaseThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

destructor TBaseThread.Destroy;
begin
  inherited Destroy;
end;

end.

