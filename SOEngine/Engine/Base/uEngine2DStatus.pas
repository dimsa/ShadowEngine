unit uEngine2DStatus;

interface

uses
  uEngine2DThread, uEngine2DClasses;

type

// It have only information about Engine Status. It can not change anything and it always must have actual info.
TEngine2DStatus = class
private
  FStatus: Byte;
  FEngineThread: TEngineThread;
  FWidth, FHeight: PInteger;
  FClicked, FMouseDowned, FMouseUpped: PIntArray;
  FIsMouseDowned: PBoolean;
  function GetClicked: TIntArray;
  function GetMouseDowned: TIntArray;
  function GetMouseUpped: TIntArray;
  function GetHeight: integer;
  function GetWidth: integer;
  function GetIsHor: Boolean;
  function GetIsMouseDowned: Boolean;
  function GetEngineFPS: Single;
  function GetEngineSpeed: Single;
public
  property Status: Byte read FStatus write FStatus;
  property Clicked: TIntArray read GetClicked;
  property Downed: TIntArray read GetMouseDowned;
  property Upped: TIntArray read GetMouseUpped;
  property Width: Integer read GetWidth;
  property Height: Integer read GetHeight;
  property IsHor: Boolean read GetIsHor;
  property IsMouseDowned: Boolean read GetIsMouseDowned;
  property EngineFPS: Single read GetEngineFPS;
  property EngineSpeed: Single read GetEngineSpeed;
  constructor Create(
    const AEngineThread: TEngineThread;
    const AWidth, AHeight: PInteger;
    const AIsMouseDowned: PBoolean;
    const AMouseDowned, AMouseUpped, AClicked: PIntArray
  );
end;

implementation

{ TEngine2DStatus }
constructor TEngine2DStatus.Create(
    const AEngineThread: TEngineThread;
    const AWidth, AHeight: PInteger;
    const AIsMouseDowned: PBoolean;
    const AMouseDowned, AMouseUpped, AClicked: PIntArray);
begin
  FEngineThread := AEngineThread;
  FWidth := AWidth;
  FHeight := AHeight;
  FIsMouseDowned := AIsMouseDowned;
  FMouseDowned := AMouseDowned;
  FMouseUpped := AMouseUpped;
  FClicked := AClicked;
end;

function TEngine2DStatus.GetClicked: TIntArray;
begin
  Result := FClicked^;
end;

function TEngine2DStatus.GetEngineFPS: Single;
begin
  Result := FEngineThread.FPS;
end;

function TEngine2DStatus.GetEngineSpeed: Single;
begin
  Result := FEngineThread.Speed;
end;

function TEngine2DStatus.GetHeight: integer;
begin
  Result := FHeight^;
end;

function TEngine2DStatus.GetIsHor: Boolean;
begin
  Result := FWidth^ > FHeight^;
end;

function TEngine2DStatus.GetIsMouseDowned: Boolean;
begin
  Result := FIsMouseDowned^;
end;

function TEngine2DStatus.GetMouseDowned: TIntArray;
begin
  Result := FMouseDowned^;
end;

function TEngine2DStatus.GetMouseUpped: TIntArray;
begin
  Result := FMouseUpped^;
end;

function TEngine2DStatus.GetWidth: integer;
begin
  Result := FWidth^;
end;

end.
