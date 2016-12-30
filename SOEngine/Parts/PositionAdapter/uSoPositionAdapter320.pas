unit uSoPositionAdapter320;

interface

uses
  uSoTypes,
  uISoPositionAdapter;

type
  TSoPositionAdapter320 = class(TInterfacedOBject, ISoPositionAdapter)
  private
    FWidth, FHeight: PInteger;
  const
    divide320 = 0.003125;
    divide240 = 0.004167;
  public
    function AdaptScaleX(const AValue: Single): Single;
    function AdaptScaleY(const AValue: Single): Single;
    function AdaptScale(const AValue: TPointF): TPointF;
    function AdaptRotate(const AValue: Single): Single;
    function AdaptX(const AValue: Single): Single;
    function AdaptY(const AValue: Single): Single;
    function AdaptWidth(const AValue: Single): Single;
    function AdaptHeight(const AValue: Single): Single;
    function AdaptCenter(const AValue: TPointF): TPointF;
    constructor Create(const AWidth, AHeight: PInteger);
  end;

implementation

{ TSoPositionAdapter320 }

function TSoPositionAdapter320.AdaptCenter(const AValue: TPointF): TPointF;
begin
  Result := TPointF.Create((AValue.X * divide320) * FWidth^, (AValue.Y * divide240) * FHeight^)
end;

function TSoPositionAdapter320.AdaptHeight(const AValue: Single): Single;
begin
  Result := (AValue * divide240) * FHeight^;
end;

function TSoPositionAdapter320.AdaptRotate(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapter320.AdaptScale(const AValue: TPointF): TPointF;
begin
  Result := AValue;
end;

function TSoPositionAdapter320.AdaptScaleX(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapter320.AdaptScaleY(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapter320.AdaptWidth(const AValue: Single): Single;
begin
  Result := (AValue * divide320) * FWidth^;
end;

function TSoPositionAdapter320.AdaptX(const AValue: Single): Single;
begin
  Result := (AValue * divide320) * FWidth^;
end;

function TSoPositionAdapter320.AdaptY(const AValue: Single): Single;
begin
  Result := (AValue * divide240) * FHeight^;
end;

constructor TSoPositionAdapter320.Create(const AWidth, AHeight: PInteger);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

end.
