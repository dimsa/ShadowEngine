unit uSoPositionAdapter;

interface

uses
  uSoTypes;

type
  TSoPositionAdapter = class // Make static?
  public
    function AdaptScaleX(const AValue: Single): Single;
    function AdaptScaleY(const AValue: Single): Single;
    function AdaptScale(const AValue: TPointF): TPointF;
    function AdaptRotate(const AValue: Single): Single;
    function AdaptX(const AValue: Single): Single;
    function AdaptY(const AValue: Single): Single;
    function AdaptCenter(const AValue: TPointF): TPointF;
  end;

implementation

{ TSoPositionAdapter }

function TSoPositionAdapter.AdaptCenter(const AValue: TPointF): TPointF;
begin
  Result := AValue;
end;

function TSoPositionAdapter.AdaptRotate(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapter.AdaptScale(const AValue: TPointF): TPointF;
begin
  Result := AValue;
end;

function TSoPositionAdapter.AdaptScaleX(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapter.AdaptScaleY(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapter.AdaptX(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapter.AdaptY(const AValue: Single): Single;
begin
  Result := AValue;
end;

end.
