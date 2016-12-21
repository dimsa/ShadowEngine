unit uSoPositionAdapter;

interface

uses
  uSoBasePart;

type
  TSoPositionAdapter = class
  public
    function AdaptScaleX(const AValue: Single): Single;
    function AdaptScaleY(const AValue: Single): Single;
    function AdaptRotate(const AValue: Single): Single;
    function AdaptX(const AValue: Single): Single;
    function AdaptY(const AValue: Single): Single;
  end;

implementation

{ TSoPositionAdapter }

function TSoPositionAdapter.AdaptRotate(const AValue: Single): Single;
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
