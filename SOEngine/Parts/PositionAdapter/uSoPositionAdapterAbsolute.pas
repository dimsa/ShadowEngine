unit uSoPositionAdapterAbsolute;

interface

uses
  uSoTypes, uISoPositionAdapter;

type
  TSoPositionAdapterAbsolute = class(TInterfacedObject, ISoPositionAdapter) // Make static?
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
  end;

implementation

{ TSoPositionAdapter }

function TSoPositionAdapterAbsolute.AdaptCenter(const AValue: TPointF): TPointF;
begin
  Result := AValue;
end;

function TSoPositionAdapterAbsolute.AdaptHeight(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapterAbsolute.AdaptRotate(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapterAbsolute.AdaptScale(const AValue: TPointF): TPointF;
begin
  Result := AValue;
end;

function TSoPositionAdapterAbsolute.AdaptScaleX(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapterAbsolute.AdaptScaleY(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapterAbsolute.AdaptWidth(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapterAbsolute.AdaptX(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TSoPositionAdapterAbsolute.AdaptY(const AValue: Single): Single;
begin
  Result := AValue;
end;

end.
