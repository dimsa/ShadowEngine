unit uSoPositionAdapter;

interface

uses
  uSoTypes, uISoPositionAdapter;

type
  TAbsolutePositionAdapter = class(TInterfacedObject, ISoPositionAdapter) // Make static?
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

function TAbsolutePositionAdapter.AdaptCenter(const AValue: TPointF): TPointF;
begin
  Result := AValue;
end;

function TAbsolutePositionAdapter.AdaptHeight(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TAbsolutePositionAdapter.AdaptRotate(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TAbsolutePositionAdapter.AdaptScale(const AValue: TPointF): TPointF;
begin
  Result := AValue;
end;

function TAbsolutePositionAdapter.AdaptScaleX(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TAbsolutePositionAdapter.AdaptScaleY(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TAbsolutePositionAdapter.AdaptWidth(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TAbsolutePositionAdapter.AdaptX(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TAbsolutePositionAdapter.AdaptY(const AValue: Single): Single;
begin
  Result := AValue;
end;

end.
