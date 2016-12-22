unit uISoPositionAdapter;

interface

uses
  uSoTypes;

type
  ISoPositionAdapter = interface
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

end.
