unit uClasses;

interface

uses
  System.SysUtils, System.Types,   {$IFDEF VER290} System.Math.Vectors, {$ENDIF} System.Math, System.UITypes, FMX.Controls;

type
  TProcedure = procedure of Object;
  TVCLProcedure = procedure(ASender: TObject) of Object;

  ISerializable = interface
    function Serialize: String;
    procedure Deserialize(const AJsonText: String);
  end;

  TAdvancedRectF = record helper for TRectF
  private
    function GetPoint(Index: Integer): TPointF;
    procedure SetPoint(Index: Integer; AValue: TPointF);
    function GetBottomLeft: TPointF;
    function GetTopRight: TPointF;
    procedure SetBottomLeft(const Value: TPointF);
    procedure SetTopRight(const Value: TPointF);
  public
    property Points[Index: Integer]: TPointF read GetPoint write SetPoint; default;
    property TopRight: TPointF read GetTopRight write SetTopRight;
    property BottomLeft: TPointF read GetBottomLeft write SetBottomLeft;
  end;

  TAdvancedControl = class helper for TControl
  private
    function GetPoint(Index: Integer): TPointF;
    procedure SetPoint(Index: Integer; AValue: TPointF);
  public
    property Points[Index: Integer]: TPointF read GetPoint write SetPoint;
  end;

  TAdvancedPolygon = record helper for TPolygon
  public
    function Count: Integer;
  end;


  function Random64: Int64;
  procedure NormalizeAngle(var AAngle: Single);
  function RGBColor(const AR, AG, AB, AA: Byte): TAlphaColorRec;

implementation

function RGBColor(const AR, AG, AB, AA: Byte): TAlphaColorRec;
begin
  Result.R := AR;
  Result.G := AG;
  Result.B := AB;
  Result.A := AA;
end;

procedure NormalizeAngle(var AAngle: Single);
begin
  if AAngle < -180 then
  begin
    AAngle := AAngle + 360;
    NormalizeAngle(AAngle);
  end;

  if AAngle > 180 then
  begin
    AAngle := AAngle - 360;
    NormalizeAngle(AAngle);
  end;

end;

function Random64: Int64;
begin
   Int64Rec(result).Words[0] := Random(Word.MaxValue);
   Int64Rec(result).Words[1] := Random(Word.MaxValue);
   Int64Rec(result).Words[2] := Random(Word.MaxValue);
   Int64Rec(result).Words[3] := Random(Word.MaxValue);
end;

{ TAdvancedRectF }

function TAdvancedRectF.GetBottomLeft: TPointF;
begin
  Result := PointF(Left, Bottom);
end;

function TAdvancedRectF.GetPoint(Index: Integer): TPointF;
begin
  case Index of
    0: Result := TopLeft;
    1: Result := TopRight;
    2: Result := BottomRight;
    3: Result := BottomLeft;
  end;
end;

function TAdvancedRectF.GetTopRight: TPointF;
begin
  Result := PointF(Right, Top);
end;

procedure TAdvancedRectF.SetBottomLeft(const Value: TPointF);
begin
  Left := Value.X;
  Bottom := Value.Y;
end;

procedure TAdvancedRectF.SetPoint(Index: Integer; AValue: TPointF);
begin
  case Index of
    0: TopLeft := AValue;
    1: TopRight := AValue;
    2: BottomRight := AValue;
    3: BottomLeft := AValue;
  end;
end;

procedure TAdvancedRectF.SetTopRight(const Value: TPointF);
begin
  Right := Value.X;
  Top := Value.Y;
end;

{ TAdvancedButton }

function TAdvancedControl.GetPoint(Index: Integer): TPointF;
begin
  case Index of
    0: Result := Position.Point;
    1: Result := PointF(Position.X + Width, Position.Y);
    2: Result := PointF(Position.X + Width, Position.Y + Height);
    3: Result := PointF(Position.X, Position.Y + Height);
  end;
end;

procedure TAdvancedControl.SetPoint(Index: Integer; AValue: TPointF);
begin
  case Index of
    0: Position.Point := AValue;
    1: Position.Point := PointF(AValue.X - Width, AValue.Y);
    2: Position.Point := PointF(AValue.X - Width, AValue.Y - Height);
    3: Position.Point := PointF(AValue.X, AValue.Y - Height);
  end;
end;

{ TAdvancedPolygon }

function TAdvancedPolygon.Count: Integer;
begin
  Result := Length(Self);
end;

end.




