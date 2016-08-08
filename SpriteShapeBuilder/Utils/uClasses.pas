unit uClasses;

interface

uses
  System.SysUtils, System.Types, System.Math, {$I 'Utils\DelphiCompatability.inc'}
  System.UITypes, FMX.Controls, FMX.Dialogs,
  System.JSON;

type


  TProcedure = procedure of Object;
  TVCLProcedure = procedure(ASender: TObject) of object;
  TItemSelectEvent = procedure (Sender: TObject) of object;
  TDelegate<T> = function: T of object;

  ISerializable = interface
    function Serialize: TJSONObject;
    procedure Deserialize(const AJson: TJSONObject);
  end;

  TAdvancedRectF = record helper for TRectF
  private
//    FIsHard: Boolean; // If it is hard, when you setting the point width and height are not changing
    function GetPoint(Index: Integer): TPointF;
    procedure SetPoint(Index: Integer; AValue: TPointF);
    function GetBottomLeft: TPointF;
    function GetTopRight: TPointF;
    procedure SetBottomLeft(const Value: TPointF);
    procedure SetTopRight(const Value: TPointF);
    procedure SetAnchor(Index: Integer; const AValue: TPointF);
  public
//    property IsHard: Boolean read FIsHard write FIsHard;
    property Points[Index: Integer]: TPointF read GetPoint write SetPoint; default;
    property Anchors[Index: Integer]: TPointF write SetAnchor;
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
    procedure Add(const APoint: TPointF);
  end;

  function Random64: Int64;
  procedure NormalizeAngle(var AAngle: Single);
  function RGBColor(const AR, AG, AB, AA: Byte): TAlphaColorRec;
  function ToInt(const AValue: string): Integer;
  function ToFloat(const AValue: string): Double;

implementation

function ToInt(const AValue: string): Integer;
var
  vErr: Integer;
begin
  Val(AValue, Result, vErr);
  if vErr <> 0 then
  begin
    Result := -1;
    ShowMessage('Can not convert to Integer ' + AValue);
  end;
end;

function ToFloat(const AValue: string): Double;
var
  vErr: Integer;
begin
  Val(AValue, Result, vErr);
  if vErr <> 0 then
  begin
    Result := -1;
    ShowMessage('Can not convert to Float ' + AValue);
  end;
end;

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

procedure TAdvancedRectF.SetAnchor(Index: Integer; const AValue: TPointF);
var
  vTmp: TPointF;
begin
  vTmp := PointF(Width, Height);
  case Index of
    0: begin Self := RectF(AValue.X,          AValue.Y,          AValue.X + vTmp.X, AValue.Y + vTmp.Y); end; // TopLeft  TopLeft := AValue; BottomRight := AValue + vTmp;
    1: begin Self := RectF(AValue.X - vTmp.X, AValue.Y,          AValue.X,          AValue.Y + vTmp.Y); end;// TopRight TopRight := AValue; BottomLeft := PointF(AValue.X - vTmp.X, AValue.Y + vTmp.Y);
    2: begin Self := RectF(AValue.X - vTmp.X, AValue.Y - vTmp.Y, AValue.X,          AValue.Y); end;// BottomRight BottomRight := AValue; TopLeft := AValue - vTmp;
    3: begin Self := RectF(AValue.X,          AValue.Y - vTmp.Y, AValue.X + vTmp.X, AValue.Y); end; //BottomLeft BottomLeft := AValue; TopRight := PointF(AValue.X + vTmp.X, AValue.Y - vTmp.Y);
  end;
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
    0: Position.Point := PointF(AValue.X, AValue.Y);
    1: Position.Point := PointF(AValue.X - Width, AValue.Y);
    2: Position.Point := PointF(AValue.X - Width, AValue.Y - Height);
    3: Position.Point := PointF(AValue.X, AValue.Y - Height);
  end;
end;

{ TAdvancedPolygon }

procedure TAdvancedPolygon.Add(const APoint: TPointF);
begin
  SetLength(Self, Length(Self) + 1);
  Self[High(Self)] := APoint;
end;

function TAdvancedPolygon.Count: Integer;
begin
  Result := Length(Self);
end;

end.




