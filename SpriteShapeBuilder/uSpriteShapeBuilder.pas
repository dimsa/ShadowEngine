unit uSpriteShapeBuilder;

interface

uses
  System.Generics.Collections, FMX.Objects, FMX.StdCtrls, System.Classes,
  FMX.Dialogs, System.SysUtils, System.UITypes, FMX.Types, System.Types,
  uSSBElement, uNamedList, uEasyDevice;

type
  TSpriteShapeBuilder = class
  private
    FPanel: TPanel;
    FElements: TNamedList<TSSBElement>;
    FSelectedElement: TSSBElement;
    FIsMouseDown: Boolean;
    FMouseStartPoint, FMouseElementPoint, FElementStartPosition: TPointF;
    procedure DoSelect(ASender: TObject);
    procedure DoZoom(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
  public
    property SelectedElement: TSSBElement read FSelectedElement write FSelectedElement;
    procedure AddElement(const AFileName: string);
    procedure LoadProject(const AFileName: string);
    procedure SaveProject(const AFileName: string);
    constructor Create;
    procedure Init(const APanel: TPanel);
    destructor Destroy; override;
  const
    CPrec = 5;
  end;


implementation

{ TSpriteShapeBuilder }

procedure TSpriteShapeBuilder.AddElement(const AFileName: string);
var
  vSSBElement: TSSBElement;
begin
  vSSBElement := TSSBElement.Create(FPanel);
  vSSBElement.Parent := FPanel;
  vSSBElement.Bitmap.LoadFromFile(AFileName);
  vSSBElement.Width := vSSBElement.Bitmap.Width;
  vSSBElement.Height := vSSBElement.Bitmap.Height;

  vSSBElement.OnClick := DoSelect;
  vSSBElement.OnMouseDown := DoMouseDown;
  vSSBElement.OnMouseUp := DoMouseUp;
  vSSBElement.OnMouseMove := DoMouseMove;
  vSSBElement.OnMouseWheel := DoZoom;

  FElements.Add(vSSBElement);
end;

constructor TSpriteShapeBuilder.Create;
begin
  FElements := TNamedList<TSSBElement>.Create;
end;

destructor TSpriteShapeBuilder.Destroy;
var
  vSSBElement: TSSBElement;
begin
  for vSSBElement in FElements do
    vSSBElement.Free;
  FElements.Clear;
  FElements.Free;

  inherited;
end;

procedure TSpriteShapeBuilder.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := True;
  FMouseStartPoint := MousePos;
  FMouseElementPoint.X := X;
  FMouseElementPoint.Y := Y;
  if Sender is TSSBElement then
  begin
    FSelectedElement := TSSBElement(Sender);
    FElementStartPosition := FSelectedElement.Position.Point;
  end;

end;

procedure TSpriteShapeBuilder.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  i: Integer;
begin
  if Sender = FSelectedElement then
    if FIsMouseDown then
      with FSelectedElement{TSSBElement(Sender)} do
      begin
        Position.Point := FElementStartPosition + MousePos - FMouseStartPoint;
      //  Position.X := Position.X - FMouseElementPoint.X + X;
       // Position.Y := Position.Y - FMouseElementPoint.Y + Y;

        for i := 0 to FElements.Count - 1 do
          if FElements[i] <> FSelectedElement then
          begin
        { TODO : Need refactor and add some expression to find nearest point }
            if (Position.Point.X <= FElements[i].BoundsRect.Left + CPrec) and
               (Position.Point.X >= FElements[i].BoundsRect.Left  - CPrec)  then
                 Position.Point := PointF(FElements[i].Position.Point.X, Position.Point.Y);

            if (Position.Point.X <= FElements[i].BoundsRect.Right + CPrec) and
               (Position.Point.X >= FElements[i].BoundsRect.Right  - CPrec)  then
               Position.Point := PointF(FElements[i].BoundsRect.Right, Position.Point.Y);

            if (Position.Point.Y <= FElements[i].BoundsRect.Top + CPrec) and
               (Position.Point.Y >= FElements[i].BoundsRect.Top  - CPrec)  then
               Position.Point := PointF(Position.Point.X, FElements[i].BoundsRect.Top);

            if (Position.Point.Y <= FElements[i].BoundsRect.Bottom + CPrec) and
               (Position.Point.Y >= FElements[i].BoundsRect.Bottom  - CPrec)  then
               Position.Point := PointF(Position.Point.X, FElements[i].BoundsRect.Bottom);

            if (Position.Point.X + Width <= FElements[i].BoundsRect.Left + CPrec) and
               (Position.Point.X >= FElements[i].BoundsRect.Left  - CPrec)  then
                 Position.Point := PointF(FElements[i].Position.Point.X - Width, Position.Point.Y);

            if (Position.Point.X + Width <= FElements[i].BoundsRect.Right + CPrec) and
               (Position.Point.X >= FElements[i].BoundsRect.Right  - CPrec)  then
               Position.Point := PointF(FElements[i].BoundsRect.Right - Width, Position.Point.Y);

            if (Position.Point.Y + Height <= FElements[i].BoundsRect.Top + CPrec) and
               (Position.Point.Y + Height >= FElements[i].BoundsRect.Top  - CPrec)  then
               Position.Point := PointF(Position.Point.X, FElements[i].BoundsRect.Top - Height);

            if (Position.Point.Y + Height <= FElements[i].BoundsRect.Bottom + CPrec) and
               (Position.Point.Y + Height >= FElements[i].BoundsRect.Bottom  - CPrec)  then
               Position.Point := PointF(Position.Point.X, FElements[i].BoundsRect.Bottom - Height);
          end;

      end;
end;

procedure TSpriteShapeBuilder.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := False;
end;

procedure TSpriteShapeBuilder.DoSelect(ASender: TObject);
begin
  if ASender is TSSBElement then
    FSelectedElement := TSSBElement(ASender);
end;

procedure TSpriteShapeBuilder.DoZoom(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if FPanel.Scale.X + ((WheelDelta / 120) * 0.1) > 0.1 then
  begin
    FPanel.Scale.X := FPanel.Scale.X + ((WheelDelta / 120) * 0.1);
    FPanel.Scale.Y := FPanel.Scale.X;
  end;
end;

procedure TSpriteShapeBuilder.Init(const APanel: TPanel);
begin
  FPanel := APanel;
  FPanel.OnMouseWheel := DoZoom;
  FPanel.OnMouseDown := DoMouseDown;
  FPanel.OnMouseUp := DoMouseUp;
  FPanel.OnMouseMove := DoMouseMove;
  try
    FPanel.Canvas.BeginScene;
    FPanel.Canvas.Fill.Color := TAlphaColorRec.Blanchedalmond;
    FPanel.Canvas.FillRect(FPanel.BoundsRect, 0, 0, [], 1, FMX.Types.TCornerType.Round);
  finally
    FPanel.Canvas.EndScene;
  end;
end;

procedure TSpriteShapeBuilder.LoadProject(const AFileName: string);
begin

end;

procedure TSpriteShapeBuilder.SaveProject(const AFileName: string);
begin

end;

end.


