unit uItemObjecterPresenter;

interface

uses
  System.Types, System.Generics.Collections,
  uItemBasePresenter, uItemShaperPresenter, uIItemView, uSSBModels;

type
  TItemShaperPresenterFriend = class(TItemShaperPresenter);

  TItemObjecterPresenter = class(TItemBasePresenter)
  private
    FShapes: TList<TItemShaperPresenterFriend>;
    FIsShapeVisible: Boolean;
    FCapturedShape: TItemShaperPresenterFriend;
    FSelectedShaper: TItemShaperPresenterFriend;
    function GetHeight: Integer;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
  public
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
    procedure ShowShapes;
    procedure HideShapes;
    procedure AddPoly;
    procedure AddCircle;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure Delete; override;
    constructor Create(const AItemView: IItemView; const AModel: TSSBModel); override;
    destructor Destroy; override;
  end;

implementation

{ TObjecterItemPresenter }

procedure TItemObjecterPresenter.AddCircle;
var
  vShape: TItemShaperPresenterFriend;
begin
  vShape := TItemShaperPresenterFriend.Create(FView, FModel);
  vShape.MakeCircle;
  FShapes.Add(vShape);
  vShape.Repaint;
end;

procedure TItemObjecterPresenter.AddPoly;
var
  vShape: TItemShaperPresenterFriend;
begin
  vShape := TItemShaperPresenterFriend.Create(FView, FModel);
  vShape.MakePoly;
  FShapes.Add(vShape);
  vShape.Repaint;
end;

constructor TItemObjecterPresenter.Create(const AItemView: IItemView; const AModel: TSSBModel);
begin
  inherited;
  FShapes := TList<TItemShaperPresenterFriend>.Create;
end;

procedure TItemObjecterPresenter.Delete;
begin
  inherited;

end;

destructor TItemObjecterPresenter.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Free;
  FShapes.Free;
  inherited;
end;

function TItemObjecterPresenter.GetHeight: Integer;
begin
  Result := FView.Height;
end;

function TItemObjecterPresenter.GetPosition: TPoint;
begin
  Result := Point(FView.Left, FView.Top);
end;

function TItemObjecterPresenter.GetWidth: Integer;
begin
  Result := FView.Width;
end;

procedure TItemObjecterPresenter.HideShapes;
begin
  FIsShapeVisible := False;
end;

procedure TItemObjecterPresenter.MouseDown;
var
  i: Integer;
begin
  inherited;

  if FIsShapeVisible then
    for i := 0 to FShapes.Count - 1 do
      if FShapes[i].IsPointIn(FView.MousePos) then
        FShapes[i].MouseDown;

  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self);
end;

procedure TItemObjecterPresenter.MouseMove;
var
  i: Integer;
begin
  inherited;

  if FIsShapeVisible then
    for i := 0 to FShapes.Count - 1 do
      if FShapes[i].IsPointIn(FView.MousePos) then
        FShapes[i].MouseMove;

  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self);
end;

procedure TItemObjecterPresenter.MouseUp;
var
  i: Integer;
begin
  inherited;

  if FIsShapeVisible then
    for i := 0 to FShapes.Count - 1 do
      if FShapes[i].IsPointIn(FView.MousePos) then
        FShapes[i].MouseUp;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self)
end;

procedure TItemObjecterPresenter.SetHeight(const Value: Integer);
begin
  FView.Height := Value;
end;

procedure TItemObjecterPresenter.SetPosition(const Value: TPoint);
begin
  FView.Left := Value.X;
  FView.Top := Value.Y;
end;

procedure TItemObjecterPresenter.SetWidth(const Value: Integer);
begin
  FView.Width := Value;
end;

procedure TItemObjecterPresenter.ShowShapes;
var
  i: Integer;
begin
  FIsShapeVisible := True;

  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Repaint;
end;

end.
