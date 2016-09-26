unit uWorldManager;

interface

uses
  uSoTypes, uClasses, uEngine2DClasses, uSoModel, uCommonClasses;

type
  TSoModelFriend = class(TSoModel);

  TWorldManager = class
  private
    FModel: TSoModelFriend;
    FEngineSize: TPointF;
    FOnResize: TEventList<TAnonImage>;
    procedure SetOnPaintBackground(const Value: TEvent<TAnonImage>);
    procedure SetOnBeginPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnEndPaint(const Value: TEvent<TAnonImage>);
    function GetSize: TPointF;
  public
    constructor Create(const AModel: TSoModel; const AOnResize: TEventList<TAnonImage>);
    property Size: TPointF read GetSize;
    property OnPaintBackground: TEvent<TAnonImage> write SetOnPaintBackground;
    property OnBeginPaint: TEvent<TAnonImage> write SetOnBeginPaint;
    property OnEndPaint: TEvent<TAnonImage> write SetOnEndPaint;
    property OnResize: TEventList<TAnonImage> read FOnResize;
    property EngineSize: TPointF read FEngineSize;
  end;

implementation

{ TWorldManager }

constructor TWorldManager.Create(const AModel: TSoModel; const AOnResize: TEventList<TAnonImage>);
begin
  FModel := TSoModelFriend(AModel);
  FOnResize := AOnResize;
end;

function TWorldManager.GetSize: TPointF;
begin
  Result := FModel.EngineSize;
end;

procedure TWorldManager.SetOnBeginPaint(const Value: TEvent<TAnonImage>);
begin
  FModel.Renderer.OnBeginPaint := Value;
end;

procedure TWorldManager.SetOnEndPaint(const Value: TEvent<TAnonImage>);
begin
  FModel.Renderer.OnEndPaint := Value;
end;

procedure TWorldManager.SetOnPaintBackground(const Value: TEvent<TAnonImage>);
begin
  FModel.Renderer.OnPaintBackground := Value;
end;

end.

