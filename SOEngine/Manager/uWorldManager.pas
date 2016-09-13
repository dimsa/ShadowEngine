unit uWorldManager;

interface

uses
  uClasses, uEngine2DClasses, uSoModel;

type
  TSoModelFriend = class(TSoModel);

  TWorldManager = class
  private
    FModel: TSoModelFriend;
    procedure SetOnPaintBackground(const Value: TEvent<TAnonImage>);
    procedure SetOnBeginPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnEndPaint(const Value: TEvent<TAnonImage>);
  public
    constructor Create(const AModel: TSoModel);
    property OnPaintBackground: TEvent<TAnonImage> write SetOnPaintBackground;
    property OnBeginPaint: TEvent<TAnonImage> write SetOnBeginPaint;
    property OnEndPaint: TEvent<TAnonImage> write SetOnEndPaint;
  end;

implementation

{ TWorldManager }

constructor TWorldManager.Create(const AModel: TSoModel);
begin
  FModel := TSoModelFriend(AModel);
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

