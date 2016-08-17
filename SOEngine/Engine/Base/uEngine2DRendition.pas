// It's base class for views of objects. It's may be Sprite, Figure, Text and etc.
// It's only for rendering. It doesnt't know anything except Image where paint and object position

unit uEngine2DRendition;

interface

uses
  System.Classes, FMX.Objects,
  uBaseContainer, uEngine2DClasses;

type
  TEngine2DRendition = class abstract
  strict private
    FSubject: TBaseUnitContainer;
    FJustify: TObjectJustify;
    FBringToBack, FSendToFront: TNotifyEvent;
    FImage: TImage;
  protected
    procedure SetJustify(const Value: TObjectJustify); virtual;
  public
    property Justify: TObjectJustify read FJustify write SetJustify;
    property OnBringToBack: TNotifyEvent read FBringToBack write FBringToBack;
    property OnSendToFront: TNotifyEvent read FSendToFront write FSendToFront;
    procedure BringToBack; // Ставит спрайт первым в списке отрисовки. Т.е. Переносит назад
    procedure SendToFront; // Ставит спрайт последним в списке отрисовки. Т.е. Переносит вперед
    procedure Repaint; virtual; abstract; // Процедура отрисовки объекта, переписывается спрайтом или текстом и т.д.

    constructor Create(const ASubject: TBaseUnitContainer; const AImage: TImage);
    destructor Destroy; override;
  end;

implementation

{ TEngine2DRendition }

procedure TEngine2DRendition.BringToBack;
begin
  FBringToBack(Self);
end;

constructor TEngine2DRendition.Create(const ASubject: TBaseUnitContainer; const AImage: TImage);
begin
  FSubject := ASubject;
  FImage := AImage;
end;

destructor TEngine2DRendition.Destroy;
begin
  FSubject := nil;
  FImage := nil;
  FBringToBack := nil;
  FSendToFront := nil;
  inherited;
end;

procedure TEngine2DRendition.SendToFront;
begin
  FSendToFront(Self);
end;

procedure TEngine2DRendition.SetJustify(const Value: TObjectJustify);
begin
  FJustify := Value;
end;

end.
