// It's base class for views of objects. It's may be Sprite, Figure, Text and etc.
// It's only for rendering. It doesnt't know anything except Image where paint and object position

unit uE2DRendition;

interface

uses
  System.Classes, FMX.Objects,
  uSoContainer, uEngine2DClasses, uSoBasePart;

type
  TEngine2DRendition = class abstract(TSoBasePart)
  strict private
    FBringToBack, FSendToFront: TNotifyEvent;
    procedure SetOpacity(const Value: Single);
  protected
    FImage: TImage;
    FOpacity: Single;
    FJustify: TObjectJustify;
    procedure SetJustify(const Value: TObjectJustify); virtual;
    function GetHeight: Single; virtual; abstract;
    function GetWidth: Single; virtual; abstract;
  public
    property Justify: TObjectJustify read FJustify write SetJustify;
    property Opacity: Single read FOpacity write SetOpacity;
    property OnBringToBack: TNotifyEvent read FBringToBack write FBringToBack;
    property OnSendToFront: TNotifyEvent read FSendToFront write FSendToFront;
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;
    procedure BringToBack; // Ставит спрайт первым в списке отрисовки. Т.е. Переносит назад
    procedure SendToFront; // Ставит спрайт последним в списке отрисовки. Т.е. Переносит вперед
    procedure Repaint; virtual; abstract; // Процедура отрисовки объекта, переписывается спрайтом или текстом и т.д.

    constructor Create(const ASubject: TSoContainer; const AImage: TImage);
    destructor Destroy; override;
  end;

implementation

{ TEngine2DRendition }

procedure TEngine2DRendition.BringToBack;
begin
  FBringToBack(Self);
end;

constructor TEngine2DRendition.Create(const ASubject: TSoContainer; const AImage: TImage);
begin
  inherited Create(ASubject);
  FImage := AImage;
end;

destructor TEngine2DRendition.Destroy;
begin
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

procedure TEngine2DRendition.SetOpacity(const Value: Single);
begin
  FOpacity := Value;
end;

end.
