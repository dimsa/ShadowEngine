unit uSoEngineSize;

interface

uses
  uSoEngineEvents, uSoTypes;

type

  TSoEngineSize = class
  private
    FWidth: Single;
    FHeight: Single;
    FRect: TRectObject;
    procedure OnEngineResize(ASender: TObject);
  public
    property Width: Single read FWidth;
    property Height: Single read FHeight;
    property Rect: TRectObject read FRect;
    function IsHor: Boolean;
    constructor Create(const AEngineEvents: TSoEngineEvents; const AInitWidth, AInitHeight: Single);
    destructor Destroy; override;
  end;

implementation

{ TSoEngineSize }

constructor TSoEngineSize.Create(const AEngineEvents: TSoEngineEvents; const AInitWidth, AInitHeight: Single);
begin
  AEngineEvents.OnResize.Add(OnEngineResize);

  FRect := TRectObject.Create;
  FRect.TopLeft := TPointF.Create(0, 0);
  FRect.BottomRight := TPointF.Create(AInitWidth, AInitHeight);
  FWidth := AInitWidth;
  FHeight := AInitHeight;
end;

destructor TSoEngineSize.Destroy;
begin
  FRect.Free;
  inherited;
end;

function TSoEngineSize.IsHor: Boolean;
begin
  Result := FWidth >= FHeight;
end;

procedure TSoEngineSize.OnEngineResize(ASender: TObject);
begin
  FRect.Width := TAnonImage(ASender).Width;
  FRect.Height := TAnonImage(ASender).Height;
end;

end.
