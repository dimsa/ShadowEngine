unit uSoFormatterDirective;

interface

uses
  uSoExpressionParser, uNamedList, uCommonClasses, uSoObject, uSoTypes,
  uSoObjectDefaultProperties;

type
  // It's storage of Expressions for fast formatting in Engine
  TFormatterDirective = class
  protected
    FObject: TSoObject;
    FExpression: TExpression;
  public
    procedure Format; virtual; abstract;
    function Value: Double;
    constructor Create(const AObject: TSoObject; const AExpression: TExpression);
  end;

  TWidthDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  THeightDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TMaxWidthDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TMaxHeightDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TMinWidthDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TMinHeightDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TRotateDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TScaleDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TScaleXDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TScaleYDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TXDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TYDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TConditionalDirective = class(TFormatterDirective)
  protected
    FDirective: TFormatterDirective;
  public
    function IsSatisfy: Boolean; virtual; abstract;
    constructor Create(const ADirective: TFormatterDirective);
    procedure Format; override;
  end;

  TIfHorCondition = class(TConditionalDirective)
  strict private
    FIsHor: TDelegate<Boolean>;
  public
    function IsSatisfy: Boolean; override;
    constructor Create(const ADirective: TFormatterDirective; AIsHor: TDelegate<Boolean>);

  end;

implementation

{ TFormatterDirective }

constructor TFormatterDirective.Create(const AObject: TSoObject; const AExpression: TExpression);
begin
  FExpression := AExpression;
  FObject := AObject;
end;

function TFormatterDirective.Value: Double;
begin
  Result := FExpression.Value;
end;

{ TWidthDir }

procedure TWidthDir.Format;
begin
  FObject.Position.ScaleX := Self.Value / (TSizeObject(FObject[RenditionRect].Obj).Width);
end;

{ THeightDir }

procedure THeightDir.Format;
begin
  FObject.Position.ScaleY := Self.Value / (TSizeObject(FObject[RenditionRect].Obj).Height);
end;

{ TMaxWidthDir }

procedure TMaxWidthDir.Format;
begin
  if TSizeObject(FObject[RenditionRect].Obj).Width * FObject.Position.ScaleX > Value then
    FObject.Position.ScaleX := Value / TSizeObject(FObject[RenditionRect].Obj).Width;
end;

{ TMaxHeightDir }

procedure TMaxHeightDir.Format;
begin
  if TSizeObject(FObject[RenditionRect].Obj).Height * FObject.Position.ScaleY  > Value then
    FObject.Position.ScaleY := Value / TSizeObject(FObject[RenditionRect].Obj).Height;
end;

{ TMinWidthDir }

procedure TMinWidthDir.Format;
begin
  if TSizeObject(FObject[RenditionRect].Obj).Width * FObject.Position.ScaleX  < Value then
    FObject.Position.ScaleX := Value / TSizeObject(FObject[RenditionRect].Obj).Width;
end;

{ TMinHeightDir }

procedure TMinHeightDir.Format;
begin
  if TSizeObject(FObject[RenditionRect].Obj).Height * FObject.Position.ScaleY  < Value then
    FObject.Position.ScaleY := Value / TSizeObject(FObject[RenditionRect].Obj).Height;
end;

{ TRotateDir }

procedure TRotateDir.Format;
begin
  FObject.Position.Rotate := Value;
end;

{ TScaleDir }

procedure TScaleDir.Format;
begin
  FObject.Position.ScaleX := Value;
  FObject.Position.ScaleY := Value;
end;

{ TScaleXDir }

procedure TScaleXDir.Format;
begin
  FObject.Position.ScaleX := Value;
end;

{ TScaleYDir }

procedure TScaleYDir.Format;
begin
  FObject.Position.ScaleY := Value;
end;

{ TXDir }

procedure TXDir.Format;
begin
  FObject.Position.X := Value;
end;

{ TYDir }

procedure TYDir.Format;
begin
  FObject.Position.Y := Value;
end;

{ TConditionalDirective }

constructor TConditionalDirective.Create(const ADirective: TFormatterDirective);
begin
  FObject := ADirective.FObject; // Сейчас это дружественные классы, но вообще надо следить за таким
  FDirective := ADirective;
end;

procedure TConditionalDirective.Format;
begin
  if IsSatisfy then
    FDirective.Format;
end;

{ TIfHorCondition }

constructor TIfHorCondition.Create(const ADirective: TFormatterDirective;
  AIsHor: TDelegate<Boolean>);
begin
  FIsHor := AIsHor;
  inherited Create(ADirective);
end;

function TIfHorCondition.IsSatisfy: Boolean;
begin
  Result := FIsHor;
end;

end.
