unit uEngine2DObjectCreator;

interface

uses
  System.Types, System.UITypes, FMX.Graphics, System.SysUtils,
  uEngine2DClasses, uEngine2DObject, uEngineFormatter, uEngine2DAnimation,
  uEngine2DText, uEngine2DShape, uEngine2DResources, uEngine2DAnimationList,
  uFormatterList, uEngine2DSprite,
  uSpriteList;

type

  TEngine2DObjectCreator = class
  private
    FEngine: Pointer;
    fObjects: TObjectsList; // Массив спрайтов для отрисовки
    fResources: TEngine2DResources;//tResourceArray; // Массив битмапов
    fFormatters: TFormatterList; // Массив Форматтеров спрайтов
    fAnimationList: TEngine2DAnimationList; // Массив анимаций
  public
    constructor Create(
      const AEngine: Pointer;
      const AResourcesList: TEngine2DResources;
      const AObjectsList: TObjectsList;
      const AAnimationsList: TEngine2DAnimationList;
      const AFormattersList: TFormatterList);
    destructor Destroy; override;
    function Formatter(const ASubject: tEngine2DObject; const AText: String; const AIndex: Integer = -1): TEngineFormatter;
    function SeCSSFormatter(const ASubject: tEngine2DObject; const AName: String; const AParam: array of const; const AIndex: Integer = -1): TEngineFormatter;
    function Text(const AText: string = ''; const AColor: TAlphaColor = TAlphaColorRec.White; const AName: string = ''; const AGroup: string = ''; const AJustify: TObjectJustify = Center): TEngine2DText;
    function Sprite(const AResource: string; const AName: string = ''; const AGroup: string = ''; const AJustify: TObjectJustify = Center): TSprite; overload;
    function Sprite(const AResource: Integer; const AName: string = ''; const AGroup: string = ''; const AJustify: TObjectJustify = Center): TSprite; overload;
  end;

implementation

uses
  uEngine2D;

{ TEngine2DObjectCreator }

constructor TEngine2DObjectCreator.Create(const AEngine: Pointer;
  const AResourcesList: TEngine2DResources; const AObjectsList: TObjectsList;
  const AAnimationsList: TEngine2DAnimationList;
  const AFormattersList: TFormatterList);
begin
  FEngine := AEngine;
  fObjects := AObjectsList;
  fResources := AResourcesList;
  fAnimationList := AAnimationsList;
  fFormatters := AFormattersList;
end;

destructor TEngine2DObjectCreator.Destroy;
begin
  FEngine := Nil;
  fObjects := Nil;
  fResources := Nil;
  fAnimationList := Nil;
  fFormatters := Nil;
  inherited;
end;

function TEngine2DObjectCreator.Formatter(const ASubject: tEngine2DObject;
  const AText: String; const AIndex: Integer): TEngineFormatter;
begin
  Result := TEngineFormatter.Create(ASubject);
  Result.Text := AText;
  if AIndex = -1 then
    tEngine2d(FEngine).FormatterList.Add(Result)
  else
    tEngine2d(FEngine).FormatterList.Insert(AIndex, Result);
end;

function TEngine2DObjectCreator.SeCSSFormatter(const ASubject: tEngine2DObject;
  const AName: String; const AParam: array of const; const AIndex: Integer): TEngineFormatter;
begin
  Formatter(ASubject, Format(fFormatters.StyleByName[AName], AParam), AIndex);
end;

function TEngine2DObjectCreator.Sprite(const AResource: Integer; const AName,
  AGroup: string; const AJustify: TObjectJustify): TSprite;
begin
  Result := TSprite.Create(FEngine);
  Result.Resources := tEngine2d(FEngine).Resources;
  Result.CurRes := AResource;
  Result.Group := AGroup;
  Result.Justify := AJustify;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

function TEngine2DObjectCreator.Sprite(const AResource, AName, AGroup: string;
  const AJustify: TObjectJustify): TSprite;
begin
  if AResource <> '' then
    Result := Self.Sprite(tEngine2d(FEngine).Resources.IndexOf(AResource), AName, AGroup, AJustify);
end;

function TEngine2DObjectCreator.Text(const AText: string; const AColor: TAlphaColor; const AName,
  AGroup: string; const AJustify: TObjectJustify): TEngine2DText;
begin
  Result := TEngine2DText.Create(FEngine);
  Result.Text := AText;
  Result.Group := AGroup;
  Result.Color := AColor;
  Result.Justify := AJustify;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

end.
