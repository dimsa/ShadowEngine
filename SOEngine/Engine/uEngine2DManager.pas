unit uEngine2DManager;

interface

uses
  System.Types, System.UITypes, FMX.Graphics, System.SysUtils,
  uEngine2DClasses, uEngine2DObject, uEngineFormatter, uEngine2DAnimation,
  uEngine2DText, uEngine2DShape, uEngine2DResources, uEngine2DAnimationList,
  uFormatterList, uEngine2DSprite, uFastFields, uEngine2DThread,
  uSpriteList;

type

  TEngine2DObjectCreator = class
  private
    FEngine: Pointer;
    fObjects: TObjectsList; // Массив спрайтов для отрисовки
    fResources: TEngine2DResources;//tResourceArray; // Массив битмапов
    fFormatters: TFormatterList; // Массив Форматтеров спрайтов
    fAnimationList: TEngine2DAnimationList; // Массив анимаций
    FEngineThread: TEngineThread;
    FFastFields: TFastFields;
    function GetEngineHeight: Integer;
    function GetEngineWidth: Integer;
    function GetEngineSpeed: Single;
    function GetItem(AIndex: Integer): tEngine2DObject;
    function GetItemS(AName: string): tEngine2DObject;
  public
    constructor Create(
      const AEngine: Pointer;
      const AResourcesList: TEngine2DResources;
      const AObjectsList: TObjectsList;
      const AAnimationsList: TEngine2DAnimationList;
      const AFormattersList: TFormatterList;
      const AFastFields: TFastFields;
      const AEngineThread: TEngineThread
      );
    destructor Destroy; override;
    function Formatter(const ASubject: tEngine2DObject; const AText: String; const AIndex: Integer = -1): TEngineFormatter; overload;
    function Formatter(const ASubject: tEngine2DObject; const AName: String; const AParam: array of const; const AIndex: Integer = -1): TEngineFormatter; overload;

    function Add(const ASprite: TSprite; const AName: string = ''): TSprite; overload;
    function Add(const AShape: TEngine2DShape; const AName: string = ''): TEngine2DShape; overload;
    function Add(const AText: TEngine2DText; const AName: string = ''): TEngine2DText; overload;
    function Add(const AAnimation: TAnimation): TAnimation; overload;

    function Sprite(const AName: string = ''): TSprite;
    function Text(const AName: string = ''): TEngine2DText;
    function FillEllipse(const AName: string = ''): TFillEllipse;
    function FillRect(const AName: string = ''): TFillRect;

    procedure AniClearAndRecover(const ASubject: tEngine2DObject);
    procedure AniClear(const ASubject: tEngine2DObject);

    property Items[AIndex: Integer]: tEngine2DObject read GetItem; default;
    property Items[AName: string]: tEngine2DObject read GetItemS; default;
    function ResourceIndex(const AName: string): Integer;
    function AutoSprite(const AResource: string; const AName: string; const AParam: array of const; const AGroup: string = ''; const AJustify: TObjectJustify = Center): TSprite; overload;
    property EngineWidth: Integer read GetEngineWidth;
    property EngineHeight: Integer read GetEngineHeight;
    property EngineSpeed: Single read GetEngineSpeed;
    procedure ShowGroup(const AGroup: String);
    procedure HideGroup(const AGroup: String);
    procedure Resize;
    procedure RemoveObject(AObject: tEngine2DObject);
  end;

implementation

uses
  uEngine2D;

{ TEngine2DObjectCreator }

function TEngine2DObjectCreator.Add(const ASprite: TSprite;
  const AName: string): TSprite;
begin
  Result := ASprite;
  ASprite.Resources := fResources;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

function TEngine2DObjectCreator.Add(const AShape: TEngine2DShape;
  const AName: string): TEngine2DShape;
begin
  Result := AShape;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

function TEngine2DObjectCreator.Add(const AText: TEngine2DText;
  const AName: string): TEngine2DText;
begin
  Result := AText;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

procedure TEngine2DObjectCreator.AniClear(const ASubject: tEngine2DObject);
begin
  fAnimationList.ClearForSubject(ASubject);
end;

procedure TEngine2DObjectCreator.AniClearAndRecover(
  const ASubject: tEngine2DObject);
begin
  fAnimationList.ClearAndRecoverForSubject(ASubject);
end;

function TEngine2DObjectCreator.Add(
  const AAnimation: TAnimation): TAnimation;
begin
  AAnimation.Parent := tEngine2d(FEngine);
  FAnimationList.Add(AAnimation);
end;

function TEngine2DObjectCreator.AutoSprite(const AResource, AName: string;
  const AParam: array of const; const AGroup: string;
  const AJustify: TObjectJustify): TSprite;
begin
  Result := Sprite(AName).Config(AResource, AGroup, AJustify);
  Formatter(Result, AName, []);
end;

constructor TEngine2DObjectCreator.Create(const AEngine: Pointer;
  const AResourcesList: TEngine2DResources; const AObjectsList: TObjectsList;
  const AAnimationsList: TEngine2DAnimationList;
  const AFormattersList: TFormatterList; const AFastFields: TFastFields; const AEngineThread: TEngineThread);
begin
  FEngine := AEngine;
  fObjects := AObjectsList;
  fResources := AResourcesList;
  fAnimationList := AAnimationsList;
  fFormatters := AFormattersList;
  FFastFields := AFastFields;
  FEngineThread := AEngineThread;
end;

destructor TEngine2DObjectCreator.Destroy;
begin
  FEngine := Nil;
  fObjects := Nil;
  fResources := Nil;
  fAnimationList := Nil;
  fFormatters := Nil;
  FFastFields := Nil;
  FEngineThread := Nil;
  inherited;
end;

function TEngine2DObjectCreator.Formatter(const ASubject: tEngine2DObject;
  const AText: String; const AIndex: Integer): TEngineFormatter;
begin
  Result := TEngineFormatter.Create(ASubject, fObjects, FFastFields);
  Result.Text := AText;
  if AIndex = -1 then
    fFormatters.Add(Result)
  else
    fFormatters.Insert(AIndex, Result);
end;

function TEngine2DObjectCreator.FillEllipse(const AName: string): TFillEllipse;
begin
  Result := TFillEllipse.Create;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

function TEngine2DObjectCreator.FillRect(const AName: string): TFillRect;
begin
  Result := TFillRect.Create;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

function TEngine2DObjectCreator.Formatter(const ASubject: tEngine2DObject;
  const AName: String; const AParam: array of const; const AIndex: Integer): TEngineFormatter;
var
  vS: string;
begin
  vS := Format(fFormatters.StyleByName[AName], AParam);
  Result := Formatter(ASubject, vS, AIndex);
end;

function TEngine2DObjectCreator.GetEngineHeight: Integer;
begin
  Result := tEngine2d(FEngine).Height;
end;

function TEngine2DObjectCreator.GetEngineSpeed: Single;
begin
  Result := FEngineThread.Speed;
end;

function TEngine2DObjectCreator.GetEngineWidth: Integer;
begin
  Result := tEngine2d(FEngine).Width;
end;

function TEngine2DObjectCreator.GetItem(AIndex: Integer): tEngine2DObject;
begin
  Result := FObjects[AIndex];
end;

function TEngine2DObjectCreator.GetItemS(AName: string): tEngine2DObject;
begin
  Result := FObjects[AName];
end;

procedure TEngine2DObjectCreator.HideGroup(const AGroup: String);
begin
  tEngine2d(FEngine).HideGroup(AGroup);
end;

procedure TEngine2DObjectCreator.RemoveObject(AObject: tEngine2DObject);
begin
  tEngine2d(FEngine).DeleteObject(AObject);
end;


procedure TEngine2DObjectCreator.Resize;
begin
  tEngine2d(FEngine).Resize;
end;

function TEngine2DObjectCreator.ResourceIndex(const AName: string): Integer;
begin
  Result := fResources.IndexOf(AName);
end;

procedure TEngine2DObjectCreator.ShowGroup(const AGroup: String);
begin
  tEngine2d(FEngine).ShowGroup(AGroup);
end;

function TEngine2DObjectCreator.Sprite(const AName: string): TSprite;
begin
  Result := TSprite.Create;
  Result.Resources := fResources;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

function TEngine2DObjectCreator.Text(const AName: string): TEngine2DText;
begin
  Result := TEngine2DText.Create;
  tEngine2d(FEngine).AddObject(Result, AName);
end;

end.
