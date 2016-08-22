unit uEngine2DManager;

interface

uses
  System.Types, System.UITypes, FMX.Graphics, System.SysUtils, System.SyncObjs,
  System.RegularExpressions, FMX.Objects, System.Classes,
  uEngine2DClasses, uEngine2DObject, uEngineFormatter, uEngine2DAnimation,
  uEngine2DText, uEngine2DShape, uEngine2DResources, uEngine2DAnimationList,
  uFormatterList, uEngine2DSprite, uFastFields, uEngine2DThread,
  uSpriteList, uEngine2DStatus, uClasses, uEngine2DModel;

type
  TEngine2DManager = class
  private
    FStatus: TEngine2DStatus;
    FImage: TImage;
    FResize: TProcedure;
    FCritical: TCriticalSection;
    FModel: TEngine2DModel;
    FEngineThread: TEngineThread;
    FAddedObjects: Integer; // Quantity of Added sprites Считает сколько спрайтов добавлено всего. Без учета удалений
    function GetEngineHeight: Integer;
    function GetEngineWidth: Integer;
    function GetEngineSpeed: Single;
    function GetItem(AIndex: Integer): tEngine2DObject;
    function GetItemS(AName: string): tEngine2DObject;
    procedure DeleteHandler(ASender: TObject);
    procedure BringToBackHandler(ASender: TObject);
    procedure SendToFrontHandler(ASender: TObject);
    procedure DelObject(const AObject: tEngine2DObject); // Del object from paint // Убирает спрайт из отрисовки
    procedure AddObject(const AObject: tEngine2DObject; const AName: String = ''); // Добавляет спрайт на отрисовку
    procedure SpriteToBack(const n: integer); // Moves sprite to back // Передвигает в массиве отрисовки спрайт
    procedure SpriteToFront(const n: integer); // Moves sprite to front // Передвигает в массиве отрисовки спрайт
  public
    constructor Create(
      const AStatus: TEngine2DStatus;
      const AImage: TImage;
      const ACritical: TCriticalSection;
      const AModel: TEngine2DModel;
      const AEngineThread: TEngineThread;
      const AResize: TProcedure
      );
    destructor Destroy; override;
    function Formatter(const ASubject: tEngine2DObject; const AText: String; const AIndex: Integer = -1): TEngineFormatter; overload;
    function Formatter(const ASubject: tEngine2DObject; const AName: String; const AParam: array of const; const AIndex: Integer = -1): TEngineFormatter; overload;

    procedure ApplyFormatter(const ASubject: tEngine2DObject; const AFormatterName: string);
    procedure ApplyCollider(const ASubject: tEngine2DObject; const AColliderName: string);

    // Adding Created Object
    function Add(const ASprite: TSprite; const AName: string = ''): TSprite; overload;
    function Add(const AShape: TEngine2DShape; const AName: string = ''): TEngine2DShape; overload;
    function Add(const AText: TEngine2DText; const AName: string = ''): TEngine2DText; overload;
    function Add(const AAnimation: TAnimation): TAnimation; overload;

    // Created and Add Object
    function Sprite(const AName: string = ''): TSprite;
    function Text(const AName: string = ''): TEngine2DText;
    function FillEllipse(const AName: string = ''): TFillEllipse;
    function FillRect(const AName: string = ''): TFillRect;

    procedure AniClearAndRecover(const ASubject: tEngine2DObject); // Kills all animation of object and Recover state before they were applied
    procedure AniClear(const ASubject: tEngine2DObject); // Kills all animation of object

    property Items[AIndex: Integer]: tEngine2DObject read GetItem; default;
    property Items[AName: string]: tEngine2DObject read GetItemS; default;
    function ResourceIndex(const AName: string): Integer;
    function AutoSprite(const AResource: string; const AName: string; const AParam: array of const; const AGroup: string = ''; const AJustify: TObjectJustify = Center): TSprite; overload;

    // Deprecated! It's shouldn't be here. You shouldn't use it
    property EngineWidth: Integer read GetEngineWidth;
    property EngineHeight: Integer read GetEngineHeight;
    property EngineSpeed: Single read GetEngineSpeed;

    // Прячем или показывает группы
    procedure ShowGroup(const AGroup: String);
    procedure HideGroup(const AGroup: String);
    procedure SendToFrontGroup(const AGroup: String); // Send all object group to front // Ставит группу на передний план
    procedure BringToBackGroup(const AGroup: String); // Bring all object group to back Отодвигает группу на задний план

    procedure Resize;
    procedure RemoveObject(AObject: tEngine2DObject);
  end;

implementation

uses
  uEngine2D, uEngine2DStandardAnimations;

{ TEngine2DObjectCreator }

function TEngine2DManager.Add(const ASprite: TSprite;
  const AName: string): TSprite;
begin
  Result := ASprite;
  ASprite.Resources := FModel.Resources;
  AddObject(Result, AName);
end;

function TEngine2DManager.Add(const AShape: TEngine2DShape;
  const AName: string): TEngine2DShape;
begin
  Result := AShape;
  AddObject(Result, AName);
end;

function TEngine2DManager.Add(const AText: TEngine2DText;
  const AName: string): TEngine2DText;
begin
  Result := AText;
  AddObject(Result, AName);
end;

procedure TEngine2DManager.AniClear(const ASubject: tEngine2DObject);
begin
  FModel.AnimationList.ClearForSubject(ASubject);
end;

procedure TEngine2DManager.AniClearAndRecover(
  const ASubject: tEngine2DObject);
begin
  FModel.AnimationList.ClearAndRecoverForSubject(ASubject);
end;

procedure TEngine2DManager.ApplyCollider(const ASubject: tEngine2DObject;
  const AColliderName: string);
begin

end;

procedure TEngine2DManager.ApplyFormatter(const ASubject: tEngine2DObject;
  const AFormatterName: string);
begin

end;

function TEngine2DManager.Add(
  const AAnimation: TAnimation): TAnimation;
begin
  AAnimation.OnDeleteSubject := DeleteHandler;
  AAnimation.Status := FStatus;

  FModel.AnimationList.Add(AAnimation);
  Result := AAnimation;
end;

procedure TEngine2DManager.AddObject(const AObject: tEngine2DObject;
  const AName: String);
var
  l: integer;
  vName: string;
begin
  Inc(FAddedObjects);
  if AName = '' then
    vName := 'genname'+IntToStr(FAddedObjects)+'x'+IntToStr(Random(65536))
  else
    vName := AName;

  if FModel.ObjectList.IsHere(AObject) then
    raise Exception.Create('You are trying to add Object to Engine that already Exist')
  else
  begin
    FCritical.Enter;
    l := FModel.ObjectList.Count;
    FModel.ObjectList.Add(vName, AObject);
    setLength(FModel.ObjectOrder, l + 1);
    FModel.ObjectList[l].Image := FImage;
    AObject.OnBringToBack := BringToBackHandler;
    AObject.OnSendToFront := SendToFrontHandler;
    FModel.ObjectOrder[l] := l;
    FCritical.Leave;
  end;
end;

//procedure TEngine2DManager.AddObject(const AObject: tEngine2DObject;
//  const AName: String);
//var
//  l: integer;
//  vName: string;
//begin
//  // It's bad analog of generating GUID
//  Inc(FAddedObjects);
//  if AName = '' then
//    vName := 'genname'+IntToStr(FAddedObjects)+'x'+IntToStr(Random(65536))
//  else
//    vName := AName;
//
//  if FObjects.IsHere(AObject) then
//    raise Exception.Create('You are trying to add Object to Engine that already Exist')
//  else
//  begin
//    FCritical.Enter;
//    l := FObjects.Count;
//    FObjects.Add(vName, AObject);
//    setLength(FObjectOrder, l + 1);
//    FObjects[l].Image := FImage;
//    AObject.OnBringToBack := BringToBackHandler;
//    AObject.OnSendToFront := SendToFrontHandler;
//    FObjectOrder[l] := l;
//    FCritical.Leave;
//  end;
//end;

function TEngine2DManager.AutoSprite(const AResource, AName: string;
  const AParam: array of const; const AGroup: string;
  const AJustify: TObjectJustify): TSprite;
begin
  Result := Sprite(AName).Config(AResource, AGroup, AJustify);
  Formatter(Result, AName, []);
end;

procedure TEngine2DManager.BringToBackGroup(const AGroup: String);
var
  i, iObject, iG: Integer;
  vReg: TRegEx;
  vStrs: TArray<string>;
  vN: Integer;
begin
  vReg := TRegEx.Create(',');
  vStrs := vReg.Split(AGroup);
  vN := FModel.ObjectList.Count - 1;
  for iG := 0 to Length(vStrs) - 1 do
  begin
    i := vN;
    iObject := vN;
    vStrs[iG] := Trim(vStrs[iG]);
    while iObject > 1 do
    begin
      if FModel.ObjectList[FModel.ObjectOrder[i]].Group = vStrs[iG] then
      begin
        FModel.ObjectList[FModel.ObjectOrder[i]].BringToBack;
        Inc(i);
      end;
      Dec(i);
      Dec(iObject);
    end;
  end;
end;

procedure TEngine2DManager.BringToBackHandler(ASender: TObject);
begin
 SpriteToBack(
    FModel.ObjectList.IndexOfItem(TEngine2DObject(ASender), FromBeginning)
  );
end;

constructor TEngine2DManager.Create(
      const AStatus: TEngine2DStatus;
      const AImage: TImage;
      const ACritical: TCriticalSection;
      const AModel: TEngine2dModel;
      const AEngineThread: TEngineThread;
      const AResize: TProcedure);
begin
  FStatus := AStatus;
  FImage := AImage;
  FCritical := ACritical;
  FAddedObjects := 0;
  FModel := AModel;
  FEngineThread := AEngineThread;
  FResize := AResize;
end;

procedure TEngine2DManager.DeleteHandler(ASender: TObject);
begin
  DelObject(TEngine2DObject(ASender));
end;

procedure TEngine2DManager.DelObject(const AObject: tEngine2DObject);
var
  i, vN, vNum, vPos: integer;
begin
  FCritical.Enter;
  vNum := FModel.ObjectList.IndexOfItem(AObject, FromEnd);
  if vNum > -1 then
  begin
    vN := FModel.ObjectList.Count - 1;
    FModel.AnimationList.ClearForSubject(AObject);
    FModel.FormatterList.ClearForSubject(AObject);
    FModel.FastFields.ClearForSubject(AObject);
    FModel.ObjectList.Delete(vNum{AObject});

   // AObject.Free;

    vPos := vN + 1;
    // Находим позицию спрайта
    for i := vN downto 0 do
      if FModel.ObjectOrder[i] = vNum then
      begin
        vPos := i;
        Break;
      end;

    // От этой позиции сдвигаем порядок отрисовки
    vN := vN - 1;
    for i := vPos to vN do
      FModel.ObjectOrder[i] := FModel.ObjectOrder[i+1];

    // Все индексы спрайтов, которые больше vNum надо уменьшить на 1
    for i := 0 to vN do
      if FModel.ObjectOrder[i] >= vNum then
        FModel.ObjectOrder[i] := FModel.ObjectOrder[i] - 1;

    // Уменьшаем длину массива
    SetLength(FModel.ObjectOrder, vN + 1);
  end;
  FCritical.Leave;
//  FDebug := True;
end;

destructor TEngine2DManager.Destroy;
begin
  FStatus := nil;
  FImage := nil;
  FCritical := nil;
  FModel := nil;
  FEngineThread := nil;
  FResize := nil;
  inherited;
end;

function TEngine2DManager.Formatter(const ASubject: tEngine2DObject;
  const AText: String; const AIndex: Integer): TEngineFormatter;
begin
  Result := TEngineFormatter.Create(ASubject, FModel.ObjectList, FModel.FastFields);
  Result.Text := AText;
  if AIndex = -1 then
    FModel.FormatterList.Add(Result)
  else
    FModel.FormatterList.Insert(AIndex, Result);
end;

function TEngine2DManager.FillEllipse(const AName: string): TFillEllipse;
begin
  Result := TFillEllipse.Create;
  AddObject(Result, AName);
end;

function TEngine2DManager.FillRect(const AName: string): TFillRect;
begin
  Result := TFillRect.Create;
  AddObject(Result, AName);
end;

function TEngine2DManager.Formatter(const ASubject: tEngine2DObject;
  const AName: String; const AParam: array of const; const AIndex: Integer): TEngineFormatter;
var
  vS: string;
begin
  vS := Format(FModel.FormatterList.StyleByName[AName], AParam);
  Result := Formatter(ASubject, vS, AIndex);
end;

function TEngine2DManager.GetEngineHeight: Integer;
begin
  Result := FStatus.Height; //TEngine2d(FEngine).Height;
end;

function TEngine2DManager.GetEngineSpeed: Single;
begin
  Result := FEngineThread.Speed;
end;

function TEngine2DManager.GetEngineWidth: Integer;
begin
  Result := FStatus.Width;//TEngine2d(FEngine).Width;
end;

function TEngine2DManager.GetItem(AIndex: Integer): tEngine2DObject;
begin
  Result := FModel.ObjectList[AIndex];
end;

function TEngine2DManager.GetItemS(AName: string): tEngine2DObject;
begin
  Result := FModel.ObjectList[AName];
end;

procedure TEngine2DManager.HideGroup(const AGroup: String);
var
  i, iG: Integer;
  vReg: TRegEx;
  vStrs: TArray<string>;
begin
  vReg := TRegEx.Create(',');
  vStrs := vReg.Split(AGroup);
  for iG := 0 to Length(vStrs) - 1 do
  begin
    vStrs[iG] := Trim(vStrs[iG]);
    for i := 0 to FModel.ObjectList.Count - 1 do
      if FModel.ObjectList[i].group = vStrs[iG] then
        FModel.ObjectList[i].visible := False;
  end;
end;

procedure TEngine2DManager.RemoveObject(AObject: tEngine2DObject);
begin
  DelObject(AObject);
end;


procedure TEngine2DManager.Resize;
begin
  FResize;
end;

function TEngine2DManager.ResourceIndex(const AName: string): Integer;
begin
  Result := FModel.Resources.IndexOf(AName);
end;

procedure TEngine2DManager.SendToFrontGroup(const AGroup: String);
var
  i, iObject, iG: Integer;
  vReg: TRegEx;
  vStrs: TArray<string>;
  vN: Integer;
begin
  vReg := TRegEx.Create(',');
  vStrs := vReg.Split(AGroup);
  vN := FModel.ObjectList.Count - 1;
  for iG := 0 to Length(vStrs) - 1 do
  begin
    i := 1;
    iObject := 1;
    vStrs[iG] := Trim(vStrs[iG]);
    while iObject < vN do
    begin
      if FModel.ObjectList[FModel.ObjectOrder[i]].Group = vStrs[iG] then
      begin
        FModel.ObjectList[FModel.ObjectOrder[i]].SendToFront;
        Dec(i);
      end;
      Inc(i);
      Inc(iObject);
    end;
  end;
end;

procedure TEngine2DManager.SendToFrontHandler(ASender: TObject);
begin
 SpriteToFront(
    FModel.ObjectList.IndexOfItem(TEngine2DObject(ASender), FromBeginning)
  );
end;

{procedure TEngine2DManager.SendToFrontHandler(ASender: TObject);
begin
  SpriteToFront(
    FObjects.IndexOfItem(TEngine2DObject(ASender), FromBeginning)
  );
end;   }

procedure TEngine2DManager.ShowGroup(const AGroup: String);
var
  i, iG: Integer;
  vReg: TRegEx;
  vStrs: TArray<string>;
begin
  vReg := TRegEx.Create(',');
  vStrs := vReg.Split(AGroup);
  for iG := 0 to Length(vStrs) - 1 do
  begin
    vStrs[iG] := Trim(vStrs[iG]);
    for i := 0 to FModel.ObjectList.Count - 1 do
      if FModel.ObjectList[i].Group = vStrs[iG] then
        FModel.ObjectList[i].Visible := True;
  end;
end;

function TEngine2DManager.Sprite(const AName: string): TSprite;
begin
  Result := TSprite.Create;
  Result.Resources := FModel.Resources;
  AddObject(Result, AName);
end;

procedure TEngine2DManager.SpriteToBack(const n: integer);
var
  i, l, oldOrder: integer;
begin
  l := length(FModel.ObjectOrder);

  oldOrder := FModel.ObjectOrder[n]; // Узнаём порядок отрисовки спрайта номер n

  for i := 1 to l - 1 do
    if FModel.ObjectOrder[i] < oldOrder then
      FModel.ObjectOrder[i] := FModel.ObjectOrder[i] + 1;

  FModel.ObjectOrder[n] := 1;

end;

procedure TEngine2DManager.SpriteToFront(const n: integer);
var
  i, l, oldOrder: integer;
begin
  l := length(FModel.ObjectOrder);
  oldOrder := l - 1;

  for i := 1 to l - 1 do
    if FModel.ObjectOrder[i] = n then
    begin
      oldOrder := i;
      break;
    end;

  for i := oldOrder to l - 2 do
  begin
    FModel.ObjectOrder[i] := FModel.ObjectOrder[i + 1];
  end;

  FModel.ObjectOrder[l - 1] := n;
end;

function TEngine2DManager.Text(const AName: string): TEngine2DText;
begin
  Result := TEngine2DText.Create;
  AddObject(Result, AName);
end;

end.
