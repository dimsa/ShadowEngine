unit uE2DResourceList;

interface

uses
  FMX.Graphics, System.Types, System.SysUtils,
  uEngine2DClasses, uE2DResource;

type
  TEngine2DResourceList = class(TEngine2DNamedList<TSoSpriteResource>)
  strict private
    FInputBmp: TBitmap; // Service Bitmap for temporary storage // Служебный битмап для временного хранения
    procedure RenewBitmap;
  public
    property InputBmp: TBitmap read FInputBmp write FInputBmp;
    procedure LoadBmp(const AFileName: string);
    function AddFromRes(const x, y, w, h: Integer; const AName: string = ''): Integer; // Добавляет ресурс из бмп с именем
    function AddResFromLoadFileRes(const AFileName: string): Integer; // Добавляет ресурс из текстового файла особого формата
    function AddResource(const ABitmap: TBitmap): Integer; // Add Bitmap to resources and returns its number  // Добавляет битмап в массив ресурсов и говорит его номер
    destructor Destroy; override;
  end;

implementation

uses
  uEasyDevice;

{ TEngine2DResourceList }

function TEngine2DResourceList.AddFromRes(const x, y, w, h: Integer; const AName: string): Integer;
var
  vBmp: TBitmap;
  vSprRes: TSoSpriteResource;
  vN: Integer;
begin
  FCriticalSection.Enter;
  vBmp := TBitmap.Create;
  vBmp.Width := w;
  vBmp.Height := h;
  vBmp.Canvas.BeginScene();
  vBmp.Clear(1);
  vBmp.Canvas.DrawBitmap(
    FInputBmp,
    RectF(x, y, x + w, y + h),
    RectF(0, 0, w, h), 1, False);
  vBmp.Canvas.EndScene;

  vSprRes := TSoSpriteResource.Create(vBmp);
  vN := Self.Add(AName, vSprRes);

  Result := vN;
  FCriticalSection.Leave;
end;

function TEngine2DResourceList.AddResFromLoadFileRes(const AFileName: string): Integer;
var
  vF: TextFile;
  vS, vResName: string;
  vX, vY, vW, vH: Integer;
  vRes: Integer;
begin
  if not FileExists(UniPath(AFileName)) then
    raise Exception.Create('File "' + UniPath(AFileName) + '" not found!');
  AssignFile(vF, UniPath(AFileName));
  Reset(vF);
  Readln(vF, vS);

  if not FileExists(UniPath(vS)) then
    raise Exception.Create('File "' + UniPath(vS) + '" not found!');

  fInputBmp := uEasyDevice.LoadImageFromFile(UniPath(vS));
  repeat
    Readln(vF, vX, vY, vW, vH, vResName);
    System.delete(vResName, 1, 1);
    vRes := AddFromRes(vX,vY, vW, vH, vResName);
  until EOF(vF);

  CloseFile(vF);
  fInputBmp.Free;

  Result := vRes;
end;

function TEngine2DResourceList.AddResource(const ABitmap: TBitmap): Integer;
var
  vTmp: TSoSpriteResource;
begin
  vTmp := TSoSpriteResource.Create(ABitmap);
  Result := Self.Add(vTmp);
end;

destructor TEngine2DResourceList.Destroy;
begin
  FInputBmp := nil;
  inherited;
end;

procedure TEngine2DResourceList.LoadBmp(const AFileName: string);
begin
  RenewBitmap;
  FInputBmp.LoadFromFile(AFileName);
end;

procedure TEngine2DResourceList.RenewBitmap;
begin
  if FInputBmp <> nil then
    FreeAndNil(FInputBmp);

  if fInputBmp = nil then
    FInputBmp := TBitmap.Create;
end;

end.
