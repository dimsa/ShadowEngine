unit uEngine2DResources;

interface

Uses
  System.SyncObjs, System.SysUtils, Classes, System.Types, Generics.Collections,
  FMX.Graphics, FMX.Dialogs, System.IOUtils,
  uEngine2DCLasses, uEasyDevice;

type
  TEngine2DResources = class(TEngine2DNamedList<TSpriteResource>)
  strict private
    fInputBmp: tBitmap; // Service Bitmap for temporary storage // Служебный битмап для временного хранения
    procedure RenewBitmap;
  public
    property InputBmp: TBitmap read FInputBmp write FInputBmp;
    procedure LoadBmp(const AFileName: String);
    function AddFromRes(const x, y, w, h: integer): integer; overload; // Добавляет ресурс из бмп
    function AddFromRes(const x, y, w, h: integer; const AName: String): Integer; overload; // Добавляет ресурс из бмп с именем
    function AddResFromLoadFileRes(const AFileName: string): integer; // Добавляет ресурс из текстового файла особого формата
    function AddResource(const ABitmap: tBitmap): integer; // Add Bitmap to resources and returns its number  // Добавляет битмап в массив ресурсов и говорит его номер
    destructor Destroy; override;
  end;


implementation

uses
  uEngine2D;
{ TEngine2DResources }

function TEngine2DResources.AddResource(const ABitmap: tBitmap): integer;
var
  vTmp: TSpriteResource;
begin
  vTmp.bmp := ABitmap;
  vTmp.rect := RectF(0, 0, vTmp.bmp.Width, vTmp.bmp.Height);

  Result := Self.Add(vTmp);
end;

destructor TEngine2DResources.Destroy;
begin
  if FInputBmp <> nil then
    FInputBmp.Free;

  inherited;
end;

procedure TEngine2DResources.LoadBmp(const AFileName: String);
begin
  RenewBitmap;
  FInputBmp.LoadFromFile(AFileName);
end;

procedure TEngine2DResources.RenewBitmap;
begin
  if FInputBmp <> nil then
    FreeAndNil(FInputBmp);

  if fInputBmp = nil then
    FInputBmp := TBitmap.Create;
end;

function TEngine2DResources.AddFromRes(const x, y, w, h: integer): integer;
var
  bmp: tBitmap;
begin
  FCriticalSection.Enter;
  bmp := tBitmap.Create;
  bmp.Width := w;
  bmp.Height := h;
  bmp.Canvas.BeginScene();
  bmp.Clear(1);
  bmp.Canvas.DrawBitmap(
    fInputBmp,
    rectF(x, y, x + w, y + h),
    rectF(0, 0, w, h), 1, False);
  bmp.Canvas.EndScene;
  result := addResource(bmp);
  bmp.Free;
  FCriticalSection.Leave;
end;

function TEngine2DResources.AddFromRes(const x, y, w, h: integer; const AName: String): Integer;
var
  bmp: tBitmap;
  vSprRes: TSpriteResource;
  vN: Integer;
begin
  FCriticalSection.Enter;
  bmp := tBitmap.Create;
  bmp.Width := w;
  bmp.Height := h;
  bmp.Canvas.BeginScene();
  bmp.Clear(1);
  bmp.Canvas.DrawBitmap(
    fInputBmp,
    rectF(x, y, x + w, y + h),
    rectF(0, 0, w, h), 1, False);
  bmp.Canvas.EndScene;

  vSprRes.rect := RectF(0, 0, w, h);
  vSprRes.bmp := bmp;

  vN := Self.Add(AName, vSprRes);

  Result := vN;
  FCriticalSection.Leave;
end;

function TEngine2DResources.AddResFromLoadFileRes(const AFileName: string): integer;
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

end.





