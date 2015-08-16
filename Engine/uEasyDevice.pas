unit uEasyDevice;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Edit, FMX.Ani, FMX.Controls3D, FMX.Layers3D,
  FMX.Effects, System.IOUtils, FMX.Platform, FMX.VirtualKeyboard;

  function getDisplaySizeInPx: tPointF;
  function getScreenScale: single;
  function getMinLength: single;
  function getDisplaySizeInDp: tPointF;
  function LoadImageFromFile(AFileName: String): TBitmap; // Открывает битмап по пути внезависимости от названия
  function UniPath(const AFileName: String): String; // Даёт универсальный путь вне зависимости от платформы
  function ReturnPressed(const AKey: Word): Boolean;
  procedure StrStartEnd(const AString: String; var AStart, AEnd: Integer);

implementation

uses
  mainUnit;

function ReturnPressed(const AKey: Word): Boolean;
var
  FService : IFMXVirtualKeyboardService;
begin
  if AKey = vkHardwareBack then
  begin
    TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FService));
    if (FService <> nil) and
      (TVirtualKeyboardState.Visible in FService.VirtualKeyBoardState)
    then
      Exit(True);
  end;
  Result := False;
end;

// У фаерманки Стринги начинаются с 0, а не с 1 как у делфи
procedure StrStartEnd(const AString: String; var AStart, AEnd: Integer);
begin
 {$IFDEF WIN32}
 AStart := 1;
 AEnd := Length(AString);
{$ENDIF WIN32}
{$IFDEF ANDROID}
 AStart := 0;
 AEnd := Length(AString) - 1;
{$ENDIF ANDROID}
end;

function getDisplaySizeInDp: tPointF;
var
  ScreenSvc: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
    IInterface(ScreenSvc)) then
    Result := ScreenSvc.GetScreenSize;
end;

function getDisplaySizeInPx: tPointF;
var
  res: tPointF;
  screenSizeInDp: tPointF;
  ScreenSvc: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
    IInterface(ScreenSvc)) then
  begin
    screenSizeInDp := ScreenSvc.GetScreenSize;
    res.X := screenSizeInDp.X * ScreenSvc.getScreenScale;
    res.Y := screenSizeInDp.Y * ScreenSvc.getScreenScale;
  end;

  {$IFDEF WIN32}
  // Это неправильно, но используется только для отладки в Виндоус и демонстрации форматтерсов
  res.X := mainForm.ClientWidth;
  res.Y := mainForm.ClientHeight;
  {$ENDIF}

  result := res;
end;

function UniPath(const AFileName: String): String;
var
  vS: String;
begin
  vS := AFileName;
  {$IFDEF WIN32}
    vS := 'art\'+AFileName;
  {$ENDIF WIN32}
  {$IFDEF ANDROID}
//    vS := TPath.Combine(TPath.GetSharedDocumentsPath, AFileName); { Внешний доступ }
    vS := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath, AFileName); { Внутренний доступ }

  {$ENDIF ANDROID}
  Result := vS;
end;

function LoadImageFromFile(AFileName: String): TBitmap;
var
  vBMP: TBitmap;
  {$IFDEF ANDROID}
  vs: String;
  {$ENDIF ANDROID}
begin
  vBMP := tBitmap.Create;
  {$IFDEF WIN32}
    vBMP.LoadFromFile(AFileName);
  {$ENDIF WIN32}
  {$IFDEF ANDROID}
    vS := TPath.Combine(TPath.GetDocumentsPath, AFileName); { Внутренний доступ}
    vBMP.LoadFromFile(vS);
  {$ENDIF ANDROID}
  Result := vBMP;
end;

function getScreenScale: single;
var
//  screenSizeInDp: tPointF;
  ScreenSvc: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
    IInterface(ScreenSvc)) then
  begin
    result := ScreenSvc.getScreenScale;
  end else
    Result := 1;
end;

function getMinLength: single;
var
  screenSizeInPx: tPointF;
begin
  screenSizeInPx := getDisplaySizeInPx;

  if screenSizeInPx.X > screenSizeInPx.Y then
    result:=screenSizeInPx.Y
  else
    result:=screenSizeInPx.X;

end;

end.
