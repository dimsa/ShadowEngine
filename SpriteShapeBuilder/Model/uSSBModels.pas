unit uSSBModels;

interface

uses
  System.Generics.Collections, System.Classes,
  FMX.Objects, FMX.StdCtrls, FMX.Controls, System.Types, FMX.Graphics,
  uNamedList, uClasses, uSSBTypes;

type
  TSSBModel = class abstract
  private
    function GetBackground: TBitmap; virtual; abstract;
    function GetElements: TList<TControl>; virtual; abstract;
    function GetSelectedBitmap: TBitmap; virtual; abstract;
  public
    property SelectedBitmap: TBitmap read GetSelectedBitmap;
    property Background: TBitmap read GetBackground;
    property Elements: TList<TControl> read GetElements;
  end;

  TSSBImagerModel = class(TSSBModel)
  private
    FSelected: TImage;
    FBackground: TBitmap;
    FImages: TNamedList<TImage>; //Картинки из которых состоит подложока объектов
    FImgToCtrlAdapter: TImgToCtrlAdapter;
    FNotifyEvent: TNotifyEvent;
    function GetBackground: TBitmap; override;
    function GetElements: TList<TControl>; override;
    function GetSelectedBitmap: TBitmap; override;
    function GetImageCount: Integer;
    function GetImage(AIndex: Integer): TImage;
    procedure SetImage(AIndex: Integer; const Value: TImage);
  public
    constructor Create(const ANotifyEvent: TNotifyEvent);
    destructor Destroy;
    procedure DelSelected;
    property ImageCount: Integer read GetImageCount;
    property Images[AIndex: Integer]: TImage read GetImage write SetImage;
    function Add(const AImage: TImage): Boolean;
    function Select(const AImage: TImage): Boolean;
    property Selected: TImage read FSelected;
  const
    CPrec = 5;
end;

implementation

{ TSSBImagerModel }

function TSSBImagerModel.Add(const AImage: TImage): Boolean;
begin
  FImages.Add(AImage);
  FNotifyEvent(Self);
end;

constructor TSSBImagerModel.Create(const ANotifyEvent: TNotifyEvent);
var
  vList: TList<TImage>;
begin
  FImages := TNamedList<TImage>.Create;

  FImgToCtrlAdapter := TImgToCtrlAdapter.Create(FImages);
  FBackground := TBitmap.Create;

  FNotifyEvent := ANotifyEvent;
end;

procedure TSSBImagerModel.DelSelected;
begin
  if FImages.IsHere(FSelected) then
  begin
    FImages.Delete(FSelected);
    FSelected.Free;
    FSelected := nil;
  end;
  FNotifyEvent(Self);
end;

destructor TSSBImagerModel.Destroy;
var
  vImg: TControl;
begin
  for vImg in FImages do
    vImg.Free;
  FImages.Clear;
  FImages.Free;

  FBackground.Free;

  FImgToCtrlAdapter.Free;
end;

function TSSBImagerModel.GetBackground: TBitmap;
begin
  Result := FBackground;
end;

function TSSBImagerModel.GetElements: TList<TControl>;
begin
  Result := FImgToCtrlAdapter.ControlList;
end;

function TSSBImagerModel.GetImage(AIndex: Integer): TImage;
begin
  Result := FImages[AIndex];
end;

function TSSBImagerModel.GetImageCount: Integer;
begin
  Result := FImages.Count;
end;

function TSSBImagerModel.GetSelectedBitmap: TBitmap;
begin
  if FSelected <> nil then
    Exit(FSelected.Bitmap);

  Result := nil;
end;

function TSSBImagerModel.Select(const AImage: TImage): Boolean;
begin
  if FImages.IsHere(AImage) then
    FSelected.Assign(AImage);
end;

procedure TSSBImagerModel.SetImage(AIndex: Integer; const Value: TImage);
begin
  FImages[AIndex] := Value;
end;

end.

