unit uSSBTypes;

interface

uses
  System.Generics.Collections, FMX.Controls, FMX.Objects,
  uNamedList;

type
  TSSBStatus = (sPicture, sObject, sShape);

  TImgToCtrlAdapter = class
  private
    FControlList: TList<TControl>;
    FImageList: TNamedList<TImage>;
    function GetControlList: TList<TControl>;
  public
    property ControlList: TList<TControl> read GetControlList;
    constructor Create(const AImageList: TNamedList<TImage>);
    destructor Destroy; override;
  end;

implementation

{ TImageToControlAdapter }

constructor TImgToCtrlAdapter.Create(const AImageList: TNamedList<TImage>);
begin
  FImageList := AImageList;
  FControlList := TList<TControl>.Create;
end;

destructor TImgToCtrlAdapter.Destroy;
begin
  FControlList.Free;
  inherited;
end;

function TImgToCtrlAdapter.GetControlList: TList<TControl>;
var
  i: Integer;
begin
  FControlList.Clear;
  for i := 0 to FImageList.Count - 1 do
    FControlList.Add(FImageList[i]);

  Result := FControlList;
end;

end.
