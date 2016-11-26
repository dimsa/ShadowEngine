unit uObjectListItemView;

interface

uses
  FMX.Graphics,
  uIObjectListItemView;

type

  TObjectListItemView = class(TInterfacedObject, IObjectListItemView)
  private
    FName: string;
    FBitmap: TBitmap;
  public
    procedure SetName(AValue: string);
    function GetName: string;
    procedure AssignBitmap(ABitmap: TBitmap);
    property Bitmap: TBitmap read FBitmap;
    constructor Create;
  end;

implementation

{ TObjectListItemView }

procedure TObjectListItemView.AssignBitmap(ABitmap: TBitmap);
begin
  FBitmap.Assign(ABitmap);
end;

constructor TObjectListItemView.Create;
begin
  FBitmap := TBitmap.Create;
end;

function TObjectListItemView.GetName: string;
begin
  Result := FName;
end;

procedure TObjectListItemView.SetName(AValue: string);
begin
  FName := AValue;
end;

end.
