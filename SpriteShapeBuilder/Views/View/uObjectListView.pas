unit uObjectListView;

interface

uses
  FMX.Forms, System.SysUtils, FMX.Graphics, System.Generics.Collections,
  uIObjectListItemView, uIObjectListView, uItemListFrame, uObjectListItemView;

type
  TObjectListView = class(TInterfacedObject, IObjectListView)
  private
    FItems: TList<IObjectListItemView>;
    FSelected: IObjectListItemView;
    FFrame: TItemListFrame;
  public
    function GetSelectedItem: IObjectListItemView;
    function RemSelectedItem: IObjectListItemView;
    function AddItem: IObjectListItemView;
    function Items: TList<IObjectListItemView>;
    constructor Create(const AFrame: TItemListFrame);
    destructor Destroy; override;
  end;

implementation

{ TObjectListView }

function TObjectListView.AddItem: IObjectListItemView;
begin
  with FItems do begin
    Add(TObjectListItemView.Create);
    Last.SetName('Rendition #' + IntToStr(FFrame.ListView.Items.Count));
    Last.AssignBitmap(TBitmap.Create(100, 100));
  end;
  { TODO : Check memory leaks }

  with FFrame.ListView.Items.Add do begin
    Data['Name'] := FItems.Last.GetName;
    Data['Bitmap'] := TObjectListItemView(FItems.Last).Bitmap;
  end;
end;

constructor TObjectListView.Create(const AFrame: TItemListFrame);
begin
  FItems := TList<IObjectListItemView>.Create;
  FFrame := AFrame;
end;

destructor TObjectListView.Destroy;
begin
  FItems.Free;
  FSelected := nil;
  inherited;
end;

function TObjectListView.GetSelectedItem: IObjectListItemView;
begin
  result := FSelected;
end;

function TObjectListView.Items: TList<IObjectListItemView>;
begin
  Result := FItems;
end;

function TObjectListView.RemSelectedItem: IObjectListItemView;
begin
  if FSelected <> nil then
    FItems.Remove(FSelected);

  FSelected := nil;
end;

end.
