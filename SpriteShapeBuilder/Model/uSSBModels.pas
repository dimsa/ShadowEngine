unit uSSBModels;

interface

uses
  System.Generics.Collections, System.Classes,
  FMX.Objects, FMX.StdCtrls, FMX.Controls, System.Types, FMX.Graphics,
  uNamedList, uClasses, uSSBTypes, uMVPFrameWork;

type

  TElement = class

  end;

  TSSBModel = class(TModel)
  private
    FBitmap: TBitmap; // Подложка объекта
    FItems: TList<TElement>;
    function Add(const AImage: TImage): Boolean;
    procedure DelSelected;
    function GetElementCount: Integer;
    function GetItem(AIndex: Integer): TElement;
    procedure SetItem(AIndex: Integer; const Value: TElement);
  public
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
    destructor Destroy;
    property ElementCount: Integer read GetElementCount;
    property Items[AIndex: Integer]: TElement read GetItem write SetItem;
    property Image: TBitmap read FBitmap;
end;

implementation

{ TSSBModel }

function TSSBModel.Add(const AImage: TImage): Boolean;
begin

end;

constructor TSSBModel.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;
  FBitmap := TBitmap.Create;
end;

procedure TSSBModel.DelSelected;
begin

end;

destructor TSSBModel.Destroy;
begin

end;

function TSSBModel.GetElementCount: Integer;
begin

end;

function TSSBModel.GetItem(AIndex: Integer): TElement;
begin

end;

procedure TSSBModel.SetItem(AIndex: Integer; const Value: TElement);
begin

end;

end.

