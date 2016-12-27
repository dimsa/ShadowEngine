unit uSoContainer;

interface

uses
  uSoTypes, uCommonClasses,
  uSoContainerGetter, uSoContainerAdder,
  uSoModel, uSoObject;

type
  TSoContainer = class
  private
    FGetter: TSoContainerGetter;
    FAdder: TSoContainerAdder;
    FOnDestroyHandlers: TNotifyEventList;
    procedure OnObjectDestroy(ASender: TObject);
    procedure RaiseOnDestroy;
  public
    property Get: TSoContainerGetter read FGetter;
    property Add: TSoContainerAdder read FAdder;
    procedure AddOnDestroy(const AHandler: TNotifyEvent);
    procedure RemOnDestroy(const AHandler: TNotifyEvent);
    constructor Create(const AObject: TSoObject; const AModel: TSoModel);
    destructor Destroy; override;
  end;

implementation

{ TSoContainer }

procedure TSoContainer.AddOnDestroy(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Add(AHandler);
end;

constructor TSoContainer.Create(const AObject: TSoObject; const AModel: TSoModel);
begin
  FOnDestroyHandlers := TNotifyEventList.Create;
  FGetter := TSoContainerGetter.Create(AObject, AModel);
  FAdder := TSoContainerAdder.Create(AObject, AModel);
  AObject.AddDestroyHandler(OnObjectDestroy);
end;

destructor TSoContainer.Destroy;
begin
  FGetter.Free;
  FAdder.Free;
  FOnDestroyHandlers.Free;
  inherited;
end;

procedure TSoContainer.OnObjectDestroy(ASender: TObject);
begin
  RaiseOnDestroy;
end;

procedure TSoContainer.RaiseOnDestroy;
begin
  FOnDestroyHandlers.RaiseEvent(Self);
end;

procedure TSoContainer.RemOnDestroy(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Remove(AHandler);
end;

end.
