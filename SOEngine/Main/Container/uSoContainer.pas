unit uSoContainer;

interface

uses
  uSoTypes, uCommonClasses, uSoContainerTypes,
  uSoContainerGetter, uSoContainerAdder, uSoObject,
  uSoModel;

type
  TSoContainer = class
  private
    FSubject: TSoObject;
    FGetter: TSoContainerGetter;
    FAdder: TSoContainerAdder;
    FOnDestroyHandlers: TNotifyEventList;
    procedure OnObjectDestroy(ASender: TObject);
    procedure RaiseOnDestroy;
    function OnAnyAdd(AClass: TClass; ADescription: TContainerElementDescription): TObject;
    function OnElementAdd(AClass: TClass; AElement: TContainerElement): TObject;
    function OnAnyGet(AClass: TClass; AName: string): TObject;
  public
    property Subject: TSoObject read FSubject;
    property Get: TSoContainerGetter read FGetter;
    property Add: TSoContainerAdder read FAdder;
    procedure AddOnDestroy(const AHandler: TNotifyEvent);
    procedure RemOnDestroy(const AHandler: TNotifyEvent);
    constructor Create(const ASubject: TSoObject);
    destructor Destroy; override;
  end;

implementation

{ TSoContainer }

procedure TSoContainer.AddOnDestroy(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Add(AHandler);
end;

constructor TSoContainer.Create(const ASubject: TSoObject);
begin
  FSubject := ASubject;
  FOnDestroyHandlers := TNotifyEventList.Create;

  FSubject.AddDestroyHandler(OnObjectDestroy);

  FGetter := TSoContainerGetter.Create(OnAnyGet);
  FAdder := TSoContainerAdder.Create(OnAnyAdd, OnElementAdd);
end;

destructor TSoContainer.Destroy;
begin
  FGetter.Free;
  FAdder.Free;
  FOnDestroyHandlers.Free;
  inherited;
end;

function TSoContainer.OnAnyAdd(AClass: TClass; ADescription: TContainerElementDescription): TObject;
begin

end;

function TSoContainer.OnAnyGet(AClass: TClass; AName: string): TObject;
begin

end;

function TSoContainer.OnElementAdd(AClass: TClass; AElement: TContainerElement): TObject;
begin

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

