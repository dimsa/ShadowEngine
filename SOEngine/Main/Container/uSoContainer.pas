unit uSoContainer;

interface

uses
  uSoTypes, uCommonClasses, uSoContainerTypes,
  uSoContainerGetter, uSoContainerAdder, uSoObject, uSoContainerDelegateCollector;

type
  TSoContainer = class
  private
    FSubject: TSoObject;
    FGetter: TSoContainerGetter;
    FAdder: TSoContainerAdder;
    FOnGetDelegate: TContainerOnGetDelegate;
    FOnAddDelegate: TContainerOnAddDelegate;
    FOnAddByTemplateDelegate: TContainerOnAddByTemplateDelegate;
    FOnDestroyHandlers: TNotifyEventList;
    procedure OnObjectDestroy(ASender: TObject);
    procedure RaiseOnDestroy;
    function OnElementAddByTemplate(AClass: TClass; ADescription: TContainerElementByTemplate): TObject;
    function OnElementAdd(AClass: TClass; AElement: TContainerElement): TObject;
    function OnAnyGet(AClass: TClass; AName: string): TObject;
  public
    property Subject: TSoObject read FSubject;
    property Get: TSoContainerGetter read FGetter;
    property Add: TSoContainerAdder read FAdder;
    procedure AddOnDestroy(const AHandler: TNotifyEvent);
    procedure RemOnDestroy(const AHandler: TNotifyEvent);
    constructor Create(
      const ASubject: TSoObject;
      const AOnGetDelegate: TContainerOnGetDelegate;
      const AOnAddDelegate: TContainerOnAddDelegate;
      const AOnAddByTemplateDelegate: TContainerOnAddByTemplateDelegate;
      const AContainerDelegateCollector: TSoContainerDelegateCollector
    );
    destructor Destroy; override;
  end;

implementation

{ TSoContainer }

procedure TSoContainer.AddOnDestroy(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Add(AHandler);
end;

constructor TSoContainer.Create(
      const ASubject: TSoObject;
      const AOnGetDelegate: TContainerOnGetDelegate;
      const AOnAddDelegate: TContainerOnAddDelegate;
      const AOnAddByTemplateDelegate: TContainerOnAddByTemplateDelegate;
      const AContainerDelegateCollector: TSoContainerDelegateCollector
      );
begin
  FSubject := ASubject;
  FOnDestroyHandlers := TNotifyEventList.Create;

  FOnGetDelegate := AOnGetDelegate;
  FOnAddDelegate := AOnAddDelegate;
  FOnAddByTemplateDelegate := AOnAddByTemplateDelegate;

  FSubject.AddDestroyHandler(OnObjectDestroy);

  FGetter := TSoContainerGetter.Create(OnAnyGet);
  FAdder := TSoContainerAdder.Create(FSubject, AContainerDelegateCollector);
end;

destructor TSoContainer.Destroy;
begin
  FGetter.Free;
  FAdder.Free;
  FOnDestroyHandlers.Free;
  inherited;
end;

function TSoContainer.OnElementAddByTemplate(AClass: TClass; ADescription: TContainerElementByTemplate): TObject;
begin
  Result := FOnAddByTemplateDelegate(FSubject, AClass, ADescription);
end;

function TSoContainer.OnAnyGet(AClass: TClass; AName: string): TObject;
begin
  Result := FOnGetDelegate(FSubject, AClass, AName);
end;

function TSoContainer.OnElementAdd(AClass: TClass; AElement: TContainerElement): TObject;
begin
  Result := FOnAddDelegate(FSubject, AClass, AElement);
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

