unit uSoContainerDelegateCollector;

interface

uses
  uSoContainerDelegates;

type
  TSoContainerDelegateCollector = class
  private
    FColliderObjContainerDelegateByTemplate: TColliderObjContainerDelegateByTemplate;
    FMouseHandlerContainerDelegate: TMouseHandlerContainerDelegate;
    FPropertiesContainerDelegateByTemplate: TPropertiesContainerDelegateByTemplate;
    FFormatter—ontainerDelegate: TFormatter—ontainerDelegate;
    FFormatterContainerDelegateByCode: TFormatterContainerDelegateByCode;
    FPropertiesContainerDelegate: TPropertiesContainerDelegate;
    FRenditionContainerDelegateByTemplate: TRenditionContainerDelegateByTemplate;
    FLogicContainerDelegateByTemplate: TLogicContainerDelegateByTemplate;
    FRenditionContainerDelegate: TRenditionContainerDelegate;
    FLogicContainerDelegate: TLogicContainerDelegate;
    FColliderObjContainerDelegateByDefinition: TColliderObjContainerDelegateByDefinition;
    FKeyHandlerContainerDelegateByTemplate: TKeyHandlerContainerDelegateByTemplate;
    FAnimationContainerDelegateByTemplate: TAnimationContainerDelegateByTemplate;
    FSoundContainerDelegateByFileName: TSoundContainerDelegateByFileName;
    FKeyHandlerContainerDelegate: TKeyHandlerContainerDelegate;
    FAnimationContainerDelegate: TAnimationContainerDelegate;
    FSoundContainerDelegateByTemplate: TSoundContainerDelegateByTemplate;
    FSoundContainerDelegate: TSoundContainerDelegate;
    FFormatter—ontainerDelegateByTemplate: TFormatter—ontainerDelegateByTemplate;
    FMouseHandlerContainerDelegateByTemplate: TMouseHandlerContainerDelegateByTemplate;
  public
    property AnimationContainerDelegate: TAnimationContainerDelegate read FAnimationContainerDelegate;
    property AnimationContainerDelegateByTemplate: TAnimationContainerDelegateByTemplate read FAnimationContainerDelegateByTemplate;

    property ColliderObjContainerDelegateByTemplate: TColliderObjContainerDelegateByTemplate read FColliderObjContainerDelegateByTemplate;
    property ColliderObjContainerDelegateByDefinition: TColliderObjContainerDelegateByDefinition read FColliderObjContainerDelegateByDefinition;

    property Formatter—ontainerDelegate: TFormatter—ontainerDelegate read FFormatter—ontainerDelegate;
    property Formatter—ontainerDelegateByTemplate:  TFormatter—ontainerDelegateByTemplate read FFormatter—ontainerDelegateByTemplate;
    property FormatterContainerDelegateByCode : TFormatterContainerDelegateByCode read FFormatterContainerDelegateByCode;

    property MouseHandlerContainerDelegate: TMouseHandlerContainerDelegate read FMouseHandlerContainerDelegate;
    property MouseHandlerContainerDelegateByTemplate: TMouseHandlerContainerDelegateByTemplate read FMouseHandlerContainerDelegateByTemplate;

    property KeyHandlerContainerDelegate: TKeyHandlerContainerDelegate read FKeyHandlerContainerDelegate;
    property KeyHandlerContainerDelegateByTemplate: TKeyHandlerContainerDelegateByTemplate read FKeyHandlerContainerDelegateByTemplate;

    property LogicContainerDelegate: TLogicContainerDelegate read FLogicContainerDelegate;
    property LogicContainerDelegateByTemplate: TLogicContainerDelegateByTemplate read FLogicContainerDelegateByTemplate;

    property PropertiesContainerDelegate: TPropertiesContainerDelegate read FPropertiesContainerDelegate;
    property PropertiesDelegateByTemplate: TPropertiesContainerDelegateByTemplate read FPropertiesContainerDelegateByTemplate;

    property RenditionContainerDelegate: TRenditionContainerDelegate read FRenditionContainerDelegate;
    property RenditionContainerDelegateByTemplate: TRenditionContainerDelegateByTemplate read FRenditionContainerDelegateByTemplate;

    property SoundContainerDelegate:  TSoundContainerDelegate read FSoundContainerDelegate;
    property SoundContainerDelegateByTemplate: TSoundContainerDelegateByTemplate read FSoundContainerDelegateByTemplate;
    property SoundContainerDelegateByFileName: TSoundContainerDelegateByFileName read FSoundContainerDelegateByFileName;
  constructor Create(
    AAnimationContainerDelegate: TAnimationContainerDelegate;
    AAnimationContainerDelegateByTemplate: TAnimationContainerDelegateByTemplate;

    AColliderObjContainerDelegateByTemplate: TColliderObjContainerDelegateByTemplate;
    AColliderObjContainerDelegateByDefinition: TColliderObjContainerDelegateByDefinition;

    AFormatter—ontainerDelegate: TFormatter—ontainerDelegate;
    AFormatter—ontainerDelegateByTemplate:  TFormatter—ontainerDelegateByTemplate;
    AFormatterContainerDelegateByCode : TFormatterContainerDelegateByCode;

    AMouseHandlerContainerDelegate: TMouseHandlerContainerDelegate;
    AMouseHandlerContainerDelegateByTemplate: TMouseHandlerContainerDelegateByTemplate;

    AKeyHandlerContainerDelegate: TKeyHandlerContainerDelegate;
    AKeyHandlerContainerDelegateByTemplate: TKeyHandlerContainerDelegateByTemplate;

    ALogicContainerDelegate: TLogicContainerDelegate;
    ALogicContainerDelegateByTemplate: TLogicContainerDelegateByTemplate;

    APropertiesContainerDelegate: TPropertiesContainerDelegate;
    APropertiesContainerDelegateByTemplate: TPropertiesContainerDelegateByTemplate;

    ARenditionContainerDelegate: TRenditionContainerDelegate;
    ARenditionContainerDelegateByTemplate: TRenditionContainerDelegateByTemplate;

    ASoundContainerDelegate:  TSoundContainerDelegate;
    ASoundContainerDelegateByTemplate: TSoundContainerDelegateByTemplate;
    ASoundContainerDelegateByFileName: TSoundContainerDelegateByFileName);
  end;

implementation

 constructor TSoContainerDelegateCollector.Create(
    AAnimationContainerDelegate: TAnimationContainerDelegate;
    AAnimationContainerDelegateByTemplate: TAnimationContainerDelegateByTemplate;

    AColliderObjContainerDelegateByTemplate: TColliderObjContainerDelegateByTemplate;
    AColliderObjContainerDelegateByDefinition: TColliderObjContainerDelegateByDefinition;

    AFormatter—ontainerDelegate: TFormatter—ontainerDelegate;
    AFormatter—ontainerDelegateByTemplate:  TFormatter—ontainerDelegateByTemplate;
    AFormatterContainerDelegateByCode : TFormatterContainerDelegateByCode;

    AMouseHandlerContainerDelegate: TMouseHandlerContainerDelegate;
    AMouseHandlerContainerDelegateByTemplate: TMouseHandlerContainerDelegateByTemplate;

    AKeyHandlerContainerDelegate: TKeyHandlerContainerDelegate;
    AKeyHandlerContainerDelegateByTemplate: TKeyHandlerContainerDelegateByTemplate;

    ALogicContainerDelegate: TLogicContainerDelegate;
    ALogicContainerDelegateByTemplate: TLogicContainerDelegateByTemplate;

    APropertiesContainerDelegate: TPropertiesContainerDelegate;
    APropertiesContainerDelegateByTemplate: TPropertiesContainerDelegateByTemplate;

    ARenditionContainerDelegate: TRenditionContainerDelegate;
    ARenditionContainerDelegateByTemplate: TRenditionContainerDelegateByTemplate;

    ASoundContainerDelegate:  TSoundContainerDelegate;
    ASoundContainerDelegateByTemplate: TSoundContainerDelegateByTemplate;
    ASoundContainerDelegateByFileName: TSoundContainerDelegateByFileName);
begin
    FColliderObjContainerDelegateByTemplate := AColliderObjContainerDelegateByTemplate;
    FMouseHandlerContainerDelegate := AMouseHandlerContainerDelegate;
    FPropertiesContainerDelegateByTemplate := APropertiesContainerDelegateByTemplate;
    FFormatter—ontainerDelegate := AFormatter—ontainerDelegate;
    FFormatterContainerDelegateByCode := AFormatterContainerDelegateByCode;
    FPropertiesContainerDelegate := APropertiesContainerDelegate;
    FRenditionContainerDelegateByTemplate := ARenditionContainerDelegateByTemplate;
    FLogicContainerDelegateByTemplate := ALogicContainerDelegateByTemplate;
    FRenditionContainerDelegate := ARenditionContainerDelegate;
    FLogicContainerDelegate := ALogicContainerDelegate;
    FColliderObjContainerDelegateByDefinition := AColliderObjContainerDelegateByDefinition;
    FKeyHandlerContainerDelegateByTemplate := AKeyHandlerContainerDelegateByTemplate;
    FAnimationContainerDelegateByTemplate := AAnimationContainerDelegateByTemplate;
    FSoundContainerDelegateByFileName := ASoundContainerDelegateByFileName;
    FKeyHandlerContainerDelegate := AKeyHandlerContainerDelegate;
    FAnimationContainerDelegate := AAnimationContainerDelegate;
    FSoundContainerDelegateByTemplate := ASoundContainerDelegateByTemplate;
    FSoundContainerDelegate := ASoundContainerDelegate;
    FFormatter—ontainerDelegateByTemplate := AFormatter—ontainerDelegateByTemplate;
    FMouseHandlerContainerDelegateByTemplate := AMouseHandlerContainerDelegateByTemplate;
end;

end.
