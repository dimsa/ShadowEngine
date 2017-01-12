unit uSoContainerDelegates;

interface

uses
  System.Generics.Collections,
  uSoObject,
  uSoAnimation, uSoColliderObject, uColliderDefinition, uSoFormatter,
  uSoMouseHandler, uSoKeyHandler, uSoLogic, uSoProperties, uE2DRendition,
  uSoSound;

type
  TAnimationContainerDelegate = procedure(const AAnimation: TSoAnimation; const AName: string = '');
  TAnimationContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoAnimation;

  TColliderObjContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoColliderObj;
  TColliderObjContainerDelegateByDefinition = function(const ASubject: TSoObject; const AColliderDef: TColliderDefinition; const AName: string = ''): TSoColliderObj;

  TFormatter—ontainerDelegate = procedure(const AFormatter: TSoFormatter; const AName: string);
  TFormatter—ontainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoFormatter;
  TFormatterContainerDelegateByCode = function(const ASubject: TSoObject; const AFormatterCode: string; const AName: string = ''): TSoFormatter;

  TMouseHandlerContainerDelegate = procedure(const AMouseHandler: TSoMouseHandler; const AName: string = '');
  TMouseHandlerContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoMouseHandler;

  TKeyHandlerContainerDelegate = procedure(const AKeyHandler: TSoKeyHandler; const AName: string = '');
  TKeyHandlerContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoKeyHandler;

  TLogicContainerDelegate = procedure(const ALogic: TSoLogic; const AName: string = '');
  TLogicContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoLogic;

  TPropertiesContainerDelegate = procedure(const AProperty: TSoProperties; const AName: string = '');
  TPropertiesContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoProperties;

  TRenditionContainerDelegate = procedure(const AProperty: TEngine2DRendition; const AName: string = '');
  TRenditionContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TEngine2DRendition;

  TSoundContainerDelegate = procedure(const AProperty: TSoSound; const AName: string = '');
  TSoundContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoSound;
  TSoundContainerDelegateByFileName = function(const ASubject: TSoObject; const AFileName: string; const AName: string = ''): TSoSound;
implementation

end.
