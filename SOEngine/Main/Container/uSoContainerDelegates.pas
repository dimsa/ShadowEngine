unit uSoContainerDelegates;

interface

uses
  System.Generics.Collections,
  uSoObject,
  uSoAnimation, uSoColliderObject, uColliderDefinition, uSoFormatter,
  uSoMouseHandler, uSoKeyHandler, uSoLogic, uSoProperties, uE2DRendition,
  uSoSound;

type
  TAnimationContainerDelegate = procedure(const AAnimation: TSoAnimation; const AName: string = '') of object;
  TAnimationContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoAnimation of object;

  TColliderObjContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoColliderObj of object;
  TColliderObjContainerDelegateByDefinition = function(const ASubject: TSoObject; const AColliderDef: TColliderDefinition; const AName: string = ''): TSoColliderObj of object;

  TFormatter—ontainerDelegate = procedure(const AFormatter: TSoFormatter; const AName: string) of object;
  TFormatter—ontainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoFormatter of object;
  TFormatterContainerDelegateByCode = function(const ASubject: TSoObject; const AFormatterCode: string; const AName: string = ''): TSoFormatter of object;

  TMouseHandlerContainerDelegate = procedure(const AMouseHandler: TSoMouseHandler; const AName: string = '') of object;
  TMouseHandlerContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoMouseHandler of object;

  TKeyHandlerContainerDelegate = procedure(const AKeyHandler: TSoKeyHandler; const AName: string = '') of object;
  TKeyHandlerContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoKeyHandler of object;

  TLogicContainerDelegate = procedure(const ALogic: TSoLogic; const AName: string = '') of object;
  TLogicContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoLogic of object;

  TPropertiesContainerDelegate = procedure(const AProperty: TSoProperties; const AName: string = '') of object;
  TPropertiesContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoProperties of object;

  TRenditionContainerDelegate = procedure(const AProperty: TEngine2DRendition; const AName: string = '') of object;
  TRenditionContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TEngine2DRendition of object;

  TSoundContainerDelegate = procedure(const AProperty: TSoSound; const AName: string = '') of object;
  TSoundContainerDelegateByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoSound of object;
  TSoundContainerDelegateByFileName = function(const ASubject: TSoObject; const AFileName: string; const AName: string = ''): TSoSound of object;
implementation

end.
