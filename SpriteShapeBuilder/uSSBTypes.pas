unit uSSBTypes;

interface

uses
  System.Classes,
  System.Generics.Collections, FMX.Controls, FMX.Objects, System.Types,
  uNamedList;

type
  TSSBStatus = (sPicture, sObject, sShape);

  TResizeType = (rtWE, rtEW, rtNS, rtSN, rtCenter, rtNone);

  TCaptureMode = (cmMove, cmResize, cmNone);

//  TPositionFunc = Function(const APoint: TPointF) : TPointF of Object;

  TPointFunction = Function : TPointF of Object;

  TAct = (Subscribe, Unsubscribe);

implementation

end.
