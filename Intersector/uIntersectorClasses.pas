unit uIntersectorClasses;

interface

uses
  System.Types;

type
  TPosition = record // Нужен для аниманиции спрайтов
    X, Y: Single;
    Rotate: Single;
    ScaleX, ScaleY: single;
  end;

  TCircle = packed record
    X, Y: Single;
    Radius: Single;
  end;

  TEllipse = record
    X, Y: Single;
    R1, R2: Single; // Больший и меньший радиус. Или наоборот.
    Angle: Single; // Угол поворота эллипса
  end;

  TLine = record
    A, B: TPointF;
  end;

  const
    pi180 = 0.017453292519943295769236907684886; // (1/180) * pi
    Zero = 0.0;

implementation

end.
