unit Drawer;

interface

uses
  System.SysUtils, System.Types, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Objects;

procedure ColorizeImage(Image: TImage; color : TAlphaColor);

procedure DrawColoredImage(Image: TImage; sprite : string; color : TAlphaColor);

procedure RandomColorizeImage(Image: TImage);

procedure DrawOutline(Image: TImage; BorderColor: TAlphaColor; BorderWidth: Integer);

implementation

procedure ColorizeImage(Image: TImage; color : TAlphaColor);
var
  Bitmap: TBitmap;
  BitmapData: TBitmapData;
  X, Y: Integer;
  PixelColor: TAlphaColor;
  TintRec: TAlphaColorRec;
  PixelRec: TAlphaColorRec;
begin
  Bitmap := Image.Bitmap;

  TintRec.A := 255;
  TintRec.R := TAlphaColorRec(color).R;
  TintRec.G := TAlphaColorRec(color).G;
  TintRec.B := TAlphaColorRec(color).B;

  if Bitmap.Map(TMapAccess.ReadWrite, BitmapData) then
    for Y := 0 to Bitmap.Height - 1 do
      for X := 0 to Bitmap.Width - 1 do
      begin
        PixelColor := BitmapData.GetPixel(X, Y);
        PixelRec := TAlphaColorRec(PixelColor);
        if TAlphaColorRec(PixelColor).A > 0 then
        begin
          PixelRec.R := (PixelRec.R + TintRec.R) div 2;
          PixelRec.G := (PixelRec.G + TintRec.G) div 2;
          PixelRec.B := (PixelRec.B + TintRec.B) div 2;

          BitmapData.SetPixel(X, Y, TAlphaColor(PixelRec));
        end;
      end;
    Bitmap.Unmap(BitmapData);

  Image.InvalidateRect(Image.BoundsRect);
end;

procedure RandomColorizeImage(Image: TImage);
var c : TAlphaColorRec;
begin
  c.A := 255;
  c.R := Random(256);
  c.G := Random(256);
  c.B := Random(256);
  ColorizeImage(Image, TAlphaColor(c));
end;

procedure DrawColoredImage(Image: TImage; sprite : string; color : TAlphaColor);
begin
  Image.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' + sprite);
  ColorizeImage(Image, color);
end;

procedure DrawOutline(Image: TImage; BorderColor: TAlphaColor; BorderWidth: Integer);
var
  Bitmap: TBitmap;
  DestRect, BorderRect: TRectF;
  BitmapData : TBitmapData;
  PixelRec: TAlphaColorRec;
  PixelColor: TAlphaColor;

begin
  Bitmap := TBitmap.Create;
  Bitmap.SetSize(Image.Bitmap.Width + 2 * BorderWidth, Image.Bitmap.Height + 2 * BorderWidth);
  Bitmap.Canvas.BeginScene;

  BorderRect := RectF(0, 0,
                        Bitmap.Width, Bitmap.Height);

  DestRect := RectF(BorderWidth, BorderWidth/2,
                        Bitmap.Width - BorderWidth, Bitmap.Height - BorderWidth);

  Bitmap.Canvas.DrawBitmap(Image.Bitmap,
                               RectF(0, 0, Image.Bitmap.Width, Image.Bitmap.Height),
                               BorderRect,
                               1, True);


  Bitmap.Canvas.EndScene;
  if Bitmap.Map(TMapAccess.ReadWrite, BitmapData) then
    for var Y := 0 to Bitmap.Height - 1 do
      for var X := 0 to Bitmap.Width - 1 do
      begin
        PixelColor := BitmapData.GetPixel(X, Y);
        PixelRec := TAlphaColorRec(PixelColor);
        if TAlphaColorRec(PixelColor).A > 0 then
        begin
          BitmapData.SetPixel(X, Y, BorderColor);
        end;
      end;
    Bitmap.Unmap(BitmapData);

  Bitmap.Canvas.BeginScene;
  Bitmap.Canvas.DrawBitmap(Image.Bitmap,
                               RectF(0, 0, Image.Bitmap.Width, Image.Bitmap.Height),
                               DestRect,
                               1, True);

  Bitmap.Canvas.EndScene;

  Image.Bitmap.Assign(Bitmap);
  Bitmap.Free;
end;
end.
