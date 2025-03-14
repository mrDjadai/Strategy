unit Drawer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.StdCtrls;

procedure ColorizeImage(Image: TImage);

procedure DrawOutline(Image: TImage; BorderColor: TAlphaColor; BorderWidth: Integer);

implementation

procedure ColorizeImage(Image: TImage);
var
  Bitmap: TBitmap;
  BitmapData: TBitmapData;
  X, Y: Integer;
  PixelColor: TAlphaColor;
  TintColor: TAlphaColor;
  TintRec: TAlphaColorRec;
  PixelRec: TAlphaColorRec;
begin
  Bitmap := Image.Bitmap;

  TintRec.A := 255;
  TintRec.R := Random(256);
  TintRec.G := Random(256);
  TintRec.B := Random(256);

  TintColor := TAlphaColor(TintRec);

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
