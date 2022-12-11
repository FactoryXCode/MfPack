unit Helper;

interface

uses
  WinApi.Windows,
  {Vcl}
  Vcl.Graphics;

implementation


type
  TRGBArray = ARRAY[0..32767] OF TRGBTriple;
  pRGBArray = ^TRGBArray;




procedure SmoothResize(aBmp: TBitmap;
                       NuWidth: integer;
                       NuHeight: integer);
var
  xscale, yscale: Single;
  sfrom_y, sfrom_x: Single;
  ifrom_y, ifrom_x: Integer;
  to_y, to_x: Integer;
  weight_x, weight_y: array[0..1] of Single;
  weight: Single;
  new_red, new_green: Integer;
  new_blue: Integer;
  total_red, total_green: Single;
  total_blue: Single;
  ix, iy: Integer;
  bTmp: TBitmap;
  sli, slo: pRGBArray;

begin
  abmp.PixelFormat := pf24bit;

  bTmp := TBitmap.Create;
  bTmp.PixelFormat := pf24bit;
  bTmp.Width := NuWidth;
  bTmp.Height := NuHeight;

  xscale := bTmp.Width / (abmp.Width - 1);
  yscale := bTmp.Height / (abmp.Height - 1);

  for to_y := 0 to bTmp.Height - 1 do
    begin
      sfrom_y := to_y / yscale;
      ifrom_y := Trunc(sfrom_y);
      weight_y[1] := sfrom_y - ifrom_y;
      weight_y[0] := 1 - weight_y[1];

      for to_x := 0 to bTmp.Width - 1 do
        begin
          sfrom_x := to_x / xscale;
          ifrom_x := Trunc(sfrom_x);
          weight_x[1] := sfrom_x - ifrom_x;
          weight_x[0] := 1 - weight_x[1];
          total_red := 0.0;
          total_green := 0.0;
          total_blue := 0.0;

          for ix := 0 to 1 do
            begin
              for iy := 0 to 1 do
                begin
                  sli := abmp.Scanline[ifrom_y + iy];
                  new_red := sli[ifrom_x + ix].rgbtRed;
                  new_green := sli[ifrom_x + ix].rgbtGreen;
                  new_blue := sli[ifrom_x + ix].rgbtBlue;
                  weight := weight_x[ix] * weight_y[iy];
                  total_red := total_red + new_red * weight;
                  total_green := total_green + new_green * weight;
                  total_blue := total_blue + new_blue * weight;
                end;
            end;

         slo := bTmp.ScanLine[to_y];
         slo[to_x].rgbtRed := Round(total_red);
         slo[to_x].rgbtGreen := Round(total_green);
         slo[to_x].rgbtBlue := Round(total_blue);
        end;
    end;

  abmp.Width := bTmp.Width;
  abmp.Height := bTmp.Height;
  abmp.Canvas.Draw(0,
                   0,
                   bTmp);

  SafeDelete(bTmp);
end;

end.
