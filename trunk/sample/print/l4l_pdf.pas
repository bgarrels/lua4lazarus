unit l4l_pdf;
{
  Lua4Lazalus

    sample:

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, l4l_print;

procedure DrawPDF(stream: TStream; LPO: TLuaPrintObject; page: integer;
 x1, y1, x2, y2: integer);

implementation
uses
  graphics, contnrs, paszlib;

const
  PRUN_NAME = 'P_';

type
  TDblXY = class
  public
    re: boolean;
    x, y: double;
  end;

procedure DrawPDF(stream: TStream; LPO: TLuaPrintObject; page: integer;
 x1, y1, x2, y2: integer);
var
  PageW, PageH, RateW, RateH, Rate: double;

  function TokenFloat(var p: PChar): double;
  var
    i, l: integer;
  begin
    i := 0;
    l := strlen(p);
    while (i < l) and ((p+i)^ in ['0'..'9','.','-']) do Inc(i);
    if i > 0 then begin
      Result := StrToFloat(Copy(p, 1, i));
      while (i < l) and ((p+i)^ in [#1..' ']) do Inc(i);
      Inc(p, i);
    end else begin
      Result := 0;
    end;
  end;

  function TokenLine(var p: PChar): string;
  var
    pp: PChar;
  begin
    pp := p;
    while (p^ <> #0) and not(p^ in [#$0a, #$0d]) do Inc(p);
    if p <> pp then begin
      Result := Trim(Copy(pp, 1, p-pp));
    end else begin
      Result := '';
    end;
    while (p^ <> #0) and (p^ in [#$0a, #$0d]) do Inc(p);
  end;

  function TokenStr(var p: PChar; c: char): string;
  var
    pp: PChar;
  begin
    pp := p;
    while (p^ <> #0) and (p^ <> c) do Inc(p);
    if p <> pp then begin
      Result := Trim(Copy(pp, 1, p-pp));
    end else begin
      Result := '';
    end;
    if p^ = c then Inc(p);
  end;

  procedure DrawPage(cmd: string);
  var
    params: TObjectList;
    i: integer;
    s, s1, s2, cm: string;
    sx, sy, x1, y1, x2, y2: double;
    sp, sp1: PChar;
    xy: TDblXY;
    bt: boolean;
  begin
    params := TObjectList.Create(True);
    try
      sp := PChar(cmd);
      bt := False;
      while sp^ <> #0 do begin
        s := TokenLine(sp);
        if s = '' then continue;
        i := Length(s);
        while i >= 1 do begin
          if not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z']) then break;
          Dec(i);
        end;
        if i > 0 then begin
          cm := Copy(s, i+1, Length(s));
          Delete(s, i+1, Length(s));
        end else begin
          cm := s;
          s := '';
        end;

        sp1 := PChar(s);
        if cm = 'BT' then begin
          bt := True;
        end else if cm = 'ET' then begin
          bt := False;
        end else if cm = 'm' then begin
          xy := TDblXY.Create;
          xy.re:= False;
          xy.x:= TokenFloat(sp1); xy.y := TokenFloat(sp1);
          params.Add(xy);
        end else if cm = 'l' then begin
          xy := TDblXY.Create;
          xy.x:= TokenFloat(sp1); xy.y := TokenFloat(sp1);
          params.Add(xy);
        end else if cm = 'h' then begin
          if params.Count > 0 then begin
            xy := TDblXY.Create;
            xy.x:= TDblXY(params[0]).x;
            xy.y := TDblXY(params[0]).y;
            params.Add(xy);
          end;
        end else if cm = 're' then begin
          xy := TDblXY.Create;
          xy.re:= True;
          xy.x:= TokenFloat(sp1);
          xy.y := TokenFloat(sp1);
          params.Add(xy);
          xy := TDblXY.Create;
          xy.x:= TokenFloat(sp1);
          xy.y := TokenFloat(sp1);
          params.Add(xy);
          ////////////////////////////////
        end else if cm = 'G' then begin
          x1 := TokenFloat(sp1);
          i := RGBToColor(Trunc(255*x1), Trunc(255*x1), Trunc(255*x1));
          LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)', [i]));
          LPO.LuaPrint.Canvas.Pen.Color:= i;
        end else if cm = 'g' then begin
          x1 := TokenFloat(sp1);
          i := RGBToColor(Trunc(255*x1), Trunc(255*x1), Trunc(255*x1));
          LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_color(%d)', [i]));
          LPO.LuaPrint.Canvas.Brush.Color:= i;
        ////////////////////////////////
        end else if cm = 'n' then begin
          // End the path object without filling or stroking it.
          params.Clear;
        end else if (cm = 'S') or (cm = 's') or (cm = 'f') or (cm = 'b') or
         (cm = 'f*') or (cm = 'b*') or (cm = 'B') or (cm = 'B*') then begin
          if params.Count > 0 then begin
            if (cm = 's') or (cm = 'b') then begin
              xy := TDblXY.Create;
              xy.x:= TDblXY(params[0]).x;
              xy.y := TDblXY(params[0]).y;
              params.Add(xy);
            end;

            if TDblXY(params[0]).re then begin
              x1 := TDblXY(params[0]).x;
              y1 := TDblXY(params[0]).y;
              x2 := TDblXY(params[1]).x;
              y2 := TDblXY(params[1]).y;
              s1:= Format('%d,%d,%d,%d',
               [Trunc(x1 * Rate), Trunc(y1 * Rate),
                Trunc((x1+x2) * Rate), Trunc((y1+y2) * Rate)]);

              if (cm = 's') or (cm = 'S') then begin
                LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                 [Integer(bsClear)]));
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s1]));
                LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
              end else if (cm = 'b') or (cm = 'b*')
               or (cm = 'B') or (cm = 'B*') then begin
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s1]));
              end else begin
                LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_style(%d)',
                 [Integer(psSolid)]));
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)',
                 [LPO.LuaPrint.Canvas.Brush.Color]));
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s1]));
                LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
              end;
            end else begin
              s1 := '';
              for i := 0 to params.Count-1 do begin
                sx := TDblXY(params[i]).x * Rate;
                sy := TDblXY(params[i]).y * Rate;
                s1 := s1 + Format('%d,%d,', [Trunc(sx), Trunc(sy)]);
              end;
              Delete(s1, Length(s1), 1);

              if (cm = 's') or (cm = 'S') then begin
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polyline(%s)', [s1]));
              end else if (cm = 'b') or (cm = 'b*')
               or (cm = 'B') or (cm = 'B*') then begin
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polygon(%s,%d)',
                 [s1, Integer((cm = 'b') or (cm = 'B'))]));
              end else begin
                LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_style(%d)',
                 [Integer(psSolid)]));
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)',
                 [LPO.LuaPrint.Canvas.Brush.Color]));
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polygon(%s)', [s1]));
                LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
              end;
            end;
            params.Clear;
          end;
        end else if cm = 'Tj' then begin
          TokenStr(sp, '<');
          s1 := TokenStr(sp, '>');
          SetLength(s2, Length(s1) div 2);
          for i := 1 to Length(s1) div 2 do begin
            s2[i] := Char(StrToInt('$' + s1[(i-1)*2+1] + s1[i*2]));
          end;
          LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.textout(%d,%d,"%s")',
           [0, 0, '123']));
        end;
      end;
    finally
      params.Free;
    end;
  end;

  function mempos(str1, str2: PChar; l: integer) : PChar;
  var
    i, j, l2: integer;
  begin
    l2 := strlen(str2);
    for i := 0 to l-1 do begin
      j := 0;
      while ((j < l2) and (i+j < l) and ((str1+i+j)^ = (str2+j)^)) do Inc(j);
      if j = l2 then begin
        Result := str1 + i;
        Exit;
      end;
    end;
    Result := nil;
  end;

const
  ZBUF_LEN = 10000;
var
  i, p1, p2, len, curpage: integer;
  src, s, s1, obj, cmd: string;
  sp, sp1, sp2: PChar;
  z: TZStream;
  flate: boolean;
begin
  SetLength(src, stream.Size);
  stream.Position:= 0;
  stream.ReadBuffer(src[1], stream.Size);
  sp := PChar(src);
  curpage := 1;
  while True do begin
    sp1 := mempos(sp, 'obj', Length(src)-Integer(sp-PChar(src)));
    if sp1 <> nil then begin
      sp2 := mempos(sp, 'endobj', Length(src)-Integer(sp-PChar(src)));
      if sp2 <> nil then begin
        s := Trim(Copy(sp1+3, 1, sp2-sp1-3));
        sp := sp2 + 6;
      end else begin
        s := Trim(Copy(sp1+3, 1, Length(src)));
        sp := PChar(src) + Length(src);
      end;
    end else
      Break;

    p1 := Pos('/Type/Page', s);
    if p1 > 0 then begin
      p1 := Pos('/Contents', s);
      if p1 > 0 then begin
        i := p1+10;
        obj := '';
        while i <=  Length(s) do begin
          if s[i] in ['0'..'9'] then begin
            obj := obj + s[i];
          end else
            Break;
          Inc(i);
        end;
        if curpage = page then begin
          PageW := 0;
          PageH := 0;
          sp1 := strpos(PChar(s), '/CropBox[');
          if sp1 <> nil then begin
            Inc(sp1, 9);
            TokenFloat(sp1);
            TokenFloat(sp1);
            PageW := TokenFloat(sp1);
            PageH := TokenFloat(sp1);

            if PageW <> 0 then begin
              RateW := (x2 - x1) / PageW;
            end else
              RateW := 1;

            if PageH <> 0 then begin
              RateH := (y2 - y1) / PageH;
            end else
              RateH := 1;

            Rate := RateW;
            if RateW > RateH then Rate := RateH;
          end;
          break;
        end;
        Inc(curpage);
      end;
    end;
  end; // while

  if obj <> '' then begin
    sp := PChar(src);
    sp1 := PChar(-1);
    while sp1 = PChar(-1) do begin
      sp1 := mempos(sp, PChar(obj + ' 0 obj'), Length(src)-Integer(sp-PChar(src)));
      if (sp1 <> nil) and ((sp1-1)^ in ['0'..'9']) then begin
        sp := sp1 + Length(obj) + 6;
        sp1 := mempos(sp, 'endobj', Length(src)-Integer(sp-PChar(src)));
        if sp1 <> nil then sp := sp1 + 6;
        sp1 := PChar(-1);
      end;
    end;
    if (sp1 <> nil) and (sp1 <> PChar(-1)) then begin
      sp := sp1 + Length(obj) + 6;
      flate := Pos('/FlateDecode', sp) > 0;
      len := 0;
      sp1:= strpos(sp, '/Length');
      if sp1 <> nil then begin
        Inc(sp1, 8);
        len:= Trunc(TokenFloat(sp1));
      end;
      if len > 0 then begin
        sp := strpos(sp, 'stream');
        if sp <> nil then begin
          Inc(sp, 6);
          while (Integer(sp-PChar(src)) < Length(src))
           and (sp^ in [#$0d, #$0a]) do Inc(sp);
        end;
        if flate then begin
          z.next_in := PByte(sp);
          z.avail_in := len;
          if inflateInit(z) = Z_OK then begin
            try
              cmd := '';
              SetLength(s1, ZBUF_LEN);
              while True do begin
                z.next_out := PByte(PChar(s1));
                z.avail_out := ZBUF_LEN;
                if inflate(z, Z_SYNC_FLUSH) <> Z_OK then break;
                if z.avail_out > 0 then s1[ZBUF_LEN-z.avail_out+1]:= #0;
                cmd := cmd + PChar(s1);
              end;
              if z.avail_out > 0 then s1[ZBUF_LEN-z.avail_out+1]:= #0;
              cmd := cmd + PChar(s1);
            finally
              inflateEnd(z);
            end;
          end;
        end else begin
          cmd := Copy(sp, 1, len);
        end;
        DrawPage(cmd);
      end;
    end;
  end;
end;

end.

