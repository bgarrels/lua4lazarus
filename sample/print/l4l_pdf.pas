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

procedure DrawPDF(stream: TStream; LPO: TLuaPrintObject; x1, y1, x2, y2: integer);

implementation
uses
  graphics, paszlib;

const
  PRUN_NAME = 'P_';

procedure DrawPDF(stream: TStream; LPO: TLuaPrintObject; x1, y1, x2, y2: integer);
var
  PageW, PageH, RateW, RateH, Rate: double;

  function TokenFloat(var s: string): double;
  var
    i: integer;
  begin
    i := 1;
    while (i <= Length(s)) and (s[i] in ['0'..'9','.','-']) do Inc(i);
    if i > 1 then begin
      Result := StrToFloat(Trim(Copy(s, 1, i-1)));
      Delete(s, 1, i);
    end else begin
      Result := 0;
      Delete(s, 1, 1);
    end;
  end;

  function TokenFloat2(var p: PChar): double;
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

  procedure DrawPage(cmd: string);
  var
    sl, params: TStringList;
    i, j: integer;
    s, cm: string;
    sx, sy, x1, y1, x2, y2: double;
  begin
    sl := TStringList.Create;
    try
      sl.Text := cmd;
      sl.SaveToFile('2.txt');
      params := TStringList.Create;
      try
        for i := 0 to sl.Count-1 do begin
          s := sl[i];
          j := Length(s);
          while j >= 1 do begin
            if not (s[j] in ['0'..'9', 'a'..'z', 'A'..'Z']) then break;
            Dec(j);
          end;
          if j > 0 then begin
            cm := Copy(s, j+1, Length(s));
            Delete(s, j+1, Length(s));
          end else begin
            cm := s;
            s := '';
          end;

          if cm = 'm' then begin
            sx:= TokenFloat(s); sy := TokenFloat(s);
            params.AddObject(Format('%d,%d',
             [Trunc(sx*Rate), Trunc(sy*Rate)]),
             TObject(0)
            );
          end else if cm = 'l' then begin
            params[params.Count-1] :=  params[params.Count-1] +
             Format(',%d,%d',
             [Trunc(TokenFloat(s)*Rate), Trunc(TokenFloat(s)*Rate)]);
            params.Objects[params.Count-1] :=
             TObject(Integer(params.Objects[params.Count-1]) + 1);
          end else if cm = 'h' then begin
            params[params.Count-1] :=  params[params.Count-1] +
             Format(',%d,%d',
             [Trunc(sx*Rate), Trunc(sy*Rate)]);
            params.Objects[params.Count-1] :=
             TObject(Integer(params.Objects[params.Count-1]) + 1);
          end else if cm = 're' then begin
            x1 := TokenFloat(s);
            y1 := TokenFloat(s);
            x2 := TokenFloat(s);
            y2 := TokenFloat(s);
            params.Add(
             Format('%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
             //[LPO.LP2DP(Trunc(x1)), LPO.LP2DP(Trunc(y1)),
             // LPO.LP2DP(Trunc(x1+TokenFloat(s))),
             // LPO.LP2DP(Trunc(y1+TokenFloat(s)))]));
             [Trunc(x1*Rate), Trunc(y1*Rate),
              Trunc((x1+x2)*Rate), Trunc(y1*Rate),
              Trunc((x1+x2)*Rate), Trunc((y1+y2)*Rate),
              Trunc(x1*Rate), Trunc((y1+y2)*Rate),
              Trunc(x1*Rate), Trunc(y1*Rate)]));
            params.Objects[params.Count-1] := TObject(4);
          ////////////////////////////////
          end else if cm = 'G' then begin
            x1 := TokenFloat(s);
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)', [0]));
            LPO.LuaPrint.Canvas.Pen.Color:= 0;
          end else if cm = 'g' then begin
            x1 := TokenFloat(s);
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_color(%d)', [0]));
            //LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)', [Ord(bsSolid)]));
            LPO.LuaPrint.Canvas.Brush.Color:= 0;
          ////////////////////////////////
          end else if cm = 'n' then begin
            // End the path object without filling or stroking it.
            params.Clear;
          end else if (cm = 'S') or (cm = 's') or (cm = 'f') or (cm = 'b') or
           (cm = 'f*') or (cm = 'b*') then begin

            if (cm = 's') or (cm = 'b') then begin
              params[params.Count-1] :=  params[params.Count-1] +
               Format(',%d,%d',
               [Trunc(sx*Rate), Trunc(sy*Rate)]);
              params.Objects[params.Count-1] :=
               TObject(Integer(params.Objects[params.Count-1]) + 1);
            end;

            for j := 0 to params.Count-1 do begin
              case Integer(params.Objects[j]) of
                0:;
                1: begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.line(%s)', [params[j]]));
                end;
                else begin
                  if (cm = 's') or (cm = 'S') then begin
                    LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polyline(%s)',
                     [params[j]]));
                  end else if (cm = 'b') or (cm = 'b*')
                   or (cm = 'B') or (cm = 'B*') then begin
                    LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polygon(%s,%d)',
                     [params[j], Integer((cm = 'b') or (cm = 'B'))]));
                  end else begin
                    LPO.LuaPrint.AddOrder(PRUN_NAME + '.SaveHandleState()');
                    LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_style(%d)',
                     [Integer(psSolid)]));
                    LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)',
                     [LPO.LuaPrint.Canvas.Brush.Color]));
                    LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polygon(%s)',
                     [params[j]]));
                    LPO.LuaPrint.AddOrder(PRUN_NAME + '.RestoreHandleState()');
                  end;
                end;
              end;
            end;
            params.Clear;
          end;
        end;
      finally
        params.Free;
      end;
    finally
      sl.Free;
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
  i, p1, p2, len, page: integer;
  src, s, s1, obj, cmd: string;
  sp, sp1, sp2: PChar;
  z: TZStream;
  flate: boolean;
begin
  SetLength(src, stream.Size);
  stream.Position:= 0;
  stream.ReadBuffer(src[1], stream.Size);
  sp := PChar(src);
  page := 1;
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
        if Page = 1 then begin
          PageW := 0;
          PageH := 0;
          sp1 := strpos(PChar(s), '/CropBox[');
          if sp1 <> nil then begin
            Inc(sp1, 9);
            TokenFloat2(sp1);
            TokenFloat2(sp1);
            PageW := TokenFloat2(sp1);
            PageH := TokenFloat2(sp1);

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
        Inc(page);
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
        len:= Trunc(TokenFloat2(sp1));
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

