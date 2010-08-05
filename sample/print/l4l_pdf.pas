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

  TMatrix = array[1..3, 1..3] of double;

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

  function matrixmul(m1, m2: TMatrix): TMatrix;
  var
    i, j, k: integer;
  begin
    for i:=1 to 3 do begin
      for j:=1 to 3 do begin
        Result[i][j]:= 0;
        for k := 1 to 3 do begin
          Result[i][j] := Result[i][j] + m1[i][k] * m2[k][j];
        end;
      end;
    end;
  end;

  procedure DrawPage(const cmd: string);
  var
    ss: TStringList;
    params: TObjectList;
    i: integer;
    s, s1, s2, cm: string;
    sx, sy, x1, y1, x2, y2: double;
    Tl, Tc, Tw, Tfs, Th, Trise: double;
    sp: PChar;
    xy: TDblXY;
    bt: boolean;
    Tm, m1, m2: TMatrix;
    c: char;
  begin
    ss:= TStringList.Create;
    try
      params := TObjectList.Create(True);
      try
        sp := PChar(cmd);
        bt := False;
        Tl:=0; Tc := 0; Tw := 0; Trise:=0; Th:=1;
        while sp^ <> #0 do begin
          while sp^ in [#$09, #$0a, #$0c, #$0d, ' '] do Inc(sp);
          if sp^ = #0 then Break;
          cm := '';
          if sp^ in ['(', '<', '[', '{'] then begin
            case sp^ of
              '(': c := ')';
              '<': c := '>';
              '[': c := ']';
              '{': c := '}';
            end;
            i:= 0;
            cm := sp^;
            Inc(sp);
            while (sp^ <> #0) and ((i > 0) or (sp^ <> c)) do begin
              if sp^ = cm[1] then begin
                Inc(i);
              end else if sp^ = c then begin
                Dec(i);
              end;
              cm := cm + sp^;
              Inc(sp);
            end;
            if sp^ <> #0 then begin
              cm := cm + sp^;
              Inc(sp);
            end;
          end else begin
            case sp^ of
              '/', '%': begin
                cm := sp^;
                Inc(sp);
              end
            end;
            while not (sp^ in [#0, #$09, #$0a, #$0c, #$0d,
              ' ', '(', ')', '<', '>', '[', ']', '{', '}', '/', '%']) do begin
              cm := cm + sp^;
              Inc(sp);
            end;
          end;

          if cm = 'BT' then begin
            bt := True;
            Tm[1][1] := 1; Tm[1][2]:=0;Tm[1][3]:=0;
            Tm[2][1] := 0; Tm[2][2]:=1;Tm[2][3]:=0;
            Tm[3][1] := 0; Tm[3][2]:=0;Tm[3][3]:=1;
            ss.Clear;
          end else if cm = 'ET' then begin
            bt := False;
            ss.Clear;
          end else if cm = 'm' then begin
            xy := TDblXY.Create;
            xy.re:= False;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
          end else if cm = 'l' then begin
            xy := TDblXY.Create;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
          end else if cm = 'h' then begin
            if params.Count > 0 then begin
              xy := TDblXY.Create;
              xy.x:= TDblXY(params[0]).x;
              xy.y := TDblXY(params[0]).y;
              params.Add(xy);
            end;
            ss.Clear;
          end else if cm = 're' then begin
            xy := TDblXY.Create;
            xy.re:= True;
            xy.x:= StrToFloat(ss[ss.Count-4]);
            xy.y := StrToFloat(ss[ss.Count-3]);
            params.Add(xy);
            xy := TDblXY.Create;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
            ////////////////////////////////
          end else if cm = 'G' then begin
            x1 := StrToFloat(ss[ss.Count-1]);
            i := RGBToColor(Trunc(255*x1), Trunc(255*x1), Trunc(255*x1));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)', [i]));
            LPO.LuaPrint.Canvas.Pen.Color:= i;
            ss.Clear;
          end else if cm = 'g' then begin
            x1 := StrToFloat(ss[ss.Count-1]);
            i := RGBToColor(Trunc(255*x1), Trunc(255*x1), Trunc(255*x1));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_color(%d)', [i]));
            LPO.LuaPrint.Canvas.Brush.Color:= i;
            ss.Clear;
          end else if cm = 'RG' then begin
            i := RGBToColor(
             Trunc(255*StrToFloat(ss[ss.Count-3])),
             Trunc(255*StrToFloat(ss[ss.Count-2])),
             Trunc(255*StrToFloat(ss[ss.Count-1])));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)', [i]));
            LPO.LuaPrint.Canvas.Pen.Color:= i;
            ss.Clear;
          end else if cm = 'rg' then begin
            i := RGBToColor(
             Trunc(255*StrToFloat(ss[ss.Count-3])),
             Trunc(255*StrToFloat(ss[ss.Count-2])),
             Trunc(255*StrToFloat(ss[ss.Count-1])));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_color(%d)', [i]));
            LPO.LuaPrint.Canvas.Brush.Color:= i;
            ss.Clear;
          ////////////////////////////////
          end else if cm = 'n' then begin
            // End the path object without filling or stroking it.
            params.Clear;
            ss.Clear;
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
                 [Trunc(x1 * Rate), Trunc((PageH-y1) * Rate),
                  Trunc((x1+x2) * Rate), Trunc((PageH-y1-y2) * Rate)]);

                if (cm = 's') or (cm = 'S') then begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsClear)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s1]));
                end else if (cm = 'b') or (cm = 'b*')
                 or (cm = 'B') or (cm = 'B*') then begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s1]));
                end else begin
                  LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_style(%d)',
                   [Integer(psSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)',
                   [LPO.LuaPrint.Canvas.Brush.Color]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s1]));
                  LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
                end;
              end else begin
                s1 := '';
                for i := 0 to params.Count-1 do begin
                  sx := TDblXY(params[i]).x * Rate;
                  sy := (PageH - TDblXY(params[i]).y) * Rate;
                  s1 := s1 + Format('%d,%d,', [Trunc(sx), Trunc(sy)]);
                end;
                Delete(s1, Length(s1), 1);

                if (cm = 's') or (cm = 'S') then begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polyline(%s)', [s1]));
                end else if (cm = 'b') or (cm = 'b*')
                 or (cm = 'B') or (cm = 'B*') then begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polygon(%s,%d)',
                   [s1, Integer((cm = 'b') or (cm = 'B'))]));
                end else begin
                  LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_style(%d)',
                   [Integer(psSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)',
                   [LPO.LuaPrint.Canvas.Brush.Color]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polygon(%s)', [s1]));
                  LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
                end;
              end;
              params.Clear;
            end;
            ss.Clear;
          ////////////////////////////////////////////////////////////////////////
          end else if cm = 'Tm' then begin
            Tm[1][1] := StrToFloat(ss[ss.Count-6]); // a
            Tm[1][2] := StrToFloat(ss[ss.Count-5]); // b
            Tm[1][3] := 0;
            Tm[2][1] := StrToFloat(ss[ss.Count-4]); // c
            Tm[2][2] := StrToFloat(ss[ss.Count-3]); // d
            Tm[2][3] := 0;
            Tm[3][1] := StrToFloat(ss[ss.Count-2]); // e = x
            Tm[3][2] := StrToFloat(ss[ss.Count-1]); // f = y
            Tm[3][3] := 1;
            ss.Clear;
          end else if cm = 'Td' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := StrToFloat(ss[ss.Count-2]);
            m1[3][2] := StrToFloat(ss[ss.Count-1]);
            m1[3][3] := 1;
            Tm := matrixmul(m1, Tm);
            ss.Clear;
          end else if cm = 'TD' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := StrToFloat(ss[ss.Count-2]);
            m1[3][2] := StrToFloat(ss[ss.Count-1]);
            m1[3][3] := 1;
            Tm := matrixmul(m1, Tm);
            Tl := m1[3][2];
            ss.Clear;
          end else if cm = 'T*' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := 0;
            m1[3][2] := Tl;
            m1[3][3] := 1;
            Tm := matrixmul(m1, Tm);
            ss.Clear;
          end else if cm = 'TL' then begin
            Tl := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tc' then begin
            Tc := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tw' then begin
            Tw := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tz' then begin
            Th := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Ts' then begin
            Trise := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tf' then begin
            //TokenStr(sp1, ' ');
            Tfs := StrToFloat(ss[ss.Count-1]);
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.font_size(%d)',
             [Trunc(Tfs)]));
            ss.Clear;
          end else if cm = 'Tj' then begin
            s1 := ss[ss.Count-1];
            if s1[1] = '(' then begin
              Delete(s1, 1, 1); Delete(s1, Length(s1), 1);
            end else if s1[1] = '<' then begin
              Delete(s1, 1, 1); Delete(s1, Length(s1), 1);
              //s1 := StringOfChar('*', Length(s1) div 4) + Format('%d', [Length(s1) div 4]);
            end;
            m1[1][1] := Tfs * Th; m1[1][2]:=0;   m1[1][3]:=0;
            m1[2][1]:=0;          m1[2][2]:=Tfs; m1[2][3]:=0;
            m1[3][1]:= 0;         m1[3][2]:= Trise; m1[3][3]:= 1;
            m2 := matrixmul(m1, Tm);
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
             [Integer(bsClear)]));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.textout(%d,%d,"%s")',
             [Trunc(m2[3][1]*Rate), Trunc((PageH-m2[3][2]-m2[2][2])*Rate), s1]));
            ss.Clear;
          end else if cm = 'TJ' then begin // ToDo
            s1 := ss[ss.Count-1];
            Delete(s1, 1, 1); Delete(s1, Length(s1), 1);
            if s1[1] = '(' then begin
              Delete(s1, 1, 1); Delete(s1, Length(s1), 1);
            end else if s1[1] = '<' then begin
              Delete(s1, 1, 1); Delete(s1, Length(s1), 1);
              //s1 := StringOfChar('*', Length(s1) div 4) + Format('%d', [Length(s1) div 4]);
            end;
            m1[1][1] := Tfs * Th; m1[1][2]:=0;   m1[1][3]:=0;
            m1[2][1]:=0;          m1[2][2]:=Tfs; m1[2][3]:=0;
            m1[3][1]:= 0;         m1[3][2]:= Trise; m1[3][3]:= 1;
            m2 := matrixmul(m1, Tm);
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
             [Integer(bsClear)]));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.textout(%d,%d,"%s")',
             [Trunc(m2[3][1]*Rate), Trunc((PageH-m2[3][2]-m2[2][2])*Rate), s1]));
            ss.Clear;
          end else
            ss.Add(cm);
        end; // while
      finally
        params.Free;
      end;
    finally
      ss.Free;
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
  sl: TStringList; // for debug
  i, p1, p2, len, curpage: integer;
  src, s, s1, obj, cmd: string;
  sp, sp1, sp2: PChar;
  z: TZStream;
  flate: boolean;
begin
  sl:= TStringList.Create;
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
        SetLength(s, sp2-sp1-3);
        move((sp1+3)^, s[1], sp2-sp1-3);
        s := Trim(s);
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
          sp1 := strpos(PChar(s), '/MediaBox[');
          if sp1 <> nil then begin
            Inc(sp1, 10);
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
        TokenFloat(sp1);
        if sp1^ = 'R' then begin
          obj := IntToStr(len);
          len := 0;
          sp1 := PChar(src);
          sp2 := PChar(-1);
          while sp2 = PChar(-1) do begin
            sp2 := mempos(sp1, PChar(obj + ' 0 obj'), Length(src)-Integer(sp1-PChar(src)));
            if (sp2 <> nil) and ((sp2-1)^ in ['0'..'9']) then begin
              sp1 := sp2 + Length(obj) + 6;
              sp2 := mempos(sp1, 'endobj', Length(src)-Integer(sp1-PChar(src)));
              if sp2 <> nil then sp1 := sp2 + 6;
              sp2 := PChar(-1);
            end;
          end;
          if sp2 <> nil then begin
            sp1 := sp2 + Length(obj) + 6;
            sp2 := mempos(sp1, 'endobj', Length(src)-Integer(sp1-PChar(src)));
            len := StrToInt(Trim(Copy(sp1, 1, sp2-sp1)));
          end;
        end;
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
        //sl.Text:=cmd; sl.SaveToFile('3.txt');
        DrawPage(cmd);
      end;
    end;
  end;
  sl.Free;
end;

end.

