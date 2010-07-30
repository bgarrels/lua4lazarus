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

  procedure translatePage(AInput: string);

    function TokenFloat(var s: string): double;
    var
      i: integer;
    begin
      i := Pos(' ', s);
      if i > 0 then begin
        Result := StrToFloat(Trim(Copy(s, 1, i-1)));
        Delete(s, 1, i);
      end else begin
        Result := StrToFloat(Trim(s));
        s := '';
      end;
    end;

  var
    sl, params: TStringList;
    i, j: integer;
    s, cm: string;
    sx, sy, x1, y1, x2, y2: double;
  begin
    sl := TStringList.Create;
    try
      sl.Text := AInput;
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
          end else if cm = 'n' then begin
          end else if (cm = 's') or (cm = 'f') then begin
            for j := 0 to params.Count-1 do begin
              case Integer(params.Objects[j]) of
                0:;
                1: begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.line(%s)', [params[j]]));
                end;
                else begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)', [Ord(bsClear)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.drawpoly(%s)', [params[j]]));
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

const
  ZBUF_LEN = 10000;
var
  i, p1, p2, len, page: integer;
  s, s1, s2, obj, cmd: string;
  z: TZStream;
  zout: PByte;
  d: double;
begin
  SetLength(s, stream.Size);
  stream.Position:= 0;
  stream.ReadBuffer(s[1], stream.Size);
  page := 1;
  while True do begin
    p1 := Pos('obj', s);
    if p1 > 0 then begin
      p2 := Pos('endobj', s);
      if p2 > 0 then begin
        s1 := Trim(Copy(s, p1+3, p2-p1-3));
        Delete(s, 1, p2+5);
      end else begin
        s1 := Trim(Copy(s, p1+3, Length(s)));
        s := '';
      end;
    end else
      Break;

    p1 := Pos('/Type/Page', s1);
    if p1 > 0 then begin
      p1 := Pos('/Contents', s1);
      if p1 > 0 then begin
        i := p1+10;
        obj := '';
        while i <=  Length(s1) do begin
          if s1[i] in ['0'..'9'] then begin
            obj := obj + s1[i];
          end else
            Break;
          Inc(i);
        end;
        if Page = 1 then begin
          PageW := 0;
          PageH := 0;
          p1 := Pos('/CropBox[', s1);
          if p1 > 0 then begin
            i := p1+9;
            s2 := '';
            while i <=  Length(s1) do begin
              if s1[i] in ['0'..'9','.','-'] then begin
                s2 := s2 + s1[i];
              end else
                Break;
              Inc(i);
            end;

            Inc(i);
            s2 := '';
            while i <=  Length(s1) do begin
              if s1[i] in ['0'..'9','.','-'] then begin
                s2 := s2 + s1[i];
              end else
                Break;
              Inc(i);
            end;

            Inc(i);
            s2 := '';
            while i <=  Length(s1) do begin
              if s1[i] in ['0'..'9','.','-'] then begin
                s2 := s2 + s1[i];
              end else
                Break;
              Inc(i);
            end;
            PageW := StrToFloat(s2);
            if PageW <> 0 then begin
              RateW := (x2 - x1) / PageW;
            end else
              RateW := 1;

            Inc(i);
            s2 := '';
            while i <=  Length(s1) do begin
              if s1[i] in ['0'..'9','.','-'] then begin
                s2 := s2 + s1[i];
              end else
                Break;
              Inc(i);
            end;
            PageH := StrToFloat(s2);
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
    SetLength(s, stream.Size);
    stream.Position:= 0;
    stream.ReadBuffer(s[1], stream.Size);
    p1 := -1;
    while p1 = -1 do begin
      p1 := Pos(obj + ' 0 obj', s);
      if (p1 > 0) and (s[p1-1] in ['0'..'9']) then begin
        Delete(s, 1, p1+Length(obj)+5);
        p2 := Pos('endobj', s);
        if p2 > 0 then Delete(s, 1, p2+5);
        p1 := -1;
      end;
    end;
    if p1 > 0 then begin
      Delete(s, 1, p1+Length(obj)+5);
      p2 := Pos('endobj', s);
      if p2 > 0 then s := Trim(Copy(s, 1, p2-1));
      len := 0;
      p1:= Pos('/Length', s);
      if p1 > 0 then begin
        i := p1+8;
        s1:= '';
        while i <=  Length(s) do begin
          if s[i] in ['0'..'9'] then begin
            s1 := s1 + s[i];
          end else
            Break;
          Inc(i);
        end;
        len := StrToInt(s1);
      end;
      if len > 0 then begin
        p1 := Pos('stream', s);
        if p1 > 0 then begin
          i := 0;
          while (p1+6+i <= Length(s)) and (s[p1+6+i] in [#$0d, #$0a]) do begin
            Inc(i);
          end;
        end;
        if Pos('/FlateDecode', s) > 0 then begin
          z.next_in := PByte(@s[p1+6+i]);
          z.avail_in := len;
          if inflateInit(z) = Z_OK then begin
            try
              GetMem(zout, ZBUF_LEN);
              try
                cmd := '';
                while True do begin
                  FillChar(zout^, ZBUF_LEN, 0);
                  z.next_out := zout;
                  z.avail_out := ZBUF_LEN-1;
                  if inflate(z, Z_SYNC_FLUSH) <> Z_OK then break;
                  cmd := cmd + PChar(zout);
                end;
                cmd := cmd + PChar(zout);
              finally
                FreeMem(zout);
              end;
            finally
              inflateEnd(z);
            end;
          end;
        end else begin
          cmd := Copy(s, p1+6+i, len);
        end;
        translatePage(cmd);
      end;
    end;
  end;
end;

end.

