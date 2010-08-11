unit l4l_pdf;
{
  Lua4Lazalus

    sample:

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.


  ToDo:
    /Type/ObjStm .
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
  TStrokeType = (tstNone, tstM, tstRe, tstBez);

  TDblXY = class
  public
    tst: TStrokeType;
    x, y: double;
  end;

  TMatrix = array[1..3, 1..3] of double;

  { TPDFObj }

  TPDFObj = class
  public
    no: integer;
    val: string;
    decoded: boolean;
    stream: string;
  end;

  { TPDFReader }

  TPDFReader = class
  public
    stream: TStream;
    objs: TStringList;
    buf: string;
    buf_p: PChar;
    buf_l: integer;
    constructor Create(Astream: TStream);
    destructor Destroy; override;
    function FindObj(const no: string): TPDFObj;
    function FindPageObj(no: integer): TPDFObj;
    procedure DecodeObj(obj: TPDFObj);
    procedure ReadBuf;
    function GetVal(const name, arr: string): string;
  end;

  { TFontObj }

  TFontObj = class
  public
    b: integer;
    l: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure make_l(const s: string);
    function cid2utf8(const cid: string): string;
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

function TokenStr(var p: PChar): string;
begin
  Result := '';
  while p^ in [#$09, #$0a, #$0c, #$0d, ' '] do Inc(p);
  if p^ = #0 then Exit;
  while (p^ <> #0) and (p^ in ['0'..'9','.','-']) do begin
    Result:= Result + p^;
    Inc(p);
  end;
  while p^ in [#$09, #$0a, #$0c, #$0d, ' '] do Inc(p);
end;

{ TPDF }

constructor TPDFReader.Create(Astream: TStream);
begin
  objs := TStringList.Create;
  objs.Sorted:= True;
  stream := Astream;
  stream.Position:= 0;
  buf_l := 0;
end;

destructor TPDFReader.Destroy;
var
  i : integer;
begin
  for i := 0 to objs.Count-1 do objs.Objects[i].Free;
  objs.Free;
  inherited Destroy;
end;

function TPDFReader.FindObj(const no: string): TPDFObj;
var
  i, j, c: integer;
  sp1, sp2: PChar;
  s1: string;
  o, o1: TPDFObj;
  ofs: array of integer;
begin
  Result := nil;
  i := objs.IndexOf(no);
  if i >= 0 then begin
    Result := TPDFObj(objs.Objects[i]);
    if Result.decoded then Exit;
  end else begin
    while True do begin
      if (buf_l = 0) or (buf_p >= PChar(buf)+buf_l) then ReadBuf;
      sp1 := mempos(buf_p, 'obj', buf_l-Integer(buf_p-PChar(buf)));
      if sp1 <> nil then begin
        sp2 := mempos(buf_p, 'endobj', buf_l-Integer(buf_p-PChar(buf)));
        if sp2 <> nil then begin
          o := TPDFObj.Create;
          o.decoded:= False;
          SetLength(o.val, sp2-sp1-3);
          move((sp1+3)^, o.val[1], sp2-sp1-3);
          o.val := Trim(o.val);
          s1 := '';
          Dec(sp1, 4);
          while sp1^ in ['0'..'9'] do begin
            s1 := sp1^ + s1;
            Dec(sp1);
          end;
          o.no:= StrToInt(s1);
          Objs.AddObject(s1, o);
          buf_p := sp2 + 6;
          if s1 = no then begin
            Result := o;
            Break;
          end;
        end else
          Exit; // broken pdf
      end else begin
        for i:= 0 to objs.Count-1 do begin
          o := TPDFObj(objs.Objects[i]);
          if Pos('/Type/ObjStm', o.val) > 0 then begin
            sp1 := strpos(PChar(o.val), '/N');
            if sp1 = nil then Exit; // broken pdf
            Inc(sp1, 3);
            c := StrToInt(TokenStr(sp1));
            DecodeObj(o);
            sp1 := PChar(o.stream);
            SetLength(ofs, c);
            for j:= 1 to c do begin
              TokenStr(sp1);
              ofs[j-1] := StrToInt(TokenStr(sp1));
            end;
            sp2 := PChar(o.stream);
            for j:= 1 to c do begin
              o1 := TPDFObj.Create;
              s1 := TokenStr(sp2);
              TokenStr(sp2);
              if s1 = no then Result := o1;
              Objs.AddObject(s1, o1);
              o1.no:= StrToInt(s1);
              if j < c then begin
                SetLength(o1.val, ofs[j]-ofs[j-1]);
                move((sp1+ofs[j-1])^, o1.val[1], ofs[j]-ofs[j-1]);
              end else begin
                SetLength(o1.val, Length(o.stream)-(sp1-PChar(o.stream)));
                move((sp1+ofs[j-1])^, o1.val[1], Length(o.stream)-(sp1-PChar(o.stream)));
              end;
            end;
            o.val := '';
            if Result <> nil then Break;
          end;
        end;
        Break;
      end;
    end; // while
  end;

  if (Result <> nil) and (Result.decoded = False) then begin
    DecodeObj(Result);
  end;
end;

function TPDFReader.FindPageObj(no: integer): TPDFObj;
var
  i: integer;
  sp1, sp2: PChar;
  s1: string;
  o: TPDFObj;
begin
  Result := nil;
  for i := 0 to objs.Count-1 do begin
    o := TPDFObj(objs.Objects[i]);
    if (Pos('/Type/Page', o.val) > 0) and (Pos('/Contents', o.val) > 0) then begin
      Dec(no);
      if no = 0 then begin
        Result := o;
        Break;
      end;
    end;
  end;

  if Result = nil then begin
    while True do begin
      if (buf_l = 0) or (buf_p >= PChar(buf)+buf_l) then ReadBuf;
      sp1 := mempos(buf_p, 'obj', buf_l-Integer(buf_p-PChar(buf)));
      if sp1 <> nil then begin
        sp2 := mempos(buf_p, 'endobj', buf_l-Integer(buf_p-PChar(buf)));
        if sp2 <> nil then begin
          o := TPDFObj.Create;
          o.decoded:= False;
          SetLength(o.val, sp2-sp1-3);
          move((sp1+3)^, o.val[1], sp2-sp1-3);
          o.val := Trim(o.val);
          s1 := '';
          Dec(sp1, 4);
          while sp1^ in ['0'..'9'] do begin
            s1 := sp1^ + s1;
            Dec(sp1);
          end;
          o.no:= StrToInt(s1);
          Objs.AddObject(s1, o);
          buf_p := sp2 + 6;
          if (Pos('/Type/Page', o.val) > 0) and (Pos('/Contents', o.val) > 0) then begin
            Dec(no);
            if no = 0 then begin
              Result := o;
              Break;
            end;
          end;
        end else
          Exit; // broken pdf
      end else
        Exit; // finished
    end; // while
  end;

  if (Result <> nil) and (Result.decoded = False) then begin
    DecodeObj(Result);
  end;
end;

procedure TPDFReader.DecodeObj(obj: TPDFObj);
const
  ZBUF_LEN = 10000;
var
  sp, sp1, sp2: PChar;
  s1, s2: string;
  len: integer;
  o: TPDFObj;
  z: TZStream;
begin
  obj.decoded:= True;
  sp := PChar(obj.val);
  sp1 := strpos(sp, 'stream');
  if sp1 = nil then Exit;
  s1:= Trim(Copy(sp, 1, sp1-sp));
  len := 0;
  sp2:= strpos(PChar(s1), '/Length');
  if sp2 <> nil then begin
    Inc(sp2, 8);
    len:= StrToInt(TokenStr(sp2));
    TokenStr(sp2);
    if sp2^ = 'R' then begin
      o := FindObj(IntToStr(len));
      len := 0;
      if o <> nil then len := StrToInt(o.val);
    end;
  end;
  if len > 0 then begin
    Inc(sp1, 6);
    while (sp1^ <> #0) and (sp1^ in [#$0d, #$0a]) do Inc(sp1);
    if Pos('/FlateDecode', s1) > 0 then begin
      z.next_in := PByte(sp1);
      z.avail_in := len;
      if inflateInit(z) = Z_OK then begin
        try
          obj.stream := '';
          SetLength(s2, ZBUF_LEN);
          while True do begin
            z.next_out := PByte(PChar(s2));
            z.avail_out := ZBUF_LEN;
            if inflate(z, Z_SYNC_FLUSH) <> Z_OK then break;
            if z.avail_out > 0 then s2[ZBUF_LEN-z.avail_out+1]:= #0;
            obj.stream := obj.stream + PChar(s2);
          end;
          if z.avail_out > 0 then s2[ZBUF_LEN-z.avail_out+1]:= #0;
          obj.stream := obj.stream + PChar(s2);
        finally
          inflateEnd(z);
        end;
      end;
    end else begin
      obj.stream := Copy(sp1, 1, len);
    end;
    obj.val:= s1;
  end;
end;

procedure TPDFReader.ReadBuf;
const
  BUF_LEN = 10000;
var
  i, l: integer;
  sp1, sp2: PChar;
begin
  l := BUF_LEN;
  while True do begin
    SetLength(buf, l);
    buf_l := stream.Read(buf[1], l);
    buf_p := PChar(buf);
    if buf_l = l then begin
      sp1 := mempos(buf_p, 'obj', buf_l-Integer(buf_p-PChar(buf)));
      sp2 := mempos(buf_p, 'endobj', buf_l-Integer(buf_p-PChar(buf)));
      if (sp1 = nil) or (sp2 = nil) or (sp1 > sp2) then begin
        stream.Position:= stream.Position - l;
        Inc(l, BUF_LEN);
        continue;
      end;
      sp1 := PChar(buf) + l;
      i := stream.Position;
      while Copy(sp1-6, 1, 6) <> 'endobj' do begin
        Dec(sp1);
        Dec(buf_l);
        Dec(i);
      end;
      stream.Position:= i;
    end;
    Break;
  end; // while
end;

function TPDFReader.GetVal(const name, arr: string): string;

  function GetValSub(sp: PChar): string;
  var
    c: integer;
    s: string;
    o: TPDFObj;
    sp1: PChar;
  begin
    Result := '';
    if (sp^ = '<') and ((sp+1)^ = '<') then begin
      c:= 0;
      sp1 := sp + 2;
      while sp1^ <> #0 do begin
        if (sp1^ = '<') and ((sp1+1)^ = '<') then begin
          Inc(sp1);
          Inc(c);
        end;
        if (sp1^ = '>') and ((sp1+1)^ = '>') then begin
          if c = 0 then begin
            Result := Copy(sp, 1, sp1-sp+2);
            Exit;
          end else
            Dec(c);
        end;
        Inc(sp1);
      end;
    end else begin
      s := TokenStr(sp);
      TokenStr(sp);
      if sp^ = 'R' then begin
        o := FindObj(s);
        if o <> nil then Result := GetValSub(PChar(o.val));
      end else
        Result := s;
    end;
  end;

var
  sp: PChar;
begin
  Result := '';
  sp := strpos(PChar(arr), PChar(name));
  if sp = nil then Exit;
  Inc(sp, Length(name));
  while sp^ in [#$09, #$0a, #$0c, #$0d, ' '] do Inc(sp);
  if sp^ = #0 then Exit;
  Result := GetValSub(sp);
end;

{ TFontObj }

constructor TFontObj.Create;
begin
  inherited Create;
  l := TStringList.Create;
end;

destructor TFontObj.Destroy;
begin
  l.Free;
  inherited Destroy;
end;

procedure TFontObj.make_l(const s: string);
var
  p, p1, p2, p3, e: PChar;
begin
  p := strpos(PChar(s), 'beginbfchar');
  e := strpos(PChar(s), 'endbfchar');
  b := 0;
  l.Clear;
  while (p <> nil) and (p < e) do begin
    p := strpos(p, '<');
    if p <> nil then begin
      p1 := strpos(p+1, '>');
      if p1 <> nil then begin
        p2 := strpos(p1+1, '<');
        if p2 <> nil then begin
          p3 := strpos(p2+1, '>');
          b := p1 - p - 1;
          l.AddObject(
           Copy(p+1, 1, b), TObject(StrToInt('$' + Copy(p2+1, 1, 4))));
          p := p3 + 1;
          continue;
        end;
      end;
      p:= nil;
    end;
  end;
end;

function TFontObj.cid2utf8(const cid: string): string;
var
  ws: string;
  i, j: integer;
  w: word;
begin
  ws := '';
  for i := 1 to Length(cid) div Self.b do begin
    j := l.IndexOf(cid[i*2-1]+cid[i*2]);
    if j < 0 then break;
    w := Integer(l.Objects[j]);
    ws := ws + WideChar(w);
  end;
  Result := UTF8Encode(ws);
end;

procedure DrawPDF(stream: TStream; LPO: TLuaPrintObject; page: integer;
 x1, y1, x2, y2: integer);
var
  PageW, PageH, RateW, RateH, Rate: double;
  fonts: TStringList;

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
    i, j, Tf_index: integer;
    s, s1, cm, Tf: string;
    sx, sy, x1, y1, x2, y2: double;
    Tl, Tc, Tw, Tfs, Th, Trise: double;
    sp: PChar;
    xy: TDblXY;
    bt: boolean;
    Tlm, Tm, m1, m2: TMatrix;
    c: char;
    poly: boolean;
  begin
    ss:= TStringList.Create;
    try
      params := TObjectList.Create(True);
      try
        sp := PChar(cmd);
        bt := False;
        Tl:=0; Tc := 0; Tw := 0; Trise:=0; Th:=1;
        Tf := ''; Tf_index := -1;
        LPO.LuaPrint.Canvas.Pen.JoinStyle:= pjsMiter;
        LPO.LuaPrint.Canvas.Pen.EndCap:= pecFlat;
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
            while not (sp^ in [#0, #$09, #$0a, #$0c, #$0d, ' ',
             '(', ')', '<', '>', '[', ']', '{', '}', '/', '%']) do begin
              cm := cm + sp^;
              Inc(sp);
            end;
          end;

          if cm = 'BT' then begin
            bt := True;
            Tlm[1][1] := 1; Tlm[1][2]:=0;Tlm[1][3]:=0;
            Tlm[2][1] := 0; Tlm[2][2]:=1;Tlm[2][3]:=0;
            Tlm[3][1] := 0; Tlm[3][2]:=0;Tlm[3][3]:=1;
            Tm := Tlm;
            ss.Clear;
          end else if cm = 'ET' then begin
            bt := False;
            ss.Clear;
          end else if cm = 'q' then begin
            LPO.LuaPrint.PushCanvas;
            LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
            ss.Clear;
          end else if cm = 'Q' then begin
            LPO.LuaPrint.PopCanvas;
            LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
            ss.Clear;
          end else if cm = 'm' then begin
            xy := TDblXY.Create;
            xy.tst:= tstM;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            sx := xy.x;
            sy := xy.y;
            params.Add(xy);
            ss.Clear;
          end else if cm = 'l' then begin
            xy := TDblXY.Create;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
          end else if cm = 'c' then begin
            xy := TDblXY.Create;
            xy.tst:= tstBez;
            xy.x:= StrToFloat(ss[ss.Count-6]);
            xy.y := StrToFloat(ss[ss.Count-5]);
            params.Add(xy);
            xy := TDblXY.Create;
            xy.tst:= tstBez;
            xy.x:= StrToFloat(ss[ss.Count-4]);
            xy.y := StrToFloat(ss[ss.Count-3]);
            params.Add(xy);
            xy := TDblXY.Create;
            xy.tst:= tstBez;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
          end else if cm = 'h' then begin
            if (params.Count > 0) and
             ((TDblXY(params[params.Count-1]).x <> sx) or
              (TDblXY(params[params.Count-1]).y <> sy)) then begin
              if TDblXY(params[params.Count-1]).tst = tstBez then begin
                xy := TDblXY.Create;
                xy.tst:= tstM;
                xy.x:= TDblXY(params[params.Count-1]).x;
                xy.y := TDblXY(params[params.Count-1]).y;
                params.Add(xy);
              end;
              xy := TDblXY.Create;
              xy.x:= sx;
              xy.y := sy;
              params.Add(xy);
            end;
            ss.Clear;
          end else if cm = 're' then begin
            xy := TDblXY.Create;
            xy.tst:= tstRe;
            xy.x:= StrToFloat(ss[ss.Count-4]);
            xy.y := StrToFloat(ss[ss.Count-3]);
            params.Add(xy);
            xy := TDblXY.Create;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
            ////////////////////////////////
          end else if cm = 'J' then begin
            case Trunc(StrToFloat(ss[ss.Count-1])) of
              1: i:= Integer(pecRound);
              2: i:= Integer(pecSquare);
              else i:= Integer(pecFlat);
            end;
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_EndCap(%d)', [i]));
            LPO.LuaPrint.Canvas.Pen.EndCap:= TPenEndCap(i);
            ss.Clear;
          end else if cm = 'j' then begin
            case Trunc(StrToFloat(ss[ss.Count-1])) of
              1: i:= Integer(pjsRound);
              2: i:= Integer(pjsBevel);
              else i:= Integer(pjsMiter);
            end;
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_JoinStyle(%d)', [i]));
            LPO.LuaPrint.Canvas.Pen.JoinStyle:= TPenJoinStyle(i);
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
          end else if cm = 'w' then begin
            i := Trunc(StrToFloat(ss[ss.Count-1]) * Rate);
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_width(%d)', [i]));
            ss.Clear;
          ////////////////////////////////
          end else if cm = 'n' then begin
            // End the path object without filling or stroking it.
            params.Clear;
            ss.Clear;
          end else if (cm = 'S') or (cm = 's') or (cm = 'f') or (cm = 'b') or
           (cm = 'f*') or (cm = 'b*') or (cm = 'B') or (cm = 'B*') then begin
            if params.Count > 0 then begin
              if ((cm = 's') or (cm = 'b')) and
               ((TDblXY(params[params.Count-1]).x <> sx) or
                (TDblXY(params[params.Count-1]).y <> sy)) then begin
                if TDblXY(params[params.Count-1]).tst = tstBez then begin
                  xy := TDblXY.Create;
                  xy.tst:= tstM;
                  xy.x:= TDblXY(params[params.Count-1]).x;
                  xy.y := TDblXY(params[params.Count-1]).y;
                  params.Add(xy);
                end;
                xy := TDblXY.Create;
                xy.x:= sx;
                xy.y := sy;
                params.Add(xy);
              end;

              i := 0;
              poly:= False;
              while i < params.Count do begin
                case TDblXY(params[i]).tst of
                  tstM: begin
                    poly := True;
                    case TDblXY(params[i+1]).tst of
                      tstBez: begin
                        s := '';
                        repeat
                          sx := TDblXY(params[i]).x * Rate;
                          sy := (PageH - TDblXY(params[i]).y) * Rate;
                          s := s + Format('%d,%d,', [Trunc(sx), Trunc(sy)]);
                          Inc(i);
                        until (i >= params.Count) or
                         (TDblXY(params[i]).tst <> tstBez);
                        Delete(s, Length(s), 1);
                        LPO.LuaPrint.AddOrder(
                         Format(PRUN_NAME + '.AddBezierPoint(%s)', [s]));
                      end;
                      else begin
                        s := '';
                        repeat
                          sx := TDblXY(params[i]).x * Rate;
                          sy := (PageH - TDblXY(params[i]).y) * Rate;
                          s := s + Format('%d,%d,', [Trunc(sx), Trunc(sy)]);
                          Inc(i);
                        until (i >= params.Count) or
                         (TDblXY(params[i]).tst <> tstNone);
                        Delete(s, Length(s), 1);
                        LPO.LuaPrint.AddOrder(
                         Format(PRUN_NAME + '.AddPolyPoint(%s)', [s]));
                      end;
                    end;
                  end;
                  tstRe: begin
                    x1 := TDblXY(params[i]).x;
                    y1 := TDblXY(params[i]).y;
                    x2 := TDblXY(params[i+1]).x;
                    y2 := TDblXY(params[i+1]).y;
                    s:= Format('%d,%d,%d,%d',
                     [Trunc(x1 * Rate), Trunc((PageH-y1) * Rate),
                      Trunc((x1+x2) * Rate), Trunc((PageH-y1-y2) * Rate)]);

                    if (cm = 's') or (cm = 'S') then begin
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                       [Integer(bsClear)]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s]));
                    end else if (cm = 'b') or (cm = 'b*')
                     or (cm = 'B') or (cm = 'B*') then begin
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                       [Integer(bsSolid)]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s]));
                    end else begin
                      LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_style(%d)',
                       [Integer(psSolid)]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)',
                       [LPO.LuaPrint.Canvas.Brush.Color]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                       [Integer(bsSolid)]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s]));
                      LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
                    end;
                    Inc(i, 2);
                  end;
                end;
              end; // while

              if poly then begin
                if (cm = 's') or (cm = 'S') then begin
                  LPO.LuaPrint.AddOrder(PRUN_NAME + '.polyline()');
                end else if (cm = 'b') or (cm = 'b*')
                 or (cm = 'B') or (cm = 'B*') then begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polygon(%d)',
                   [Integer((cm = 'b') or (cm = 'B'))]));
                end else begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polyfill(%d)',
                   [Integer(cm = 'f')]));
                end;
                LPO.LuaPrint.AddOrder(PRUN_NAME + '.AddPolyPoint()');
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
            Tlm := Tm;
            ss.Clear;
          end else if cm = 'Td' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := StrToFloat(ss[ss.Count-2]);
            m1[3][2] := StrToFloat(ss[ss.Count-1]);
            m1[3][3] := 1;
            Tlm := matrixmul(m1, Tlm);
            Tm := Tlm;
            ss.Clear;
          end else if cm = 'TD' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := StrToFloat(ss[ss.Count-2]);
            m1[3][2] := StrToFloat(ss[ss.Count-1]);
            m1[3][3] := 1;
            Tlm := matrixmul(m1, Tlm);
            Tm := Tlm;
            Tl := m1[3][2];
            ss.Clear;
          end else if cm = 'T*' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := 0;
            m1[3][2] := Tl;
            m1[3][3] := 1;
            Tlm := matrixmul(m1, Tlm);
            Tm := Tlm;
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
            Tf := ss[ss.Count-2];
            Tf_index := fonts.IndexOf(Tf);
            Tfs := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tj' then begin
            s := ss[ss.Count-1];
            if s[1] = '(' then begin
              Delete(s, 1, 1); Delete(s, Length(s), 1);
            end else if s[1] = '<' then begin
              Delete(s, 1, 1); Delete(s, Length(s), 1);
              if (Tf_index >= 0) and (fonts.Objects[Tf_index] <> nil) then
                s := TFontObj(fonts.Objects[Tf_index]).cid2utf8(s);
            end;
            m1[1][1] := Tfs * Th; m1[1][2]:=0;   m1[1][3]:=0;
            m1[2][1]:=0;          m1[2][2]:=Tfs; m1[2][3]:=0;
            m1[3][1]:= 0;         m1[3][2]:= Trise; m1[3][3]:= 1;
            m2 := matrixmul(m1, Tm);
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.font_height(%d)',
             [-Trunc(m2[2][2]*Rate)]));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
             [Integer(bsClear)]));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.textout(%d,%d,"%s")',
             [Trunc(m2[3][1]*Rate), Trunc((PageH-m2[3][2]-m2[2][2])*Rate), s]));

            m1[1][1]:= 1; m1[1][2]:=0; m1[1][3]:=0;
            m1[2][1]:= 0; m1[2][2]:=1; m1[2][3]:=0;
            LPO.LuaPrint.Canvas.Font.Height:= -Trunc(Tfs*Th);
            m1[3][1]:= (LPO.LuaPrint.Canvas.TextWidth(s)+Tc+Tw)*Th;
            m1[3][2]:= 0; m1[3][3]:= 1;
            Tm := matrixmul(m1, Tm);
            ss.Clear;
          end else if cm = 'TJ' then begin
            s := ss[ss.Count-1];
            Delete(s, 1, 1); Delete(s, Length(s), 1);
            i := 1;
            while i <= Length(s) do begin
              j := i;
              if s[j] = '(' then begin
                Inc(i);
                while (i <= Length(s)) and (s[i] <> ')') do Inc(i);
                s1 := Copy(s, j+1, i-j-1);
                Inc(i);
              end else if s[j] = '<' then begin
                Inc(i);
                while (i <= Length(s)) and (s[i] <> '>') do Inc(i);
                s1 := Copy(s, j+1, i-j-1);
                Inc(i);
                if (Tf_index >= 0) and (fonts.Objects[Tf_index] <> nil) then
                  s1 := TFontObj(fonts.Objects[Tf_index]).cid2utf8(s1);
              end else begin
                while (i <= Length(s)) and (s[i] in ['0'..'9', '.', '-']) do Inc(i);
                s1 := Copy(s, j, i-j);
                m1[1][1]:= 1; m1[1][2]:=0; m1[1][3]:=0;
                m1[2][1]:= 0; m1[2][2]:=1; m1[2][3]:=0;
                m1[3][1]:= (-StrToFloat(s1)/1000*Tfs)*Th;
                m1[3][2]:= 0; m1[3][3]:= 1;
                Tm := matrixmul(m1, Tm);
                continue;
              end;
              m1[1][1] := Tfs * Th; m1[1][2]:=0;   m1[1][3]:=0;
              m1[2][1]:=0;          m1[2][2]:=Tfs; m1[2][3]:=0;
              m1[3][1]:= 0;         m1[3][2]:= Trise; m1[3][3]:= 1;
              m2 := matrixmul(m1, Tm);
              LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.font_height(%d)',
               [-Trunc(m2[2][2]*Rate)]));
              LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
               [Integer(bsClear)]));
              LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.textout(%d,%d,"%s")',
               [Trunc(m2[3][1]*Rate), Trunc((PageH-m2[3][2]-m2[2][2])*Rate), s1]));

              m1[1][1]:= 1; m1[1][2]:=0; m1[1][3]:=0;
              m1[2][1]:= 0; m1[2][2]:=1; m1[2][3]:=0;
              LPO.LuaPrint.Canvas.Font.Height:= -Trunc(Tfs*Th);
              m1[3][1]:= (LPO.LuaPrint.Canvas.TextWidth(s1)+Tc+Tw)*Th;
              m1[3][2]:= 0; m1[3][3]:= 1;
              Tm := matrixmul(m1, Tm);
            end;
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

var
  pdfr: TPDFReader;
  pageobj, pdfobj: TPDFObj;
  i: integer;
  s: string;
  sp1: PChar;
  sl: TStringList; // for debug
begin
  sl:= TStringList.Create;
  pdfr := TPDFReader.Create(stream);
  fonts:= TStringList.Create;
  try
    pageobj := pdfr.FindPageObj(page);
    if pageobj = nil then Exit;

    PageW := 0;
    PageH := 0;
    sp1 := strpos(PChar(pageobj.val), '/MediaBox[');
    if sp1 <> nil then begin
      Inc(sp1, 10);
      TokenStr(sp1);
      TokenStr(sp1);
      PageW := StrToFloat(TokenStr(sp1));
      PageH := StrToFloat(TokenStr(sp1));

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

    s := pdfr.GetVal('/Resources', pageobj.val);
    s := pdfr.GetVal('/Font', s);
    sp1 := PChar(s);
    while sp1^ <> #0 do begin
      if sp1^ = '/' then begin
        fonts.Add('');
        while not(sp1^ in [#0, ' ']) do begin
          fonts[fonts.Count-1] := fonts[fonts.Count-1] + sp1^;
          Inc(sp1);
        end;
        i := StrToInt(TokenStr(sp1));
        fonts.Objects[fonts.Count-1]:= TObject(i);
      end;
      Inc(sp1);
    end;

    for i := 0 to fonts.Count-1 do begin
      pdfobj := pdfr.FindObj(IntToStr(Integer(fonts.Objects[i])));
      fonts.Objects[i] := nil;
      if pdfobj <> nil then begin
        sp1 := strpos(PChar(pdfobj.val), '/ToUnicode');
        if sp1 <> nil then begin
          Inc(sp1, 11);
          pdfobj := pdfr.FindObj(TokenStr(sp1));
          if pdfobj <> nil then begin
            fonts.Objects[i] := TFontObj.Create;
            TFontObj(fonts.Objects[i]).make_l(pdfobj.stream);
          end;
        end;
      end;
    end;

    sp1 := strpos(PChar(pageobj.val), '/Contents');
    if sp1 = nil then Exit;
    Inc(sp1, 10);
    pageobj := pdfr.FindObj(TokenStr(sp1));
    //sl.Text:=cmd; sl.SaveToFile('3.txt');
    DrawPage(pageobj.stream);
  finally
    for i := 0 to fonts.Count-1 do fonts.Objects[i].Free;
    fonts.Free;
    pdfr.Free;
    sl.Free;
  end;
end;

end.

