{
  Lua4Lazalus

    sample:

    License: New BSD
}
unit l4l_print;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, graphics, printers, types,
  lua, l4l_object;

type

  { TLuaPrint }

  TLuaPrint = class(TObject)
  private
    FInitCanvas: TCanvas;
    function GetPageCount: integer;
    function GetPageSize: TSize;
    function GetPaperSize: TSize;
    procedure CopyCanvas(src, dst: TCanvas);
  protected
    LS: Plua_State;
    FPageList: TObjectList;
    FBmpList: TObjectList;
    FResList: TObjectList;
    FUserMargin, FRealMargin: TRect;
    FPaperRect: TPaperRect;
    FDPI, FPlayDPI: integer;
    FCanvas: TCanvas;
    FOffset: TPoint;
    FZoom: integer;
    function DP2LP(dp: integer): integer;
    function LP2DP(lp: integer): integer;
    function z(i: integer): integer;
    procedure AddOrder(const s: string);
  public
    constructor Create(L : Plua_State);
    destructor Destroy; override;
    procedure BeginDoc;
    procedure BeginDoc(Margin: TRect);
    procedure Run(const SourceCode: string);
    procedure EndDoc;
    procedure NewPage;
    procedure Play(pageNumber: integer; Canvas: TCanvas;
     dpi: integer = 0; Zoom: integer = 100);
    procedure Play(pageNumber: integer; Canvas: TCanvas; Margin: TRect;
     dpi, Zoom: integer);
    procedure Print(beginPage: integer=0; endPage: integer=0);
    property PageCount: integer read GetPageCount;
    property PaperSize: TSize read GetPaperSize;
    property PageSize: TSize read GetPageSize;
    property DPI: integer read FDPI;
  end;

  { TLuaPrintObject }

  TLuaFontObject = class;
  TLuaPenObject = class;
  TLuaBrushObject = class;

  TLuaPrintObject  = class(TLuaObject)
  private
    FUnits: char;
    function DP2LP(dp: integer): integer;
    function GetBrushObject: TLuaBrushObject;
    function GetFontObject: TLuaFontObject;
    function GetPageHeight: integer;
    function GetPageNumber: integer;
    function GetPageWidth: integer;
    function GetPenObject: TLuaPenObject;
    function LP2DP(lp: integer): integer;
  protected
    LuaPrint: TLuaPrint;
  public
    constructor Create(L : Plua_State; lp: TLuaPrint); overload;
    destructor Destroy; override;
  published
    function l4l_TextOut: integer;
    function l4l_Rectangle: integer;
    function l4l_Line: integer;
    function l4l_TextWidth: integer;
    function l4l_TextHeight: integer;
    function l4l_DrawImage: integer;
    function l4l_NewPage: integer;
    property l4l_pageWidth: integer read GetPageWidth;
    property l4l_pageHeight: integer read GetPageHeight;
    property l4l_pageNumber: integer read GetPageNumber;
    property l4l_units: char read FUnits write FUnits;
    property l4l_Font: TLuaFontObject read GetFontObject;
    property l4l_Pen: TLuaPenObject read GetPenObject;
    property l4l_Brush: TLuaBrushObject read GetBrushObject;
  end;

  { TLuaFontObject }

  TLuaFontObject  = class(TLuaObject)
  private
    function GetColor: string;
    function GetHeight: integer;
    function GetName: string;
    function GetSize: integer;
    function GetStyle: string;
    procedure SetColor(const AValue: string);
    procedure SetHeight(const AValue: integer);
    procedure SetName(const AValue: string);
    procedure SetSize(const AValue: integer);
    procedure SetStyle(const AValue: string);
  protected
    LPO: TLuaPrintObject;
  public
    constructor Create(L : Plua_State;  aLPO: TLuaPrintObject); overload;
    destructor Destroy; override;
  published
    property l4l_color: string read GetColor write SetColor;
    property l4l_Name: string read GetName write SetName;
    property l4l_Size: integer read GetSize write SetSize;
    property l4l_Height: integer read GetHeight write SetHeight; // Only "DP".
    property l4l_Style: string read GetStyle write SetStyle;
  end;

  { TLuaPenObject }

  TLuaPenObject  = class(TLuaObject)
  private
    function GetColor: string;
    function GetMode: string;
    function GetStyle: string;
    function GetWidth: integer;
    procedure SetColor(const AValue: string);
    procedure SetMode(const AValue: string);
    procedure SetStyle(const AValue: string);
    procedure SetWidth(const AValue: integer);
  protected
    LPO: TLuaPrintObject;
  public
    constructor Create(L : Plua_State; aLPO: TLuaPrintObject); overload;
    destructor Destroy; override;
  published
    property l4l_color: string read GetColor write SetColor;
    property l4l_style: string read GetStyle write SetStyle;
    property l4l_mode: string read GetMode write SetMode;
    property l4l_width: integer read GetWidth write SetWidth;
  end;

  { TLuaBrushObject }

  TLuaBrushObject  = class(TLuaObject)
  private
    function GetColor: string;
    function GetStyle: string;
    procedure SetColor(const AValue: string);
    procedure SetStyle(const AValue: string);
  protected
    LPO: TLuaPrintObject;
  public
    constructor Create(L : Plua_State; aLPO: TLuaPrintObject); overload;
    destructor Destroy; override;
  published
    property l4l_color: string read GetColor write SetColor;
    property l4l_style: string read GetStyle write SetStyle;
  end;

  { TLuaPrintRunObject }

  TLuaPrintRunObject  = class(TLuaObject)
  private
  protected
    LuaPrint: TLuaPrint;
    function w(i: integer): integer;
    function zx(i: integer): integer;
    function zy(i: integer): integer;
  public
    constructor Create(L : Plua_State; lp: TLuaPrint); overload;
    destructor Destroy; override;
  published
    function l4l_TextOut: integer;
    function l4l_Rectangle: integer;
    function l4l_Line: integer;
    function l4l_DrawImage: integer;
    function l4l_font_color: integer;
    function l4l_font_name: integer;
    function l4l_font_size: integer;
    function l4l_font_height: integer;
    function l4l_font_style: integer;
    function l4l_pen_color: integer;
    function l4l_pen_style: integer;
    function l4l_pen_mode: integer;
    function l4l_pen_width: integer;
    function l4l_brush_color: integer;
    function l4l_brush_style: integer;
  end;

implementation
uses
  LCLType, LCLIntf, typinfo, lauxlib;

const
  MM_P_INCH = 2540;
  PRUN_NAME = 'P_';

{ TLuaPrint }

function TLuaPrint.GetPageCount: integer;
begin
  Result := FPageList.Count;
end;

function TLuaPrint.DP2LP(dp: integer): integer;
begin
  Result:= Trunc(MM_P_INCH * dp / FDPI + 0.5);
end;

function TLuaPrint.GetPageSize: TSize;
begin
  Result.cx := FPaperRect.PhysicalRect.Right-FRealMargin.Left-FRealMargin.Right;
  Result.cy:= FPaperRect.PhysicalRect.Bottom-FRealMargin.Top-FRealMargin.Bottom;
end;

function TLuaPrint.GetPaperSize: TSize;
begin
  Result.cx := FPaperRect.PhysicalRect.Right;
  Result.cy:= FPaperRect.PhysicalRect.Bottom;
end;

procedure TLuaPrint.CopyCanvas(src, dst: TCanvas);
begin
  dst.Font.Assign(src.Font);
  dst.Pen.Assign(src.Pen);
  dst.Brush.Assign(src.Brush);
end;

function TLuaPrint.LP2DP(lp: integer): integer;
begin
  Result:= Trunc(lp * FDPI / MM_P_INCH + 0.5);
end;

function TLuaPrint.z(i: integer): integer;
begin
  Result := i*FPlayDpi*FZoom div (FDPI*100);
end;

procedure TLuaPrint.AddOrder(const s: string);
begin
  TStringList(FPageList[FPageList.Count-1]).Add(s);
end;

constructor TLuaPrint.Create(L : Plua_State);
begin
  LS := L;
  FPageList:= TObjectList.Create(True);
  FBmpList:= TObjectList.Create(True);
  FResList:= TObjectList.Create(True);
  FInitCanvas := TCanvas.Create;
end;

destructor TLuaPrint.Destroy;
begin
  FPageList.Free;
  FBmpList.Free;
  FResList.Free;
  FInitCanvas.Free;
  inherited Destroy;
end;

procedure TLuaPrint.BeginDoc;
begin
  BeginDoc(Rect(0,0,0,0));
end;

procedure TLuaPrint.BeginDoc(Margin: TRect);
var
  bmp: TBitmap;
begin
  FDPI := Printer.YDPI;
  FPageList.Clear;
  FPageList.Add(TStringList.Create);
  FPaperRect := Printer.PaperSize.PaperRect;
  FUserMargin :=
   Rect(LP2DP(Margin.Left), LP2DP(Margin.Top),
        LP2DP(Margin.Right), LP2DP(Margin.Bottom));
  FRealMargin := FUserMargin;
  if FPaperRect.WorkRect.Left > FRealMargin.Left then
    FRealMargin.Left := FPaperRect.WorkRect.Left;
  if FPaperRect.WorkRect.Top > FRealMargin.Top then
    FRealMargin.Top := FPaperRect.WorkRect.Top;
  if FPaperRect.PhysicalRect.Right-FPaperRect.WorkRect.Right > FRealMargin.Right then
    FRealMargin.Right := FPaperRect.PhysicalRect.Right-FPaperRect.WorkRect.Right;
  if FPaperRect.PhysicalRect.Bottom-FPaperRect.WorkRect.Bottom > FRealMargin.Bottom then
    FRealMargin.Bottom := FPaperRect.PhysicalRect.Bottom-FPaperRect.WorkRect.Bottom;
  FBmpList.Clear;
  bmp := TBitmap.Create;
  FBmpList.Add(bmp);
  FCanvas:= bmp.Canvas;
  FCanvas.Font.PixelsPerInch:= FDPI;
  FCanvas.Font.Size:= 10;
  CopyCanvas(FCanvas, FInitCanvas);
  FResList.Clear;
end;

procedure TLuaPrint.EndDoc;
begin
  CopyCanvas(FInitCanvas, FCanvas);
  if FPageList.Count > 0 then begin
    if TStringList(FPageList[FPageList.Count-1]).Count = 0 then begin
      FPageList.Delete(FPageList.Count-1);
      FBmpList.Delete(FBmpList.Count-1);
    end;
  end;
  FCanvas := nil;
end;

procedure TLuaPrint.NewPage;
var
  bmp: TBitmap;
begin
  FPageList.Add(TStringList.Create);
  bmp := TBitmap.Create;
  FBmpList.Add(bmp);
  CopyCanvas(FCanvas, bmp.Canvas);
  CopyCanvas(FInitCanvas, FCanvas);
  FCanvas:= bmp.Canvas;
  FCanvas.Font.PixelsPerInch:= Printer.YDPI;
  FCanvas.Font.Size:= FCanvas.Font.Size;
  CopyCanvas(FCanvas, FInitCanvas);
end;

procedure TLuaPrint.Run(const SourceCode: string);
begin
  if luaL_loadbuffer(LS, PChar(SourceCode), Length(SourceCode), 'print') <> 0 then
    Raise Exception.Create('');
  if lua_pcall(LS, 0, 0, 0) <> 0 then
    Raise Exception.Create('');
end;

procedure TLuaPrint.Play(pageNumber: integer; Canvas: TCanvas; dpi: integer;
  Zoom: integer);
begin
  Play(pageNumber, Canvas, Rect(0,0,0,0), dpi, Zoom);
end;

procedure TLuaPrint.Play(pageNumber: integer; Canvas: TCanvas; Margin: TRect;
  dpi, Zoom: integer);
var
  i : integer;
  sl: TStringList;
  h: HRGN;
  x, y: integer;
  bmp: TBitmap;
begin
  if (pageNumber > 0) and (pageNumber <= PageCount) then begin
    if dpi = 0 then dpi := Canvas.Font.PixelsPerInch;
    FPlayDpi:= dpi;
    FZoom := Zoom;
    FOffset.x:= FRealMargin.Left;
    FOffset.y:= FRealMargin.Top;
    x := FPaperRect.PhysicalRect.Right-FRealMargin.Right;
    y := FPaperRect.PhysicalRect.Bottom-FRealMargin.Bottom;
    Dec(FOffset.x, Margin.Left);
    Dec(FOffset.y, Margin.Top);
    Dec(x, Margin.Left);
    Dec(y, Margin.Top);

    FCanvas := Canvas;
    bmp := TBitmap(FBmpList[pageNumber-1]);
    CopyCanvas(bmp.Canvas, FCanvas);
    i := FCanvas.Font.Size;
    FCanvas.Font.PixelsPerInch:= FPlayDpi;
    FCanvas.Font.Size:= i + 1;
    FCanvas.Font.Size:= i;
    FCanvas.Font.Height:= FCanvas.Font.Height * FZoom div 100;
    FCanvas.Pen.Width:= FCanvas.Pen.Width * FPlayDpi * FZoom div (FDPI * 100);

    h := CreateRectRgn(0, 0, z(x)+1, z(y)+1);
    try
      SelectClipRgn(Canvas.Handle, h);
      sl := TStringList(FPageList[pageNumber-1]);
      l4l_PushLuaObject(TLuaPrintRunObject.Create(LS, Self));
      lua_setglobal(LS, PRUN_NAME);
      try
        if luaL_loadbuffer(LS, PChar(sl.Text), Length(sl.Text), nil) <> 0 then
          Raise Exception.Create('');
        if lua_pcall(LS, 0, 0, 0) <> 0 then
          Raise Exception.Create('');
      finally
        lua_pushnil(LS);
        lua_setglobal(LS, PRUN_NAME);
      end;
    finally
      SelectClipRgn(Canvas.Handle, 0);
      DeleteObject(h);
    end;
  end;
end;

procedure TLuaPrint.Print(beginPage: integer; endPage: integer);
var
  i: integer;
  m: TRect;
begin
  if beginPage < 1 then beginPage := 1;
  if endPage < beginPage then endPage := FPageList.Count;
  Printer.BeginDoc;
  try
    m := Rect(
     Printer.PaperSize.PaperRect.WorkRect.Left,
     Printer.PaperSize.PaperRect.WorkRect.Top,
     Printer.PaperSize.PaperRect.PhysicalRect.Right
      -Printer.PaperSize.PaperRect.WorkRect.Right,
     Printer.PaperSize.PaperRect.PhysicalRect.Bottom
      -Printer.PaperSize.PaperRect.WorkRect.Bottom);
    for i := beginPage to endPage do begin
      Play(i, Printer.Canvas, m, Printer.YDPI, 100);
      if i < endPage then Printer.NewPage;
    end;
    Printer.EndDoc;
  except
    Printer.Abort;
    Raise;
  end;
end;

function str_param(const s: string): string;
var
  i: integer;
begin
  Result := '"';
  for i:= 1 to Length(s) do begin
    case s[i] of
      '"': Result:= Result + '\"';
      '\': Result:= Result + '\\';
      else Result := Result + s[i];
    end;
  end;
  Result := Result + '"';
end;

{ TLuaPrintObject0 }

function TLuaPrintObject.DP2LP(dp: integer): integer;
var
  i : integer;
begin
  case Upcase(FUnits) of
    'M': i:= MM_P_INCH;
    'I': i:= 1000;
    'T': i:= 1440;
    else begin
      Result := dp;
      Exit;
    end;
  end;
  Result:= Trunc(dp * i / LuaPrint.FDPI + 0.5);
end;

function TLuaPrintObject.GetBrushObject: TLuaBrushObject;
begin
  Result := TLuaBrushObject.Create(LS, Self);
end;

function TLuaPrintObject.GetFontObject: TLuaFontObject;
begin
  Result := TLuaFontObject.Create(LS, Self);
end;

function TLuaPrintObject.GetPenObject: TLuaPenObject;
begin
  Result := TLuaPenObject.Create(LS, Self);
end;

function TLuaPrintObject.GetPageHeight: integer;
begin
  Result := DP2LP(LuaPrint.PageSize.cy);
end;

function TLuaPrintObject.GetPageNumber: integer;
begin
  Result := LuaPrint.PageCount;
end;

function TLuaPrintObject.GetPageWidth: integer;
begin
  Result := DP2LP(LuaPrint.PageSize.cx);
end;

function TLuaPrintObject.LP2DP(lp: integer): integer;
var
  i : integer;
begin
  case Upcase(FUnits) of
    'M': i:= MM_P_INCH;
    'I': i:= 1000;
    'T': i:= 1440;
    else begin
      Result := lp;
      Exit;
    end;
  end;
  Result:= Trunc(lp * LuaPrint.FDPI / i + 0.5);
end;

constructor TLuaPrintObject.Create(L: Plua_State; lp: TLuaPrint);
begin
  inherited Create(L);
  LuaPrint:= lp;
  FUnits := 'M';
end;

destructor TLuaPrintObject.Destroy;
begin
  inherited Destroy;
end;

function TLuaPrintObject.l4l_TextOut: integer;
begin
  LuaPrint.AddOrder(
   Format(PRUN_NAME + '.TextOut(%d,%d,%s)',
   [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
   str_param(lua_tostring(LS, 3))]));
  Result := 0;
end;

function TLuaPrintObject.l4l_Rectangle: integer;
begin
  LuaPrint.AddOrder(
   Format(PRUN_NAME + '.rectangle(%d,%d,%d,%d)',
   [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
    LP2DP(lua_tointeger(LS, 3)), LP2DP(lua_tointeger(LS, 4))]));
  Result := 0;
end;

function TLuaPrintObject.l4l_Line: integer;
var
  c: integer;
begin
  c := lua_gettop(LS);
  if c < 4 then begin
    LuaPrint.AddOrder(
     Format(PRUN_NAME + '.line(%d,%d)',
     [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2))]));
  end else begin
    LuaPrint.AddOrder(
     Format(PRUN_NAME + '.line(%d,%d,%d,%d)',
     [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
      LP2DP(lua_tointeger(LS, 3)), LP2DP(lua_tointeger(LS, 4))]));
  end;
  Result := 0;
end;

function TLuaPrintObject.l4l_TextWidth: integer;
begin
  lua_pushinteger(LS, DP2LP(LuaPrint.FCanvas.TextWidth(lua_tostring(LS, 1))));
  Result := 1;
end;

function TLuaPrintObject.l4l_TextHeight: integer;
begin
  lua_pushinteger(LS, DP2LP(LuaPrint.FCanvas.TextHeight(lua_tostring(LS, 1))));
  Result := 1;
end;

function TLuaPrintObject.l4l_DrawImage: integer;
var
  fn: string;
  ms: TMemoryStream;
begin
  fn := lua_tostring(LS, 5);
  ms := TMemoryStream.Create;
  ms.LoadFromFile(fn);
  LuaPrint.FResList.Add(ms);
  LuaPrint.AddOrder(
   Format(PRUN_NAME + '.drawimage(%d,%d,%d,%d,%d,%s)',
   [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
    LP2DP(lua_tointeger(LS, 3)), LP2DP(lua_tointeger(LS, 4)),
    LuaPrint.FResList.Count-1, str_param(ExtractFileExt(fn))]));
  Result := 0;
end;

function TLuaPrintObject.l4l_NewPage: integer;
begin
  LuaPrint.NewPage;
  Result := 0;
end;

{ TLuaFontObject }

constructor TLuaFontObject.Create(L: Plua_State; aLPO: TLuaPrintObject);
begin
  inherited Create(L);
  LPO := aLPO;
end;

destructor TLuaFontObject.Destroy;
begin
  inherited Destroy;
end;

function TLuaFontObject.GetColor: string;
begin
  Result := ColorToString(LPO.LuaPrint.FCanvas.Font.Color);
end;

function TLuaFontObject.GetHeight: integer;
begin
  Result := LPO.LuaPrint.FCanvas.Font.Height;
end;

function TLuaFontObject.GetName: string;
begin
  Result := LPO.LuaPrint.FCanvas.Font.Name;
end;

function TLuaFontObject.GetSize: integer;
begin
  Result := LPO.LuaPrint.FCanvas.Font.Size;
end;

function TLuaFontObject.GetStyle: string;
begin
  Result := SetToString(PTypeInfo(TypeInfo(TFontStyles)),
   Integer(LPO.LuaPrint.FCanvas.Font.Style), false);
end;

procedure TLuaFontObject.SetColor(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := StringToColor(AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Font.Color := TColor(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.font_color(%d)', [i]));
  end;
end;

procedure TLuaFontObject.SetHeight(const AValue: integer);
begin
  LPO.LuaPrint.FCanvas.Font.Height := AValue;
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_height(%d)', [AValue]));
end;

procedure TLuaFontObject.SetName(const AValue: string);
begin
  LPO.LuaPrint.FCanvas.Font.Name := AValue;
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_name(%s)', [str_param(AValue)]));
end;

procedure TLuaFontObject.SetSize(const AValue: integer);
begin
  LPO.LuaPrint.FCanvas.Font.Size := AValue;
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_size(%d)', [AValue]));
end;

procedure TLuaFontObject.SetStyle(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := StringToSet(PTypeInfo(TypeInfo(TFontStyles)), AValue);
  end;
  LPO.LuaPrint.FCanvas.Font.Style := TFontStyles(i);
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_style(%d)', [i]));
end;

{ TLuaPenObject }

function TLuaPenObject.GetStyle: string;
begin
  Result := GetEnumName(TypeInfo(TPenStyle),
   Integer(LPO.LuaPrint.FCanvas.Pen.Style));
end;

function TLuaPenObject.GetWidth: integer;
begin
  Result := LPO.DP2LP(LPO.LuaPrint.FCanvas.Pen.Width);
  if Result < 1 then Result := 1;
end;

function TLuaPenObject.GetColor: string;
begin
  Result := ColorToString(LPO.LuaPrint.FCanvas.Pen.Color);
end;

function TLuaPenObject.GetMode: string;
begin
  Result := GetEnumName(TypeInfo(TPenMode),
   Integer(LPO.LuaPrint.FCanvas.Pen.Mode));
end;

procedure TLuaPenObject.SetColor(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := StringToColor(AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Pen.Color := TColor(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.pen_color(%d)', [i]));
  end;
end;

procedure TLuaPenObject.SetMode(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := GetEnumValue(TypeInfo(TPenMode), AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Pen.Mode := TPenMode(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.pen_mode(%d)', [i]));
  end;
end;

procedure TLuaPenObject.SetStyle(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := GetEnumValue(TypeInfo(TPenStyle), AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Pen.Style := TPenSTyle(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.pen_style(%d)', [i]));
  end;
end;

procedure TLuaPenObject.SetWidth(const AValue: integer);
var
  i: integer;
begin
  i:= LPO.LP2DP(AValue);
  if i < 1 then i := 1;
  LPO.LuaPrint.FCanvas.Pen.Width:= i;
  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_width(%d)', [i]));
end;

constructor TLuaPenObject.Create(L: Plua_State; aLPO: TLuaPrintObject);
begin
  inherited Create(L);
  LPO := aLPO;
end;

destructor TLuaPenObject.Destroy;
begin
  inherited Destroy;
end;

{ TLuaBrushObject }

function TLuaBrushObject.GetStyle: string;
begin
  Result := GetEnumName(TypeInfo(TBrushStyle),
   Integer(LPO.LuaPrint.FCanvas.Brush.Style));
end;

function TLuaBrushObject.GetColor: string;
begin
  Result := ColorToString(LPO.LuaPrint.FCanvas.Brush.Color);
end;

procedure TLuaBrushObject.SetColor(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := StringToColor(AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Brush.Color := TColor(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.brush_color(%d)', [i]));
  end;
end;

procedure TLuaBrushObject.SetStyle(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := GetEnumValue(TypeInfo(TBrushStyle), AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Brush.Style := TBrushSTyle(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.brush_style(%d)', [i]));
  end;
end;

constructor TLuaBrushObject.Create(L: Plua_State; aLPO: TLuaPrintObject);
begin
  inherited Create(L);
  LPO := aLPO;
end;

destructor TLuaBrushObject.Destroy;
begin
  inherited Destroy;
end;

{ TLuaPrintRunObject }

function TLuaPrintRunObject.w(i: integer): integer;
begin
  Result := i * LuaPrint.FPlayDpi * LuaPrint.FZoom div (LuaPrint.FDPI * 100);
end;

function TLuaPrintRunObject.zx(i: integer): integer;
begin
  Result := (i + LuaPrint.FOffset.x) *
   LuaPrint.FPlayDpi * LuaPrint.FZoom div (LuaPrint.FDPI * 100);
end;

function TLuaPrintRunObject.zy(i: integer): integer;
begin
  Result := (i + LuaPrint.FOffset.y) *
   LuaPrint.FPlayDpi * LuaPrint.FZoom div (LuaPrint.FDPI * 100);
end;

constructor TLuaPrintRunObject.Create(L: Plua_State; lp: TLuaPrint);
begin
  inherited Create(L);
  LuaPrint:= lp;
end;

destructor TLuaPrintRunObject.Destroy;
begin
  inherited Destroy;
end;

function TLuaPrintRunObject.l4l_TextOut: integer;
begin
  LuaPrint.FCanvas.TextOut(
   zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)), lua_tostring(LS, 3));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_Rectangle: integer;
begin
  LuaPrint.FCanvas.Rectangle(
   zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)),
   zx(lua_tointeger(LS, 3)), zy(lua_tointeger(LS, 4)));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_Line: integer;
var
  c: integer;
begin
  c := lua_gettop(LS);
  if c < 4 then begin
    LuaPrint.FCanvas.LineTo(
     zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)));
  end else begin
    LuaPrint.FCanvas.Line(
     zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)),
     zx(lua_tointeger(LS, 3)), zy(lua_tointeger(LS, 4)));
  end;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_DrawImage: integer;
var
  ms: TStream;
  g: TPicture;
begin
  g := TPicture.Create;
  try
    ms := TStream(LuaPrint.FResList[lua_tointeger(LS, 5)]);
    ms.Position:= 0;
    //g.LoadFromStreamWithFileExt(ms, lua_tostring(LS, 6));
    g.LoadFromStream(ms);
    LuaPrint.FCanvas.StretchDraw(
     Rect(zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)),
          zx(lua_tointeger(LS, 3)), zy(lua_tointeger(LS, 4))),
     g.Graphic);
  finally
    g.Free;
  end;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_color: integer;
begin
  LuaPrint.FCanvas.Font.Color := TColor(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_name: integer;
begin
  LuaPrint.FCanvas.Font.Name := lua_tostring(LS, 1);
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_size: integer;
begin
  LuaPrint.FCanvas.Font.Size := lua_tointeger(LS, 1);
  LuaPrint.FCanvas.Font.Height:=
   LuaPrint.FCanvas.Font.Height * LuaPrint.FZoom div 100;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_height: integer;
begin
  LuaPrint.FCanvas.Font.Height := lua_tointeger(LS, 1) * LuaPrint.FZoom div 100;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_style: integer;
begin
  LuaPrint.FCanvas.Font.Style := TFontStyles(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_color: integer;
begin
  LuaPrint.FCanvas.Pen.Color := TColor(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_style: integer;
begin
  LuaPrint.FCanvas.Pen.Style := TPenStyle(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_mode: integer;
begin
  LuaPrint.FCanvas.Pen.Mode := TPenMode(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_width: integer;
begin
  LuaPrint.FCanvas.Pen.Width:= w(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_brush_color: integer;
begin
  LuaPrint.FCanvas.Brush.Color := TColor(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_brush_style: integer;
begin
  LuaPrint.FCanvas.Brush.Style := TBrushStyle(lua_tointeger(LS, 1));
  Result := 0;
end;

end.

