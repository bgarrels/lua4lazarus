unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FThread: TThread;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
  Lua, lualib, lauxlib, l4l_object;

{$R *.lfm}

type

  { TLuaThread }

  TLuaThread = class(TThread)
  private
    L: Plua_State;
    msg: string;
    finished: boolean;
    procedure ShowMsg;
    procedure Last;
  protected
    procedure Execute; override;
  public
    property Terminated;
    procedure Sync(AMethod: TThreadMethod);
    destructor Destroy; override;
  end;

  { TLuaMyObject }

  TLuaMyObject = class(TLuaObject)
  private
    procedure DoPrint;
    procedure DoSetCaption;
  protected
  public
  published
    function l4l_print: integer;
    function l4l_SetCaption: integer;
  end;

function Alloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t) : Pointer; cdecl;
begin
  try
    Result:= ptr;
    ReallocMem(Result, nSize);
  except
    Result:= nil;
  end;
end;

procedure hook({%H-}L: plua_State; {%H-}ar: plua_Debug); cdecl;
begin
  if TLuaThread(Form1.FThread).Terminated then SysUtils.Abort;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  L: Plua_State;
  s: string;
begin
  if Assigned(FThread) and not TLuaThread(FThread).finished then begin
    // Stop thread.
    FThread.Free;
    FThread := nil;
    Exit;
  end;

  // Run thread.
  Memo2.Clear;
  L:= lua_newstate(@alloc, nil);
  lua_sethook(L, @hook, LUA_MASKLINE, 0);
  luaopen_base(L);
  //luaopen_string(L);
  l4l_PushLuaObject(TLuaMyObject.Create(L)); lua_setglobal(L, 'my'); // set global value.
  s:= Memo1.Text;
  if luaL_loadbuffer(L, PChar(s), Length(s), 'sample') <> 0 then begin
    Form1.Memo2.Lines.Add(lua_tostring(L, -1));
    Form1.Memo2.SelStart:= 0;
    Form1.Memo2.SelLength:= 0;
    Exit;
  end;

  if Assigned(FThread) then FThread.Free;
  FThread := TLuaThread.Create(True);
  TLuaThread(FThread).L := L;
  FThread.FreeOnTerminate:= False;
  FThread.Resume;
  Form1.Button1.Caption:= 'Stop';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FThread:= nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FThread) then FThread.Free;
end;

{ TLuaThread }

procedure TLuaThread.Execute;
begin
  finished:= False;
  try
    try
      if lua_pcall(L, 0, 0, 0) <> 0 then Exception.Create('');
    except
      on E: EAbort do begin
        msg := 'Abort.';
        Synchronize(@ShowMsg);
      end;
      else begin
        msg := lua_tostring(L, -1);
        Synchronize(@ShowMsg);
      end;
    end;
  finally
    finished:= True;
    msg := ''; // For eco.
    lua_close(L);
    Synchronize(@Last);
  end;
end;

procedure TLuaThread.Sync(AMethod: TThreadMethod);
begin
  Synchronize(AMethod);
end;

procedure TLuaThread.ShowMsg;
begin
  Form1.Memo2.Lines.Add(msg);
  Form1.Memo2.SelStart:= 0;
  Form1.Memo2.SelLength:= 0;
end;

procedure TLuaThread.Last;
begin
  Form1.Button1.Caption:= 'Run';
end;

destructor TLuaThread.Destroy;
begin
  inherited Destroy;
end;

{ TLuaMyObject }

procedure TLuaMyObject.DoPrint;
var
  i, c: integer;
begin
  c:= lua_gettop(LS);
  for i:= 1 to c do
    Form1.Memo2.Lines.Add(lua_tostring(LS, i));
  Form1.Memo2.SelStart:= 0;
  Form1.Memo2.SelLength:= 0;
end;

procedure TLuaMyObject.DoSetCaption;
begin
  Form1.Label1.Caption:= lua_tostring(LS, 1);
end;

function TLuaMyObject.l4l_print: integer;
begin
  TLuaThread(Form1.FThread).Sync(@DoPrint);
  Result := 0;
end;

function TLuaMyObject.l4l_SetCaption: integer;
begin
  TLuaThread(Form1.FThread).Sync(@DoSetCaption);
  Result := 0;
end;

end.

