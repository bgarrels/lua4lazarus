{
  Lua4Lazalus

    TLuaObject

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

    ToDo:
      - Add more metatable handling.
      - Event.

    Version History:
      1.0.0 by Malcome@Japan. (with Lazarus 0.9.29 and FPC 2.4.1)

    License for Lua 5.0 and later versions:
      Copyright(c)1994–2008 Lua.org, PUC-Rio.
}
unit l4l_object;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua;

type

  { TLuaObject }

  TLuaObject = class(TPersistent)
  private
    FLS: Plua_State;
  protected
    property LS: Plua_State read FLS;
    function Iterator(index: integer): integer; virtual;
  public
    constructor Create(L : Plua_State); virtual;
  published
  end;

procedure PushLuaObject(obj: TLuaObject);
function l4l_isobject(L : Plua_State;  n: Integer): boolean;
function l4l_isobject(L : Plua_State;  n: Integer; c: TClass): boolean;
function l4l_toobject(L : Plua_State;  n: Integer): TLuaObject;

implementation
uses
  typinfo;

const
  FIELD_OBJ = '___l4lObject___';
  FIELD_FN = '___l4lFuncName___';
  FIELD_IC = '___l4lIteCount___';

  PROP_HEAD = 'l4l_';

function gc(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
begin
  p:= lua_touserdata(L, 1);
  TLuaObject(p^).Free;
  Result:= 0;
end;

function call(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
  obj: TLuaObject;
  method: function:integer of object;
begin
  lua_getfield(L, 1, FIELD_OBJ);
  p:= lua_touserdata(L, -1);
  lua_remove(L, -1);
  obj:= TLuaObject(p^);
  lua_getfield(L, 1, FIELD_FN);
  lua_remove(L, 1);
  p:= lua_touserdata(L, -1);
  lua_remove(L, -1);
  TMethod(method).Data := obj;
  TMethod(method).Code := p^;
  Result := method();
end;

function Index(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
  key: string;
  obj: TLuaObject;
  pi: PPropInfo;
  o: TObject;
begin
  Result := 0;
  lua_getfield(L, 1, FIELD_OBJ);
  p:= lua_touserdata(L, -1);
  obj:= TLuaObject(p^);
  key := lua_tostring(L, 2);
  if Assigned(obj.MethodAddress(PROP_HEAD + key)) then begin
    lua_getfield(L, 1, PChar(LowerCase(key)));
    Result:= 1;
  end else begin
    try
      pi := FindPropInfo(obj, PROP_HEAD + key);
    except
      pi := nil;
    end;
    if Assigned(pi) then begin
      case pi^.PropType^.Kind of
        tkInteger, tkQWord:
          lua_pushinteger(L, GetOrdProp(obj, pi));
        tkInt64: lua_pushinteger(L, GetInt64Prop(obj, pi));
        tkFloat: lua_pushnumber(L, GetFloatProp(obj, pi));
        tkBool: begin
          if GetOrdProp(obj, pi) = 0 then
            lua_pushboolean(L, False)
          else
            lua_pushboolean(L, True);
        end;
        tkObject: begin
          o:= GetObjectProp(obj, pi);
          if o is TLuaObject then begin
            PushLuaObject(o as TLuaObject);
          end else begin
            lua_pushnil(L);
          end;
        end;
        else begin
          lua_pushstring(L, PChar(GetStrProp(obj, pi)));
        end;
      end;
      Result := 1;
    end;
  end;
end;

function NewIndex(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
  key: string;
  obj: TLuaObject;
  pi: PPropInfo;
begin
  Result:=0;
  lua_getfield(L, 1, FIELD_OBJ);
  p:= lua_touserdata(L, -1);
  obj:= TLuaObject(p^);
  key := lua_tostring(L, 2);
  try
    pi := FindPropInfo(obj, PROP_HEAD + key);
  except
    pi := nil;
  end;
  if Assigned(pi) then begin
    case pi^.PropType^.Kind of
      tkInteger, tkInt64, tkQWord:
        SetOrdProp(obj, pi, lua_tointeger(L, 3));
      tkFloat: SetFloatProp(obj, pi, lua_tonumber(L, 3));
      tkBool: SetOrdProp(obj, pi, Ord(lua_toboolean(L, 3)));
      tkObject: begin
        lua_getfield(L, 3, FIELD_OBJ);
        p:= lua_touserdata(L, -1);
        if Assigned(p) and Assigned(p^) then begin
          SetObjectProp(obj, pi, TObject(p^));
        end else
          SetObjectProp(obj, pi, TObject(nil));
      end;
      else begin
        SetUnicodeStrProp(obj, pi, lua_tostring(L, 3));
      end;
    end;
  end;
end;

function iterator(L : Plua_State) : Integer; cdecl;
var
  i: integer;
  p: PPointer;
  obj: TLuaObject;
begin
  Result:= 0;
  lua_getfield(L, 1, FIELD_OBJ);
  p:= lua_touserdata(L, -1);
  obj:= TLuaObject(p^);
  if lua_isnil(L, 3) then begin
    i := 0;
  end else begin
    lua_getfield(L, 1, FIELD_IC);
    i:= lua_tointeger(L, -1) + 1;
  end;
  lua_pushstring(L, FIELD_IC);
  lua_pushinteger(L, i);
  lua_rawset(L, 1);
  Result := obj.Iterator(i);
end;

procedure PushLuaObject(obj: TLuaObject);
type
  TMethodRec = packed record
    name : pshortstring;
    addr : pointer;
  end;
  TMethodTable = packed record
   count : dword;
   entries : packed array[0..0] of TMethodRec;
  end;
  PMethodTable =  ^TMethodTable;
var
  p: PPointer;
  i, t: integer;
  mt: PMethodTable;
  s:string;
  cl: TClass;
begin
  lua_newtable(obj.LS); t:= lua_gettop(obj.LS);

  lua_pushstring(obj.LS, FIELD_OBJ);
  p:= lua_newuserdata(obj.LS, SizeOf(Pointer));
  p^:=obj;
  if lua_getmetatable(obj.LS, -1) = 0 then lua_newtable(obj.LS);
  lua_pushstring(obj.LS, '__gc');
  lua_pushcfunction(obj.LS, @gc);
  lua_settable(obj.LS, -3);
  lua_setmetatable(obj.LS, -2);
  lua_settable(obj.LS, -3);

  cl:= obj.ClassType;
  while Assigned(cl) do begin
    p := Pointer(Integer(cl) + vmtMethodtable);
    mt := p^;
    if Assigned(mt) then begin
      for i:=0 to mt^.count-1 do begin
        s:= LowerCase(mt^.entries[i].name^);
        if Copy(s, 1, 4) <> PROP_HEAD then continue;
        Delete(s, 1, 4);
        lua_pushstring(obj.LS, PChar(s));
        lua_newtable(obj.LS);
        lua_pushstring(obj.LS, FIELD_FN);
        p:= lua_newuserdata(obj.LS, SizeOf(Pointer));
        p^:= mt^.entries[i].addr;
        lua_settable(obj.LS, -3);
        lua_newtable(obj.LS);
        lua_pushstring(obj.LS, '__call');
        lua_pushcfunction(obj.LS, @call);
        lua_settable(obj.LS, -3);
        lua_pushstring(obj.LS, '__index');
        lua_pushvalue(obj.LS, t); // SuperClass
        lua_settable(obj.LS, -3);
        lua_setmetatable(obj.LS, -2);
        lua_settable(obj.LS, -3);
      end;
    end;
    cl := cl.ClassParent;
  end;

  lua_newtable(obj.LS);
  lua_pushstring(obj.LS, '__newindex');
  lua_pushcfunction(obj.LS, @NewIndex);
  lua_settable(obj.LS, -3);
  lua_pushstring(obj.LS, '__index');
  lua_pushcfunction(obj.LS, @Index);
  lua_settable(obj.LS, -3);
  lua_pushstring(obj.LS, '__call');
  lua_pushcfunction(obj.LS, @iterator);
  lua_settable(obj.LS, -3);
  lua_setmetatable(obj.LS, -2);
end;

function l4l_isobject(L : Plua_State;  n: Integer): boolean;
begin
  Result:= l4l_isobject(L, n, TLuaObject);
end;

function l4l_isobject(L : Plua_State;  n: Integer; c: TClass): boolean;
var
  p: PPointer;
begin
  Result:= False;
  if lua_istable(L, n) then begin
    lua_getfield(L, n, FIELD_OBJ);
    p:= lua_touserdata(L, -1);
    lua_remove(L, -1);
    Result := TObject(p^) is c;
  end;
end;

function l4l_toobject(L: Plua_State; n: Integer): TLuaObject;
var
  p: PPointer;
begin
  Result:= nil;
  if lua_istable(L, n) then begin
    lua_getfield(L, n, FIELD_OBJ);
    p:= lua_touserdata(L, -1);
    lua_remove(L, -1);
    Result := TLuaObject(p^);
  end;
end;

{ TLuaObject }

function TLuaObject.Iterator(index: integer): integer;
begin
  Result := 0;
end;

constructor TLuaObject.Create(L: Plua_State);
begin
  FLS := L;
end;

end.

