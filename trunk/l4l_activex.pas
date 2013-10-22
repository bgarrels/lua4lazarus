{
  Lua4Lazalus

    ActiveXObject

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

    Note:
       The property name is case insensitive, but
      the function name is case sensitive.
       Then, you may use the property name with the small letter
      if ActiveXObject has same name in property and function name.
       ex. excel.Visible = true; excel.Visible(); ---> ERROR!
           excel.visible = true; excel.Visible(); ---> OK!

    ToDo:
      Event handling.

    Version History:
      1.52.0 by Malcome Japan. (with Lazarus 1.3 and FPC 2.6.2)
      1.0.0 by Malcome Japan. (with Lazarus 0.9.29 and FPC 2.4.1)

    License for Lua 5.0 and later versions:
      Copyright(c)1994–2008 Lua.org, PUC-Rio.
}
unit l4l_activex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua52;

function CreateActiveXObject(L : Plua_State) : Integer; cdecl;

implementation
uses
  Windows, ComObj, ActiveX, variants, varutils;

const
  FIELD_ID = '___IDispatch___';
  FIELD_FN = '___FuncName___';
  FIELD_IC = '___IteCount___';

procedure DoCreateActiveXObject(L : Plua_State; id: IDispatch); forward;

procedure ChkErr(L : Plua_State; Val: HResult; const prop: string='');
var
  s: string;
begin
  if not(Succeeded(Val)) then begin
    s:= Format('ActiveX Error(%x)', [Val]);
    if prop <> '' then s := s + ' in "' + prop + '".';
    luaL_error(L, PChar(s));
  end;
end;

function Index(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
  key: string;
  s: string;
  ws: WideString;
  id: IDispatch;
  di: TDispID;
  param: TDispParams;
  ret: OleVariant;
begin
  Result := 0;
  lua_getfield(L, 1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  id:= IDispatch(p^);
  lua_pop(L, 1);
  key := lua_tostring(L, 2);
  ws:= UTF8Decode(key);
  ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), key);
  param.rgvarg := nil;
  param.rgdispidNamedArgs := nil;
  param.cArgs := 0;
  param.cNamedArgs := 0;
  VariantInit(TVarData({%H-}ret));
  ChkErr(L, id.Invoke(di, GUID_NULL, GetUserDefaultLCID,
   DISPATCH_PROPERTYGET, param, @ret, nil, nil), key);
  case VarType(ret) of
    varNull: lua_pushnil(L);
    varSmallint,varInteger,varByte: lua_pushinteger(L, ret);
    varSingle,varDouble: lua_pushnumber(L, ret);
    varBoolean: lua_pushboolean(L, ret);
    varDispatch: DoCreateActiveXObject(L, ret);
    else begin
      ws := ret;
      s := UTF8Encode(ws);
      lua_pushstring(L, PChar(s));
    end;
  end;
  Result := 1;
end;

function NewIndex(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
  key: string;
  ws: WideString;
  id: IDispatch;
  di, diput: TDispID;
  param: TDispParams;
  v: OleVariant;
begin
  Result:=0;
  lua_getfield(L, 1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  id:= IDispatch(p^);
  lua_pop(L, 1);
  key := lua_tostring(L, 2);
  ws:= UTF8Decode(key);
  ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), key);
  case lua_type(L, 3) of
    LUA_TNIL: VariantInit(TVarData({%H-}v));
    LUA_TBOOLEAN: v := lua_toboolean(L, 3);
    LUA_TNUMBER: v := lua_tonumber(L, 3);
    else v := lua_tostring(L, 3);
  end;
  diput := DISPID_PROPERTYPUT;
  param.rgvarg := @v;
  param.cArgs := 1;
  param.rgdispidNamedArgs := @diput;
  param.cNamedArgs := 1;
  ChkErr(L, id.Invoke(di, GUID_NULL, GetUserDefaultLCID,
   DISPATCH_PROPERTYPUT, param, nil, nil, nil), key);
end;

function call(L : Plua_State) : Integer; cdecl;
var
  i, c, t: integer;
  p: PPointer;
  id: IDispatch;
  di: TDispID;
  s, func: string;
  ws: WideString;
  arglist, pp: {$IFDEF VER2_4}lPVariantArg{$ELSE}PVariantArg{$ENDIF};
  param: TDispParams;
  v, ret: OleVariant;
begin
  Result:= 0;
  c:= lua_gettop(L);
  t:= 1;
  if lua_istable(L, 2) then Inc(t); { it's x:x() }
  lua_getfield(L, 1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  id:= IDispatch(p^);
  lua_pop(L, 1);
  lua_getfield(L, 1, FIELD_FN);
  func:= lua_tostring(L, -1);
  lua_pop(L, 1);
  ws:= UTF8Decode(func);
  ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), func);
  GetMem(arglist, SizeOf({$IFDEF VER2_4}VariantArg{$ELSE}TVariantArg{$ENDIF}) * (c-t));
  try
    // 逆順で
    pp := arglist;
    for i := c downto t+1 do begin
      VariantInit(TVarData({%H-}v));
      case lua_type(L, i) of
        LUA_TBOOLEAN: v := lua_toboolean(L, i);
        LUA_TNUMBER: v := lua_tonumber(L, i);
        LUA_TSTRING: v := lua_tostring(L, i);
      end;
      VariantInit(TVarData(pp^));
      pp^ := {$IFDEF VER2_4}VariantArg{$ELSE}TVariantArg{$ENDIF}(v);
      Inc(pp);
    end;
    param.cArgs := c - t;
{$IFDEF VER2_4}
    param.rgvarg := arglist;
{$ELSE}
    param.rgvarg := PVariantArgList(arglist);
{$ENDIF}
    param.rgdispidNamedArgs := nil;
    param.cNamedArgs := 0;
    VariantInit(TVarData({%H-}ret));

    ChkErr(L, id.Invoke(
     di,
     GUID_NULL,
     GetUserDefaultLCID,
     DISPATCH_PROPERTYGET or DISPATCH_METHOD,
     param, @ret, nil, nil), func);
    case VarType(ret) of
      varNull: lua_pushnil(L);
      varSmallint,varInteger,varByte: lua_pushinteger(L, ret);
      varSingle,varDouble: lua_pushnumber(L, ret);
      varBoolean: lua_pushboolean(L, ret);
      varDispatch: DoCreateActiveXObject(L, ret);
      else begin
        ws := ret;
        s := UTF8Encode(ws);
        lua_pushstring(L, PChar(s));
      end;
    end;
    Result := 1;
  finally
    FreeMem(arglist);
  end;
end;

function iterator(L : Plua_State) : Integer; cdecl;
var
  i: integer;
  p: PPointer;
  id: IDispatch;
  s: string;
  ws: WideString;
  param: TDispParams;
  v, ret: OleVariant;
begin
  Result:= 0;
  lua_getfield(L, 1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  id:= IDispatch(p^);
  lua_pop(L, 1);
  if lua_isnil(L, 3) then begin
    i := 0;
  end else begin
    lua_pushstring(L, FIELD_IC);
    lua_rawget(L, 1);
    i:= lua_tointeger(L, -1) + 1;
  end;
  lua_pushstring(L, FIELD_IC);
  lua_pushinteger(L, i);
  lua_rawset(L, 1);

  VariantInit(TVarData({%H-}v));
  v := i;
  param.cArgs := 1;
  param.rgvarg := @v;
  param.rgdispidNamedArgs := nil;
  param.cNamedArgs := 0;
  VariantInit(TVarData({%H-}ret));

  if id.Invoke(
   DISPID_VALUE,
   GUID_NULL,
   GetUserDefaultLCID,
   DISPATCH_PROPERTYGET or DISPATCH_METHOD,
   param, @ret, nil, nil) = 0 then begin
    case VarType(ret) of
      varNull: lua_pushnil(L);
      varSmallint,varInteger,varByte: lua_pushinteger(L, ret);
      varSingle,varDouble: lua_pushnumber(L, ret);
      varBoolean: lua_pushboolean(L, ret);
      varDispatch: begin
        if TVarData(ret).vdispatch <> nil then begin
          DoCreateActiveXObject(L, ret);
        end else begin
          lua_pushnil(L);
        end;
      end;
      else begin
        ws := ret;
        s := UTF8Encode(ws);
        lua_pushstring(L, PChar(s));
      end;
    end;
  end else begin
    lua_pushnil(L);
  end;
  Result := 1;
end;

function gc(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
begin
  p:= lua_touserdata(L, 1);
  IDispatch(p^)._Release;
  //IDispatch(p^):= nil;
  //IDispatch(p^):= Unassigned;
  Result:= 0;
end;

procedure DoCreateActiveXObject(L : Plua_State; id: IDispatch);
var
  i, t: integer;
  p: PPointer;
  s: string;
  ws: WideString;
  ti: ITypeInfo;
  ta: lPTypeAttr;
  fd: lPFuncDesc;
begin
  id._AddRef;
  lua_newtable(L); t:= lua_gettop(L);

  lua_pushstring(L, FIELD_ID);
  p:= lua_newuserdata(L, SizeOf(IDispatch));
  p^:=id;
  if lua_getmetatable(L, -1) = 0 then lua_newtable(L);
  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, @gc);
  lua_settable(L, -3);
  lua_setmetatable(L, -2);
  lua_settable(L, -3);

  ChkErr(L, id.GetTypeInfo(0, 0, ti));
  if ti = nil then Exit;
  ti._AddRef;
  try
    ChkErr(L, ti.GetTypeAttr(ta));
    try
      for i := 0 to ta^.cFuncs - 1 do
      begin
        ChkErr(L, ti.GetFuncDesc(i, fd));
        try
          ChkErr(L, ti.GetDocumentation(fd^.memid, @ws, nil, nil, nil));
          s := UTF8Encode(ws); ws:= '';
          lua_pushstring(L, PChar(s));
          lua_newtable(L);
          lua_pushstring(L, FIELD_FN);
          lua_pushstring(L, PChar(s));
          lua_settable(L, -3);
          lua_newtable(L);
          lua_pushstring(L, '__call');
          lua_pushcfunction(L, @call);
          lua_settable(L, -3);
          lua_pushstring(L, '__index');
          lua_pushvalue(L, t); // SuperClass
          lua_settable(L, -3);
          lua_setmetatable(L, -2);
          lua_settable(L, -3);
        finally
          ti.ReleaseFuncDesc(fd);
        end;
      end;
    finally
      ti.ReleaseTypeAttr(ta);
    end;
  finally
    ti._Release;
    ti:= Unassigned;
  end;

  lua_newtable(L);
  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, @NewIndex);
  lua_settable(L, -3);
  lua_pushstring(L, '__index');
  lua_pushcfunction(L, @Index);
  lua_settable(L, -3);
  lua_pushstring(L, '__call');
  lua_pushcfunction(L, @iterator);
  lua_settable(L, -3);
  lua_setmetatable(L, -2);
end;

function CreateActiveXObject(L : Plua_State) : Integer; cdecl;
begin
  DoCreateActiveXObject(L, CreateOleObject(lua_tostring(L, 1)));
  Result := 1;
end;

end.

