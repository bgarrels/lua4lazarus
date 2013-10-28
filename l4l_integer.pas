{
  Lua4Lazalus

    IntegerObject

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

    Version History:
      1.52.0 by Malcome Japan. (with Lazarus 1.3 and FPC 2.6.2)
      1.0.0 by Malcome Japan. (with Lazarus 0.9.29 and FPC 2.4.1)

    ToDo:
      Add, Sub, etc
}
unit l4l_integer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua52, l4l_object;

type

  { TLuaInteger }

  TLuaInteger = class(TLuaObject)
  private
    FValue: integer;
  protected
  public
    property Value: integer read FValue write FValue;
    constructor Create(L : Plua_State); override;
    destructor Destroy; override;
  published
  end;

  function CreateInteger(L : Plua_State) : Integer; cdecl;

implementation

{ TLuaInteger }

constructor TLuaInteger.Create(L: Plua_State);
begin
  inherited Create(L);
  FValue:= 0;
end;

destructor TLuaInteger.Destroy;
begin
  inherited Destroy;
end;

function CreateInteger(L : Plua_State) : Integer; cdecl;
var
  obj: TLuaInteger;
begin
  obj:= TLuaInteger.Create(L);
  if lua_gettop(L) > 0 then obj.Value:= lua_tointeger(L, 1);
  l4l_PushLuaObject(obj);
  Result := 1;
end;

end.

