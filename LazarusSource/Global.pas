unit Global;

{
Copyright (C) 2018-2021 Gerald Holdsworth gerald@hollypops.co.uk

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public Licence as published by the Free
Software Foundation; either version 3 of the Licence, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence for more
details.

A copy of the GNU General Public Licence is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
 Classes,SysUtils,Registry;

function ReadLine(var Stream: TFileStream;var Line: string): boolean;
function WriteLine(var Stream: TFileStream;Line: string): boolean;
procedure OpenReg(key: String);
function DeleteKey(key: String): Boolean;
function GetRegValS(V: String;D: String): String;
procedure GetRegValA(V: String;var D: array of Byte);
function GetRegValI(V: String;D: Cardinal): Cardinal;
function GetRegValB(V: String;D: Boolean): Boolean;
procedure SetRegValS(V: String;D: String);
procedure SetRegValA(V: String;var D: array of Byte);
procedure SetRegValI(V: String;D: Cardinal);
procedure SetRegValB(V: String;D: Boolean);
function ExtractKey(var V: String):String;
function DoesKeyExist(V: String):Boolean;
var
 CCReg              : TRegistry;
const
 //Registry Key to use
 RegKey = '\Software\GJH Software\Camera Control';

implementation

{-------------------------------------------------------------------------------
Reads a line from a TStream
-------------------------------------------------------------------------------}
function ReadLine(var Stream: TFileStream;var Line: string): boolean;
var
 RawLine: UTF8String;
 ch     : AnsiChar;
begin
 RawLine:='';
 Result:=False;
 ch:=#0;
 while (Stream.Read(ch,1)=1) and (ch<>#13) and (ch<>#10) do
 begin
  Result:=True;
  RawLine:=RawLine+UTF8String(ch);
 end;
 Line:=String(RawLine);
 if ch=#13 then
 begin
  Result:=True;
  if (Stream.Read(ch,1)=1) and (ch<>#10) then
   Stream.Seek(-1,soCurrent) // unread it if not LF character.
 end;
 if ch=#10 then
 begin
  Result:=True;
  if (Stream.Read(ch,1)=1) and (ch<>#13) then
   Stream.Seek(-1,soCurrent) // unread it if not CR character.
 end;
end;

{-------------------------------------------------------------------------------
Writes a string to the TFileStream, and terminates it with 0x0A
-------------------------------------------------------------------------------}
function WriteLine(var Stream: TFileStream;Line: string): boolean;
var
 l,x: Integer;
 S: UTF8String;
begin
 S:=UTF8String(Line+#10);
 l:=Length(S);
 x:=Stream.Write(S[1],l);
 Result:=x=l;
end;

{-------------------------------------------------------------------------------
Open the registry key
-------------------------------------------------------------------------------}
procedure OpenReg(key: String);
begin
 CCReg:=TRegistry.Create;
 if key<>'' then key:='\'+key;
 CCReg.OpenKey(RegKey+key,true);
end;

{-------------------------------------------------------------------------------
Function to delete a key from the registry
-------------------------------------------------------------------------------}
function DeleteKey(key: String): Boolean;
var
 x: Boolean;
begin
 x:=True;
 OpenReg(ExtractKey(key));
 if CCReg.ValueExists(key) then x:=CCReg.DeleteValue(key);
 CCReg.Free;
 Result:=x;
end;

{-------------------------------------------------------------------------------
Function to read a string from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValS(V: String;D: String): String;
var
 X: String;
begin
 OpenReg(ExtractKey(V));
 If CCReg.ValueExists(V)then X:=CCReg.ReadString(V)
 else begin X:=D;CCReg.WriteString(V,X);end;
 CCReg.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read an array from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
procedure GetRegValA(V: String;var D: array of Byte);
var
 s: Integer;
begin
 OpenReg(ExtractKey(V));
 If CCReg.ValueExists(V)then
 begin
  s:=CCReg.GetDataSize(V);
  CCReg.ReadBinaryData(V,D,s);
 end
 else
 begin
  CCReg.WriteBinaryData(V,D,SizeOf(D));
 end;
 CCReg.Free;
end;

{-------------------------------------------------------------------------------
Function to read an integer from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValI(V: String;D: Cardinal): Cardinal;
var
 X: Cardinal;
begin
 OpenReg(ExtractKey(V));
 If CCReg.ValueExists(V)then X:=CCReg.ReadInteger(V)
 else begin X:=D;CCReg.WriteInteger(V,X);end;
 CCReg.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read a boolean from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValB(V: String;D: Boolean): Boolean;
var
 X: Boolean;
begin
 OpenReg(ExtractKey(V));
 If CCReg.ValueExists(V)then X:=CCReg.ReadBool(V)
 else begin X:=D;CCReg.WriteBool(V,X);end;
 CCReg.Free;
 Result:=X;
end;

function DoesKeyExist(V: String):Boolean;
begin
 OpenReg(ExtractKey(V));
 Result:=CCReg.ValueExists(V);
 CCReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save a string to the registry
-------------------------------------------------------------------------------}
procedure SetRegValS(V: String;D: String);
begin
 OpenReg(ExtractKey(V));
 CCReg.WriteString(V,D);
 CCReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save an array to the registry
-------------------------------------------------------------------------------}
procedure SetRegValA(V: String;var D: array of Byte);
begin
 OpenReg(ExtractKey(V));
 CCReg.WriteBinaryData(V,D,SizeOf(D));
 CCReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save an integer to the registry
-------------------------------------------------------------------------------}
procedure SetRegValI(V: String;D: Cardinal);
begin
 OpenReg(ExtractKey(V));
 CCReg.WriteInteger(V,D);
 CCReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save a boolean to the registry
-------------------------------------------------------------------------------}
procedure SetRegValB(V: String;D: Boolean);
begin
 OpenReg(ExtractKey(V));
 CCReg.WriteBool(V,D);
 CCReg.Free;
end;

{-------------------------------------------------------------------------------
Function to extract key part of string
-------------------------------------------------------------------------------}
function ExtractKey(var V: String):String;
begin
 Result:='';
 if Pos('\',V)>0 then
 begin
  Result:=Copy(V,1,Pos('\',V)-1);
  V:=Copy(V,Pos('\',V)+1);
 end;
end;

end.
