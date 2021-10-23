unit MainUnit;

{
Copyright (C) 2021 Gerald Holdsworth gerald@hollypops.co.uk

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
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
 Menus, ComCtrls, LCLType,
 {$IFNDEF Darwin}Serial{$ENDIF}{$IFDEF Darwin}SerialMac{$ENDIF};

type

 { TMainForm }

 TMainForm = class(TForm)
  Cam3off: TImage;
  Cam4off: TImage;
  Cam5off: TImage;
  Cam6off: TImage;
  Cam7off: TImage;
  Cam3on: TImage;
  Cam4on: TImage;
  Cam5on: TImage;
  Cam6on: TImage;
  Cam7on: TImage;
  Cam2: TImage;
  Cam3: TImage;
  Cam4: TImage;
  Cam5: TImage;
  Cam6: TImage;
  Cam7: TImage;
  Cam1on: TImage;
  Cam2on: TImage;
  Cam1off: TImage;
  Cam2off: TImage;
  CloseWindow: TImage;
  Cam1: TImage;
  CamStatusBackground: TImage;
  CamStatus: TLabel;
  About: TImage;
  ZoomInOn: TImage;
  ZoomOutOn: TImage;
  UpOn: TImage;
  DownOn: TImage;
  LeftOn: TImage;
  RightOn: TImage;
  SelectOn: TImage;
  TempStore: TImage;
  LeftButton: TImage;
  UpButton: TImage;
  RightButton: TImage;
  DownButton: TImage;
  SelectButton: TImage;
  ZoomInButton: TImage;
  ZoomOutButton: TImage;
  procedure AboutClick(Sender: TObject);
  procedure CamMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure CamMouseUp(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure CloseWindowClick(Sender: TObject);
  procedure FindCamera;
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure ButtonMouseUp(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure MoveCamera(moveleft,moveright,moveup,movedown: Boolean);
  procedure ChangeImage(Sender: TObject; NewImage: TImage);
  procedure SelectButtonMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure SelectButtonMouseUp(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure StopCamera;
  procedure ZoomCamera(tele,wide: Boolean);
  procedure StopZoom;
  procedure FormShow(Sender: TObject);
  procedure ZoomOutButtonMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure ZoomButtonMouseUp(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure ZoomInButtonMouseDown(Sender: TObject;
   Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ChangeCamera(cam: Integer);
  procedure AllCamsOff;
  function OverActiveArea(Sender:TObject;X,Y:Integer): Boolean;
  function ControlUnderMouse(var X,Y: Integer):TControl;
 private
  commport    : TSerialHandle;
  tiltspeed,
  panspeed,
  zoomspeed,
  camnumber   : Byte;
  serspeed    : LongInt;
  Parity      : TParityType;
  bits,
  StopBits    : Integer;
  sernum      : String;
  mousePos    : TPoint;
  mouseIsDown,
  keypressed  : Boolean;
  pressedctrl : TImage;
  const
   AppName = 'Camera Control';
   AppVersion = '1.00';
 public

 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
 //Setup camera
 commport :=0;
 tiltspeed:=1;
 panspeed :=1;
 zoomspeed:=15;
 camnumber:=1;
 //Set up serial settings
 serspeed :=9600;
 bits     :=8;
 Parity   :=NoneParity;
 StopBits :=1;
 //Other
 keypressed :=False;
 mouseIsDown:=False;
 pressedctrl:=nil;
end;

procedure TMainForm.FindCamera;
var
 i,camport,
 status      : Integer;
 thiscommport: TSerialHandle;
 Flags       : TSerialFlags;
 starttime,
 nowtime     : TDateTime;
 s           : String;
 buffer      : array of Byte;
const
{$IFDEF Windows}
 commname='COM';
{$ENDIF}
{$IFDEF Darwin}
 commname='/dev/ttyS';
{$ENDIF}
{$IFDEF Linux}
 commname='/dev/ttyS';
{$ENDIF}
begin
 camport:=-1;
 i:=0;
 Application.ProcessMessages;
 while(i<10)and(camport=-1)do
 begin
  s:=commname+IntToStr(i);
  thiscommport:=SerOpen(s);
  CamStatus.Caption:='Detecting camera '+IntToStr(camnumber)+' on '+s;
  Application.ProcessMessages;
  if thiscommport<>0 then
  begin
   Flags:=[];
   SerSetParams(thiscommport,serspeed,bits,Parity,StopBits,Flags);
   SetLength(buffer,5);
   buffer[0]:=$80 OR camnumber;
   buffer[1]:=$09;
   buffer[2]:=$04;
   buffer[3]:=$24;
   buffer[4]:=$FF;
   if SerWrite(thiscommport,buffer[0],Length(buffer))>0 then
   begin
    SetLength(buffer,15);
    starttime:=Round(Time*100000);
    nowtime:=starttime;
    status:=0;
    while(status<Length(buffer))and(nowtime-starttime<2)do//2 secs timeout
    begin
     nowtime:=Round(Time*100000);
     status:=status+SerRead(thiscommport,buffer[status],Length(buffer)-status);
    end;
    if (buffer[0]=$90)
    and(buffer[1]=$50)then
    begin
     sernum:='';
     for status:=0 to 11 do sernum:=sernum+chr(buffer[2+status]);
     camport:=i;
    end;
    SerClose(thiscommport);
   end;
  end;
  inc(i);
 end;
 if camport>=0 then
 begin
  commport:=SerOpen(commname+IntToStr(camport));
  Flags:=[];
  SerSetParams(commport,serspeed,bits,Parity,StopBits,Flags);
  CamStatus.Caption:='Connected to camera on '+s+' s/n: '+sernum;
 end
 else CamStatus.Caption:='No camera found';
end;

procedure TMainForm.CloseWindowClick(Sender: TObject);
begin
 MainForm.Close;
end;

procedure TMainForm.ChangeCamera(cam: Integer);
begin
 if commport>0 then SerClose(commport);
 commport:=0;
 camnumber:=cam;
 FindCamera;
 if commport=0 then AllCamsOff;
end;

procedure TMainForm.AllCamsOff;
begin
 Cam1.Picture.Assign(Cam1off.Picture);
 Cam2.Picture.Assign(Cam2off.Picture);
 Cam3.Picture.Assign(Cam3off.Picture);
 Cam4.Picture.Assign(Cam4off.Picture);
 Cam5.Picture.Assign(Cam5off.Picture);
 Cam6.Picture.Assign(Cam6off.Picture);
 Cam7.Picture.Assign(Cam7off.Picture);
end;

procedure TMainForm.CamMouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
var
 onImage: TImage;
begin
 AllCamsOff;
 onImage:=nil;
 if TControl(Sender).Name='Cam1' then onImage:=Cam1on;
 if TControl(Sender).Name='Cam2' then onImage:=Cam2on;
 if TControl(Sender).Name='Cam3' then onImage:=Cam3on;
 if TControl(Sender).Name='Cam4' then onImage:=Cam4on;
 if TControl(Sender).Name='Cam5' then onImage:=Cam5on;
 if TControl(Sender).Name='Cam6' then onImage:=Cam6on;
 if TControl(Sender).Name='Cam7' then onImage:=Cam7on;
 if onImage<>nil then
  TImage(Sender).Picture.Assign(onImage.Picture);
end;

procedure TMainForm.AboutClick(Sender: TObject);
begin
 ShowMessage(AppName+' V'+AppVersion+#13#10+'Written by Gerald J Holdsworth');
end;

procedure TMainForm.CamMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 ChangeCamera(StrToInt(RightStr(TControl(Sender).Name,1)));
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 if commport>0 then SerClose(commport);
end;

procedure TMainForm.FormKeyDown(Sender: TObject;var Key:Word;Shift:TShiftState);
begin
 if not keypressed then
 begin
 if Key=VK_LEFT  then Sender:=LeftButton as TObject;
 if Key=VK_RIGHT then Sender:=RightButton as TObject;
 if Key=VK_UP    then Sender:=UpButton as TObject;
 if Key=VK_DOWN  then Sender:=DownButton as TObject;
 if Key in [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN]then
  ButtonMouseDown(Sender,mbLeft,Shift,-1,-1);
 end;
 keypressed:=True;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;Shift:TShiftState);
begin
 if Key=VK_LEFT  then Sender:=LeftButton as TObject;
 if Key=VK_RIGHT then Sender:=RightButton as TObject;
 if Key=VK_UP    then Sender:=UpButton as TObject;
 if Key=VK_DOWN  then Sender:=DownButton as TObject;
 if Key in [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN]then
  ButtonMouseUp(Sender,mbLeft,Shift,-1,-1);
 keypressed:=False;
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 mousePos.X:=Mouse.CursorPos.X;
 mousePos.Y:=Mouse.CursorPos.Y;
 if Button=mbLeft then mouseIsDown:=True;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
 Y: Integer);
begin
 if mouseIsDown then
 begin
  Top:=Top-(mousePos.Y-Mouse.CursorPos.Y);
  Left:=Left-(mousePos.X-Mouse.CursorPos.X);
  mousePos.X:=Mouse.CursorPos.X;
  mousePos.Y:=Mouse.CursorPos.Y;
 end;
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 mouseIsDown:=False;
end;

procedure TMainForm.MoveCamera(moveleft,moveright,moveup,movedown: Boolean);
var
 buffer: array of Byte;
 lr,ud,
 ts,ps : Byte;
begin
 lr:=0;
 ud:=0;
 if moveleft  then lr:=lr OR 1;
 if moveright then lr:=lr OR 2;
 if moveup    then ud:=ud OR 1;
 if movedown  then ud:=ud OR 2;
 if lr=0 then lr:=3;
 if ud=0 then ud:=3;
 ts:=tiltspeed;
 ps:=panspeed;
 if(lr=3)AND(ud=3)then //Used to stop the camera
 begin
  ts:=3;
  ps:=3;
 end;
 if commport>0 then
 begin
  SetLength(buffer,9);
  buffer[0]:=$80 OR camnumber;
  buffer[1]:=$01;
  buffer[2]:=$06;
  buffer[3]:=$01;
  buffer[4]:=$00 OR ps;
  buffer[5]:=$00 OR ts;
  buffer[6]:=lr;
  buffer[7]:=ud;
  buffer[8]:=$FF;
  SerWrite(commport,buffer[0],Length(buffer));
 end;
end;

procedure TMainForm.StopCamera;
begin 
 MoveCamera(True,True,True,True);
end;

procedure TMainForm.ZoomCamera(tele,wide: Boolean);
var
 buffer: array of Byte;
 zs,
 dir   : Byte;
begin
 dir:=0;
 if tele then dir:=2;
 if wide then dir:=3;         
 if(tele)AND(wide)then dir:=0;
 zs:=zoomspeed;
 if dir=0 then zs:=0;
 if commport>0 then
 begin
  SetLength(buffer,6);
  buffer[0]:=$80 OR camnumber;
  buffer[1]:=$01;
  buffer[2]:=$04;
  buffer[3]:=$07;
  buffer[4]:=(dir<<4)OR zs;
  buffer[5]:=$FF;
  SerWrite(commport,buffer[0],Length(buffer));
 end;
end;

function TMainForm.OverActiveArea(Sender:TObject;X,Y:Integer): Boolean;
var
 c       : TColor;
 newimage: TImage;
begin
 if(X>=0)and(Y>=0)then
 begin
  c:=$FF00FF;
  newImage:=TImage.Create(MainForm);
  newImage.Parent:=MainForm;
  newImage.Height:=TImage(Sender).Height;
  newImage.Width:=TImage(Sender).Width;
  newImage.Canvas.Brush.Color:=c;
  newImage.Canvas.Pen.Color:=c;
  newimage.Canvas.FillRect(0,0,newImage.Width,newImage.Height);
  newImage.Canvas.Draw(0,0,TImage(Sender).Picture.Graphic);
  Result:=newImage.Picture.Bitmap.Canvas.Pixels[X,Y]<>c;
  newImage.Free;
 end
 else
  Result:=True;
end;

procedure TMainForm.StopZoom;
begin
 ZoomCamera(True,True);
end;

procedure TMainForm.ChangeImage(Sender: TObject; NewImage: TImage);
begin
 pressedctrl:=TImage(Sender);
 TempStore.Picture.Assign(pressedctrl.Picture);
 pressedctrl.Picture.Assign(NewImage.Picture);
end;

procedure TMainForm.ButtonMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 if pressedctrl<>nil then pressedctrl.Picture.Assign(TempStore.Picture);
 pressedctrl:=nil;
 StopCamera;
 FormMouseUp(Sender,Button,Shift,X,Y);
end;

procedure TMainForm.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
var
 ctrl: TControl;
begin
 if OverActiveArea(Sender,X,Y) then
 begin
  if TControl(Sender).Name='LeftButton' then
  begin
   ChangeImage(Sender,LeftOn);
   MoveCamera(True,False,False,False);
  end;
  if TControl(Sender).Name='UpButton' then
  begin
   ChangeImage(Sender,UpOn);
   MoveCamera(False,False,True,False);
  end;
  if TControl(Sender).Name='DownButton' then
  begin
   ChangeImage(Sender,DownOn);
   MoveCamera(False,False,False,True);
  end;
  if TControl(Sender).Name='RightButton' then
  begin
   ChangeImage(Sender,RightOn);
   MoveCamera(False,True,False,False);
  end;
 end
 else
 begin
  if(X=-1)and(Y=-1)then ctrl:=TControl(Sender) else ctrl:=nil;
  if(X>=0)and(Y>=0)then
  begin
   ctrl:=ControlUnderMouse(X,Y);
   if ctrl=TControl(Sender) then ctrl:=nil;
  end;
  if ctrl<>nil then
   ButtonMouseDown(ctrl as TObject,Button,Shift,X,Y)
  else
  begin
   ChangeImage(Sender,TImage(Sender));
   FormMouseDown(Sender,Button,Shift,X,Y);
  end;
 end;
end;

procedure TMainForm.SelectButtonMouseDown(Sender: TObject;
 Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if OverActiveArea(Sender,X,Y) then
  ChangeImage(Sender,SelectOn)
 else
  ChangeImage(Sender,SelectButton);
end;

procedure TMainForm.SelectButtonMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 TImage(Sender).Picture.Assign(TempStore.Picture);
 if commport>0 then SerClose(commport);
 commport:=0;
 AllCamsOff;
 CamStatus.Caption:='No camera selected';
end;

procedure TMainForm.ZoomButtonMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 TImage(Sender).Picture.Assign(TempStore.Picture);
 StopZoom;
end;

procedure TMainForm.ZoomInButtonMouseDown(Sender: TObject;
 Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 ChangeImage(Sender,ZoomInOn);
 ZoomCamera(True,False);
end;

procedure TMainForm.ZoomOutButtonMouseDown(Sender: TObject;
 Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 ChangeImage(Sender,ZoomOutOn);
 ZoomCamera(False,True);
end;

function TMainForm.ControlUnderMouse(var X,Y: Integer):TControl;
var
 ctrl  : TControl;
 pt    : TPoint;
 cX,cY,
 index : Integer;
begin
 Result:=nil;
 pt:=ScreenToClient(Mouse.CursorPos);
 ctrl:=ControlAtPos(pt,[]);
 if Assigned(ctrl) then
 begin
  cX:=pt.X-ctrl.Left;
  cY:=pt.Y-ctrl.Top;
  if OverActiveArea(ctrl as TObject,cX,cY) then
   Result:=ctrl
  else
   for index:=0 to ControlCount-1 do
   begin
    if (Controls[index].Left<pt.X)
    and(Controls[index].Left+Controls[index].Width>pt.X)
    and(Controls[index].Top<pt.Y)
    and(Controls[index].Top+Controls[index].Height>pt.Y)
    and(Controls[index]<>ctrl)
    and(Controls[index] is TImage)then Result:=Controls[index];
   end;
 end;
 if Result<>nil then
 begin
  X:=pt.X-Result.Left;
  Y:=pt.Y-Result.Top;
 end;
end;

end.
