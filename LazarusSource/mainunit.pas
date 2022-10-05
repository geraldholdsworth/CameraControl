unit MainUnit;

{
Copyright (C) 2021-22 Gerald Holdsworth gerald@hollypops.co.uk

Ararat Synapse is (c)2001-2011, Lukas Gebauer

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
 Menus, ComCtrls, LCLType, GJHRegistryClass,CC_SynaSer;

type

 TCameras = record
   OnGraphic,
   OffGraphic,
   Control      : TImage;
   SerialNumber : String;
   CamMoving    : Boolean;
 end;

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
  CamSettingsOn: TImage;
  CloseWindow: TImage;
  Cam1: TImage;
  CamStatusBackground: TImage;
  CamStatus: TLabel;
  CamSettingsBtn: TImage;
  SavePreset1: TImage;
  Preset1: TImage;
  Preset2: TImage;
  Preset3: TImage;
  Preset4: TImage;
  Preset5: TImage;
  Preset6: TImage;
  Preset7: TImage;
  Preset8: TImage;
  Preset9: TImage;
  MenuAbout: TMenuItem;
  MenuShowHide: TMenuItem;
  MenuExit: TMenuItem;
  PopupMenu1: TPopupMenu;
  SavePreset2: TImage;
  SavePreset3: TImage;
  SavePreset4: TImage;
  SavePreset5: TImage;
  SavePreset6: TImage;
  SavePreset7: TImage;
  SavePreset8: TImage;
  SavePreset9: TImage;
  TrayIcon1: TTrayIcon;
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
  procedure FormShow(Sender: TObject);
  function GetSerialNumber: String;
  procedure SelectCamera(index: Integer);
  procedure MenuExitClick(Sender: TObject);
  procedure MenuSettingsClick(Sender: TObject);
  procedure ShowCamSettings(ctrl: TObject=nil);
  procedure MenuShowHideClick(Sender: TObject);
  procedure MoveCamera(moveleft,moveright,moveup,movedown: Boolean);
  procedure ChangeImage(Sender: TObject; NewImage: TImage);
  procedure PresetClick(Sender: TObject);                  
  procedure SavePresetClick(Sender: TObject);
  procedure SelectButtonMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure SelectButtonMouseUp(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure StopCamera;
  procedure ZoomCamera(tele,wide: Boolean);
  procedure StopZoom;
  procedure FormCreate(Sender: TObject);
  procedure ZoomOutButtonMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure ZoomButtonMouseUp(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
  procedure ZoomInButtonMouseDown(Sender: TObject;
   Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure AllCamsOff;
  function OverActiveArea(Sender:TObject;X,Y:Integer): Boolean;
  function ControlUnderMouse(var X,Y: Integer):TControl;
  function SendSerialCommand(buffer: String): String;
  function ValidReturnMessage(buffer: String): Boolean;
  procedure SerClose;
  function ValidateCamNumber(cam: Integer=0;sernum: Boolean=True): Boolean;
 private
  Connection  : TBlockSerial;
  ConnectName : String;
  camnumber   : Byte;
  serspeed    : LongInt;
  Parity      : Char;
  bits,
  StopBits    : Integer;
  mousePos    : TPoint;
  mouseIsDown,
  keypressed  : Boolean;
  pressedctrl : TImage;
  Cameras     : array[1..7] of TCameras;
  CCReg       : TGJHRegistry;
  const
   AppVersion = '1.02';
 public
  tiltspeed,
  panspeed,
  zoomspeed   : Byte;
 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

uses AboutUnit, SettingsUnit;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
 i: Integer;
begin
 Connection:=nil;
 //Setup camera
 tiltspeed:=1;
 panspeed :=1;
 zoomspeed:=15;
 camnumber:=1;
 //Set up serial settings
 serspeed :=9600;
 bits     :=8;
 Parity   :='N';//NoneParity;
 StopBits :=SB1;
 //Other
 keypressed :=False;
 mouseIsDown:=False;
 pressedctrl:=nil;
 //Camera Buttons
 Cameras[1].Control   :=Cam1;
 Cameras[1].OnGraphic :=Cam1on;
 Cameras[1].OffGraphic:=Cam1off;
 Cameras[2].Control   :=Cam2;
 Cameras[2].OnGraphic :=Cam2on;
 Cameras[2].OffGraphic:=Cam2off;
 Cameras[3].Control   :=Cam3;
 Cameras[3].OnGraphic :=Cam3on;
 Cameras[3].OffGraphic:=Cam3off;
 Cameras[4].Control   :=Cam4;
 Cameras[4].OnGraphic :=Cam4on;
 Cameras[4].OffGraphic:=Cam4off;
 Cameras[5].Control   :=Cam5;
 Cameras[5].OnGraphic :=Cam5on;
 Cameras[5].OffGraphic:=Cam5off;
 Cameras[6].Control   :=Cam6;
 Cameras[6].OnGraphic :=Cam6on;
 Cameras[6].OffGraphic:=Cam6off;
 Cameras[7].Control   :=Cam7;
 Cameras[7].OnGraphic :=Cam7on;
 Cameras[7].OffGraphic:=Cam7off;
 for i:=1 to 7 do
 begin
  Cameras[i].CamMoving:=False;
  Cameras[i].SerialNumber:='';
 end;
 CCReg:=TGJHRegistry.Create('\Software\GJH Software\Camera Control');
end;

procedure TMainForm.CloseWindowClick(Sender: TObject);
begin
 MainForm.Hide;
end;

procedure TMainForm.AllCamsOff;
var
 index: Integer;
begin
 for index:=1 to 7 do
  Cameras[index].Control.Picture.Assign(Cameras[index].OffGraphic.Picture);
end;

procedure TMainForm.CamMouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
var
 index: Integer;
begin
 AllCamsOff;
 index:=StrToInt(RightStr(TImage(Sender).Name,1));
 if(index>0)and(index<8)then
  TImage(Sender).Picture.Assign(Cameras[index].onGraphic.Picture);
end;

procedure TMainForm.AboutClick(Sender: TObject);
begin
 AboutForm.lb_Title.Caption:=Application.Title;
 AboutForm.lb_Version.Caption:='Version '+AppVersion;
 AboutForm.ShowModal;
end;

procedure TMainForm.CamMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 SelectCamera(StrToInt(RightStr(TControl(Sender).Name,1)));
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
 i: Integer;
begin
 for i:=1 to 7 do Cameras[i].SerialNumber:='';
 SerClose;
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
 lr,ud,
 ts,ps : Byte;
begin
 if not ValidateCamNumber then exit;
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
  Cameras[camnumber].CamMoving:=False;
 end;
 if(Cameras[camnumber].SerialNumber<>'')and(not Cameras[camnumber].CamMoving)then
  SendSerialCommand(#$01#$06#$01+chr(ps)+chr(ts)+chr(lr)+chr(ud));
 Cameras[camnumber].CamMoving:=(ts<>3)or(ps<>3);
end;

procedure TMainForm.StopCamera;
begin 
 MoveCamera(True,True,True,True);
end;

procedure TMainForm.ZoomCamera(tele,wide: Boolean);
var
 zs,
 dir   : Byte;
begin
 if not ValidateCamNumber then exit;
 dir:=0;
 if tele then dir:=2;
 if wide then dir:=3;         
 if(tele)AND(wide)then dir:=0;
 zs:=zoomspeed;
 if dir=0 then //Stop zoom
 begin
  zs:=0;
  Cameras[camnumber].CamMoving:=False;
 end;
 if(Cameras[camnumber].SerialNumber<>'')and(not Cameras[camnumber].CamMoving)then
  SendSerialCommand(#$01#$04#$07+chr((dir<<4)OR zs));
 Cameras[camnumber].CamMoving:=dir<>0;
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

procedure TMainForm.PresetClick(Sender: TObject);
var
 camsettings: array[0..16] of Byte;
 buffer     : String;
 index      : Integer;
begin
 if ValidateCamNumber then
  if CCReg.DoesKeyExist('Preset'+RightStr(TImage(Sender).Name,1))then
  begin
   CCReg.GetRegValA('Preset'+RightStr(TImage(Sender).Name,1),camsettings);
   //Pan/Tilt Pos
   buffer:=#$01#$06#$02+chr(panspeed)+chr(tiltspeed);
   for index:=0 to 7 do buffer:=buffer+chr(camsettings[index+0]AND$F);
   SendSerialCommand(buffer);
   //Zoom Pos
   buffer:=#$01#$04#$47;
   for index:=0 to 3 do
    buffer:=buffer+chr(camsettings[index+8]AND$F);
   SendSerialCommand(buffer);
   //Set focus to manual first
   SendSerialCommand(#$01#$04#$38#$03);
   //Focus Pos
   buffer:=#$01#$04#$48;
   for index:=0 to 3 do
    buffer:=buffer+chr(camsettings[index+12]AND$F);
   SendSerialCommand(buffer);
   //Set focus to stored state
   SendSerialCommand(#$01#$04#$38+chr(camsettings[16]AND$F));
  end else ShowMessage('No camera selected');
end; 

procedure TMainForm.SavePresetClick(Sender: TObject);
var
 camsettings: array[0..16] of Byte;
 buffer     : String;
 index      : Integer;
begin
 if ValidateCamNumber then
 begin
  for index:=0 to 15 do camsettings[index]:=0;
  //Zoom Pos
  buffer:=SendSerialCommand(#$09#$04#$47);
  if ValidReturnMessage(buffer) then
   for index:=0 to 3 do camsettings[index+8]:=Ord(buffer[index+3]);
  //Focus Pos
  buffer:=SendSerialCommand(#$09#$04#$48);
  if ValidReturnMessage(buffer) then
   for index:=0 to 3 do camsettings[index+12]:=Ord(buffer[index+3]);
  //Get focus state
  buffer:=SendSerialCommand(#$09#$04#$38);
  if ValidReturnMessage(buffer) then
   camsettings[16]:=Ord(buffer[3]);
  //Pan/Tilt Pos
  buffer:=SendSerialCommand(#$09#$06#$12);
  if ValidReturnMessage(buffer) then
   for index:=0 to 7 do camsettings[index]:=Ord(buffer[index+3]);
  //Save to registry
  CCReg.SetRegValA('Preset'+RightStr(TImage(Sender).Name,1),camsettings);
 end else ShowMessage('No camera selected');
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
 if not ValidateCamNumber then
  ShowMessage('No camera selected')
 else
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

procedure TMainForm.FormShow(Sender: TObject);
var
 commname : String;
 commnames: TStringArray;
 i,c      : Integer;
begin
 //We'll check to see if we have a serial connection, if not we'll find one
 if Connection=nil then //If this is nil, then there is no connection
 begin 
  //Get device names
  commname:=GetSerialPortNames;
  if commname<>'' then//No ports then quit
  begin
   commnames:=commname.Split(','); //Split into an array
   //Go through each serial port to find camera 1
   i:=0;
   camnumber:=1;
   while i<Length(commnames) do
   begin
    if commnames[i]<>'' then
    begin
     CamStatus.Caption:='Looking for cameras on '+commnames[i];
     Application.ProcessMessages;
     //Open each port in turn
     Connection:=TBlockSerial.Create;
     Connection.LinuxLock:=False;
     {$IFDEF UNIX}
     Connection.NonBlock:=True;
     {$ENDIF}
     Connection.Connect(commnames[i]);
     Connection.Config(serspeed,bits,Parity,stopbits,false,false);
     //See if there is a serial number being returned
     Cameras[camnumber].SerialNumber:=GetSerialNumber;
     if Cameras[camnumber].SerialNumber<>'' then //Yes, look for other cameras
     begin
      ConnectName:=commnames[i];
      for c:=2 to Length(Cameras)-1 do
      begin
       camnumber:=c;
       CamStatus.Caption:='Looking for camera '+IntToStr(camnumber)+' on '+ConnectName;
       Application.ProcessMessages;
       Cameras[camnumber].SerialNumber:=GetSerialNumber;
      end;
      i:=Length(commnames);//Exit the loop
     end
     else
     begin
      Connection.Free; //Nothing here, try the next one
      Connection:=nil; //If it was the last one, this needs to be nil
     end;
    end;
    inc(i);
   end;
  end;
 end;
 //Show or hide buttons depending on whether we have any cameras
 for c:=Low(Cameras) to High(Cameras) do
  Cameras[c].Control.Visible:=Cameras[c].SerialNumber<>'';
 //Now if we have a connection
 if Connection<>nil then SelectCamera(1)//Select camera 1, by default
 else CamStatus.Caption:='No camera selected';
end;

function TMainForm.GetSerialNumber: String;
var
 buffer : String;
 j      : Integer;
begin
 Result:='';
 //Now we send a basic command, asking for the serial number
 buffer:=SendSerialCommand(#$09#$04#$24);
 //If we got a valid message back, extract the serial number
 if ValidReturnMessage(buffer) then
  for j:=0 to 11 do
  begin
   if Ord(buffer[j+3])>31 then
    Result:=Result+chr(Ord(buffer[j+3])AND$7F);
  end
 else
 begin
  //Didn't get a valid message back, so we'll try another command
  buffer:=SendSerialCommand(#$09#$04#$22);
  //And check again
  if ValidReturnMessage(buffer) then
   Result:=IntToHex((Ord(buffer[3])AND$F)<<12
                  OR(Ord(buffer[4])AND$F)<<8
                  OR(Ord(buffer[5])AND$F)<<4
                  OR(Ord(buffer[6])AND$F),4);
 end;
end;

procedure TMainForm.SelectCamera(index: Integer);
begin
 if ValidateCamNumber(index) then
 begin
  Cameras[index].Control.Picture.Assign(Cameras[index].onGraphic.Picture);
  CamStatus.Caption:='Serial Number: '+Cameras[index].SerialNumber
                    +' Connected to: '+ConnectName;
  camnumber:=index;
 end;
end;

procedure TMainForm.MenuExitClick(Sender: TObject);
begin
 MainForm.Close;
end;

procedure TMainForm.MenuSettingsClick(Sender: TObject);
begin
 if ValidateCamNumber then
 begin
  ShowCamSettings;
  //Show the form
  ChangeImage(Sender,CamSettingsOn);
  SettingsForm.ShowModal;
  CamSettingsBtn.Picture.Assign(TempStore.Picture);
 end;
end;

procedure TMainForm.ShowCamSettings(ctrl: TObject=nil);
var
 buffer: String;
begin
 if not ValidateCamNumber then exit;
 //Disable updates
 SettingsForm.ignorechange:=True;
 //Cemera Serial Number
 SettingsForm.lbCamDetails.Caption:='Camera serial number: '
                                   +Cameras[camnumber].SerialNumber;
 //Camera Power Status
 if(TRadioButton(ctrl)=SettingsForm.CamPowerOn)
 or(TRadioButton(ctrl)=SettingsForm.CamPowerOff)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.CamPowerOn.Checked      :=False;
   SettingsForm.CamPowerOff.Checked     :=False;
   SettingsForm.CamPowerLabel.Font.Color:=$000000;
   SettingsForm.CamPowerPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#$09#$04#$00);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
     2: SettingsForm.CamPowerOn.Checked        :=True;
     3: SettingsForm.CamPowerOff.Checked       :=True;
     else SettingsForm.CamPowerLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.CamPowerPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Auto Focus Sensitivity
 if(TRadioButton(ctrl)=SettingsForm.AFSensNorm)
 or(TRadioButton(ctrl)=SettingsForm.AFSensLow)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.AFSensNorm.Checked:=False;
   SettingsForm.AFSensLow.Checked :=False;
   SettingsForm.AFLabel.Font.Color:=$000000;
   SettingsForm.AFPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#$09#$04#$58);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
     2: SettingsForm.AFSensNorm.Checked  :=True;
     3: SettingsForm.AFSensLow.Checked   :=True;
     else SettingsForm.AFLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.AFPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //White Balance
 if(TRadioButton(ctrl)=SettingsForm.WBModeAuto)
 or(TRadioButton(ctrl)=SettingsForm.WBModeIndoor)
 or(TRadioButton(ctrl)=SettingsForm.WBModeOutdoor)
 or(TRadioButton(ctrl)=SettingsForm.WBOnePush)
 or(TRadioButton(ctrl)=SettingsForm.WBModeATW)
 or(TRadioButton(ctrl)=SettingsForm.WBManual) 
 or(TRadioButton(ctrl)=SettingsForm.WBManualTable)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.WBModeAuto.Checked   :=False;
   SettingsForm.WBModeIndoor.Checked :=False;
   SettingsForm.WBModeOutdoor.Checked:=False;
   SettingsForm.WBOnePush.Checked    :=False;
   SettingsForm.WBModeATW.Checked    :=False;
   SettingsForm.WBManual.Checked     :=False;
   SettingsForm.WBManualTable.Checked:=False;
   SettingsForm.WBLabel.Font.Color   :=$000000;
   SettingsForm.WBPanel.Enabled      :=True;
   buffer:=SendSerialCommand(#09#$04#$35);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
     0: SettingsForm.WBModeAuto.Checked   :=True;
     1: SettingsForm.WBModeIndoor.Checked :=True;
     2: SettingsForm.WBModeOutdoor.Checked:=True;
     3: SettingsForm.WBOnePush.Checked    :=True;
     4: SettingsForm.WBModeATW.Checked    :=True;
     5: SettingsForm.WBManual.Checked     :=True;
     6: SettingsForm.WBManualTable.Checked:=True;
     else SettingsForm.WBLabel.Font.Color :=$0000FF;
    end
   else SettingsForm.WBPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Auto Exposure Mode
 if(TRadioButton(ctrl)=SettingsForm.AEFullAuto)
 or(TRadioButton(ctrl)=SettingsForm.AEManual)
 or(TRadioButton(ctrl)=SettingsForm.AEShutPrior)
 or(TRadioButton(ctrl)=SettingsForm.AEIrisPrior)
 or(TRadioButton(ctrl)=SettingsForm.AEBright)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.AEFullAuto.Checked :=False;
   SettingsForm.AEManual.Checked   :=False;
   SettingsForm.AEShutPrior.Checked:=False;
   SettingsForm.AEIrisPrior.Checked:=False;
   SettingsForm.AEBright.Checked   :=False;
   SettingsForm.AELabel.Font.Color :=$000000;
   SettingsForm.AEPanel.Enabled    :=True;
   buffer:=SendSerialCommand(#09#$04#$39);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
      0: SettingsForm.AEFullAuto.Checked :=True;
      3: SettingsForm.AEManual.Checked   :=True;
     $A: SettingsForm.AEShutPrior.Checked:=True;
     $B: SettingsForm.AEIrisPrior.Checked:=True;
     $D: SettingsForm.AEBright.Checked   :=True;
     else SettingsForm.AELabel.Font.Color:=$0000FF;
    end
   else SettingsForm.AEPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Slow Shutter Mode
 if(TRadioButton(ctrl)=SettingsForm.SlowShutterAuto)
 or(TRadioButton(ctrl)=SettingsForm.SlowShutterManual)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.SlowShutterAuto.Checked    :=False;
   SettingsForm.SlowShutterManual.Checked  :=False;
   SettingsForm.SlowShutterLabel.Font.Color:=$000000;
   SettingsForm.SlowShutterPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#09#$04#$5A);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
      2: SettingsForm.SlowShutterAuto.Checked     :=True;
      3: SettingsForm.SlowShutterManual.Checked   :=True;
     else SettingsForm.SlowShutterLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.SlowShutterPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Exposure Compensation Mode
 if(TRadioButton(ctrl)=SettingsForm.ExposureOn)
 or(TRadioButton(ctrl)=SettingsForm.ExposureOff)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.ExposureOn.Checked      :=False;
   SettingsForm.ExposureOff.Checked     :=False;
   SettingsForm.ExposureLabel.Font.Color:=$000000;
   SettingsForm.ExposurePanel.Enabled   :=True;
   buffer:=SendSerialCommand(#09#$04#$3E);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
      2: SettingsForm.ExposureOn.Checked :=True;
      3: SettingsForm.ExposureOff.Checked:=True;
     else SettingsForm.ExposureLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.ExposurePanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Back Light Mode Mode
 if(TRadioButton(ctrl)=SettingsForm.BackLightOn)
 or(TRadioButton(ctrl)=SettingsForm.BackLightOff)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.BackLightOn.Checked      :=False;
   SettingsForm.BackLightOff.Checked     :=False;
   SettingsForm.BackLightLabel.Font.Color:=$000000;
   SettingsForm.BackLightPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#09#$04#$33);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
      2: SettingsForm.BackLightOn.Checked :=True;
      3: SettingsForm.BackLightOff.Checked:=True;
     else SettingsForm.BackLightLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.BackLightPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Mirror Mode
 if(TRadioButton(ctrl)=SettingsForm.MirrorOn)
 or(TRadioButton(ctrl)=SettingsForm.MirrorOff)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.MirrorOn.Checked      :=False;
   SettingsForm.MirrorOff.Checked     :=False;
   SettingsForm.MirrorLabel.Font.Color:=$000000;
   SettingsForm.MirrorPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#$09#$04#$61);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
     2: SettingsForm.MirrorOn.Checked        :=True;
     3: SettingsForm.MirrorOff.Checked       :=True;
     else SettingsForm.MirrorLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.MirrorPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Flip Mode
 if(TRadioButton(ctrl)=SettingsForm.FlipOn)
 or(TRadioButton(ctrl)=SettingsForm.FlipOff)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.FlipOn.Checked      :=False;
   SettingsForm.FlipOff.Checked     :=False;
   SettingsForm.FlipLabel.Font.Color:=$000000;
   SettingsForm.FlipPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#$09#$04#$66);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
     2: SettingsForm.FlipOn.Checked        :=True;
     3: SettingsForm.FlipOff.Checked       :=True;
     else SettingsForm.FlipLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.FlipPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Gamma Mode
 if(TRadioButton(ctrl)=SettingsForm.GammaOn)
 or(TRadioButton(ctrl)=SettingsForm.GammaOff)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.GammaOn.Checked      :=False;
   SettingsForm.GammaOff.Checked     :=False;
   SettingsForm.GammaLabel.Font.Color:=$000000;
   SettingsForm.GammaPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#$09#$04#$51);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
     2: SettingsForm.GammaOn.Checked        :=True;
     3: SettingsForm.GammaOff.Checked       :=True;
     else SettingsForm.GammaLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.GammaPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Call LED Mode
 if(TRadioButton(ctrl)=SettingsForm.CallLEDOn)
 or(TRadioButton(ctrl)=SettingsForm.CallLEDOff)
 or(TRadioButton(ctrl)=SettingsForm.CallLEDBlink)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.CallLEDOn.Checked      :=False;
   SettingsForm.CallLEDOff.Checked     :=False;
   SettingsForm.CallLEDBlink.Checked   :=False;
   SettingsForm.CallLEDLabel.Font.Color:=$000000;
   SettingsForm.CallLEDPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#$09#$01#$33#$01);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
     2: SettingsForm.CallLEDOn.Checked        :=True;
     3: SettingsForm.CallLEDOff.Checked       :=True;
     4: SettingsForm.CallLEDBlink.Checked     :=True;
     else SettingsForm.CallLEDLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.CallLEDPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Power LED Mode
 if(TRadioButton(ctrl)=SettingsForm.PowerLEDOn)
 or(TRadioButton(ctrl)=SettingsForm.PowerLEDOff)
 or(ctrl=nil)then
 begin
  repeat
   SettingsForm.PowerLEDOn.Checked      :=False;
   SettingsForm.PowerLEDOff.Checked     :=False;
   SettingsForm.PowerLEDLabel.Font.Color:=$000000;
   SettingsForm.PowerLEDPanel.Enabled   :=True;
   buffer:=SendSerialCommand(#$09#$01#$33#$02);
   if ValidReturnMessage(buffer) then
    case Ord(buffer[3]) of
     2: SettingsForm.PowerLEDOn.Checked        :=True;
     3: SettingsForm.PowerLEDOff.Checked       :=True;
     else SettingsForm.PowerLEDLabel.Font.Color:=$0000FF;
    end
   else SettingsForm.PowerLEDPanel.Enabled:=False;
   Application.ProcessMessages;
  until(ctrl=nil)or(ValidReturnMessage(buffer));
 end;
 //Red Gain (0-255)
 if(TTrackBar(ctrl)=SettingsForm.RedGain)
 or(ctrl=nil)then
 begin
  buffer:=SendSerialCommand(#09#$04#$43);
  SettingsForm.RedGainPanel.Enabled:=True;
  if ValidReturnMessage(buffer) then
   SettingsForm.RedGain.Position:=(Ord(buffer[4])AND$F)<<4
                               or (Ord(buffer[5])AND$F)
  else SettingsForm.RedGainPanel.Enabled:=False;
 end;
 //Blue Gain (0-255)
 if(TTrackBar(ctrl)=SettingsForm.BlueGain)
 or(ctrl=nil)then
 begin
  buffer:=SendSerialCommand(#09#$04#$44);
  SettingsForm.BlueGainPanel.Enabled:=True;
  if ValidReturnMessage(buffer) then
   SettingsForm.BlueGain.Position:=(Ord(buffer[4])AND$F)<<4
                                or (Ord(buffer[5])AND$F)
  else SettingsForm.BlueGainPanel.Enabled:=False;
 end;
 //Shutter Position (0-21)
 if(TTrackBar(ctrl)=SettingsForm.ShutterPosition)
 or(ctrl=nil)then
 begin
  buffer:=SendSerialCommand(#09#$04#$4A);
  SettingsForm.ShutterPanel.Enabled:=True;
  if ValidReturnMessage(buffer) then
   SettingsForm.ShutterPosition.Position:=(Ord(buffer[4])AND$F)<<4
                                       or (Ord(buffer[5])AND$F)
  else SettingsForm.ShutterPanel.Enabled:=False;
 end;
 //Iris Position (0-17)
 if(TTrackBar(ctrl)=SettingsForm.IrisPosition)
 or(ctrl=nil)then
 begin
  buffer:=SendSerialCommand(#09#$04#$4B);
  SettingsForm.IrisPanel.Enabled:=True;
  if ValidReturnMessage(buffer) then
   SettingsForm.IrisPosition.Position:=(Ord(buffer[4])AND$F)<<4
                                    or (Ord(buffer[5])AND$F)
  else SettingsForm.IrisPanel.Enabled:=False;
 end;
 //Gain Position (0-15)
 if(TTrackBar(ctrl)=SettingsForm.GainPosition)
 or(ctrl=nil)then
 begin
  buffer:=SendSerialCommand(#09#$04#$4C);
  SettingsForm.GainPanel.Enabled:=True;
  if ValidReturnMessage(buffer) then
   SettingsForm.GainPosition.Position:=(Ord(buffer[4])AND$F)<<4
                                    or (Ord(buffer[5])AND$F)
  else SettingsForm.GainPanel.Enabled:=False;
 end;
 //Bright Position (0-31)
 if(TTrackBar(ctrl)=SettingsForm.BrightPosition)
 or(ctrl=nil)then
 begin
  buffer:=SendSerialCommand(#09#$04#$4D);
  if ValidReturnMessage(buffer) then
   SettingsForm.BrightPosition.Position:=(Ord(buffer[4])AND$F)<<4
                                      or (Ord(buffer[5])AND$F)
  else SettingsForm.BrightPanel.Enabled:=False;
 end;
 //Exposure Compensation Position (0-14)
 if(TTrackBar(ctrl)=SettingsForm.ExposurePosition)
 or(ctrl=nil)then
 begin
  buffer:=SendSerialCommand(#09#$04#$4E);
  SettingsForm.ExpGainPanel.Enabled:=True;
  if ValidReturnMessage(buffer) then
   SettingsForm.ExposurePosition.Position:=(Ord(buffer[4])AND$F)<<4
                                        or (Ord(buffer[5])AND$F)
  else SettingsForm.ExpGainPanel.Enabled:=False;
 end;
 //Pan speed (1-24)
 if(TTrackBar(ctrl)=SettingsForm.PanSpeedSlider)
 or(ctrl=nil)then
  SettingsForm.PanSpeedSlider.Position :=panspeed;
 //Tilt speed (1-23)
 if(TTrackBar(ctrl)=SettingsForm.TiltSpeedSlider)
 or(ctrl=nil)then
  SettingsForm.TiltSpeedSlider.Position:=tiltspeed;
 //Zoom speed
 if(TTrackBar(ctrl)=SettingsForm.ZoomSpeedSlider)
 or(ctrl=nil)then
  SettingsForm.ZoomSpeedSlider.Position:=zoomspeed;
 //Allow updates again
 SettingsForm.ignorechange:=False;
end;

procedure TMainForm.MenuShowHideClick(Sender: TObject);
begin
 MainForm.Show;
 MainForm.Repaint;
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
 if not ValidateCamNumber then exit;
 TImage(Sender).Picture.Assign(TempStore.Picture);
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
 if ValidateCamNumber then
 begin
  ChangeImage(Sender,ZoomInOn);
  ZoomCamera(True,False);
 end;
end;

procedure TMainForm.ZoomOutButtonMouseDown(Sender: TObject;
 Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if ValidateCamNumber then
 begin
  ChangeImage(Sender,ZoomOutOn);
  ZoomCamera(False,True);
 end;
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

function TMainForm.SendSerialCommand(buffer: String): String;
var
 return: String;
 status: Integer;
 //temp  : String;
begin
 if not ValidateCamNumber(0,False) then exit;
 if Connection=nil then exit;
 //Is there anything in the buffer? if so, then flush it out
 if Connection.WaitingData>0 then Connection.RecvPacket(0);
 //Reset the return string
 Result:='';
 //Add the camera number and terminator
 buffer:=chr($80 OR camnumber)+buffer+#$FF;
 //Send the command
 Connection.SendString(AnsiString(buffer));
 Sleep(100); //Don't go too fast
 if Connection.LastError=0 then //If sent OK
 begin
  Result:='';
  status:=0;
  //And while there is data to be read
  while Connection.WaitingData>0 do
  begin
   return:=Connection.RecvPacket(0);
   status:=Connection.LastError;
   if status=0 then Result:=Result+return; //Add it to the return string
  end;
 end;
end;

function TMainForm.ValidReturnMessage(buffer: String): Boolean;
begin
 if Length(buffer)>3 then
  Result:= (buffer[1]=chr((camnumber+8)<<4))
        and(buffer[2]=#$50)
        and(buffer[Length(buffer)]=#$FF)
 else Result:=False;
end;

procedure TMainForm.SerClose;
begin
 if Connection<>nil then
 begin
  Connection.Free;
  Connection:=nil;
 end;
end;

function TMainForm.ValidateCamNumber(cam: Integer=0;sernum: Boolean=True): Boolean;
begin
 if cam=0 then cam:=camnumber;
 Result:=(cam>=Low(Cameras))and(cam<=High(Cameras))and(Connection<>nil);
 if(Result)and(sernum)then Result:=Cameras[cam].SerialNumber<>'';
end;

end.
