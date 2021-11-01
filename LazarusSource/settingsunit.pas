unit SettingsUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
 ComCtrls;

type

 { TSettingsForm }

 TSettingsForm = class(TForm)
  CallLEDBlink: TRadioButton;
  CamPowerLabel: TLabel;
  BlueGainLabel: TLabel;
  GammaLabel: TLabel;
  CallLEDLabel: TLabel;
  PowerLEDLabel: TLabel;
  GammaOff: TRadioButton;
  CallLEDOff: TRadioButton;
  PowerLEDOff: TRadioButton;
  GammaOn: TRadioButton;
  CallLEDOn: TRadioButton;
  PowerLEDOn: TRadioButton;
  GammaPanel: TPanel;
  CallLEDPanel: TPanel;
  PowerLEDPanel: TPanel;
  MirrorLabel: TLabel;
  FlipLabel: TLabel;
  MirrorOff: TRadioButton;
  FlipOff: TRadioButton;
  MirrorOn: TRadioButton;
  FlipOn: TRadioButton;
  MirrorPanel: TPanel;
  FlipPanel: TPanel;
  ShutterLabel: TLabel;
  IrisLabel: TLabel;
  GainLabel: TLabel;
  BrightLabel: TLabel;
  ExpGainLabel: TLabel;
  ApertureLabel: TLabel;
  PanSpeedLabel: TLabel;
  TiltSpeedLabel: TLabel;
  WBManualTable: TRadioButton;
  WBModeATW: TRadioButton;
  ZoomSpeedLabel: TLabel;
  AFLabel: TLabel;
  WBLabel: TLabel;
  AELabel: TLabel;
  SlowShutterLabel: TLabel;
  ExposureLabel: TLabel;
  BackLightLabel: TLabel;
  RedGainLabel: TLabel;
  lbCamDetails: TLabel;
  CamPowerPanel: TPanel;
  ShutterPanel: TPanel;
  IrisPanel: TPanel;
  GainPanel: TPanel;
  BrightPanel: TPanel;
  ExpGainPanel: TPanel;
  AperturePanel: TPanel;
  PanSpeedPanel: TPanel;
  TiltSpeedPanel: TPanel;
  ZoomSpeedPanel: TPanel;
  AFPanel: TPanel;
  WBPanel: TPanel;
  AEPanel: TPanel;
  SlowShutterPanel: TPanel;
  ExposurePanel: TPanel;
  BackLightPanel: TPanel;
  RedGainPanel: TPanel;
  BlueGainPanel: TPanel;
  CamPowerOn: TRadioButton;
  AEFullAuto: TRadioButton;
  AEManual: TRadioButton;
  AEShutPrior: TRadioButton;
  AEIrisPrior: TRadioButton;
  AEBright: TRadioButton;
  SlowShutterAuto: TRadioButton;
  SlowShutterManual: TRadioButton;
  ExposureOn: TRadioButton;
  ExposureOff: TRadioButton;
  BackLightOn: TRadioButton;
  CamPowerOff: TRadioButton;
  BackLightOff: TRadioButton;
  AFSensNorm: TRadioButton;
  AFSensLow: TRadioButton;
  WBModeAuto: TRadioButton;
  WBModeIndoor: TRadioButton;
  WBModeOutdoor: TRadioButton;
  WBOnePush: TRadioButton;
  WBManual: TRadioButton;
  RedGain: TTrackBar;
  TiltSpeedSlider: TTrackBar;
  ZoomSpeedSlider: TTrackBar;
  BlueGain: TTrackBar;
  ShutterPosition: TTrackBar;
  IrisPosition: TTrackBar;
  GainPosition: TTrackBar;
  BrightPosition: TTrackBar;
  ExposurePosition: TTrackBar;
  ApertureGain: TTrackBar;
  PanSpeedSlider: TTrackBar;
  procedure PanSpeedSliderChange(Sender: TObject);
  procedure RadioOnChange(Sender: TObject);
  procedure SliderChange(Sender: TObject);
  procedure TiltSpeedSliderChange(Sender: TObject);
  procedure ZoomSpeedSliderChange(Sender: TObject);
 private

 public
  ignorechange: Boolean;
 end;

var
 SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TSettingsForm }

procedure TSettingsForm.RadioOnChange(Sender: TObject);
var
 ctrl: TRadioButton;
 cmd : String;
begin
 if not ignorechange then
 begin
  if Sender is TRadioButton then
  begin
   cmd:='';
   ctrl:=Sender as TRadioButton;
   if ctrl.Checked then
   begin
    if ctrl=CamPowerOn        then cmd:=#$00#$02;//Camera Power
    if ctrl=CamPowerOff       then cmd:=#$00#$03;
    if ctrl=AFSensNorm        then cmd:=#$58#$02;//AF Sensitivity
    if ctrl=AFSensLow         then cmd:=#$58#$03;
    if ctrl=WBModeAuto        then cmd:=#$35#$00;//White Balance Mode
    if ctrl=WBModeIndoor      then cmd:=#$35#$01;
    if ctrl=WBModeOutdoor     then cmd:=#$35#$02;
    if ctrl=WBOnePush         then cmd:=#$35#$03;
    if ctrl=WBModeATW         then cmd:=#$35#$04;
    if ctrl=WBManual          then cmd:=#$35#$05;
    if ctrl=WBManualTable     then cmd:=#$35#$06;
    if ctrl=AEFullAuto        then cmd:=#$39#$00;//Auto Exposure Mode
    if ctrl=AEManual          then cmd:=#$39#$03;
    if ctrl=AEShutPrior       then cmd:=#$39#$0A;
    if ctrl=AEIrisPrior       then cmd:=#$39#$0B;
    if ctrl=AEBright          then cmd:=#$39#$0D;
    if ctrl=SlowShutterAuto   then cmd:=#$5A#$02;//Slow Shutter Mode
    if ctrl=SlowShutterManual then cmd:=#$5A#$03;
    if ctrl=ExposureOn        then cmd:=#$3E#$02;//Exposure Compensation Mode
    if ctrl=ExposureOff       then cmd:=#$3E#$03;
    if ctrl=BackLightOn       then cmd:=#$33#$02;//Back Light Mode
    if ctrl=BackLightOff      then cmd:=#$33#$03;
    if ctrl=MirrorOn          then cmd:=#$61#$02;//Mirror Mode
    if ctrl=MirrorOff         then cmd:=#$61#$03;
    if ctrl=FlipOn            then cmd:=#$66#$02;//Flip Mode
    if ctrl=FlipOff           then cmd:=#$66#$03;
    if ctrl=GammaOn           then cmd:=#$51#$02;//Gamma Mode
    if ctrl=GammaOff          then cmd:=#$51#$03;
    if ctrl=CallLEDOn         then cmd:=#$33#$01#$01;//Call LED Mode
    if ctrl=CallLEDOff        then cmd:=#$33#$01#$00;
    if ctrl=CallLEDBlink      then cmd:=#$33#$01#$02;
    if ctrl=PowerLEDOn        then cmd:=#$33#$02#$01;//Power LED Mode
    if ctrl=PowerLEDOff       then cmd:=#$33#$02#$00;
   end;
   if cmd<>'' then
   begin
    if Length(cmd)=2 then cmd:=#$04+cmd;
    MainForm.SendSerialCommand(#$01+cmd);
    MainForm.ShowCamSettings(Sender);
   end;
  end;
 end;
end;

procedure TSettingsForm.SliderChange(Sender: TObject);
var
 ctrl : TTrackBar;
 cmd  : String;
 p,q  : Byte;
begin
 if not ignorechange then
 begin
  if Sender is TTrackBar then
  begin
   cmd:='';
   ctrl:=Sender as TTrackBar;
   p:=(ctrl.Position AND $F0)>>4;
   q:=ctrl.Position AND $F;
   if ctrl=RedGain          then cmd:=#$43;
   if ctrl=BlueGain         then cmd:=#$44;
   if ctrl=ShutterPosition  then cmd:=#$4A;
   if ctrl=IrisPosition     then cmd:=#$4B;
   if ctrl=GainPosition     then cmd:=#$4C;
   if ctrl=BrightPosition   then cmd:=#$4D;
   if ctrl=ExposurePosition then cmd:=#$4E;
   if ctrl=ApertureGain     then cmd:=#$42;
   if cmd<>'' then
   begin
    MainForm.SendSerialCommand(#$01#$04+cmd+#$00#$00+chr(p)+chr(q));
    MainForm.ShowCamSettings(Sender);
   end;
  end;
 end;
end;

procedure TSettingsForm.PanSpeedSliderChange(Sender: TObject);
begin
 if not ignorechange then
  MainForm.panspeed:=PanSpeedSlider.Position;
end;

procedure TSettingsForm.TiltSpeedSliderChange(Sender: TObject);
begin
 if not ignorechange then
  MainForm.tiltspeed:=TiltSpeedSlider.Position;
end;

procedure TSettingsForm.ZoomSpeedSliderChange(Sender: TObject);
begin
 if not ignorechange then
  MainForm.zoomspeed:=ZoomSpeedSlider.Position;
end;

end.

