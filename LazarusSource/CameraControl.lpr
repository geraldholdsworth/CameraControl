program CameraControl;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
 cthreads,
 {$ENDIF}{$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, MainUnit
 { you can add units after this };

{$R *.res}

begin
 RequireDerivedFormResource:=True;
 Application.Title:='Camera Control';
 Application.Scaled:=True;
 Application.Initialize;
 //Application.ShowMainForm:=False;
 Application.CreateForm(TMainForm, MainForm);
 Application.Run;
end.
