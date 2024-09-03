program XIDE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$ifdef linux}
  cthreads,
  {$endif}
  {$ifdef Chromium}
  uCEFApplication, uCEFConstants,
  extctrls,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, XIDEMain, XObjectInsp, LazsUtils, datetimectrls, lazmouseandkeyinput,
  CodeEditor, CompileUserCode, PropertyEditUnit, popupmemo, AboutUnit,
  XIDEHelpUnit, DllInterface, SavedSystems, StylesUtils, InputSelectUnit,
  {$ifdef Python}
  PyXUtils,PythonEngine,
  {$endif}
  CEFXUtils, FrameViewer09, XGPUEditor, XIDESettings, intfparamunit,
  IntfEventUnit;

{$R *.res}
{$ifdef Chromium}
{$IFDEF MSWINDOWS}
  // CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}
{$endif}
const
  CEFLibDir:String = 'C:\cef4Master\FrameworkDir';

begin
  {$ifdef Python}
  // start the python engine
  SetupPyEngine('', '');
  {$endif}
  {$ifdef Chromium}
  SetupCEF4(CEFLibDir);

  if GlobalCEFApp.StartMainProcess then
  {$endif}
  begin
    RequireDerivedFormResource:=True;
    Application.Initialize;
    // creating forms.... these auto-generated statements create the project forms.
    // In the process, they run constructors for class TForm (not TXForm), so TXForm constructor stuff has to be added.
    Application.CreateForm(TXIDEForm, XIDEForm);
    Application.CreateForm(TCodeEditForm, CodeEditForm);
    Application.CreateForm(TInputSelectForm, InputSelectForm);
    Application.CreateForm(TPropertyEditForm, PropertyEditForm);
    Application.CreateForm(TPopupMemoForm, PopupMemoForm);
    Application.CreateForm(TAboutXIDEForm, AboutXIDEForm);
    Application.CreateForm(TXIDEHelpForm, XIDEHelpForm);
    Application.CreateForm(TSavedSystemsForm, SavedSystemsForm);
    Application.CreateForm(TXIDESettingsForm, XIDESettingsForm);
    Application.CreateForm(TIntfParamForm, IntfParamForm);
    Application.CreateForm(TIntfEventForm, IntfEventForm);
    Application.ProcessMessages;
    Application.Run;

  end;

  {$ifdef Chromium}
  CloseCEF4;
  {$endif}
end.


