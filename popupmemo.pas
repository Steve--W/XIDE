(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit PopupMemo;

interface

uses
  Classes, SysUtils, StringUtils, NodeUtils,
  {$ifndef JScript}
  LazsUtils,
  {$endif}
  XVBox,  XMemo, XForm, XHTMLText;

type

  { TPopupMemoForm }

  TPopupMemoForm = class(TXForm)
    PopupMemoVBox: TXVBox;
    PopupHelpText: TXMemo;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    {$endif}
  private

  public
    procedure InitialiseMemo;

  end;

var
  PopupMemoForm: TPopupMemoForm;

implementation
uses
  XObjectInsp;

{$R *.lfm}

{ TPopupMemoForm }

{$ifndef JScript}
procedure TPopupMemoForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
  InitialiseMemo;
end;

procedure TPopupMemoForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, PopupMemoVBox);
end;

procedure TPopupMemoForm.FormShow(Sender: TObject);
begin
end;

{$endif}

procedure TPopupMemoForm.InitialiseMemo;
begin
  PopupHelpText.ItemValue:=
  'A number of interface functions have been provided for interaction with the XIDE framework and project data.' + LineEnding +
  ' ' + LineEnding +
  'General Functions' + LineEnding +
  '=================' + LineEnding +
  'procedure SetPropertyValue(nodeName,propName,newValue:String);  ' + LineEnding +
  '                            set a component property (string value)  ' + LineEnding +
  'function  GetPropertyValue(nodeName,propName:String);  '  + LineEnding +
  '                            returns a component property value (as string) '   + LineEnding +
  'procedure SetPropertyValueIndexed(nodeName,propName:String;newValue:TStringArray; x,y:integer); '  + LineEnding +
  '                            set a portion of a component array property  '  + LineEnding +
  'procedure CopyToClip(str:String); '    + LineEnding +
  '                            copies the given string to the clipboard '  + LineEnding +
  'function  CopyFromClip(e:TEventStatus):String; ' + LineEnding +
  '          CopyFromClip is an async function (required for browser use), so it must be coded in the  ' + LineEnding +
  '          ''Init'' section of an event handler. The result here is a blank string.                  ' + LineEnding +
  '          The clipboard string is held in e.ReturnString, which can be picked up in the             ' + LineEnding +
  '          ''Main'' section of the event handler.                                                    ' + LineEnding +
  'procedure ShowXForm(XFormID:String; modal:Boolean);  ' + LineEnding +
  '                            opens a TXForm. [note:''modal=false'' currently only effective on desktop] ' + LineEnding +
  'procedure CloseXForm(XFormID:String);  ' + LineEnding +
  '                            closes a TXForm.  ' + LineEnding +
  'procedure DoEvent(EventType,NodeId,myValue:String); '  + LineEnding +
  '                            executes the event handler defined for the given event type and component. '  + LineEnding +
  'procedure MoveComponent(nodeId:string;NewParentId:string); '  + LineEnding +
  '                            re-parents the given UI component '  + LineEnding +
  'procedure CopyComponent(nodeId,NewParentId,NewName:string); '  + LineEnding +
  '                            copies the given UI component and places the clone under the given parent '  + LineEnding +
  'function  DeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean; '  + LineEnding +
  '                            deletes the given UI component. Returns false if not done. '   + LineEnding +
  'function  UserSystemAsString():String;'  + LineEnding +
  '                            Returns the string representation of the current user system (can be imported to XIDE via System>Load)  '  + LineEnding +
  'function  LoadUserSystemString(SystemString:String);'  + LineEnding +
  '                            Imports a new user system to the XIDE framework (can only be done in ''Design'' mode) '  + LineEnding +
  'procedure ShowBusy(e:TEventStatus);'  + LineEnding +
  '                            Shows the busy cursor  '  + LineEnding +
  '          ShowBusy is an async function (required for browser use), so it must be coded in the  ' + LineEnding +
  '          ''Init'' section of an event handler.         ' + LineEnding +
  'procedure HideBusy;'   + LineEnding +
  '                            Hides the busy cursor  '  + LineEnding +
  'procedure ProcessMessages;'  + LineEnding +
  '                            Functional in Desktop execution only.  Executes a pascal Application.ProcessMessages statement.  '  + LineEnding +
  'procedure DebugStart;'  + LineEnding +
  '                            Functional in Browser execution only.  Executes a Javascript ''debugger;'' statement - starts the native browser debug facility.  '  + LineEnding+
  'procedure RunPython(str:String);'  + LineEnding +
  '                            Executes the given Python script  '  + LineEnding +
  'procedure PyodideLoadPackage(nm:String); '  + LineEnding +
  '                            Functional in Browser execution only.  Loads the requested Pyodide/python package, so that it will be available for import.  '  + LineEnding +
  '                            Note this runs an asynchronous process. Check the console for progress.  '  + LineEnding +
  'function PyodidePackageLoaded(nm:String):Boolean; '  + LineEnding +
  '                            Functional in Browser execution only.  Returns true if the named package has been loaded into the Pyodide environment.  '  + LineEnding;

  PopupHelpText.ItemValue:=PopupHelpText.ItemValue +
  ' ' + LineEnding +
  'Messages and Dialogs' + LineEnding +
  '====================' + LineEnding +
  'procedure ShowMessage(msg:String);       ' + LineEnding +
  '                            displays a popup alert ' + LineEnding +
  'function  Confirm(TextMessage:string):boolean;   '  + LineEnding +
  '                            displays a confirmation alert; returns true/false '  + LineEnding +
  'function  Prompt(TextMessage,promptString:string):string; '  + LineEnding +
  '                            displays an input box; returns user-entered string '   + LineEnding +
  'procedure ConsoleLog(txt:String);'  + LineEnding +
  '                            Writes a debug message to the console log  '  + LineEnding;

  PopupHelpText.ItemValue:=PopupHelpText.ItemValue +
  ' ' + LineEnding +
  'TXTable Functions' + LineEnding +
  '=================' + LineEnding +
  'procedure  LoadTableFromExcelCopy(TableName,CopiedString:String);  '  + LineEnding +
  '                            Populate the given TXTable component with a string in Excel format (eg. as copied from a spreadsheet)  '  + LineEnding +
  'procedure  LoadTableFromNumArray(TableName,NumArray:T2DNumArray);  '  + LineEnding +
  '                            Populate the given TXTable component from a 2D numeric array  '  + LineEnding +
  'function  GetTableDataArray(TableName:String;SkipHeader:Boolean):T2DStringArray;  '  + LineEnding +
  '                            Fetch the cells from the given TXTable component as a 2D string array  '  + LineEnding +
  'function  Array2DToString(arr:T2DNumArray):String;'  + LineEnding +
  '                            Convert numeric 2D array to string form eg. [[...],...,[...]]  '  + LineEnding;

  PopupHelpText.ItemValue:=PopupHelpText.ItemValue +
  ' ' + LineEnding +
  'TXGPUCanvas Functions' + LineEnding +
  '=====================' + LineEnding +
  'function  GetGPUParamNumValue(GPUName,pName:String):TNumArray; '  + LineEnding +
  '                            For the given TXGPUCanvas component, returns the value of the named numeric parameter as an array '  + LineEnding +
  'function  GetGPUConstIntValue(GPUName,pName:String):integer;'  + LineEnding +
  '                            For the given TXGPUCanvas component, returns the value of the named integer parameter '  + LineEnding +
  'procedure SetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);'  + LineEnding +
  '                            For the given TXGPUCanvas component, sets the value of the named numeric parameter as a 1-D array '  + LineEnding +
  'procedure SetGPUParam2DNumValue(GPUName,pName:String;pValue:T2DNumArray);'  + LineEnding +
  '                            For the given TXGPUCanvas component, sets the value of the named numeric parameter as a 2-D array '  + LineEnding +
  'procedure SetGPUConstIntValue(GPUName,pName:String;pValue:integer);'  + LineEnding +
  '                            For the given TXGPUCanvas component, sets the value of the named integer parameter '  + LineEnding +
  'function  GetGPUPixelArray(GPUName:String):T3DNumArray; '  + LineEnding +
  '                            Fetch the current Pixel array for the given TXGPUCanvas component  '  + LineEnding +
  'function  GetGPUPixelArrayAsString(GPUName:String):String;'  + LineEnding +
  '                            Fetch the current Pixel array in string format for the given TXGPUCanvas component  '  + LineEnding +
  'function  GetGPUStageArray(GPUName:String):T3DNumArray; '  + LineEnding +
  '                            Fetch the stage array (resulting from the non-graphical kernel stack) for the given TXGPUCanvas component  '  + LineEnding +
  'function  GetGPUStageArrayAsString(GPUName:String):String;'  + LineEnding +
  '                            Fetch the stage array in string format for the given TXGPUCanvas component  '  + LineEnding;

  PopupHelpText.ItemValue:=PopupHelpText.ItemValue +
  ' ' + LineEnding +
  'Python Only' + LineEnding +
  '===========' + LineEnding +
  'function  ShowPythonPlot(ImgName,fig) '  + LineEnding +
  '                            In the given TXImage component, displays the contents of fig (a matplotlib figure) '  + LineEnding;


end;

end.

