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
  'procedure ShowMessage(msg:String);       ' + LineEnding +
  '                            displays a popup alert ' + LineEnding +
  'procedure ShowXForm(XFormID:String; modal:Boolean);  ' + LineEnding +
  '                            opens a XForm. [note:''modal=false'' currently only effective on desktop] ' + LineEnding +
  'procedure CloseXForm(XFormID:String);  ' + LineEnding +
  '                            closes a XForm.  ' + LineEnding +
  'procedure SetPropertyValue(nodeName,propName,newValue:String);  ' + LineEnding +
  '                            set a component property  ' + LineEnding +
  'procedure SetPropertyValueIndexed(nodeName,propName:String;newValue:TStringArray; x,y:integer); '  + LineEnding +
  '                            set a portion of a component array property  '  + LineEnding +
  'function  GetPropertyValue(nodeName,propName:String);  '  + LineEnding +
  '                            returns a component property value (as string) '   + LineEnding +
//  'function  GetPropertyValueIndexed(nodeName,propName:String; x,y,w,h:integer):TstringArray; '  + LineEnding +
//  '                            returns a portion of a component array property ' + LineEnding +
  'function  Confirm(TextMessage:string):boolean;   '  + LineEnding +
  '                            displays a confirmation alert; returns true/false '  + LineEnding +
  'function  Prompt(TextMessage,promptString:string):string; '  + LineEnding +
  '                            displays an input box; returns user-entered string '   + LineEnding +
  'procedure CopyToClip(str:String); '    + LineEnding +
  '                            copies the given string to the clipboard '  + LineEnding +
  'function  CopyFromClip(e:TEventStatus):String; ' + LineEnding +
  '   CopyFromClip is an async function (required for browser use), so it must be coded in the  ' + LineEnding +
  '   ''Init'' section of an event handler. The result here is a blank string.                  ' + LineEnding +
  '   The clipboard string is held in e.ReturnString, which cn be picked up in the              ' + LineEnding +
  '   ''Main'' section of the event handler.                                                    ' + LineEnding +
  'procedure DoEvent(EventType,NodeId,myValue:String); '  + LineEnding +
  '                            executes the event handler defined for the given event type and component. '  + LineEnding +
  'procedure MoveComponent(nodeId:string;NewParentId:string); '  + LineEnding +
  '                            re-parents the given UI component '  + LineEnding +
  'procedure CopyComponent(nodeId,NewParentId,NewName:string); '  + LineEnding +
  '                            copies the given UI component and places the clone under the given parent '  + LineEnding +
  'function  DeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true):Boolean; '  + LineEnding +
  '                            deletes the given UI component. Returns false if not done. '   + LineEnding +
  'function  GetGPUParamNumValue(GPUName,pName:String):TNumArray; '  + LineEnding +
  '                            For the given TXGPUCanvas component, returns the value of the named numeric parameter as an array '  + LineEnding +
  'function  GetGPUConstIntValue(GPUName,pName:String):integer;'  + LineEnding +
  '                            For the given TXGPUCanvas component, returns the value of the named integer parameter '  + LineEnding +
  'function  GetGPUParamImgValue(GPUName,pName:String):TImgArray;'  + LineEnding +
  '                            For the given TXGPUCanvas component, returns the value of the named image parameter as an array '  + LineEnding +
  'procedure SetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);'  + LineEnding +
  '                            For the given TXGPUCanvas component, sets the value of the named image parameter as an array '  + LineEnding +
  'procedure SetGPUConstIntValue(GPUName,pName:String;pValue:integer);'  + LineEnding +
  '                            For the given TXGPUCanvas component, sets the value of the named integer parameter '  + LineEnding +
  'procedure SetGPUParamImgValue(GPUName,pName:String;pValue:TImgArray);'  + LineEnding +
  '                            For the given TXGPUCanvas component, sets the value of the named image parameter as an array  '  + LineEnding +
  'procedure ShowBusy;'  + LineEnding +
  '                            Shows the busy cursor  '  + LineEnding +
  'procedure HideBusy;'   + LineEnding +
  '                            Hides the busy cursor  '  + LineEnding +
  'procedure ProcessMessages;'  + LineEnding +
  '                            tba  '  + LineEnding +
  'procedure MovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2:String);'  + LineEnding +
  '                            Runs an animated pointer on screen between the two given UI components  '  + LineEnding +
  'procedure HidePointer;'  + LineEnding +
  '                            Hides the animated pointer (that was raised by MovePointerBetweenComponents)  '  + LineEnding +
  'function  UserSystemAsString():String;'  + LineEnding +
  '                            Returns the string representation of the current user system (can be imported to XIDE via System>Load)  '  + LineEnding +
  'function  LoadUserSystemString(SystemString:String);'  + LineEnding +
  '                            Imports a new user system to the XIDE framework (can only be done in ''Design'' mode) '  + LineEnding +
  'procedure ConsoleLog(txt:String);'  + LineEnding +
  '                            Writes a debug message to the console log  '  + LineEnding;
end;

end.

