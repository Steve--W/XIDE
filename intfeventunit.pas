unit IntfEventUnit;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
Classes, SysUtils, StringUtils, NodeUtils, XForm,
{$ifndef JScript}
Forms, Controls, Dialogs, StdCtrls, LazsUtils,
{$else}
HTMLUtils,
{$endif}
WrapperPanel, EventsInterface, XVBox, XHBox, XMemo, XButton,
XLabel, XEditBox, XComboBox,XCompositeIntf;

type

  { TIntfEventForm }

  TIntfEventForm = class(TXForm)
    IntfEventDoneBtn: TXButton;
    IntfEventType: TXComboBox;
    IntfEventName: TXEditBox;
    IntfEventVBox: TXVBox;
    IntfEventHint: TXEditBox;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    {$endif}
    procedure IntfEventDoneBtnHandleButtonClick(e: TEventStatus;
      nodeID: AnsiString; myValue: AnsiString);
  private

  public

  end;

procedure ClearForm;

var
  IntfEventForm: TIntfEventForm;
  InterfaceElement : TXCompositeIntf;
  EventToEdit:TEventHandlerRec;

implementation
uses XObjectInsp;

{$R *.lfm}

{ TIntfEventForm }

{$ifndef JScript}
procedure TIntfEventForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
end;
{$endif}

procedure TIntfEventForm.IntfEventDoneBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
var
  ok,IsReadOnly:Boolean;
  NewName:String;
begin
  if EventToEdit=nil then
  begin
    // creating a new interface event
    NewName:=self.IntfEventName.ItemValue;
    // check valid name string
    if NameStringIsValid(NewName) then
    begin
      ok:=true;
      // Is the event named uniquely?
      if (NewName<>'') then
      begin
        if (InterfaceElement.EventNameIsUnique(NewName)) then
        begin
          IsReadOnly:=(self.IntfEventType.ItemValue='Read-Only');
          InterfaceElement.myNode.AddEvent(NewName,'','','',IsReadOnly,IntfEventHint.ItemValue);
          TXForm(self).Showing:='No';
          RefreshObjectInspectorLater(ObjectInspectorSelectedNavTreeNode);
        end
        else
        begin
          ok:=false;
          showmessage(NewName+' already exists');
        end;
      end;
    end;
  end
  else
  begin
    // editing an event that already exists...
    TXForm(self).Showing:='No';
    EventToEdit.EventHint := IntfEventHint.ItemValue;
    RefreshObjectInspectorLater(ObjectInspectorSelectedNavTreeNode);
  end;
end;


procedure ClearForm;
begin
  IntfEventForm.IntfEventName.ItemValue:='';
  IntfEventForm.IntfEventType.ItemIndex:=0;
  IntfEventForm.IntfEventHint.ItemValue:='';
  EventToEdit:=nil;
  IntfEventForm.IntfEventName.readOnly:=false;
  //IntfEventForm.IntfEventType.IsEnabled???
end;


end.

