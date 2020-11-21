unit intfparamunit;

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

  { TIntfParamForm }

  TIntfParamForm = class(TXForm)
    IntfParamName: TXEditBox;
    IntfParamVBox: TXVBox;
    IntfParamType: TXComboBox;
    IntfParamDoneButton: TXButton;
    IntfParamHint: TXEditBox;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    {$endif}
    procedure IntfParamDoneButtonHandleButtonClick(e: TEventStatus; nodeID: AnsiString;
      myValue: AnsiString);
  private

  public

  end;

procedure ClearForm;

var
  IntfParamForm: TIntfParamForm;
  InterfaceElement : TXCompositeIntf;
  AttribToEdit:TNodeAttribute;

implementation
uses XObjectInsp;

{$R *.lfm}

{ TIntfParamForm }


procedure TIntfParamForm.IntfParamDoneButtonHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
var
  ok,ro:Boolean;
  NewName:String;
begin
  if AttribToEdit=nil then
  begin
    // Create new attribute
    NewName:=self.IntfParamName.ItemValue;
    // check valid name string
    if NameStringIsValid(NewName) then
    begin
      ok:=true;
      // Is the property named uniquely?
      if (NewName<>'') then
      begin
        if (InterfaceElement.PropertyNameIsUnique(NewName)) then
        begin
          ro:=(self.IntfParamType.ItemValue='Input');
          InterfaceElement.myNode.AddAttribute(NewName,'String','',ro);
          RefreshObjectInspectorLater(ObjectInspectorSelectedNavTreeNode);
          TXForm(self).Showing:='No';
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
    // edit existing attribute
    TXForm(self).Showing:='No';
    AttribToEdit.AttribHint := IntfParamHint.ItemValue;
    RefreshObjectInspectorLater(ObjectInspectorSelectedNavTreeNode);
  end;
end;

procedure ClearForm;
begin
  IntfParamForm.IntfParamName.ItemValue:='';
  IntfParamForm.IntfParamType.ItemIndex:=0;
  IntfParamForm.IntfParamHint.ItemValue:='';
  AttribToEdit:=nil;
  IntfParamForm.IntfParamName.readOnly:=false;
end;

{$ifndef JScript}
procedure TIntfParamForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
end;

{$endif}

end.

