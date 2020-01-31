(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit InputSelectUnit;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, StringUtils, NodeUtils,
  {$ifndef JScript}
  FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLIntf, ExtCtrls, Menus, ComCtrls, TypInfo, LazIDEIntf,
  LazsUtils,
  UtilsJSCompile, Events, Types,
  {$else}
  HTMLUtils,
  {$endif}
  XScrollBox, XVBox, XHBox, XTree, XMemo, XTabControl, XButton, XLabel,
  XEditBox, XCheckBox, XHyperLink, XRadioBtns, XCode, XForm, XComboBox,
  XColorPicker,EventsInterface ;

type

  { TInputSelectForm }

  TInputSelectForm = class(TXForm)
    InputSelectTree: TXTree;
    InputSelectVBox1: TXVBox;
    InputSelectDoneBtn: TXButton;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    {$endif}
    procedure Initialise;
    procedure InputSelectClosed;
    procedure InputSelectDoneBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure InputSelectTreeHandleTreeNodeClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  private

  public
    ExclNodeName, ExclPropertyName, PropType : String;
    SelectedItem:String;
    TargetEditBoxNode:TDataNode;

  end;

var
  InputSelectForm: TInputSelectForm;
  InputSelectStatus:String;

implementation
uses xobjectinsp,CodeEditor;
{$R *.lfm}

{ TInputSelectForm }
procedure TInputSelectForm.Initialise;
var
  treestring, excludemyname:string;
  Exclusions:TStringList;
begin
  SelectedItem:='';
  InputSelectStatus:='Cancel';
  Exclusions:=TStringList.Create;
  Exclusions.Add(ResourceDataRootName);
  excludemyname:=ExclNodeName;
  if ExclPropertyName<>'' then
    excludemyname:=excludemyname+'.'+exclPropertyName;
  Exclusions.Add(excludemyname);
  //showmessage('ConstructSystemTreeString in inputselectform');
  treestring:= ConstructSystemTreeString(SystemNodeTree,0,true,true,Exclusions,PropType);
  InputSelectTree.TreeData:=treestring;
  FreeAndNil(Exclusions);
end;

procedure TInputSelectForm.InputSelectDoneBtnHandleButtonClick( e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  InputSelectStatus:='ok';
  TXForm(self).Showing:='No';
  {$ifdef JScript}
  InputSelectClosed;
  {$endif}
end;

{$ifndef JScript}
procedure TInputSelectForm.FormCreate(Sender: TObject);
//var
//  myNode:TDataNode;
begin
  myNode:=DoXFormCreated(self);
end;

procedure TInputSelectForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, InputSelectVBox1);
end;

{$endif}

procedure TInputSelectForm.InputSelectTreeHandleTreeNodeClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
// myValue is the internal ID of the clicked treenode.
// SelectedItem stores the selected node Text.
var
  NodeText:String;
begin
  NodeText:=InputSelectTree.SelectedNodeText;
  SelectedItem:=NodeText;
end;

procedure TInputSelectForm.InputSelectClosed;
begin
  if InputSelectStatus='ok' then
  begin
    TXEditBox(TargetEditBoxNode.ScreenObject).ItemValue:=self.SelectedItem;
    CodeEditForm.EditInputSource(nil,TargetEditBoxNode.NodeName,self.SelectedItem);
    //CodeEditForm.EditInputCombo(TargetEditBoxNode.NodeName,self.SelectedItem);
  end;
end;

end.

