(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XIDEComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XGPUEditor;

{$mode objfpc}{$H+}

interface
uses
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, XIFrame, Math,
    UtilsJSCompile, XForm, XCode, XButton, XVBox, XTabControl, XMemo, XComboBox, EventsInterface,
    WebTranspilerUtils,
  {$ifndef JScript}
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf,
    LCLType, gettext,
    {$ifdef Chromium}
    uCEFChromium, uCEFInterfaces, uCEFTypes,
    {$endif}
  {$else}
    webfilecache, pas2jswebcompiler,
    HTMLUtils,
  {$endif}
    WrapperPanel, Events;

type TAnimCodeArray = Array of TStringList;
type  TGPUEventClass = class
    procedure CloseCodeEditor(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure GPUCodeEditHandleClickMessage(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure LaunchGPUHTML(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure GPUComboBoxChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure TabChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    {$ifndef JScript}
    procedure EditorResize(Sender: TObject);
    {$endif}
  end;

var
  GPUEditorForm:TXForm;
  GPUEditorTabControl:TXTabControl;
  GPUCodeEditor:TXCode;
  GPUComboBox:TXComboBox;
  GPUMemo:TXMemo;
  EditingGPUNode:TDataNode;
  {$ifndef JScript}
    GPUEditorTopControl:TWinControl;
    {$endif}
  GPUEditorMode:String;
  GPUEvents:TGPUEventClass;

procedure CreateGPUEditForm;
procedure ShowGPUEditor(GPUNode:TDataNode);
procedure ShowGPUKernel(GPUNode:TDataNode;filename:string;targetLine:integer;CharPos:String);


implementation
uses XGPUCanvas;

procedure CreateGPUEditForm;
var
  FormNode,TabControlNode,TabPageNode1,TabPageNode2,EditorNode,VBNode,BtnNode:TDataNode;
  MemoNode,VBNode2,BtnNode2,ComboNode:TDataNode;
  DoneBtn,LaunchBtn:TXButton;
  VB,vb2:TXVBox;
  tmp, Olist:String;
begin


  // Create the popup form for editing the GPU animation code block...
  if GPUEditorForm=nil then
  begin
    {$ifndef JScript}
    GPUEditorForm:=TXForm.CreateNew(Application);
    GPUEditorForm.Name:='XGPUCodeEditorForm';
    FormNode:=CreateFormNode(GPUEditorForm);
    GPUEditorForm.BorderStyle:=bsSizeable;           // allows resizing
    GPUEditorForm.OnResize:=@GPUEvents.EditorResize;

    {$else}
    FormNode:=AddDynamicWidget('TXForm',nil,nil,'XGPUCodeEditorForm','','Left',-1);
    GPUEditorForm:=TXForm(FormNode);
    GPUEditorForm.Name:='XGPUCodeEditorForm';
    {$endif}
    GPUEditorForm.Caption:='XGPUCanvas Animation Code Editor';
    GPUEditorForm.Top:=100;
    GPUEditorForm.Left:=100;
    GPUEditorForm.Height:=500;
    GPUEditorForm.Width:=900;
    FormNode.IsDynamic:=false;

    addchildtoparentnode(SystemNodeTree,FormNode,-1);   //!!!! check this doesn't upset things...don't want these in a systemsave...

    TabControlNode:=AddDynamicWidget('TXTabControl',GPUEditorForm,FormNode,'XGPUTabControl','','Left',-1);
    TabControlNode.IsDynamic:=false;
    GPUEditorTabControl:=TXTabControl(TabControlNode.ScreenObject);
    GPUEditorTabControl.ContainerHeight:='100%';
    GPUEditorTabControl.ContainerWidth:='100%';
    TabControlNode.registerEvent('Change',@GPUEvents.TabChange);

    TabPageNode1:=AddDynamicWidget('TXTabSheet',GPUEditorForm,TabControlNode,'XGPUTabSheet1','','Left',-1);
    TabPageNode2:=AddDynamicWidget('TXTabSheet',GPUEditorForm,TabControlNode,'XGPUTabSheet2','','Left',-1);
    TabPageNode1.IsDynamic:=false;
    TabPageNode2.IsDynamic:=false;
    TXTabSheet(TabPageNode1.ScreenObject).Caption:='GPU Kernel Code';
    TXTabSheet(TabPageNode2.ScreenObject).Caption:='Generated HTML';
    TXTabControl(TabControlNode.ScreenObject).TabIndex:=0;

    VBNode:=AddDynamicWidget('TXVBox',GPUEditorForm,TabPageNode1,'XGPUVBox','','Left',-1);
    VB:=TXVBox(VBNode.ScreenObject);
    VB.ContainerHeight:='100%';
    VB.Border:=false;
    VBNode.IsDynamic:=false;

    ComboNode:=AddDynamicWidget('TXComboBox',GPUEditorForm,VBNode,'XGPUComboBox','','Left',-1);
    GPUComboBox:=TXComboBox(ComboNode.ScreenObject);
    GPUComboBox.OptionList:='["Graphical (Final)"]';
    GPUComboBox.ItemIndex:=0;
    GPUComboBox.LabelPos:='Left';
    GPUComboBox.LabelText:='Select Kernel Code to Edit';
    ComboNode.IsDynamic:=false;
    GPUComboBox.myNode.registerEvent('Change',@GPUEvents.GPUComboBoxChange);

    EditorNode:=AddDynamicWidget('TXCode',GPUEditorForm,VBNode,'XGPUCodeEditor','','Left',-1);
    GPUCodeEditor:=TXCode(EditorNode.ScreenObject);
    GPUCodeEditor.ContainerHeight:='90%';
    GPUCodeEditor.ContainerWidth:='100%';
    GPUCodeEditor.MessagesHeight:='30%';
    GPUCodeEditor.LabelText:='';
    GPUCodeEditor.myNode.registerEvent('ClickMessage',@GPUEvents.GPUCodeEditHandleClickMessage);
    EditorNode.IsDynamic:=false;

    BtnNode:=AddDynamicWidget('TXButton',GPUEditorForm,VBNode,'XGPUDoneBtn','','Left',-1);
    DoneBtn:=TXButton(BtnNode.ScreenObject);
    DoneBtn.Caption:='Done';
    DoneBtn.myNode.registerEvent('ButtonClick',@GPUEvents.CloseCodeEditor);
    BtnNode.IsDynamic:=false;

    VBNode2:=AddDynamicWidget('TXVBox',GPUEditorForm,TabPageNode2,'XGPUVBox2','','Left',-1);
    VB2:=TXVBox(VBNode2.ScreenObject);
    VB2.ContainerHeight:='100%';
    VB2.Border:=false;
    VBNode2.IsDynamic:=false;

    MemoNode:=AddDynamicWidget('TXMemo',GPUEditorForm,VBNode2,'XGPUHTMLMemo','','Left',-1);
    GPUMemo:=TXMemo(MemoNode.ScreenObject);
    GPUMemo.MemoHeight:='85%';
    GPUMemo.MemoWidth:='100%';
    GPUMemo.LabelPos:='Top';
    GPUMemo.LabelText:='HTML generated at the last GPU activation';
    MemoNode.IsDynamic:=false;

    BtnNode2:=AddDynamicWidget('TXButton',GPUEditorForm,VBNode2,'XGPULaunchBtn','','Left',-1);
    LaunchBtn:=TXButton(BtnNode2.ScreenObject);
    LaunchBtn.Caption:='Launch HTML in browser';
    LaunchBtn.myNode.registerEvent('ButtonClick',@GPUEvents.LaunchGPUHTML);
    LaunchBtn.Hint:='Launch the generated GPU HTML in a separate browser page to aid diagnostics';
    BtnNode2.IsDynamic:=false;

    {$ifndef JScript}
    GPUEditorTopControl:=GPUEditorTabControl;
    {$endif}

  end;
end;

{$ifndef JScript}
procedure TGPUEventClass.EditorResize(Sender: TObject);
begin
  DoFormResize(TXForm(Sender), GPUEditorTopControl);
end;

{$endif}

procedure SaveThisCodeBlock(idx:integer);
var
  AllKernels:TAnimCodeArray;
  AllCode:String;
  i:integer;
begin
  AllKernels:=TXGPUCanvas(EditingGPUNode.ScreenObject).FetchAllAnimCode;
//  for i:=0 to length(AllKernels)-1 do
//    showmessage('Existing '+inttostr(i)+': '+AllKernels[i].Text);
  // Re-concatenate the kernel code blocks
  for i:=0 to length(AllKernels)-1 do
  begin
    if i<>idx then
      AllCode:=AllCode+AllKernels[i].Text
    else
      AllCode:=AllCode+GPUCodeEditor.ItemValue;

    if i<length(AllKernels)-1 then
      AllCode:=AllCode+eventListdelimiter;
  end;
  EditAttributeValue(EditingGPUNode,'AnimationCode',AllCode);
//  AllKernels:=TXGPUCanvas(EditingGPUNode.ScreenObject).FetchAllAnimCode;
//  for i:=0 to length(AllKernels)-1 do
//    showmessage('After Save '+inttostr(i)+': '+AllKernels[i].Text);
end;

procedure TGPUEventClass.CloseCodeEditor(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  // careful here... if the compile has failed the editor may be showing the whole gpu code unit,
  // and not just the user code from the AnimationCode property.
  TXGPUCanvas(EditingGPUNode.ScreenObject).Active:=false;

  GPUEditorForm.Showing:='No';
  GPUMemo.ItemValue:='';  //!! Lazarus bug? Can't reopen the form (crashes) if this is pre-populated with a long string.

  if GPUEditorMode='Animation' then
  begin
    // Update the property value ...         !! BUT NOT IF THE WHOLE UNIT IS ON DISPLAY>>>>>
    SaveThisCodeBlock(GPUComboBox.ItemIndex);
  end;
end;

procedure TGPUEventClass.GPUCodeEditHandleClickMessage(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var linenumber:integer;
    SelectedLine,FileName,CharPos:string;
    FoundLineNum:Boolean;
 //   tmp1:TStringList;
    LineNum:String;
    Messages:TStringList;
begin
   //showmessage('GPUCodeEditHandleClickMessage '+ nodeID + ' '+myValue);
  {$ifndef JScript}
  linenumber:=GPUCodeEditor.TheMessages.CaretPos.Y;
  SelectedLine:= GPUCodeEditor.TheMessages.lines[linenumber];
  GPUCodeEditor.GetFileNameLineNumAndCharPos(FoundLineNum,SelectedLine, '(',FileName,LineNum,CharPos );
  {$else}
  // Find the message line thats been clicked on
  try
  linenumber:=StrToInt(myValue);
  except
    On E : EConvertError do
    EXIT;
  end;
  Messages:=TStringList.Create;
  Messages.Text:=GPUCodeEditor.MessageLines;
  if linenumber>Messages.Count then
    EXIT;
  SelectedLine:= Messages[linenumber-1];
  Messages.Free;

  // Find the indicated line number from the message
  GPUCodeEditor.GetFileNameLineNumAndCharPos(FoundLineNum,SelectedLine, '(',FileName,LineNum,CharPos );

  //showmessage('FileName='+FileName+' Indicated Linenum is '+linenum);
  {$endif}

  //load the indicated code block into the code edit box
  if trim(FileName)<>'' then
  begin
    ShowGPUKernel(EditingGPUNode,FileName,linenumber,CharPos);
  end;
end;

procedure TGPUEventClass.LaunchGPUHTML(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  myHTML:String;
begin
  if GPUMemo.ItemValue<>'' then
  begin
    myHTML:=GPUMemo.ItemValue;
    TXGPUCanvas(EditingGPUNode.ScreenObject).LaunchHTML('Data',myHTML,'GPU Diagnostic');
  end;
end;

procedure TGPUEventClass.TabChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  if GPUEditorTabControl.TabIndex=1 then
    GPUMemo.ItemValue:=TXGPUCanvas(EditingGPUNode.ScreenObject).GeneratedHTML;
end;

procedure TGPUEventClass.GPUComboBoxChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  thisNode:TDataNode;
  AllKernels:TAnimCodeArray;
  i:integer;
begin
  // Select the given Kernel code for display in the editor.
  if (EditingGPUNode<>nil) then
  begin
    // Save the current code block.
    //showmessage('saving prior index '+inttostr(GPUComboBox.PriorIndex));
    if (GPUComboBox.PriorIndex>-1) then
      SaveThisCodeBlock(GPUComboBox.PriorIndex);
    // Fetch the required code block
    AllKernels:=TXGPUCanvas(EditingGPUNode.ScreenObject).FetchAllAnimCode;
    GPUCodeEditor.ItemValue:=AllKernels[GPUComboBox.ItemIndex].Text;
  end;
end;

procedure ShowGPUEditor(GPUNode:TDataNode);
var
  AllKernels:TAnimCodeArray;
begin
  // Edit the AnimationCode in a TXGPUCanvas component using the dedicated popup editor...
  // the animation code may consist of several kernel procedures.
  // These are delimited by the EventListDelimiter string.
  // The GPU Code Editor needs to show the first kernel proc.
  AllKernels:=TXGPUCanvas(GPUNode.ScreenObject).FetchAllAnimCode;
  //XGPUCanvas.GPUCodeEditor.ItemValue:=ObjectInspectorSelectedCodeTreeNode.GetAttribute('AnimationCode',true).AttribValue;
  GPUCodeEditor.ItemValue:=AllKernels[0].Text;
  GPUCodeEditor.MessageLines:='';
  GPUCodeEditor.MessagesHeight:='1';
  //   GPUMemo.ItemValue:=TXGPUCanvas(ObjectInspectorSelectedCodeTreeNode.ScreenObject).GeneratedHTML;
  //!! Lazarus bug?     Have to populate GPUMemo later (eg. on tab change), otherwise the popup form crashes.
  EditingGPUNode:=GPUNode;
  GPUEditorMode:='Animation';
  GPUComboBox.OptionList:=TXGPUCanvas(EditingGPUNode.ScreenObject).BuildKernelList;
  GPUComboBox.ItemIndex:=0;
  GPUComboBox.PriorIndex:=0;
  XGPUEditor.GPUCodeEditor.ReadOnly:=false;
  GPUEditorForm.Showing:='Modal';

end;

procedure ShowGPUKernel(GPUNode:TDataNode;filename:string;targetLine:integer;CharPos:String);
var
  bits:TStringList;
  targetKernel:integer;
  AllKernels:TAnimCodeArray;
  i:integer;
  tmp:string;
begin
  if (GPUNode<>nil) then
  begin
    EditingGPUNode:=GPUNode;
  end;
  // FileName is NodeName.num.AnimationCode  If num is zero, this is the final kernel, otherwise one of the stage kernels.
  // Set the comboBox to the relevant item.
  bits:=stringsplit(FileName,EditingGPUNode.NodeName+'.');
  for i:=0 to bits.count-1 do
  begin
    tmp:=bits[i];
  end;
  if (bits.Count=2)
  and (bits[1]<>'') then
  begin
    bits:=stringsplit(bits[1],'.');
    targetKernel:=strtoint(bits[0]);
    AllKernels:=TXGPUCanvas(EditingGPUNode.ScreenObject).FetchAllAnimCode;
    GPUComboBox.OptionList:=TXGPUCanvas(EditingGPUNode.ScreenObject).BuildKernelList;
    GPUComboBox.ItemIndex:=targetKernel;
    GPUComboBox.PriorIndex:=targetKernel;
    GPUCodeEditor.ItemValue:=AllKernels[targetKernel].Text;

    GPUCodeEditor.ReadOnly:=false;
    GPUEditorMode:='Animation';

    {$ifndef JScript}
    // set cursor position
    if (CharPos<>'') then
    begin
      GPUCodeEditor.GoToLineCharPos(targetLine,StrToInt(CharPos));
    end;
    {$endif}
    if (GPUEditorForm.Showing='No') then
    begin
      GPUCodeEditor.MessageLines:='';
      GPUCodeEditor.MessagesHeight:='1';
      GPUEditorForm.Showing:='Modal';
    end;
    {$ifdef JScript}
    // set cursor position
    if (CharPos<>'') then
    begin
      GPUCodeEditor.GoToLineCharPos(targetLine,StrToInt(CharPos));
    end;
    {$endif}
  end
  else
  begin
    showmessage('This code is not available for edit');
  end;
  bits.Free;
end;

begin
  GPUEvents:=TGPUEventClass.Create;
end.

