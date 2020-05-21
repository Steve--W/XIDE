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
    UtilsJSCompile, XForm, XCode, XButton, XVBox, XHBox, XTabControl, XMemo, XComboBox, XEditBox,
    X3DTable, EventsInterface,
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

type TAnimCodeRec = record
  CodeBlock:TStringList;
end;
type TAnimCodeArray = Array of TAnimCodeRec;
type TDimsArray = Array of array of integer;
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
  GPUEditBox:TXEditBox;
  GPUMemo:TXMemo;
  GPUTableEditor:TX3DTable;
  EditingGPUNode:TDataNode;
  {$ifndef JScript}
    GPUEditorTopControl:TWinControl;
    {$endif}
  GPUEditorMode:String;
  GPUEvents:TGPUEventClass;
  GPUEditorDoneBtn:TXButton;

procedure CreateGPUEditForm;
procedure ShowGPUEditor(GPUNode:TDataNode;TabPage:integer);
procedure ShowGPUKernel(GPUNode:TDataNode;filename:string;targetLine:integer;CharPos:String);


implementation
uses XGPUCanvas;

procedure CreateGPUEditForm;
var
  FormNode,MainVBNode,TabControlNode,EditorNode,VBNode,BtnNode,TableNode:TDataNode;
  MemoNode,VBNode2,BtnNode2,ComboNode,HBTopNode,EditNode:TDataNode;
  TabPageNode1,TabPageNode2,TabPageNode3:TdataNode;
  LaunchBtn:TXButton;
  MainVB,VB,vb2:TXVBox;
  HB:TXHBox;
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

    MainVBNode:=AddDynamicWidget('TXVBox',GPUEditorForm,FormNode,'XGPUMainVBox','','Left',-1);
    MainVB:=TXVBox(MainVBNode.ScreenObject);
    MainVB.ContainerHeight:='100%';
    MainVB.Border:=false;
    MainVBNode.IsDynamic:=false;

    TabControlNode:=AddDynamicWidget('TXTabControl',GPUEditorForm,MainVBNode,'XGPUTabControl','','Left',-1);
    TabControlNode.IsDynamic:=false;
    GPUEditorTabControl:=TXTabControl(TabControlNode.ScreenObject);
    GPUEditorTabControl.ContainerHeight:='90%';
    GPUEditorTabControl.ContainerWidth:='100%';
    TabControlNode.registerEvent('Change',@GPUEvents.TabChange);

    TabPageNode1:=AddDynamicWidget('TXTabSheet',GPUEditorForm,TabControlNode,'XGPUTabSheet1','','Left',-1);
    TabPageNode2:=AddDynamicWidget('TXTabSheet',GPUEditorForm,TabControlNode,'XGPUTabSheet2','','Left',-1);
    TabPageNode3:=AddDynamicWidget('TXTabSheet',GPUEditorForm,TabControlNode,'XGPUTabSheet3','','Left',-1);
    TabPageNode1.IsDynamic:=false;
    TabPageNode2.IsDynamic:=false;
    TabPageNode3.IsDynamic:=false;
    TXTabSheet(TabPageNode1.ScreenObject).Caption:='GPU Kernel Code';
    TXTabSheet(TabPageNode2.ScreenObject).Caption:='Generated HTML';
    TXTabSheet(TabPageNode3.ScreenObject).Caption:='Initial Stage Matrix';
    TXTabControl(TabControlNode.ScreenObject).TabIndex:=0;



    VBNode:=AddDynamicWidget('TXVBox',GPUEditorForm,TabPageNode1,'XGPUVBox','','Left',-1);
    VB:=TXVBox(VBNode.ScreenObject);
    VB.ContainerHeight:='100%';
    VB.Border:=false;
    VBNode.IsDynamic:=false;

    HBTopNode:=AddDynamicWidget('TXHBox',GPUEditorForm,VBNode,'XGPUHBox','','Left',-1);
    HB:=TXHBox(HBTopNode.ScreenObject);
    HB.ContainerHeight:='';
    HB.Border:=false;
    HBTopNode.IsDynamic:=false;

    ComboNode:=AddDynamicWidget('TXComboBox',GPUEditorForm,HBTopNode,'XGPUComboBox','','Top',-1);
    GPUComboBox:=TXComboBox(ComboNode.ScreenObject);
    GPUComboBox.OptionList:='["Graphical (Final)"]';
    GPUComboBox.ItemIndex:=0;
    GPUComboBox.LabelPos:='Left';
    GPUComboBox.LabelText:='Select Kernel Code to Edit';
    ComboNode.IsDynamic:=false;
    GPUComboBox.myNode.registerEvent('Change',@GPUEvents.GPUComboBoxChange);

    EditNode:=AddDynamicWidget('TXEditBox',GPUEditorForm,HBTopNode,'XGPUEditBox','','Top',-1);
    GPUEditBox:=TXEditBox(EditNode.ScreenObject);
    GPUEditBox.LabelPos:='Left';
    GPUEditBox.LabelText:='Output Dimensions for this Kernel';
    GPUEditBox.ReadOnly:=true;
    GPUEditBox.Hint:='X, Y, Z dimensions - from latest settings of KernelXDims, KernelYDims, KernelZDims';
    EditNode.IsDynamic:=false;
//    GPUEditBox.myNode.registerEvent('Change',@GPUEvents.GPUEditBoxChange);

    EditorNode:=AddDynamicWidget('TXCode',GPUEditorForm,VBNode,'XGPUCodeEditor','','Left',-1);
    GPUCodeEditor:=TXCode(EditorNode.ScreenObject);
    GPUCodeEditor.ContainerHeight:='90%';
    GPUCodeEditor.ContainerWidth:='100%';
    GPUCodeEditor.MessagesHeight:='30%';
    GPUCodeEditor.LabelText:='';
    GPUCodeEditor.myNode.registerEvent('ClickMessage',@GPUEvents.GPUCodeEditHandleClickMessage);
    EditorNode.IsDynamic:=false;

    BtnNode:=AddDynamicWidget('TXButton',GPUEditorForm,MainVBNode,'XGPUDoneBtn','','Left',-1);
    GPUEditorDoneBtn:=TXButton(BtnNode.ScreenObject);
    GPUEditorDoneBtn.Caption:='Done';
    GPUEditorDoneBtn.myNode.registerEvent('ButtonClick',@GPUEvents.CloseCodeEditor);
    BtnNode.IsDynamic:=false;

    VBNode2:=AddDynamicWidget('TXVBox',GPUEditorForm,TabPageNode2,'XGPUVBox2','','Left',-1);
    VB2:=TXVBox(VBNode2.ScreenObject);
    VB2.ContainerHeight:='100%';
    VB2.Border:=false;
    VBNode2.IsDynamic:=false;

    MemoNode:=AddDynamicWidget('TXMemo',GPUEditorForm,VBNode2,'XGPUHTMLMemo','','Left',-1);
    GPUMemo:=TXMemo(MemoNode.ScreenObject);
    GPUMemo.MemoHeight:='85%';
    GPUMemo.MemoWidth:='99%';
    GPUMemo.Border:=false;
    GPUMemo.LabelPos:='Top';
    GPUMemo.LabelText:='HTML generated at the last GPU activation';
    MemoNode.IsDynamic:=false;

    BtnNode2:=AddDynamicWidget('TXButton',GPUEditorForm,VBNode2,'XGPULaunchBtn','','Left',-1);
    LaunchBtn:=TXButton(BtnNode2.ScreenObject);
    LaunchBtn.Caption:='Launch HTML in browser';
    LaunchBtn.myNode.registerEvent('ButtonClick',@GPUEvents.LaunchGPUHTML);
    LaunchBtn.Hint:='Launch the generated GPU HTML in a separate browser page to aid diagnostics';
    BtnNode2.IsDynamic:=false;

    TableNode:=AddDynamicWidget('TX3DTable',GPUEditorForm,TabPageNode3,'XGPU3DTable','','Left',-1);
    GPUTableEditor := TX3DTable(TableNode.ScreenObject);
    TableNode.IsDynamic:=false;
    GPUTableEditor.ContainerWidth:='99%';
    GPUTableEditor.ContainerHeight:='100%';

    {$ifndef JScript}
    GPUEditorTopControl:=MainVB;
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
  i,d:integer;
begin
  AllKernels:=TXGPUCanvas(EditingGPUNode.ScreenObject).FetchAllAnimCode;
  // Re-concatenate the kernel code blocks
  for i:=0 to length(AllKernels)-1 do
  begin
    if i<>idx then
    begin
      AllCode:=AllCode+AllKernels[i].CodeBlock.Text
    end
    else
    begin
      AllCode:=AllCode+GPUCodeEditor.ItemValue;
    end;

    if i<length(AllKernels)-1 then
      AllCode:=AllCode+eventListdelimiter;
  end;
  EditAttributeValue(EditingGPUNode,'AnimationCode',AllCode);
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
    // and the init Stage Array
    EditingGPUNode.SetAttributeValue('InitStageData',GPUTableEditor.Table3DData);
  end;
  // Clear the 3D table data
  GPUTableEditor.Table3DData:='[[[0]]]';
//  if ObjectInspectorSelectedNavTreeNode<>nil then
//    RefreshObjectInspector(ObjectInspectorSelectedNavTreeNode);
end;

procedure TGPUEventClass.GPUCodeEditHandleClickMessage(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var linenumber:integer;
    SelectedLine,FileName,CharPos:string;
    FoundLineNum:Boolean;
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
  {$ifndef JScript}
  GPUEvents.EditorResize(GPUEditorForm);
  {$endif}
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
    GPUCodeEditor.ItemValue:=AllKernels[GPUComboBox.ItemIndex].CodeBlock.Text;
    if  GPUComboBox.ItemIndex>0 then
      GPUEditBox.ItemValue:=TXGPUCanvas(EditingGPUNode.ScreenObject).KernelDimsString(GPUComboBox.ItemIndex-1)
    else
      GPUEditBox.ItemValue:='* pixelmap size *';
  end;
end;

procedure ShowGPUEditor(GPUNode:TDataNode;TabPage:integer);
var
  AllKernels:TAnimCodeArray;
  tmp:string;
begin
  // Edit the AnimationCode in a TXGPUCanvas component using the dedicated popup editor...
  // the animation code may consist of several kernel procedures.
  // These are delimited by the EventListDelimiter string.
  // The GPU Code Editor needs to show the first kernel proc.
  AllKernels:=TXGPUCanvas(GPUNode.ScreenObject).FetchAllAnimCode;
  GPUCodeEditor.ItemValue:=AllKernels[0].CodeBlock.Text;
  GPUCodeEditor.MessageLines:='';
  GPUCodeEditor.MessagesHeight:='1';
  GPUEditBox.ItemValue:='* pixelmap size *';
  //   GPUMemo.ItemValue:=TXGPUCanvas(ObjectInspectorSelectedCodeTreeNode.ScreenObject).GeneratedHTML;
  //!! Lazarus bug?     Have to populate GPUMemo later (eg. on tab change), otherwise the popup form crashes.
  EditingGPUNode:=GPUNode;
  GPUEditorMode:='Animation';
  GPUComboBox.OptionList:=TXGPUCanvas(EditingGPUNode.ScreenObject).BuildKernelList;
  GPUComboBox.ItemIndex:=0;
  GPUComboBox.PriorIndex:=0;
  XGPUEditor.GPUCodeEditor.ReadOnly:=false;

  GPUEditorTabControl.TabIndex:=TabPage;

  tmp:=EditingGPUNode.GetAttribute('InitStageData',true).AttribValue;
  GPUTableEditor.Table3DData:='[[[1]]]';
  GPUTableEditor.Table3DData:=EditingGPUNode.GetAttribute('InitStageData',true).AttribValue;
  {$ifndef JScript}
  GPUTableEditor.ResequenceComponents;
  {$endif}

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
    GPUCodeEditor.ItemValue:=AllKernels[targetKernel].CodeBlock.Text;

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

