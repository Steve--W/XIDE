unit X3DTable;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, XIFrame, Math,
    UtilsJSCompile, XForm, XButton, XVBox, XTabControl, XEditBox, XNumberSpinner, EventsInterface,
    PasteDialogUnit,
  {$ifndef JScript}
    fpjson  , jsonparser,
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf,
    LCLType, gettext,
  {$else}
    HTMLUtils,
  {$endif}
    WrapperPanel, Events, XTable;

type TStringArray = Array of String;
type T2DStringArray = Array of TStringArray;
type T3DStringArray = Array of T2DStringArray;

type
  TX3DTable = class(TXVBox)
  private
    { Private declarations }
    IsBuilt:Boolean;

    function GetZIndex:integer;
    function GetXDimension:integer;
    function GetYDimension:integer;
    function GetZDimension:integer;
    function GetTable3DData:string;

    procedure SetZIndex(AValue:integer);
    procedure SetXDimension(AValue:integer);
    procedure SetYDimension(AValue:integer);
    procedure SetZDimension(AValue:integer);
    procedure SetTable3DData(AValue:string);

 //   procedure SetMyEventTypes;  override;
    procedure SetPropertyDefaults;
    procedure BuildWidget;
    procedure ReBuild3DTableData;
    procedure SetNew3DTableData;
    function Construct3DTableStringFromArray(arr:T3DStringArray):String;
    procedure Resize3DData;

    {$ifndef JScript}
    procedure DoConstructor;
    {$endif}

    procedure ZSelectorChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure TableChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure XEditBoxChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure YEditBoxChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure ZEditBoxChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure PasteData(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);

  protected
    { Protected declarations }
  public
    { Public declarations }
    myTableView:TXTable;
    PasteBtn:TXButton;
    ZSelector:TXNumberSpinner;
    XDimEdit:TXEditBox;
    YDimEdit:TXEditBox;
    ZDimEdit:TXEditBox;
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    destructor Destroy; override;
    procedure ResequenceComponents;
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);  override;
    {$endif}

published
    { Published declarations }

    // Properties defined for this class...
    property ZIndex: integer read GetZIndex write SetZIndex;
    property XDimension: integer read GetXDimension write SetXDimension;
    property YDimension: integer read GetYDimension write SetYDimension;
    property ZDimension: integer read GetZDimension write SetZDimension;
    property Table3DData:string read GetTable3DData write SetTable3DData;

  end;

implementation

const MyNodeType='TX3DTable';
var
  myDefaultAttribs:TDefaultAttributesArray;

//procedure TXGPUCanvas.SetMyEventTypes;
//begin
//  MyEventTypes.Add('Click');
//end;

procedure TX3DTable.SetPropertyDefaults;
begin
end;

procedure TX3DTable.BuildWidget;
var
  TblNode,ZNumNode,XEditNode,YEditNode,ZEditNode,BtnNode:TDataNode;
begin

  ZNumNode:=AddDynamicWidget('TXNumberSpinner',self.myNode.MyForm,self.myNode,'ZSelector',self.myNode.NodeName,'Left',-1);
  ZSelector:=TXNumberSpinner(ZNumNode.ScreenObject);
  ZSelector.MinVal:=0;
  ZSelector.MaxVal:=self.ZDimension-1;
  ZSelector.ItemValue:=0;
  ZSelector.LabelPos:='Left';
  ZSelector.LabelText:='Select Z Index:';
  ZNumNode.IsDynamic:=false;
  ZSelector.myNode.registerEvent('Change',@self.ZSelectorChange);

  TblNode:=AddDynamicWidget('TXTable',self.myNode.MyForm,self.myNode,'Table',self.myNode.NodeName,'Left',-1);
  TblNode.IsDynamic:=false;
  myTableView:=TXTable(TblNode.ScreenObject);
  myTableView.TableHeight:='70%';
  myTableView.TableWidth:='98%';
  myTableView.LabelText:='';
  myTableView.HasHeaderRow:=false;
  myTableView.IsNumeric:=true;
  TblNode.registerEvent('Change',@self.TableChange);

  BtnNode:=AddDynamicWidget('TXButton',self.myNode.MyForm,self.myNode,'PasteBtn',self.myNode.NodeName,'Left',-1);
  PasteBtn:=TXButton(BtnNode.ScreenObject);
  PasteBtn.Caption:='Paste Grid';
  PasteBtn.Hint:='Paste grid data from clipboard (eg. as copied from Excel)';
  PasteBtn.myNode.registerEvent('ButtonClick',@self.PasteData);
  BtnNode.IsDynamic:=false;

  XEditNode:=AddDynamicWidget('TXEditBox',self.myNode.MyForm,self.myNode,'XNum',self.myNode.NodeName,'Left',-1);
  XDimEdit:=TXEditBox(XEditNode.ScreenObject);
  XDimEdit.LabelPos:='Left';
  XDimEdit.LabelText:='Set X Dimension (number of Columns):';
  XEditNode.IsDynamic:=false;
  XDimEdit.ItemValue:='1';
  XDimEdit.myNode.registerEvent('Change',@self.XEditBoxChange);

  YEditNode:=AddDynamicWidget('TXEditBox',self.myNode.MyForm,self.myNode,'YNum',self.myNode.NodeName,'Left',-1);
  YDimEdit:=TXEditBox(YEditNode.ScreenObject);
  YDimEdit.LabelPos:='Left';
  YDimEdit.LabelText:='Set Y Dimension (number of Rows):';
  YEditNode.IsDynamic:=false;
  YDimEdit.ItemValue:='1';
  YDimEdit.myNode.registerEvent('Change',@self.YEditBoxChange);

  ZEditNode:=AddDynamicWidget('TXEditBox',self.myNode.MyForm,self.myNode,'ZNum',self.myNode.NodeName,'Left',-1);
  ZDimEdit:=TXEditBox(ZEditNode.ScreenObject);
  ZDimEdit.LabelPos:='Left';
  ZDimEdit.LabelText:='Set Z Dimension (Z Depth):';
  ZEditNode.IsDynamic:=false;
  ZDimEdit.ItemValue:='1';
  ZDimEdit.myNode.registerEvent('Change',@self.ZEditBoxChange);

  IsBuilt:=true;
end;

{$ifndef JScript}
procedure TX3DTable.ResequenceComponents;
begin      // fix for Lazarus 'feature'....
  self.RemoveControl(ZSelector);
  self.RemoveControl(myTableView);
  self.RemoveControl(PasteBtn);
  self.RemoveControl(XDimEdit);
  self.RemoveControl(YDimEdit);
  self.InsertControl(YDimEdit,0);
  self.InsertControl(XDimEdit,0);
  self.InsertControl(PasteBtn,0);
  self.InsertControl(myTableView,0);
  self.InsertControl(ZSelector,0);
end;

function Create3DTableWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TX3DTable',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  TX3DTable(NewNode.ScreenObject).BuildWidget;

  result:=NewNode;
end;

procedure TX3DTable.DoConstructor;
begin
  IsBuilt:=false;

  self.IsContainer:=false;
  self.myNode.NodeType:='TX3DTable';
  AddDefaultAttribs(self,self.myNode,mydefaultAttribs);

  SetPropertyDefaults;
end;

constructor TX3DTable.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor;
end;

constructor TX3DTable.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor;
end;

destructor TX3DTable.Destroy;
begin
  if (not (csDesigning in componentState)) then
  begin
  end;
  inherited Destroy;
end;

{$else}
constructor TX3DTable.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(MyForm,NodeName,NameSpace);
  self.NodeType:='TX3DTable';
  self.IsContainer:=false;
  IsBuilt:=false;

  SetNodePropDefaults(self,myDefaultAttribs);
  SetPropertyDefaults;
end;

function Create3DTableWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  ShowBorder:boolean;
  myObj:TX3DTable;
  OnClickString:String;
begin

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', '''');" ';

  asm
  try{
      var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

      var HTMLString='';
      var wrapperid = NameSpace+ScreenObjectName;
      var MyObjectName=wrapperid+'Contents';

      HTMLString = '<div  id="'+MyObjectName+'" class="vboxNoStretch '+NameSpace+ScreenObjectName+'" '  +
                     ' style="height:100%;width:100%; "' +
                     OnClickString +
                     '></div>  ';

      var wrapper=document.getElementById(wrapperid);
      wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }catch(err) { alert(err.message+'  in XVBox.CreateVHBox');}
  end;
  MyNode.ScreenObject:=MyNode;


  TX3DTable(MyNode.ScreenObject).BuildWidget;
  RefreshComponentProps(myNode);

  result:=myNode;
end;



function CreateinterfaceObj3DTable(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TX3DTable.Create(MyForm,NodeName,NameSpace));
end;
{$endif}

function JsonStringTo3DStringArray(str:String):T3DStringArray;
{$ifndef JScript}
var
   Data,zData : TJSONData;
   zCount:integer;
   arr:T3DStringArray;
   ArrayStr:String;
    zItem,yItem,xItem : TJSONData;
    z,y,x:integer;
    object_type:string;

begin
  // "[[[...],[...]],[[...]]]"
  setlength(arr,0);
  try
    Data := GetJSON(str);
  except
    on E: Exception do
    begin
      showmessage('JSON error: '+e.Message);
      Data := nil;
    end;
  end;
  if Data<>nil then
  begin
    ArrayStr:=str;

    zData := GetJSON(ArrayStr);
    zcount:=zData.Count;
    setlength(arr,zCount);
    for z :=0 to zcount-1 do
    begin
      zItem := zData.Items[z];
      setlength(arr[z],zItem.Count);
      for y:=0 to zItem.Count-1 do
      begin
        yItem := zItem.Items[y];
        setlength(arr[z,y],yItem.Count);
        object_type := GetEnumName(TypeInfo(TJSONtype), Ord(yItem.JSONType));
        if object_type='jtArray' then
        begin
          for x:=0 to yItem.Count-1 do
          begin
            xItem:= yItem.Items[x];
            object_type := GetEnumName(TypeInfo(TJSONtype), Ord(xItem.JSONType));
            if object_type='jtString' then
            begin
              arr[z,y,x]:=QuoteIt(xItem.AsString);
            end
            else if object_type='jtNumber' then
            begin
              arr[z,y,x]:=xItem.AsString;
            end
            else
              arr[z,y,x]:='""';
          end;
        end;
      end;
    end;
  end;

  result:=arr;
end;
{$else}
var
   arr:T3DStringArray;
begin
  // "[[[...],[...]],[[...]]]"
  setlength(arr,0);
  asm
    arr = JSON.parse(str);
  end;
  result:=arr;
end;
{$endif}

function TX3DTable.Construct3DTableStringFromArray(arr:T3DStringArray):String;
var
    z:integer;
    str:String;
begin
  for z:=0 to length(arr)-1 do
  begin
    if z>0 then
      str:=str+',';
    str:=str+myTableView.ConstructTableStringFromArray(arr[z]);
  end;
  result:='['+str+']';
end;

procedure TX3DTable.SetNew3DTableData;
var
    arr:T3DStringArray;
    z:integer;
    str,zData:String;
begin
//  Redisplay the data for the current Z index, from new 3d data string
  str:=self.Table3DData;
  arr:=JsonStringTo3DStringArray(self.Table3DData);
  z:=self.ZSelector.ItemValue;
  if z>length(arr)-1 then
  begin
    z:=length(arr)-1;
    self.ZSelector.ItemValue:=z;        // !!!!fires ZSelectorChange...comes back here
  end;
  if z<0 then z:=0;
  self.ZDimEdit.ItemValue:=inttostr(length(arr));
  if length(arr)>0 then
  begin
    self.YDimEdit.ItemValue:=inttostr(length(arr[0]));
    if length(arr[0])>0 then
      self.XDimEdit.ItemValue:=inttostr(length(arr[0,0]))
    else
      self.XDimEdit.ItemValue:='';
  end
  else
  begin
    self.YDimEdit.ItemValue:='';
    self.XDimEdit.ItemValue:='';
  end;
(*  {$ifdef JScript}
  asm
  console.log('ZDIM='+arr.length);
  console.log('YDIM='+arr[0].length);
  console.log('XDIM='+arr[0][0].length);
  var s='';
  for (var i=0; i<arr.length; i++) {
    console.log('i='+i);
    for (var j=0; j<arr[i].length; j++) {
      console.log('s=>'+s+'<');
      console.log('j='+j);
      s='';
      for (var k=0; k<arr[i][j].length; k++) {
        if (k>0) {s=s+',';}
        s=s+arr[i][j][k];
      }
    }
    console.log('s=>'+s+'<');
  }
  console.log('calling ConstructTableStringFromArray. z='+z);
  console.log('arr[z]='+arr[z]);
  end;
  {$endif}
  *)

  if length(arr)>0 then
  begin
    zData:=myTableView.ConstructTableStringFromArray(arr[z]);
    self.myTableView.TableData:=zData;
  end;
end;

procedure TX3DTable.ReBuild3DTableData;
var
    arr:T3DStringArray;
    z:integer;
    zData:T2DStringArray;
    NewData:String;
begin
//  Rebuild the full TableData string, substituting the current Z Table's data at the relevant Z-index
  arr:=JsonStringTo3DStringArray(self.Table3DData);
  z:=self.ZSelector.ItemValue;
  zData:=self.myTableView.GetCellsAsArray(false);
  arr[z]:=zData;
  NewData:=self.Construct3DTableStringFromArray(arr);
  self.myNode.SetAttributeValue('Table3DData',NewData);
end;

procedure TX3DTable.Resize3DData;
var
    arr,newArr:T3DStringArray;
    x,y,z:integer;
    xdim,ydim:integer;
    NewData:String;
begin
  // X or Y dimension has been changed
  ydim:=strtoint(self.YDimEdit.ItemValue);
  xdim:=strtoint(self.XDimEdit.ItemValue);
  setlength(newArr,self.ZSelector.MaxVal+1);
  arr:=JsonStringTo3DStringArray(self.Table3DData);
  for z:=0 to length(newArr)-1 do
  begin
    setlength(NewArr[z],ydim);
    if z<length(arr) then
    begin
      for y:=0 to ydim-1 do
      begin
        setlength(NewArr[z,y],xdim);
        if y<length(arr[z]) then
        begin
          for x:=0 to xdim-1 do
          begin
            if x<length(arr[z,y]) then
              NewArr[z,y,x]:=arr[z,y,x]
            else
              NewArr[z,y,x]:='';
          end;
        end
        else
          for x:=0 to xdim-1 do
            NewArr[z,y,x]:='';
      end;
    end
    else
    begin
      for y:=0 to ydim-1 do
      begin
        setlength(NewArr[z,y],xdim);
        for x:=0 to xdim-1 do
          NewArr[z,y,x]:='';
      end;
    end;
  end;
  NewData:=self.Construct3DTableStringFromArray(NewArr);
  self.Table3DData:=NewData;
end;

procedure TX3DTable.ZSelectorChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  //{$ifdef JScript}showmessage('3d ZSelectorChange');{$endif}
  // Get the table data layer for this Z index, and display.
  self.SetNew3DTableData;
end;
procedure TX3DTable.TableChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  //{$ifdef JScript}showmessage('3d TableChange');{$endif}
  self.ReBuild3DTableData;
end;
procedure TX3DTable.XEditBoxChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  //{$ifdef JScript}showmessage('3d XDim Change');{$endif}
  self.XDimension:=strtoint(myValue);
  self.ReSize3DData;
end;
procedure TX3DTable.YEditBoxChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
 // {$ifdef JScript}showmessage('3d YDim Change');{$endif}
  self.YDimension:=strtoint(myValue);
  self.ReSize3DData;
end;
procedure TX3DTable.ZEditBoxChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
 // {$ifdef JScript}showmessage('3d ZDim Change');{$endif}
  self.ZDimension:=strtoint(myValue);
  self.ZSelector.MaxVal:=strtoint(myValue)-1;
  self.ReSize3DData;
end;
procedure TX3DTable.PasteData(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
    str:String;
    msg:string;
    i:integer;
begin
  //paste Excel-format data to this grid
   if (e=nil)  or (e.InitRunning=false) then
   begin
     //showmessage('new e');
     if (e=nil) then
     begin
       e:=TEventStatus.Create('Click',nodeId);
     end;
     e.initRunning:=true;
     PasteDialogUnit.CompletionEvent:=e;
     {$ifdef JScript}
     e.AsyncProcsRunning.Add('CopyFromClip');
     {$endif}

   end
   else
   begin
     e.InitRunning:=false;
   end;

   if e.InitRunning then
   begin
     PasteDoneBtn.IsVisible:=false;
     str:=mygetClipboardData('Grid');
   end;

   if e.EventHasWaitingAsyncProcs = false then
   // this is lazarus and a confirm dialog is not needed
   // otherwise this is HTML and we have waited for a ctrl-V event from the PasteDialog form
   begin
     {$ifdef JScript}
     asm
       pas.NodeUtils.StartingUp=false;
       var pasteTarget = document.getElementById('PasteTargetContents');
       str = pasteTarget.value;
       //alert('Paste string = >'+str+'<' );
     end;
     {$endif}
//     ShowAllChars(str);
     if trim(str)<>'' then
     begin
       self.myTableView.LoadTableFromExcelCopy(str);
       if (self.ZDimension>1)
       and ((self.XDimension<>self.myTableView.NumCols)
       or (self.YDimension<>self.myTableView.NumRows)) then
         showmessage('X,Y dimensions have changed - this will affect data at all Z levels');
       self.XDimension:=self.myTableView.NumCols;
       self.YDimension:=self.myTableView.NumRows;
       self.XDimEdit.ItemValue:=inttostr(self.XDimension);
       self.YDimEdit.ItemValue:=inttostr(self.YDimension);
       self.ReBuild3DTableData;
       self.ReSize3DData;
     end
     else
       showmessage('Nothing on clipboard to paste');
   end;
 end;


function TX3DTable.GetZIndex:integer;
begin
  result:=StrToInt(myNode.getAttribute('ZIndex',true).AttribValue);
end;
function TX3DTable.GetXDimension:integer;
begin
  result:=StrToInt(myNode.getAttribute('XDimension',true).AttribValue);
end;
function TX3DTable.GetYDimension:integer;
begin
  result:=StrToInt(myNode.getAttribute('YDimension',true).AttribValue);
end;
function TX3DTable.GetZDimension:integer;
var
    v:String;
begin
  v:=myNode.getAttribute('ZDimension',true).AttribValue;
  if v<>'' then
    result:=StrToInt(v)
  else
    result:=0;
end;
function TX3DTable.GetTable3DData:string;
begin
  result:=myNode.getAttribute('Table3DData',true).AttribValue;
end;

procedure TX3DTable.SetZIndex(AValue:integer);
begin
  myNode.SetAttributeValue('ZIndex',IntToStr(AValue),'Integer');
end;
procedure TX3DTable.SetXDimension(AValue:integer);
begin
  myNode.SetAttributeValue('XDimension',IntToStr(AValue),'Integer');
end;
procedure TX3DTable.SetYDimension(AValue:integer);
begin
  myNode.SetAttributeValue('YDimension',IntToStr(AValue),'Integer');
end;
procedure TX3DTable.SetZDimension(AValue:integer);
begin
  myNode.SetAttributeValue('ZDimension',IntToStr(AValue),'Integer');
end;
procedure TX3DTable.SetTable3DData(AValue:string);
begin
  myNode.SetAttributeValue('Table3DData',AValue,'String');
  if self.ZSelector<>nil then
    SetNew3DTableData;

end;

begin
AddWrapperDefaultAttribs(myDefaultAttribs);
AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','','',false);
AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','','',false);
AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
AddDefaultAttribute(myDefaultAttribs,'InheritColor','Boolean','False','',false);
AddDefaultAttribute(myDefaultAttribs,'Table3DData','String','[[["z"]]]','',false);
AddDefaultAttribute(myDefaultAttribs,'XDimension','Integer','1','',false,false);
AddDefaultAttribute(myDefaultAttribs,'YDimension','Integer','1','',false,false);
AddDefaultAttribute(myDefaultAttribs,'ZDimension','Integer','1','',false,false);
AddDefaultAttribute(myDefaultAttribs,'ZIndex','Integer','0','',false);

AddDefaultsToTable(MyNodeType,myDefaultAttribs);

AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
{$ifndef JScript}
RegisterClass(TX3DTable);
AddNodeFuncLookup(MyNodeType,@Create3DTableWidget);
{$else}
AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj3DTable,@Create3DTableWidget);
{$endif}

end.

