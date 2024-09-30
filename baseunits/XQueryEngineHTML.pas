unit XQueryEngineHTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery, xquery_json, simplehtmltreeparser;

type

  { TXQueryEngineHTML }

  TXQueryEngineHTML = class
  private
    FEngine: TXQueryEngine;
    FTreeParser: TTreeParser;
    function Eval(const Expression: string): IXQValue; overload;
    function Eval(const Expression: string; const isCSS: boolean): IXQValue; overload;
    function Eval(const Expression: string; const isCSS: boolean;
      const ContextItem: IXQValue; const Tree: TTreeNode): IXQValue; overload;
    function Eval(const Expression: string; const isCSS: boolean;
      const ContextItem: IXQValue): IXQValue; overload;
    function EvalString(const Expression: string): String; overload;
    function EvalString(const Expression: string; const isCSS: boolean): String; overload;
    function EvalString(const Expression: String; const isCSS: Boolean;
      const ContextItem: IXQValue): String; inline;
    function EvalString(const Expression: String; const isCSS: Boolean;
      const ContextItem: IXQValue; const Tree: TTreeNode): String; inline;
    function EvalCount(const Expression: string): Integer; overload;
    function EvalCount(const Expression: string; const isCSS: boolean): Integer; overload;
    function EvalCount(const Expression: String; const isCSS: Boolean;
      const ContextItem: IXQValue): Integer; inline;
    function EvalCount(const Expression: String; const isCSS: Boolean;
      const ContextItem: IXQValue; const Tree: TTreeNode): Integer; inline;
    function EvalStringAll(const Expression: string): String; overload;
    function EvalStringAll(const Expression: string; const isCSS: boolean): String; overload;
    function EvalStringAll(const Expression: String; const isCSS: Boolean;
      const Separator: String; const ContextItem: IXQValue): String; overload;
    function EvalStringAll(const Expression: String; const isCSS: Boolean;
      const Exc: array of String): String; overload;
    function EvalStringAll(const Expression: String; const isCSS: Boolean;
      const Exc: array of String; const Separator: String;
      const ContextItem: IXQValue): String; overload;
    procedure EvalStringAll(const Expression: String; const isCSS: Boolean;
      const TheStrings: TStrings); overload;
    procedure EvalStringAll(const Expression: String; const isCSS: Boolean;
      const TheStrings: TStrings; ContextItem: IXQValue); overload;

  public
    constructor Create(const HTML: String = ''); overload;
    constructor Create(const HTMLStream: TStream); overload;
    destructor Destroy; override;
    procedure ParseHTML(const HTML: String); overload;
    procedure ParseHTML(const HTMLStream: TStream); overload;
    // xpath
    function XPath(const Expression: String; const Tree: TTreeNode = nil): IXQValue; inline;
    function XPath(const Expression: String; const ContextItem: IXQValue): IXQValue; inline;
    function XPathString(const Expression: String; const Tree: TTreeNode = nil): String; inline;
    function XPathString(const Expression: String; const ContextItem: IXQValue): String; inline;
    function XPathCount(const Expression: String; const Tree: TTreeNode = nil): Integer; inline;
    function XPathCount(const Expression: String; const ContextItem: IXQValue): Integer; inline;
    function XPathStringAll(const Expression: String): String; overload; inline;
    function XPathStringAll(const Expression: String; const Separator: String): String; overload; inline;
    function XPathStringAll(const Expression: String; const Separator: String;
      const ContextItem: IXQValue): String; overload; inline;   
    function XPathStringAll(const Expression: String; const Exc: array of String): String; overload; inline;
    function XPathStringAll(const Expression: String; const Exc: array of String; const Separator: String): String; overload; inline;
    function XPathStringAll(const Expression: String; const Exc: array of String;
      const Separator: String; const ContextItem: IXQValue): String; overload; inline;  
    procedure XPathStringAll(const Expression: String; const TheStrings: TStrings); overload; inline;
    procedure XPathStringAll(const Expression: String; const TheStrings: TStrings;
      const ContextItem: IXQValue); overload; inline;                     
    procedure XPathHREFAll(const Expression: String; const ALinks, ATexts: TStrings); overload;
    procedure XPathHREFAll(const Expression: String; const ALinks, ATexts: TStrings;
      const ContextItem: IXQValue); 
    procedure XPathHREFtitleAll(const Expression: String; const ALinks, ATitles: TStrings); overload;
    procedure XPathHREFtitleAll(const Expression: String; const ALinks, ATitles: TStrings;
      const ContextItem: IXQValue);
    // css
    function CSS(const Expression: String; const Tree: TTreeNode = nil): IXQValue; inline;
    function CSS(const Expression: String; const ContextItem: IXQValue): IXQValue; inline;
    function CSSString(const Expression: String; const Tree: TTreeNode = nil): String; inline;
    function CSSString(const Expression: String; const ContextItem: IXQValue): String; inline; 
    function CSSStringAll(const Expression: String): String; overload; inline;
    function CSSStringAll(const Expression: String; const Separator: String): String; overload; inline;
    function CSSStringAll(const Expression: String; const Separator: String;
      const ContextItem: IXQValue): String; overload; inline;  
    function CSSStringAll(const Expression: String; const Exc: array of String): String; overload; inline;
    function CSSStringAll(const Expression: String; const Exc: array of String;
      const Separator: String): String; overload; inline;
    function CSSStringAll(const Expression: String; const Exc: array of String;
      const Separator: String; const ContextItem: IXQValue): String; overload; inline; 
    procedure CSSStringAll(const Expression: String; const TheStrings: TStrings); overload; inline;
    procedure CSSStringAll(const Expression: String; const TheStrings: TStrings;
      const ContextItem: IXQValue); overload; inline;

    property Engine: TXQueryEngine read FEngine;
    property TreeParser: TTreeParser read FTreeParser;
  end;

  IXQValue = xquery.IXQValue;
  TTreeNode = simplehtmltreeparser.TTreeNode;

function XPathString(const Expression, HTMLString: String): String; overload;
function XPathString(const Expression: String; const HTMLStream: TStream): String; overload; inline;
function XPathCount(const Expression: String; const HTMLString: String): Integer; overload;
function XPathCount(const Expression: String; const HTMLStream: TStream): Integer; overload; inline;
procedure XPathStringAll(const Expression: String; const HTMLString: String; const TheStrings: TStrings); overload;
procedure XPathStringAll(const Expression: String; const HTMLStream: TStream; const TheStrings: TStrings); overload; inline;
procedure XPathHREFAll(const Expression: String; const HTMLString: String; const ALinks, ATexts: TStrings); overload;
procedure XPathHREFAll(const Expression: String; const HTMLStream: TStream; const ALinks, ATexts: TStrings); overload; inline;
procedure XPathHREFtitleAll(const Expression: String; const HTMLString: String; const ALinks, ATitles: TStrings); overload;
procedure XPathHREFtitleAll(const Expression: String; const HTMLStream: TStream; const ALinks, ATitles: TStrings); overload; inline;

implementation

function StreamToString(const Stream: TStream): String;
var
  p, x: Int64;
begin
  p := Stream.Position;
  Stream.Position := 0;
  Setlength(Result, Stream.Size);
  x := Stream.Read(PChar(Result)^, Stream.Size);
  SetLength(Result, x);
  Stream.Position := p;
end;

procedure AddSeparatorString(var Dest: String; const S: String; const Separator: String = ', ');
begin
  if Trim(S) <> '' then
    if Trim(Dest) = '' then
      Dest := Trim(S)
    else
      Dest := Trim(Dest) + Separator + Trim(S);
end;

function StringInArray(const S: String; const SS: array of String): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Length(SS) > 0 then
    for i := Low(SS) to High(SS) do
      if SameText(S, SS[i]) then
        Exit;
  Result := False;
end;

function XPathString(const Expression, HTMLString: String): String;
begin
  Result := '';
  with TXQueryEngineHTML.Create(HTMLString) do
    try
      Result := XPathString(Expression);
    finally
      Free;
    end;
end;

function XPathString(const Expression: String; const HTMLStream: TStream): String;
begin
  Result := XPathString(Expression, StreamToString(HTMLStream));
end;

function XPathCount(const Expression: String; const HTMLString: String): Integer;
begin
  Result := 0;
  with TXQueryEngineHTML.Create(HTMLString) do
    try
      Result := XPathCount(Expression);
    finally
      Free;
    end;
end;

function XPathCount(const Expression: String; const HTMLStream: TStream): Integer;
begin
  Result := XPathCount(Expression, StreamToString(HTMLStream));
end;

procedure XPathStringAll(const Expression: String; const HTMLString: String;
  const TheStrings: TStrings);
begin
  with TXQueryEngineHTML.Create(HTMLString) do
    try
      XPathStringAll(Expression, TheStrings);
    finally
      Free;
    end;
end;

procedure XPathStringAll(const Expression: String; const HTMLStream: TStream;
  const TheStrings: TStrings);
begin
  XPathStringAll(Expression, StreamToString(HTMLStream), TheStrings);
end;

procedure XPathHREFAll(const Expression: String; const HTMLString: String;
  const ALinks, ATexts: TStrings);
begin
  with TXQueryEngineHTML.Create(HTMLString) do
    try
      XPathHREFAll(Expression, ALinks, ATexts);
    finally
      Free;
    end;
end;

procedure XPathHREFAll(const Expression: String; const HTMLStream: TStream;
  const ALinks, ATexts: TStrings);
begin
  XPathHREFAll(Expression, StreamToString(HTMLStream), ALinks, ATexts);
end;

procedure XPathHREFtitleAll(const Expression: String; const HTMLString: String;
  const ALinks, ATitles: TStrings);
begin
  with TXQueryEngineHTML.Create(HTMLString) do
    try
      XPathHREFtitleAll(Expression, ALinks, ATitles);
    finally
      Free;
    end;
end;

procedure XPathHREFtitleAll(const Expression: String;
  const HTMLStream: TStream; const ALinks, ATitles: TStrings);
begin
  XPathHREFtitleAll(Expression, StreamToString(HTMLStream), ALinks, ATitles);
end;

{ TXQueryEngineHTML }

function TXQueryEngineHTML.Eval(const Expression: String): IXQValue;
begin
  Result := Eval(Expression, False, Default(IXQValue), nil);  // Default values
end;

function TXQueryEngineHTML.Eval(const Expression: String; const isCSS: Boolean): IXQValue;
begin
  Result := Eval(Expression, isCSS, Default(IXQValue), nil);  // Default values
end;

function TXQueryEngineHTML.Eval(const Expression: String; const isCSS: Boolean;
  const ContextItem: IXQValue): IXQValue;
begin
  Result := Eval(Expression, isCSS, ContextItem, nil);  // Default values
end;

function TXQueryEngineHTML.Eval(const Expression: String; const isCSS: Boolean;
  const ContextItem: IXQValue; const Tree: TTreeNode): IXQValue;
begin
  Result := xqvalue();
  try
    if Pointer(ContextItem) <> nil then //Assigned(ContextItem)
    begin
      if isCSS then
        Result := FEngine.evaluateCSS3(Expression, ContextItem)
      else
        Result := FEngine.evaluateXPath(Expression, ContextItem);
    end
    else if Assigned(Tree) then
    begin
      if isCSS then
        Result := FEngine.evaluateCSS3(Expression, Tree)
      else
        Result := FEngine.evaluateXPath(Expression, Tree);
    end
    else
    begin
      if isCSS then
        Result := FEngine.evaluateCSS3(Expression, FTreeParser.getLastTree)
      else
        Result := FEngine.evaluateXPath(Expression, FTreeParser.getLastTree);
    end;
  except
  end;
end;
      
function TXQueryEngineHTML.EvalString(const Expression: String): String;
begin
  Result := EvalString(Expression, False, Default(IXQValue), nil);  // Default values
end;

function TXQueryEngineHTML.EvalString(const Expression: String; const isCSS: Boolean): String;
begin
  Result := EvalString(Expression, isCSS, Default(IXQValue), nil);  // Default values
end;

function TXQueryEngineHTML.EvalString(const Expression: String;
  const isCSS: Boolean; const ContextItem: IXQValue): String;
begin
  Result := EvalString(Expression, isCSS, ContextItem, nil);  // Default values
end;

function TXQueryEngineHTML.EvalString(const Expression: String;
  const isCSS: Boolean; const ContextItem: IXQValue; const Tree: TTreeNode
  ): String;
begin
  Result := Eval(Expression, isCSS, ContextItem, Tree).toString;
end;
                
function TXQueryEngineHTML.EvalCount(const Expression: String): Integer;
begin
  Result := EvalCount(Expression, False, Default(IXQValue), nil);  // Default values
end;

function TXQueryEngineHTML.EvalCount(const Expression: String; const isCSS: Boolean): Integer;
begin
  Result := EvalCount(Expression, isCSS, Default(IXQValue), nil);  // Default values
end;

function TXQueryEngineHTML.EvalCount(const Expression: String;
  const isCSS: Boolean; const ContextItem: IXQValue): Integer;
begin
  Result := EvalCount(Expression, isCSS, ContextItem, nil);  // Default values
end;

function TXQueryEngineHTML.EvalCount(const Expression: String;
  const isCSS: Boolean; const ContextItem: IXQValue; const Tree: TTreeNode
  ): Integer;
begin
  Result := Eval(Expression, isCSS, ContextItem, Tree).Count;
end;

function TXQueryEngineHTML.EvalStringAll(const Expression: String): String;
begin
  Result := EvalStringAll(Expression, False, ', ', Default(IXQValue));  // Default values
end;

function TXQueryEngineHTML.EvalStringAll(const Expression: String; const isCSS: Boolean): String;
begin
  Result := EvalStringAll(Expression, isCSS, ', ', Default(IXQValue));  // Default values
end;

function TXQueryEngineHTML.EvalStringAll(const Expression: String; const isCSS: Boolean;
  const Separator: String; const ContextItem: IXQValue): String;
var
  v: IXQValue;
begin
  Result := '';
  for v in Eval(Expression, isCSS, ContextItem) do
    AddSeparatorString(Result, v.toString, Separator);
end;

function TXQueryEngineHTML.EvalStringAll(const Expression: String; const isCSS: Boolean; const Exc: array of String): String;
begin
  Result := EvalStringAll(Expression, isCSS, Exc, ', ', Default(IXQValue));  // Default values
end;

function TXQueryEngineHTML.EvalStringAll(const Expression: String; const isCSS: Boolean;
  const Exc: array of String; const Separator: String; const ContextItem: IXQValue
  ): String;
var
  v: IXQValue;
begin
  Result := '';
  for v in Eval(Expression, isCSS, ContextItem) do
    if not StringInArray(Trim(v.toString), Exc) then
      AddSeparatorString(Result, v.toString, Separator);
end; 

procedure TXQueryEngineHTML.EvalStringAll(const Expression: String; const isCSS: Boolean;
  const TheStrings: TStrings);
begin
  EvalStringAll(Expression, isCSS, TheStrings, Default(IXQValue));  // Default values
end;

procedure TXQueryEngineHTML.EvalStringAll(const Expression: String; const isCSS: Boolean;
  const TheStrings: TStrings; ContextItem: IXQValue);
var
  v: IXQValue;
begin
  for v in Eval(Expression, isCSS, ContextItem) do
    TheStrings.Add(Trim(v.toString));
end;

constructor TXQueryEngineHTML.Create(const HTML: String);
begin
  FEngine := TXQueryEngine.Create;
  FTreeParser := TTreeParser.Create;
  with FTreeParser do
  begin
    parsingModel := pmHTML;
    repairMissingStartTags := True;
    repairMissingEndTags := True;
    trimText := False;
    readComments := False;
    readProcessingInstructions := False;
    autoDetectHTMLEncoding := False;
    if HTML <> '' then
      parseTree(HTML);
  end;
end;

constructor TXQueryEngineHTML.Create(const HTMLStream: TStream);
begin
  if Assigned(HTMLStream) then
    Create(StreamToString(HTMLStream))
  else
    Create('');
end;

destructor TXQueryEngineHTML.Destroy;
begin
  FEngine.Free;
  FTreeParser.Free;
  inherited Destroy;
end;

procedure TXQueryEngineHTML.ParseHTML(const HTML: String);
begin
  if HTML <> '' then
    FTreeParser.parseTree(HTML);
end;

procedure TXQueryEngineHTML.ParseHTML(const HTMLStream: TStream);
begin
  ParseHTML(StreamToString(HTMLStream));
end;

function TXQueryEngineHTML.XPath(const Expression: String; const Tree: TTreeNode): IXQValue;
begin
  Result := Eval(Expression, False, Default(IXQValue), Tree);
end;

function TXQueryEngineHTML.XPath(const Expression: String; const ContextItem: IXQValue
  ): IXQValue;
begin
  Result := Eval(Expression, False, ContextItem);
end;

function TXQueryEngineHTML.XPathString(const Expression: String; const Tree: TTreeNode): String;
begin
  Result := EvalString(Expression, False, Default(IXQValue), Tree);
end;

function TXQueryEngineHTML.XPathString(const Expression: String;
  const ContextItem: IXQValue): String;
begin
  Result := EvalString(Expression, False, ContextItem);
end;

function TXQueryEngineHTML.XPathCount(const Expression: String;
  const Tree: TTreeNode): Integer;
begin
  Result := EvalCount(Expression, False, Default(IXQValue), Tree);
end;

function TXQueryEngineHTML.XPathCount(const Expression: String;
  const ContextItem: IXQValue): Integer;
begin
  Result := EvalCount(Expression, False, ContextItem);
end;

function TXQueryEngineHTML.XPathStringAll(const Expression: String): String;
begin
  Result := XPathStringAll(Expression, ', ', Default(IXQValue));
end;

function TXQueryEngineHTML.XPathStringAll(const Expression: String;
  const Separator: String): String;
begin
  Result := XPathStringAll(Expression, Separator, Default(IXQValue));
end;

function TXQueryEngineHTML.XPathStringAll(const Expression: String;
  const Separator: String; const ContextItem: IXQValue): String;
begin
  Result := EvalStringAll(Expression, False, Separator, ContextItem);
end;

function TXQueryEngineHTML.XPathStringAll(const Expression: String;
  const Exc: array of String): String;
begin
  Result := XPathStringAll(Expression, Exc, ', ', Default(IXQValue));
end;

function TXQueryEngineHTML.XPathStringAll(const Expression: String;
  const Exc: array of String; const Separator: String): String;
begin
  Result := XPathStringAll(Expression, Exc, Separator, Default(IXQValue));
end;

function TXQueryEngineHTML.XPathStringAll(const Expression: String;
  const Exc: array of String; const Separator: String; const ContextItem: IXQValue
  ): String;
begin
  Result := EvalStringAll(Expression, False, Exc, Separator, ContextItem);
end;

procedure TXQueryEngineHTML.XPathStringAll(const Expression: String;
  const TheStrings: TStrings);
begin
  XPathStringAll(Expression, TheStrings, Default(IXQValue));
end;

procedure TXQueryEngineHTML.XPathStringAll(const Expression: String;
  const TheStrings: TStrings; const ContextItem: IXQValue);
begin
  EvalStringAll(Expression, False, TheStrings, ContextItem);
end;

procedure TXQueryEngineHTML.XPathHREFAll(const Expression: String;
  const ALinks, ATexts: TStrings);
begin
  XPathHREFAll(Expression, ALinks, ATexts, Default(IXQValue));
end;

procedure TXQueryEngineHTML.XPathHREFAll(const Expression: String;
  const ALinks, ATexts: TStrings; const ContextItem: IXQValue);
var
  v: IXQValue;
begin
  for v in Eval(Expression, False, ContextItem) do
  begin
    ALinks.Add(v.toNode.getAttribute('href'));
    ATexts.Add(Trim(v.toString));
  end;
end;

procedure TXQueryEngineHTML.XPathHREFtitleAll(const Expression: String;
  const ALinks, ATitles: TStrings);
begin
  XPathHREFtitleAll(Expression, ALinks, ATitles, Default(IXQValue));
end;

procedure TXQueryEngineHTML.XPathHREFtitleAll(const Expression: String;
  const ALinks, ATitles: TStrings; const ContextItem: IXQValue);
var
  v: IXQValue;
begin
  for v in Eval(Expression, False, ContextItem) do
  begin
    ALinks.Add(v.toNode.getAttribute('href'));
    ATitles.Add(v.toNode.getAttribute('title'));
  end;
end;

function TXQueryEngineHTML.CSS(const Expression: String; const Tree: TTreeNode): IXQValue;
begin
  Result := Eval(Expression, True, Default(IXQValue), Tree);
end;

function TXQueryEngineHTML.CSS(const Expression: String; const ContextItem: IXQValue
  ): IXQValue;
begin
  Result := Eval(Expression, True, ContextItem);
end;

function TXQueryEngineHTML.CSSString(const Expression: String; const Tree: TTreeNode): String;
begin
  Result := EvalString(Expression, True, Default(IXQValue), Tree);
end;

function TXQueryEngineHTML.CSSString(const Expression: String;
  const ContextItem: IXQValue): String;
begin
  Result := EvalString(Expression, True, ContextItem);
end;

function TXQueryEngineHTML.CSSStringAll(const Expression: String): String;
begin
  Result := CSSStringAll(Expression, ', ', Default(IXQValue));
end;

function TXQueryEngineHTML.CSSStringAll(const Expression: String;
  const Separator: String): String;
begin
  Result := CSSStringAll(Expression, Separator, Default(IXQValue));
end;

function TXQueryEngineHTML.CSSStringAll(const Expression: String;
  const Separator: String; const ContextItem: IXQValue): String;
begin
  Result := EvalStringAll(Expression, True, Separator, ContextItem);
end;

function TXQueryEngineHTML.CSSStringAll(const Expression: String;
  const Exc: array of String): String;
begin
  Result := CSSStringAll(Expression, ', ', Default(IXQValue));
end;

function TXQueryEngineHTML.CSSStringAll(const Expression: String;
  const Exc: array of String; const Separator: String): String;
begin
  Result := CSSStringAll(Expression, Separator, Default(IXQValue));
end;

function TXQueryEngineHTML.CSSStringAll(const Expression: String;
  const Exc: array of String; const Separator: String; const ContextItem: IXQValue
  ): String;
begin
  Result := EvalStringAll(Expression, True, Exc, Separator, ContextItem);
end;

procedure TXQueryEngineHTML.CSSStringAll(const Expression: String;
  const TheStrings: TStrings);
begin
  CSSStringAll(Expression, TheStrings, Default(IXQValue));
end;

procedure TXQueryEngineHTML.CSSStringAll(const Expression: String;
  const TheStrings: TStrings; const ContextItem: IXQValue);
begin
  EvalStringAll(Expression, True, TheStrings, ContextItem);
end;

end.
