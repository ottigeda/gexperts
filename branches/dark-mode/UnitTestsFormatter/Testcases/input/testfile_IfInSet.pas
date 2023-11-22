unit testfile_IfInSet;

interface

implementation

procedure bla;
begin
  if not (node.nodeType in [ntElement_Node,
      ntAttribute_Node,
      ntText_Node,
      ntXPath_Namespace_Node]) then
    blub;
end;

end.