unit testfile_SetWithLinebreaks;

interface

implementation

procedure Bla;
begin
  FAllowedChildTypes := [ntElement_Node,
    ntText_Node,
    ntCDATA_Section_Node,
    ntEntity_Reference_Node,
    ntProcessing_Instruction_Node,
    ntComment_Node,
    ntDocument_Type_Decl_Node,
    ntDocument_Fragment_Node,
    ntNotation_Node];
end;

end.
