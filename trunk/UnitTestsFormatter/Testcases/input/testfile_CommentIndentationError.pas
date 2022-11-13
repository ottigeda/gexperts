unit XDOM_3_1;

interface

type
  TXmlSignal = class(TUtilsNoRefCount, IDomLocator)
  public
    property Reader: TXmlCustomReader read FReader;
    { IDomLocator interface properties: }      // xxx Revisit
    property EndByteNumber: Int64 read GetEndByteNumber;
  end;

implementation

end.
