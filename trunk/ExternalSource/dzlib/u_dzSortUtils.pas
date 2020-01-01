unit u_dzSortUtils;

{$INCLUDE 'dzlib.inc'}

interface

// To use any of the sorting algorithms, you need to supply either the two callbacks
// TCompareItemsMeth and TSwapItemsMeth or pass a class implementing IQSDataHandler
// ("QS" stands for "QuickSort" from the time when there was only one sorting algorithm)

type
  ///<summary>
  /// Compare items at the given indexes and return values similar to the standard Delphi
  /// Compare functions e.g. CompareStr or CompareText
  /// @returns 0 if they are equal
  ///          <0 if Item[Idx1] < Item[Idx2]
  ///          >0 if Item[Idx1] > Item[Idx2] </summary>
  TCompareItemsMeth = function(_Idx1, _Idx2: Integer): Integer of object;
  ///<summary>
  /// Swap the items at the given indexes </summary>
  TSwapItemsMeth = procedure(_Idx1, _Idx2: Integer) of object;

type
  ///<summary>
  /// Interface to be implemented for using the sorting algorithms </summary>
  ISortDataHandler = interface ['{C7B22837-F9C0-4228-A2E3-DC8BBF27DBA9}']
    /// Compare items at the given indexes and return values similar to the standard Delphi
    /// Compare functions e.g. CompareStr or CompareText
    /// @returns 0 if they are equal
    ///          <0 if Item[Idx1] < Item[Idx2]
    ///          >0 if Item[Idx1] > Item[Idx2] </summary>
    function Compare(_Idx1, _Idx2: Integer): Integer;
  ///<summary>
  /// Swap the items at the given indexes </summary>
    procedure Swap(_Idx1, _Idx2: Integer);
  end;

type
  IQSDataHandler = ISortDataHandler
{$IFDEF SUPPORTS_DEPRECATED_TYPES}
  deprecated // use ISortDataHandler instead
{$ENDIF}
  ;

implementation

end.
