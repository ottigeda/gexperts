        ÿÿ  ÿÿ                  å$  4   ÿÿ
 J V _ R U N T I M E         0 	        <RUNTIME>
<100>
<TYPE>I/O Errors</TYPE>
<NAME>Disk read error</NAME>
<TXT>Reported by Read on a typed file if you attempt to read past the end of the file.</TXT>
</100>

<101>
<TYPE>I/O Errors</TYPE>
<NAME>Disk write error</NAME>
<TXT>Reported by CloseFile, Write, WriteIn, or Flush if the disk becomes full.</TXT>
</101>

<102>
<TYPE>I/O Errors</TYPE>
<NAME>File not Assigned</NAME>
<TXT>Reported by Reset, Rewrite, Append, Rename, or Erase if the file variable has not been Assigned a name through a call to Assign or AssignFile.</TXT>
</102>

<103>
<TYPE>I/O Errors</TYPE>
<NAME>File not open</NAME>
<TXT>Reported by CloseFile, Read Write, Seek, Eof, FilePos, FileSize, Flush, BlockRead, or BlockWrite if the file is not open.</TXT>
</103>

<104>
<TYPE>I/O Errors</TYPE>
<NAME>File not open for input</NAME>
<TXT>Reported by Read, Readln, Eof, Eoln, SeekEof, or SeekEoln on a text file if the file is not open for input.</TXT>
</104>

<105>
<TYPE>I/O Errors</TYPE>
<NAME>File not open for output</NAME>
<TXT>Reported by Write or Writeln on a text file if you do not generate a Console Application.</TXT>
</105>

<106>
<TYPE>I/O Errors</TYPE>
<NAME>Invalid numeric format</NAME>
<TXT>Reported by Read or Readln if a numeric Value read from a text file does not conform to the proper numeric format.</TXT>
</106>

<200>
<TYPE>Fatal Errors</TYPE>
<NAME>Division by Zero</NAME>
<TXT>EDivByZero is raised when an Application tries to divide an integer by zero. In practice, this error is easy to trap and correct in a try...except block.</TXT>
</200>

<201>
<TYPE>Fatal Errors</TYPE>
<NAME>Range check error</NAME>
<TXT>ERangeError is an integer math Exception. It occurs when

-An integer expressions Value exceeds the bounds of the specified integer type to which it is Assigned.
-Source code attempts to access an item in an array using an index Value that is not within the defined array.

ERangeError is raised only if range checking is turned on.
To turn on range checking, include the $R+ directive in project source code, or select Project|Options, choose the Compiler tab, and check the Range-checking option in the dialog box.</TXT>
</201>

<202>
<TYPE>Fatal Errors</TYPE>
<NAME>Stack overflow</NAME>
<TXT>EStackOverflow is raised when the current threads stack grows into the final guard page-that is,
when the system cannot grow the stack dynamically. This can happen because of extremely large local variables, deeply recursive routines, or invalid assembly-language code.</TXT>
</202>

<203>
<TYPE>Fatal Errors</TYPE>
<NAME>Heap overflow error</NAME>
<TXT>EOutOfMemory occurs when an Application attempts to allocate dynamic memory, but there is not enough free memory in the system to meet the request.</TXT>
</203>

<204>
<TYPE>Fatal Errors</TYPE>
<NAME>Invalid pointer operation</NAME>
<TXT>EInvalidPointer indicates that an Application has attempted an invalid pointer operation. For example, it can occur if an Application tries to dispose of the same pointer twice,
or refers to a pointer which has already been disposed of.</TXT>
</204>

<205>
<TYPE>Fatal Errors</TYPE>
<NAME>Floating point overflow</NAME>
<TXT>EOverflow is raised when a calculated result is too large to fit in the floating-point register allocated for it and data is therefore lost.</TXT>
</205>

<206>
<TYPE>Fatal Errors</TYPE>
<NAME>Floating point underflow</NAME>
<TXT>When a floating-point math operation produces a Value that is too small to be represented in a floating-point variable,
the result ordinarily becomes zero. However, an Application can change the numeric coprocessor''s control word to unmask underflow hardware Exceptions;
in this case, EUnderflow is raised when the coprocessor reports an underflow hardware Exception. The control word is reset after the Exception.</TXT>
</206>

<207>
<TYPE>Fatal Errors</TYPE>
<NAME>Invalid floating point operation</NAME>
<TXT>EInvalidOp is raised when the processor encounters an undefined instruction, invalid operation, or floating-point processor stack overflow.</TXT>
</207>

<210>
<TYPE>Fatal Errors</TYPE>
<NAME>Abstract Method Error</NAME>
<TXT>EAbstractError is raised when an Application tries to call an abstract method. It is also raised at design time when a component with an abstract method is placed on a form.</TXT>
</210>

<215>
<TYPE>Fatal Errors</TYPE>
<NAME>Arithmetic overflow (integer only)</NAME>
<TXT>EIntOverflow is an integer math Exception. It occurs when a calculated result is too large to fit in the register allocated for it and data is therefore lost.
EIntOverflow is raised only if overflow checking is turned on. To turn on overflow checking, include the $Q+ directive in project source code, or select Project|Options, choose the Compiler tab, and check the Overflow-checking option in the dialog box.</TXT>
</215>

<216>
<TYPE>Fatal Errors</TYPE>
<NAME>Access violation</NAME>
<TXT>EAccessViolation is raised when an Application

-Dereferences a nil pointer.
-Writes to a code page.
-Attempts to access a memory address for which there is no virtual memory allocated to the Application.</TXT>
</216>

<217>
<TYPE>Fatal Errors</TYPE>
<NAME>Control-C</NAME>
<TXT>EControlC is raised when a user presses Ctrl+C to terminate a console Application.</TXT>
</217>

<218>
<TYPE>Fatal Errors</TYPE>
<NAME>Privileged instruction</NAME>
<TXT>EPrivile is raised when an Application tries to execute a processor instruction that is invalid for the current processor privilege level.</TXT>
</218>

<219>
<TYPE>Fatal Errors</TYPE>
<NAME>Invalid typecast</NAME>
<TXT>EInvalidCast is raised when an Application tries to typecast an object illegally using the as operator.
For example, the following expression generates an EInvalidTypecast Exception if AnObject is of a type incompatible with TObjectType :
AnObject as TObjectType</TXT>
</219>

<220>
<TYPE>Fatal Errors</TYPE>
<NAME>Invalid variant typecast</NAME>
<TXT>EVariantError is raised when

-An Application attempts an invalid variant typecast or operation.
-A variant does not contain a required OLE IDispatch object.
-The VarArrayCreate function is unable to Create a requested variant array.
-A variant-array operation is attempted on something that is not a variant array.
-A variant-array index is out of bounds.</TXT>
</220>

<221>
<TYPE>Fatal Errors</TYPE>
<NAME>Invalid variant operation</NAME>
<TXT>EVariantError is raised when

-An Application attempts an invalid variant typecast or operation.
-A variant does not contain a required OLE IDispatch object.
-The VarArrayCreate function is unable to Create a requested variant array.
-A variant-array operation is attempted on something that is not a variant array.
-A variant-array index is out of bounds.</TXT>
</221>

<222>
<TYPE>Fatal Errors</TYPE>
<NAME>No variant method call dispatcher</NAME>
<TXT>EVariantError is raised when

-An Application attempts an invalid variant typecast or operation.
-A variant does not contain a required OLE IDispatch object.
-The VarArrayCreate function is unable to Create a requested variant array.
-A variant-array operation is attempted on something that is not a variant array.
-A variant-array index is out of bounds.</TXT>
</222>

<223>
<TYPE>Fatal Errors</TYPE>
<NAME>Cannot Create variant array</NAME>
<TXT>EVariantError is raised when

-An Application attempts an invalid variant typecast or operation.
-A variant does not contain a required OLE IDispatch object.
-The VarArrayCreate function is unable to Create a requested variant array.
-A variant-array operation is attempted on something that is not a variant array.
-A variant-array index is out of bounds.</TXT>
</223>

<224>
<TYPE>Fatal Errors</TYPE>
<NAME>Variant does not contain array</NAME>
<TXT>EVariantError is raised when

-An Application attempts an invalid variant typecast or operation.
-A variant does not contain a required OLE IDispatch object.
-The VarArrayCreate function is unable to Create a requested variant array.
-A variant-array operation is attempted on something that is not a variant array.
-A variant-array index is out of bounds.</TXT>
</224>

<225>
<TYPE>Fatal Errors</TYPE>
<NAME>Variant array bounds error</NAME>
<TXT>EVariantError is raised when

-An Application attempts an invalid variant typecast or operation.
-A variant does not contain a required OLE IDispatch object.
-The VarArrayCreate function is unable to Create a requested variant array.
-A variant-array operation is attempted on something that is not a variant array.
-A variant-array index is out of bounds.</TXT>
</225>

<226>
<TYPE>Fatal Errors</TYPE>
<NAME>TLS initialization error</NAME>
<TXT>No description</TXT>
</226>

<227>
<TYPE>Fatal Errors</TYPE>
<NAME>Assertion failed</NAME>
<TXT>EAssertionFailed is raised by the Assert procedure when an Application passes it a Boolean expression whose Value is False.
The Exception is raised only if the $ASSERTIONS ON compiler directive is in effect.</TXT>
</227>

<228>
<TYPE>Fatal Errors</TYPE>
<NAME>Interface Cast Error</NAME>
<TXT>EIntfCastError is raised when an Application tries to typecast an interface illegally using the as operator.</TXT>
</228>

<229>
<TYPE>Fatal Errors</TYPE>
<NAME>Safecall error</NAME>
<TXT>no description</TXT>
</229>

</RUNTIME>
   L( 4   ÿÿ
 J V _ L E X I C A L         0 	        <LEXICAL>
 <NODE>
  <TEXT>Ansi strings</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Compare with case sensitivity</TEXT>
   <PROTO>function AnsiCompareStr(const S1, S2: string): integer;</PROTO>
   <DESCRIPTION>AnsiCompareStr compares S1 to S2, with case-sensitivity. The compare operation is controlled by the current Windows locale. The return Value is the same as for CompareStr.  
</DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Compare without case sensitivity</TEXT>
   <PROTO>function AnsiCompareText(const S1, S2: string): integer;</PROTO>
   <DESCRIPTION>AnsiCompareText compares S1 to S2, without case-sensitivity. The compare operation is controlled by the current Windows locale. The return Value is the same as for CompareStr. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert to LowerCase</TEXT>
   <PROTO>function AnsiLowerCase(const S: string): string;</PROTO>
   <DESCRIPTION>AnsiLowercase converts all characters in the given string to lower case. The conversion uses the current Windows locale. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert to UpperCase</TEXT>
   <PROTO>function AnsiUpperCase(const S: string): string;</PROTO>
   <DESCRIPTION>AnsiUppercase converts all characters in the given string to upper case. The conversion uses the current Windows locale. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get a pointer to the last char</TEXT>
   <PROTO>function AnsiLastChar(const S: string): PChar;</PROTO>
   <DESCRIPTION>AnsiLastChar returns a pointer to the last full character in the string. This function supports multibyte characters  
 
</DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Quote</TEXT>
   <PROTO>function AnsiQuotedStr(const S: string; Quote: Char): string;</PROTO>
   <DESCRIPTION>AnsiQuotedStr returns the given string as a quoted string, using the provided Quote character.  A Quote character is inserted at the beginning and end of thestring, and each Quote character in the string is doubled. This function supports multibyte character strings (MBCS). </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Colors utils</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Color to RGB</TEXT>
   <PROTO>function ColorToRGB(Color: TColor): Longint;</PROTO>
   <DESCRIPTION>Call ColorToRGB to obtain an RGB representation of a color for using with  Windows API function calls.  ColorToRGB strips away the information that is stored in the highest order bits  about which palette to  use for colors that are not always available. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Color to string</TEXT>
   <PROTO>function ColorTostring(Color: TColor): string;</PROTO>
   <DESCRIPTION>Call ColorTostring to obtain a string that represents a TColor Value. if there is a  symbolic constant defined for the color (such as clBlack or clWindow), ColorTostring returns the  name of the constant. Otherwise, ColorTostring returns the hex Value of the color, formatted as a string. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the color Value of a symbolic name</TEXT>
   <PROTO>function IdentToColor(const Ident: string; var Color: Longint): Boolean;</PROTO>
   <DESCRIPTION>Call IdentToColor to reverse the translation performed by the ColorToIdent  function. This method is useful  for converting the strings generated by the GeTColorValues procedure into  useable TColor Values.   IdentToColor returns True if the Ident parameter is successfully converted into a  color that is returned by the Color parameter.  IdentToColor returns False if the Ident parameter is not the  name of one of the  Delphi built-in color constants. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get color Values</TEXT>
   <PROTO>procedure GeTColorValues(Proc: TGetStrProc);</PROTO>
   <DESCRIPTION>Call GeTColorValues to execute a callback for every TColor constant in Delphi.   The Proc parameter is  the callback function that is called for color constant name.  
TGetStrProc = procedure(const S: string) of object; </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the symbolic Value of a color</TEXT>
   <PROTO>function ColorToIdent(Color: Longint; var Ident: string): Boolean;</PROTO>
   <DESCRIPTION>Call ColorToIdent to obtain the name of the symbolic constant that represents a  TColor Value, such as clBlack or clWindow. The Ident parameter is set to the symbolic name. if there is  no symbolic constant defined for the color, ColorToIdent returns False. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>string to color</TEXT>
   <PROTO>function stringToColor(const S: string): TColor;</PROTO>
   <DESCRIPTION>Call stringToColor to reverse the translation performed by the ColorTostring  function. This method is useful for converting the strings entered by the user into useable TColor Values.    
The S parameter can be either the name of a built-in color constant such as  'clBtnFace', or the string representation of a valid TColor Value such as '$02FF8800'.  stringToColor  returns the TColor Value that  corresponds to the S parameter. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Control management</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Draw a 3D frame</TEXT>
   <PROTO>procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor; Width: integer);</PROTO>
   <DESCRIPTION>Use Frame3D to Create three-dimensional effects when drawing directly on a  canvas.  The Canvas  parameter is the canvas to draw on.  The Rect parameter specifies the position  where the three-dimensional canvas should appear.  The TopColor and BottomColor  parameters indicate the colors  that appear on the left and top or the right and bottom of the rectangle  respectively.  The Width  parameter specifies the width of the frame. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Find component at a point</TEXT>
   <PROTO>function FindVCLWindow(const Pos: TPoint): TWinControl;</PROTO>
   <DESCRIPTION>Call FindVCLWindow to locate the windowed control under a certain point.  For  example, use  FindVCLWindow to identify the windowed control that is under the mouse from  another control that has captured the mouse.  The Pos parameter specifies the location which must be  over the returned  windowed control.  if there is no windowed control under the Pos parameter,  FindVCLWindow returns nil. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get captured control</TEXT>
   <PROTO>function GetCaptureControl: TControl;</PROTO>
   <DESCRIPTION>Call GetCaptureControl to determine which control is currently processing mouse  messages.  When a  control has captured the mouse, it receives all mouse messages, even if the  mouse is not over the  control. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Set captured control</TEXT>
   <PROTO>procedure SetCaptureControl(Control: TControl);</PROTO>
   <DESCRIPTION>Call SetCaptureControl to allow a control to capture the mouse.  When the  control has the mouse captured, it receives all mouse messages until another control captures the  mouse by a subsequent call to SetCaptureControl or until the Application calls the Windows API function  ReleaseCapture. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get parent form</TEXT>
   <PROTO>function GetParenTForm(Control: TControl): TCustomForm;</PROTO>
   <DESCRIPTION>Call GetParenTForm to obtain the descendant of TCustomForm that contains the  control specified by the Control parameter. if the control is not contained in a Delphi form (for example,  an ActiveX control embedded in another type of window), GetParentFrom returns nil </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Conversions</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>integer to hexadecimal</TEXT>
   <PROTO>function IntToHex(Value: integer; Digits: integer): string;</PROTO>
   <DESCRIPTION>IntToHex converts the given Value to a hexadecimal string representation with the minimum number of digits specified. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>integer to string</TEXT>
   <PROTO>function IntToStr(Value: integer): string;</PROTO>
   <DESCRIPTION>IntToStr converts the given Value to its decimal string representation. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>string to integer</TEXT>
   <PROTO>function StrToInt(const S: string): integer;</PROTO>
   <DESCRIPTION>StrToInt converts the given string to an integer Value. if the string doesn't contain a valid Value, an EConvertError Exception is raised. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>string to int64</TEXT>
   <PROTO>function StrToInt64(const S: string): integer;</PROTO>
   <DESCRIPTION>StrToInt64 converts the given string to an integer Value. if the string doesn't contain a valid Value, an EConvertError Exception is raised. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>string to integer with default</TEXT>
   <PROTO>function StrToIntDef(const S: string; Default: integer): integer;</PROTO>
   <DESCRIPTION>StrToIntDef converts the given string to an integer Value. if the string doesn't contain a valid Value, the Value given by Default is returned. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>string to int64 with default</TEXT>
   <PROTO>function StrToIntDef64(const S: string; Default: integer): integer;</PROTO>
   <DESCRIPTION>StrToIntDef64 converts the given string to an integer Value. if the string doesn't contain a valid Value, the Value given by Default is returned. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Cursor management</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Cursor to string</TEXT>
   <PROTO>function CursorTostring(Cursor: TCursor): string;</PROTO>
   <DESCRIPTION>Use CursorTostring to convert the cursor used by a control or one of the cursors  made available by the  global screen variable into a string that can be displayed to the user.  Pass the  cursor Value as the  Cursor parameter.  if there is a predefined constant for the cursor, CursorTostring  returns the constant  name.  if the cursor does not have an associated constant (for example, a  custom cursor),  CursorTostring returns the string representation of the cursors numeric Value. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>string to cursor</TEXT>
   <PROTO>function stringToCursor(const S: string): TCursor;</PROTO>
   <DESCRIPTION>Call stringToCursor to reverse the translation performed by the CursorTostring  function. This method is  useful for converting the strings entered by the user into useable TCursor Values.   
The S parameter can be either the name of a built-in cursor constant such as  'crHelp', or the string  representation of a valid TCursor Value such as '10'.  stringToCursor returns the  TCursor Value that  corresponds to the S parameter. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>DateTime routines</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Convert date to string</TEXT>
   <PROTO>function DateToStr(Date: TDateTime): string;</PROTO>
   <DESCRIPTION>DateToStr converts the date part of the given TDateTime Value to a string. The conversion uses the format specified by the ShorTDateFormat global variable. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert datetime to filedate</TEXT>
   <PROTO>function DateTimeToFileDate(DateTime: TDateTime): integer;</PROTO>
   <DESCRIPTION>DateTimeToFileDate converts a TDateTime Value to a DOS date-and-time Value. The FileAge, FileGeTDate, and FileSeTDate routines operate on DOS date-and-time Values, and the Time field of a TSearchRec used by the FindFirst and FindNext functions contains a DOS date-and-time Value </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert datetime to string</TEXT>
   <PROTO>function DateTimeToStr(DateTime: TDateTime): string;</PROTO>
   <DESCRIPTION>DateTimeToStr converts the given date and time to a string. The resulting string consists of a date and time formatted using the ShorTDateFormat and LongTimeFormat global variables. Time information is included in the resulting string only if the fractional part of the given date and time Value is non-zero. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert datetime to systemdatetime</TEXT>
   <PROTO>procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);</PROTO>
   <DESCRIPTION>DateTimeToSystemTime converts a date and time from Delphi's TDateTime format into the Win32 API's TSystemTime format. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert datetime to timestamp</TEXT>
   <PROTO>function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;</PROTO>
   <DESCRIPTION>Call DateTimeToTimeStamp to convert a TDateTime Value into a TTimeStamp  Value. TDateTime Values represent time as the number of days (including fractional days) that have elapsed since  12:00 am on December 30,  1899. TTimeStamp Values represent time as separate date and time Values,  where the date is the number of  calandar days since the start of the current calandar (that is, January 1, 0001  would have a Value of 1), and time  is the number of milliseconds since midnight. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert filedate to datetime</TEXT>
   <PROTO>function FileDateToDateTime(FileDate: integer): TDateTime;</PROTO>
   <DESCRIPTION>FileDateToDateTime converts a DOS date-and-time Value to a TDateTime Value. The FileAge, FileGeTDate, and FileSeTDate routines operate on DOS date-and-time Values, and the Time field of a TSearchRec used by the FindFirst and FindNext functions contains a DOS date-and-time Value. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert milliseconds to timestamp</TEXT>
   <PROTO>function MSecsToTimeStamp(MSecs: Comp): TTimeStamp;</PROTO>
   <DESCRIPTION>Call MSecsToTimeStamp to convert MSecs, a number of milliseconds, into the  number of days  represented, plus the number of milliseconds remaining. The days and  milliseconds are expressed as  a TTimeStamp Value, where the Date field is the number of days and the Time  field is the remainder in milliseconds. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert string to date</TEXT>
   <PROTO>function StrToDate(const S: string): TDateTime;</PROTO>
   <DESCRIPTION>StrToDate converts the given string to a date Value. The string must consist of two or three numbers, separated by the character defined by the DateSeparator global variable. The order for month, day, and year is determined by the ShorTDateFormat global variable--possible combinations are m/d/y, d/m/y, and y/m/d. if the string contains only two numbers, it is interpreted as a date (m/d or d/m) in the current year. Year Values between 0 and 99 are assumed to be in the current century. if the given string does not contain a valid date, an EConvertError Exception is raised. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert string to datetime</TEXT>
   <PROTO>function StrToDateTime(const S: string): TDateTime;</PROTO>
   <DESCRIPTION>StrToDateTime converts the given string to a date and time Value. The string must contain a date optionally followed by a time. The date and time parts of the string must follow the formats described for the StrToDate and StrToTime functions. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert string to time</TEXT>
   <PROTO>function StrToTime(const S: string): TDateTime;</PROTO>
   <DESCRIPTION>StrToTime converts the given string to a time Value. The string must consist of two or three numbers, separated by the character defined by the TimeSeparator global variable, optionally followed by an AM or PM indicator. The numbers represent hour, minute, and (optionally) second, in that order. if the time is followed by AM or PM, it is assumed to be in 12-hour clock format. if no AM or PM indicator is included, the time is assumed to be in 24-hour clock format. if the given string does not contain a valid time, an EConvertError Exception is raised. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert systemdatetime to timedate</TEXT>
   <PROTO>function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;</PROTO>
   <DESCRIPTION>SystemTimeToDateTime converts a date and time from the Win32 API's TSystemTime format into Delphi's TDateTime format. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert time to string</TEXT>
   <PROTO>function TimeToStr(Time: TDateTime): string;</PROTO>
   <DESCRIPTION>TimeToStr converts the time part of the given TDateTime Value to a string. The conversion uses the format specified by the LongTimeFormat global variable. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert timestamp to datetime</TEXT>
   <PROTO>function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;</PROTO>
   <DESCRIPTION>Call TimeStampToDateTime to convert a TTimeStamp Value into a TDateTime  Value.  TTimeStamp Values represent time as separate date and time Values, where the  date is the  number of calandar days since the start of the current calandar (that is, January  1, 0001 would  have a Value of 1), and time is the number of milliseconds since midnight.  TDateTime Values represent time as the number of days (including fractional days) that have  elapsed since 12:00  am on December 30, 1899.  </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert timestamp to milliseconds</TEXT>
   <PROTO>function TimeStampToMSecs(const TimeStamp: TTimeStamp): Comp;</PROTO>
   <DESCRIPTION>Call TimeStampToMSecs to convert TimeStamp, a TTimeStamp Value, into the  total number of  milliseconds that comprise the Date field plus the number of milliseconds given in  the Time field. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Decode date</TEXT>
   <PROTO>procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word);</PROTO>
   <DESCRIPTION>DecodeDate decodes the integral (date) part of the given TDateTime Value into its corresponding year, month, and day. if the given TDateTime Value is less than or equal to zero, the year, month, and day return parameters are all set to zero. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Decode time</TEXT>
   <PROTO>procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);</PROTO>
   <DESCRIPTION>DecodeTime decodes the fractional (time) part of the given TDateTime Value into its corresponding hour, minute, second, and millisecond. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Encode a date</TEXT>
   <PROTO>function EncodeDate(Year, Month, Day: Word): TDateTime;</PROTO>
   <DESCRIPTION>EncodeDate encodes the given year, month, and day into a TDateTime Value. The year must be between 1 and 9999, the month must be between 1 and 12, and the day must be between 1 and N, where N is the number of days in the specified month. if the specified Values are not within range, an EConvertError Exception is raised. The resulting Value is the number of days between 12/30/1899 and the given date. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Encode a time</TEXT>
   <PROTO>function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;</PROTO>
   <DESCRIPTION>EncodeTime encodes the given hour, minute, second, and millisecond into a TDateTime Value. The hour must be between 0 and 23, the minute must be between 0 and 59, the second must be between 0 and 59, and the millisecond must be between 0 and 999. if the specified Values are not within range, an EConvertError Exception is raised. The resulting Value is a number between 0 (inclusive) and 1 (not inclusive) that indicates the fractional part of a day given by the specified time. The Value 0 corresponds to midnight, 0.5 corresponds to noon, 0.75 corresponds to 6:00 pm, etc. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format datetime</TEXT>
   <PROTO>function FormaTDateTime(const Format: string; DateTime: TDateTime): string;</PROTO>
   <DESCRIPTION>FormaTDateTime formats the date-and-time Value given by DateTime using the format given by Format. The following format specifiers are supported:  
c       Displays the date using the format given by the ShorTDateFormat         global variable, followed by the time using the format given by         the LongTimeFormat global variable. The time is not displayed if         the fractional part of the DateTime Value is zero.  
d       Displays the day as a number without a leading zero (1-31).  
dd      Displays the day as a number with a leading zero (01-31).  
ddd     Displays the day as an abbreviation (Sun-Sat) using the strings         given by the ShortDayNames global variable.  
dddd    Displays the day as a full name (Sunday-Saturday) using the strings         given by the LongDayNames global variable.  
ddddd   Displays the date using the format given by the ShorTDateFormat         global variable.  
dddddd  Displays the date using the format given by the LongDateFormat         global variable.  
g       Displays the period/era as an abbreviation (Japanese and         Taiwanese locales only).  
gg      Displays the period/era as a full name.  
e       Displays the year in the current period/era as a number without         a leading zero (Japanese, Korean and Taiwanese locales only).  
ee      Displays the year in the current period/era as a number with         a leading zero (Japanese, Korean and Taiwanese locales only).  
m       Displays the month as a number without a leading zero (1-12). If         the m specifier immediately follows an h or hh specifier, the         minute rather than the month is displayed.  
mm      Displays the month as a number with a leading zero (01-12). If         the mm specifier immediately follows an h or hh specifier, the         minute rather than the month is displayed.  
mmm     Displays the month as an abbreviation (Jan-Dec) using the strings         given by the ShortMonthNames global variable.  
mmmm    Displays the month as a full name (January-December) using the         strings given by the LongMonthNames global variable.  
yy      Displays the year as a two-digit number (00-99).  
yyyy    Displays the year as a four-digit number (0000-9999).  
h       Displays the hour without a leading zero (0-23).  
hh      Displays the hour with a leading zero (00-23).  
n       Displays the minute without a leading zero (0-59).  
nn      Displays the minute with a leading zero (00-59).  
s       Displays the second without a leading zero (0-59).  
ss      Displays the second with a leading zero (00-59).  
t       Displays the time using the format given by the ShorTTimeFormat         global variable.  
tt      Displays the time using the format given by the LongTimeFormat         global variable.  
am/pm   Uses the 12-hour clock for the preceding h or hh specifier, and         displays 'am' for any hour before noon, and 'pm' for any hour         after noon. The am/pm specifier can use lower, upper, or mixed         case, and the result is displayed accordingly.  
a/p     Uses the 12-hour clock for the preceding h or hh specifier, and         displays 'a' for any hour before noon, and 'p' for any hour after         noon. The a/p specifier can use lower, upper, or mixed case, and         the result is displayed accordingly.  
ampm    Uses the 12-hour clock for the preceding h or hh specifier, and         displays the contents of the TimeAMstring global variable for any         hour before noon, and the contents of the TimePMstring global         variable for any hour after noon.  
/       Displays the date separator character given by the DateSeparator         global variable.  
:       Displays the time separator character given by the TimeSeparator         global variable.  
'xx'    Characters enclosed in single or double quotes are displayed as-is, "xx"    and do not affect formatting.  
Format specifiers may be written in upper case as well as in lower case letters--both produce the same result.  
if the string given by the Format parameter is empty, the date and time Value is formatted as if a 'c' format specifier had been given.  
The following example:  
  S := FormaTDateTime('"The meeting is on" dddd, mmmm d, yyyy, ' +     '"at" hh:mm AM/PM', StrToDateTime('2/15/95 10:30am'));  
assigns 'The meeting is on Wednesday, February 15, 1995 at 10:30 AM' to the string variable S. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format datetime to string</TEXT>
   <PROTO>procedure DateTimeTostring(var Result: string; const Format: string;DateTime: TDateTime);</PROTO>
   <DESCRIPTION>DateTimeTostring converts the date and time Value given by DateTime using the format string given by Format into the string variable given by Result. For further details, see the description of the FormaTDateTime function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the current date</TEXT>
   <PROTO>function Date: TDateTime;</PROTO>
   <DESCRIPTION>Date returns the current date. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the current date and time</TEXT>
   <PROTO>function Now: TDateTime;</PROTO>
   <DESCRIPTION>Now returns the current date and time, corresponding to Date + Time. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the current time</TEXT>
   <PROTO>function Time: TDateTime;</PROTO>
   <DESCRIPTION>Time returns the current time. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the day of the week</TEXT>
   <PROTO>function DayOfWeek(Date: TDateTime): integer;</PROTO>
   <DESCRIPTION>DayOfWeek returns the day of the week of the given date. The result is an integer between 1 and 7, corresponding to Sunday through Saturday. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Increment the number of months</TEXT>
   <PROTO>function IncMonth(const Date: TDateTime; NumberOfMonths: integer): TDateTime;</PROTO>
   <DESCRIPTION>IncMonth returns Date shifted by the specified number of months. NumberOfMonths parameter can be negative, to return a date N months ago. if the input day of month is greater than the last day of the resulting month, the day is set to the last day of the resulting month. Input time of day is copied to the DateTime result. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Is leap year ?</TEXT>
   <PROTO>function IsLeapYear(Year: Word): Boolean;</PROTO>
   <DESCRIPTION>IsLeapYear determines whether the given year is a leap year. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Directory management</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Get the current dictory</TEXT>
   <PROTO>function GetCurrentDir: string;</PROTO>
   <DESCRIPTION>GetCurrentDir returns the current directory. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Set the current dictory</TEXT>
   <PROTO>function SetCurrentDir(const Dir: string): Boolean;</PROTO>
   <DESCRIPTION>SetCurrentDir sets the current directory. The return Value is True if the current directory was successfully changed, or False if an error occurred. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Create a directory</TEXT>
   <PROTO>function CreateDir(const Dir: string): Boolean;</PROTO>
   <DESCRIPTION>CreateDir Creates a new directory. The return Value is True if a new directory was successfully Created, or False if an error occurred. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Remove a directory</TEXT>
   <PROTO>function RemoveDir(const Dir: string): Boolean;</PROTO>
   <DESCRIPTION>RemoveDir deletes an existing empty directory. The return Value is True if the directory was successfully deleted, or False if an error occurred. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Exception management</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Displays an Exception message with its physical address</TEXT>
   <PROTO>procedure ShowException(ExcepTObject: TObject; ExceptAddr: Pointer);</PROTO>
   <DESCRIPTION>This procedure displays the message associated with an Exception, together with  the Exception's  physical address. The Exception's address can be used with Search|Find Error  menu command to find the statement that raised the Exception. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the message for a win32 error code</TEXT>
   <PROTO>function SysErrorMessage(ErrorCode: integer): string;</PROTO>
   <DESCRIPTION>The SysErrorMessage returns an error message string that corresponds to the  specified Win32 API routine error code.  This function can be used to convert Windows error codes into strings. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the object associated with the current Exception.</TEXT>
   <PROTO>function ExcepTObject: TObject;</PROTO>
   <DESCRIPTION>The ExcepTObject function returns a reference to the current Exception object --  that is, the descendant  of Exception that is associated with the currently raised Exception. if there is no  current Exception,  ExcepTObject returns nil. In most cases, do not call ExcepTObject explicitly;  instead, use the language  construct  
on E: ExceptionType do This constructs maps the identifier E onto the object instance of the current  Exception statement that follows if the current Exception is of ExceptionType. However, when a default  Exception handler is  Created using an else in the Exception block, the only way to access the current  Exception object is by  calling ExcepTObject. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the address at which the current Exception was raised.</TEXT>
   <PROTO>function ExceptAddr: Pointer;</PROTO>
   <DESCRIPTION>Use ExceptAddr to obtain the address at which an Exception was raised. if there  is no current  Exception, ExceptAddr returns nil. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>File management</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Change the extension of a file</TEXT>
   <PROTO>function ChangeFileExt(const FileName, Extension: string): string;</PROTO>
   <DESCRIPTION>ChangeFileExt changes the extension of a FileName. FileName specifies a FileName with or without an extension, and Extension specifies the new extension for the FileName. The new extension can be a an empty string or a period followed by up to three characters. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Delete a file</TEXT>
   <PROTO>function DeleteFile(const FileName: string): Boolean;</PROTO>
   <DESCRIPTION>DeleteFile deletes the file given by FileName. The return Value is True if the file was successfully deleted, or False if an error occurred. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Expand a FileName</TEXT>
   <PROTO>function ExpandFileName(const FileName: string): string;</PROTO>
   <DESCRIPTION>ExpandFileName expands the given FileName to a fully qualified FileName. The resulting string consists of a drive letter, a colon, a root relative directory path, and a FileName. Embedded '.' and '..' directory references are removed. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Expand a UNC FileName</TEXT>
   <PROTO>function ExpandUNCFileName(const FileName: string): string;</PROTO>
   <DESCRIPTION>ExpandUNCFileName expands the given FileName to a fully qualified FileName. This function is the same as ExpandFileName except that it will return the drive portion of the FileName in the format '\\&lt;servername&gt;\&lt;sharename&gt; if that drive is actually a network resource instead of a local resource. Like ExpandFileName, embedded '.' and '..' directory references are removed. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Extract the directory of a file</TEXT>
   <PROTO>function ExtractFileDir(const FileName: string): string;</PROTO>
   <DESCRIPTION>ExtractFileDir extracts the drive and directory parts of the given FileName. The resulting string is a directory name suitable for passing to SetCurrentDir, CreateDir, etc. The resulting string is empty if FileName contains no drive and directory parts. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Extract the drive of a file</TEXT>
   <PROTO>function ExtractFileDrive(const FileName: string): string;</PROTO>
   <DESCRIPTION>ExtractFileDrive extracts the drive part of the given FileName.  For FileNames with drive letters, the resulting string is '&lt;drive&gt;:'. For FileNames with a UNC path, the resulting string is in the form '\\&lt;servername&gt;\&lt;sharename&gt;'.  if the given path contains neither style of FileName, the result is an empty string. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Extract the extension of a file</TEXT>
   <PROTO>function ExtractFileExt(const FileName: string): string;</PROTO>
   <DESCRIPTION>ExtractFileExt extracts the extension part of the given FileName. The resulting string includes the period character that separates the name and extension parts. The resulting string is empty if the given FileName has no extension. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Extract the FileName of a file</TEXT>
   <PROTO>function ExtractFileName(const FileName: string): string;</PROTO>
   <DESCRIPTION>ExtractFileName extracts the name and extension parts of the given FileName. The resulting string is the leftmost characters of FileName, starting with the first character after the colon or backslash that separates the path information from the name and extension. The resulting string is equal to FileName if FileName contains no drive and directory parts. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Extract the filepath of a file</TEXT>
   <PROTO>function ExtractFilePath(const FileName: string): string;</PROTO>
   <DESCRIPTION>ExtractFilePath extracts the drive and directory parts of the given FileName. The resulting string is the leftmost characters of FileName, up to and including the colon or backslash that separates the path information from the name and extension. The resulting string is empty if FileName contains no drive and directory parts. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Extract the short pathname of a file</TEXT>
   <PROTO>function ExtractShortPathName(const FileName: string): string;</PROTO>
   <DESCRIPTION>ExtractShortPathName will convert the given FileName to the short form by calling the GetShortPathName API.  Will return an empty string if the file or directory specified does not exist </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Extract the relative path</TEXT>
   <PROTO>function ExtractRelativePath(const BaseName, DestName: string): string;</PROTO>
   <DESCRIPTION>ExtractRelativePath will return a file path name relative to the given BaseName.  It strips the common path dirs and adds '..\' for each level up from the BaseName path. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the arributes of a file</TEXT>
   <PROTO>function FileGetAttr(const FileName: string): integer;</PROTO>
   <DESCRIPTION>FileGetAttr returns the file attributes of the file given by FileName. The attributes can be examined by AND-ing with the faXXXX constants defined above. A return Value of -1 indicates that an error occurred. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the date-and-time stamp of a file</TEXT>
   <PROTO>function FileAge(const FileName: string): integer;</PROTO>
   <DESCRIPTION>FileAge returns the date-and-time stamp of the specified file. The return Value can be converted to a TDateTime Value using the FileDateToDateTime function. The return Value is -1 if the file does not exist. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the free space of a drive</TEXT>
   <PROTO>function DiskFree(Drive: Byte): Int64;</PROTO>
   <DESCRIPTION>DiskFree returns the number of free bytes on the specified drive number, where 0 = Current, 1 = A, 2 = B, etc. DiskFree returns -1 if the drive number is invalid. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the total space of a drive</TEXT>
   <PROTO>function DiskSize(Drive: Byte): Int64;</PROTO>
   <DESCRIPTION>DiskSize returns the size in bytes of the specified drive number, where 0 = Current, 1 = A, 2 = B, etc. DiskSize returns -1 if the drive number is invalid. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Rename a file</TEXT>
   <PROTO>function RenameFile(const OldName, NewName: string): Boolean;</PROTO>
   <DESCRIPTION>RenameFile renames the file given by OldName to the name given by NewName. The return Value is True if the file was successfully renamed, or False if an error occurred. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Search for a file in a directory list</TEXT>
   <PROTO>function FileSearch(const Name, DirList: string): string;</PROTO>
   <DESCRIPTION>FileSearch searches for the file given by Name in the list of directories given by DirList. The directory paths in DirList must be separated by semicolons. The search always starts with the current directory of the current drive. The returned Value is a concatenation of one of the directory paths and the FileName, or an empty string if the file could not be located. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Set the attributes of a file</TEXT>
   <PROTO>function FileSetAttr(const FileName: string; Attr: integer): integer;</PROTO>
   <DESCRIPTION>FileSetAttr sets the file attributes of the file given by FileName to the Value given by Attr. The attribute Value is formed by OR-ing the appropriate faXXXX constants. The return Value is zero if the function was successful. Otherwise the return Value is a Windows error code. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Test if a file exists</TEXT>
   <PROTO>function FileExists(const FileName: string): Boolean;</PROTO>
   <DESCRIPTION>FileExists returns a boolean Value that indicates whether the specified file exists. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Floating point conversions</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Currency to string</TEXT>
   <PROTO>function CurrToStr(Value: Currency): string;</PROTO>
   <DESCRIPTION>CurrToStr converts the currency Value given by Value to its string representation. The conversion uses general number format. For further details, see the description of the CurrToStrF function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Currency to string and format</TEXT>
   <PROTO>function CurrToStrF(Value: Currency; Format: TFloaTFormat;Digits: integer): string;</PROTO>
   <DESCRIPTION>CurrToStrF converts the currency Value given by Value to its string representation. A call to CurrToStrF corresponds to a call to FloatToStrF with an implied precision of 19 digits. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Float to decimal</TEXT>
   <PROTO>procedure FloatToDecimal(var Result: TFloatRec; const Value;ValueType: TFloatValue; Precision, Decimals: integer);</PROTO>
   <DESCRIPTION>FloatToDecimal converts a floating-point Value to a decimal representation that is suited for further formatting. The Value parameter must be a variable of type Extended or Currency, as indicated by the ValueType parameter. For Values of type Extended, the Precision parameter specifies the requested number of significant digits in the result--the allowed range is 1..18. For Values of type Currency, the Precision parameter is ignored, and the implied precision of the conversion is 19 digits. The Decimals parameter specifies the requested maximum number of digits to the left of the decimal point in the result. Precision and Decimals together control how the result is rounded. To produce a result that always has a given number of significant digits regardless of the magnitude of the number, specify 9999 for the Decimals parameter. The result of the conversion is stored in the specified TFloatRec record as follows:  
Exponent - Contains the magnitude of the number, i.e. the number of significant digits to the right of the decimal point. The Exponent field is negative if the absolute Value of the number is less than one. if the number is a NAN (not-a-number), Exponent is set to -32768. if the number is INF or -INF (positive or negative infinity), Exponent is set to 32767.  
Negative - True if the number is negative, False if the number is zero or positive.  
Digits - Contains up to 18 (for type Extended) or 19 (for type Currency) significant digits followed by a null terminator. The implied decimal point (if any) is not stored in Digits. Trailing zeros are removed, and if the resulting number is zero, NAN, or INF, Digits contains nothing but the null terminator. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Float to pchar</TEXT>
   <PROTO>function FloatToText(Buffer: PChar; const Value; ValueType: TFloatValue;Format: TFloaTFormat; Precision, Digits: integer): integer;</PROTO>
   <DESCRIPTION>FloatToText converts the given floating-point Value to its decimal representation using the specified format, precision, and digits. The Value parameter must be a variable of type Extended or Currency, as indicated by the ValueType parameter. The resulting string of characters is stored in the given buffer, and the returned Value is the number of characters stored. The resulting string is not null-terminated. For further details, see the description of the FloatToStrF function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Float to string</TEXT>
   <PROTO>function FloatToStr(Value: Extended): string;</PROTO>
   <DESCRIPTION>FloatToStr converts the floating-point Value given by Value to its string representation. The conversion uses general number format with 15 significant digits. For further details, see the description of the FloatToStrF function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Float to string and format</TEXT>
   <PROTO>function FloatToStrF(Value: Extended; Format: TFloaTFormat;Precision, Digits: integer): string;</PROTO>
   <DESCRIPTION>FloatToStrF converts the floating-point Value given by Value to its string representation. The Format parameter controls the format of the resulting string. The Precision parameter specifies the precision of the given Value. It should be 7 or less for Values of type Single, 15 or less for Values of type Double, and 18 or less for Values of type Extended. The meaning of the Digits parameter depends on the particular format selected.  
The possible Values of the Format parameter, and the meaning of each, are described below.  
ffGeneral - General number format. The Value is converted to the shortest possible decimal string using fixed or scientific format. Trailing zeros are removed from the resulting string, and a decimal point appears only if necessary. The resulting string uses fixed point format if the number of digits to the left of the decimal point in the Value is less than or equal to the specified precision, and if the Value is greater than or equal to 0.00001. Otherwise the resulting string uses scientific format, and the Digits parameter specifies the minimum number of digits in the exponent (between 0 and 4).  
ffExponent - Scientific format. The Value is converted to a string of the form "-d.ddd...E+dddd". The resulting string starts with a minus sign if the number is negative, and one digit always precedes the decimal point. The total number of digits in the resulting string (including the one before the decimal point) is given by the Precision parameter. The "E" exponent character in the resulting string is always followed by a plus or minus sign and up to four digits. The Digits parameter specifies the minimum number of digits in the exponent (between 0 and 4).  
ffFixed - Fixed point format. The Value is converted to a string of the form "-ddd.ddd...". The resulting string starts with a minus sign if the number is negative, and at least one digit always precedes the decimal point. The number of digits after the decimal point is given by the Digits parameter--it must be between 0 and 18. if the number of digits to the left of the decimal point is greater than the specified precision, the resulting Value will use scientific format.  
ffNumber - Number format. The Value is converted to a string of the form "-d,ddd,ddd.ddd...". The ffNumber format corresponds to the ffFixed format, except that the resulting string contains thousand separators.  
ffCurrency - Currency format. The Value is converted to a string that represents a currency amount. The conversion is controlled by the Currencystring, CurrencyFormat, NegCurrFormat, ThousandSeparator, and DecimalSeparator global variables, all of which are initialized from the Currency Format in the International section of the Windows Control Panel. The number of digits after the decimal point is given by the Digits parameter--it must be between 0 and 18.  
For all formats, the actual characters used as decimal and thousand separators are obtained from the DecimalSeparator and ThousandSeparator global variables.  
if the given Value is a NAN (not-a-number), the resulting string is 'NAN'. if the given Value is positive infinity, the resulting string is 'INF'. If the given Value is negative infinity, the resulting string is '-INF'. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format currency</TEXT>
   <PROTO>function FormatCurr(const Format: string; Value: Currency): string;</PROTO>
   <DESCRIPTION>FormatCurr formats the currency Value given by Value using the format string given by Format. For further details, see the description of the FormatFloat function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format float</TEXT>
   <PROTO>function FormatFloat(const Format: string; Value: Extended): string;</PROTO>
   <DESCRIPTION>FormatFloat formats the floating-point Value given by Value using the format string given by Format. The following format specifiers are supported in the format string:  
0     Digit placeholder. if the Value being formatted has a digit in the       position where the '0' appears in the format string, then that digit       is copied to the output string. Otherwise, a '0' is stored in that       position in the output string.  
#     Digit placeholder. if the Value being formatted has a digit in the       position where the  </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format float to pchar</TEXT>
   <PROTO>function FloatToTextFmt(Buffer: PChar; const Value; ValueType: TFloatValue;Format: PChar): integer;</PROTO>
   <DESCRIPTION>FloatToTextFmt converts the given floating-point Value to its decimal representation using the specified format. The Value parameter must be a variable of type Extended or Currency, as indicated by the ValueType parameter. The resulting string of characters is stored in the given buffer, and the returned Value is the number of characters stored. The resulting string is not null-terminated. For further details, see the description of the FormatFloat function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>PChar to float</TEXT>
   <PROTO>function TextToFloat(Buffer: PChar; var Value;ValueType: TFloatValue): Boolean;</PROTO>
   <DESCRIPTION>TextToFloat converts the null-terminated string given by Buffer to a floating-point Value which is returned in the variable given by Value. The Value parameter must be a variable of type Extended or Currency, as indicated by the ValueType parameter. The return Value is True if the conversion was successful, or False if the string is not a valid floating-point Value. For further details, see the description of the StrToFloat function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>string to currency</TEXT>
   <PROTO>function StrToCurr(const S: string): Currency;</PROTO>
   <DESCRIPTION>StrToCurr converts the given string to a currency Value. For further details, see the description of the StrToFloat function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>string to float</TEXT>
   <PROTO>function StrToFloat(const S: string): Extended;</PROTO>
   <DESCRIPTION>StrToFloat converts the given string to a floating-point Value. The string must consist of an optional sign (+ or -), a string of digits with an optional decimal point, and an optional 'E' or 'e' followed by a signed integer. Leading and trailing blanks in the string are ignored. The DecimalSeparator global variable defines the character that must be used as a decimal point. Thousand separators and currency symbols are not allowed in the string. if the string doesn't contain a valid Value, an EConvertError Exception is raised. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Memory management</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Allocate Memory</TEXT>
   <PROTO>function AllocMem(Size: Cardinal): Pointer;</PROTO>
   <DESCRIPTION>AllocMem allocates a block of the given size on the heap. Each byte in the allocated buffer is set to zero. To dispose the buffer, use the FreeMem standard procedure. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Compare two blocks of memory</TEXT>
   <PROTO>function CompareMem(P1, P2: Pointer; Length: integer): Boolean;</PROTO>
   <DESCRIPTION>CompareMem performs a binary compare of Length bytes of memory referenced by P1 to that of P2.  CompareMem returns True if the memory referenced by P1 is identical to that of P2. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Misc</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Add an exit proc</TEXT>
   <PROTO>procedure AddExitProc(Proc: TProcedure);</PROTO>
   <DESCRIPTION>AddExitProc adds the given procedure to the run-time library's exit procedure list. When an Application terminates, its exit procedures are executed in reverse order of definition, i.e. the last procedure passed to AddExitProc is the first one to get executed upon termination. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Call terminate procs</TEXT>
   <PROTO>function CallTerminateProcs: Boolean;</PROTO>
   <DESCRIPTION>CallTerminateProcs is called by VCL when an Application is about to terminate.  It returns True only if all of the functions in the system's terminate procedure list return True.  This function is intended only to be called by Delphi, and it should not be called directly. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>Package support</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Finalize package</TEXT>
   <PROTO>procedure FinalizePackage(Module: HMODULE);</PROTO>
   <DESCRIPTION>FinalizePackage finalizes the given package DLL </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Load a package</TEXT>
   <PROTO>function LoadPackage(const Name: string): HMODULE;</PROTO>
   <DESCRIPTION>LoadPackage loads a given package DLL, checks for duplicate units and calls the initialization blocks of all the contained units </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get package description</TEXT>
   <PROTO>function GetPackageDescription(ModuleName: PChar): string;</PROTO>
   <DESCRIPTION>GetPackageDescription loads the description resource from the package library. if the description resource does not exist, an empty string is returned. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get package infos</TEXT>
   <PROTO>procedure GetPackageInfo(Module: HMODULE; Param: Pointer; var Flags: integer;InfoProc: TPackageInfoProc);</PROTO>
   <DESCRIPTION>GetPackageInfo accesses the given package's info table and enumerates all the contained units and required packages </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Initialize package</TEXT>
   <PROTO>procedure InitializePackage(Module: HMODULE);</PROTO>
   <DESCRIPTION>InitializePackage Validates and initializes the given package DLL </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Unload a package</TEXT>
   <PROTO>procedure UnloadPackage(Module: HMODULE);</PROTO>
   <DESCRIPTION>UnloadPackage does the opposite of LoadPackage by calling the finalization blocks of all contained units, then unloading the package DLL </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>PChar handling</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Allocate a buffer for a pchar</TEXT>
   <PROTO>function StrAlloc(Size: Cardinal): PChar;</PROTO>
   <DESCRIPTION>StrAlloc allocates a buffer of the given size on the heap. The size of the allocated buffer is encoded in a four byte header that immediately preceeds the buffer. To dispose the buffer, use StrDispose. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Ansi Compare up to a maximum length without case sensitivity</TEXT>
   <PROTO>function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): integer;</PROTO>
   <DESCRIPTION>AnsiStrLIComp compares S1 to S2, without case-sensitivity, up to a maximum length of MaxLen bytes. The compare operation is controlled by the current Windows locale. The return Value is the same as for CompareStr. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Ansi Compare up to a maximum length with case sensitivity</TEXT>
   <PROTO>function AnsiStrLComp(S1, S2: PChar; MaxLen: Cardinal): integer;</PROTO>
   <DESCRIPTION>AnsiStrLComp compares S1 to S2, with case-sensitivity, up to a maximum length of MaxLen bytes. The compare operation is controlled by the current Windows locale. The return Value is the same as for CompareStr. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Ansi Compare with case sensitivity</TEXT>
   <PROTO>function AnsiStrComp(S1, S2: PChar): integer;</PROTO>
   <DESCRIPTION>AnsiStrComp compares S1 to S2, with case-sensitivity. The compare operation is controlled by the current Windows locale. The return Value is the same as for CompareStr. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Ansi Compare without case sensitivity</TEXT>
   <PROTO>function AnsiStrIComp(S1, S2: PChar): integer;</PROTO>
   <DESCRIPTION>AnsiStrIComp compares S1 to S2, without case-sensitivity. The compare operation is controlled by the current Windows locale. The return Value is the same as for CompareStr. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Ansi Convert to LowerCase</TEXT>
   <PROTO>function AnsiStrLower(Str: PChar): PChar;</PROTO>
   <DESCRIPTION>AnsiStrLower converts all characters in the given string to lower case. The conversion uses the current Windows locale. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Ansi Convert to UpperCase</TEXT>
   <PROTO>function AnsiStrUpper(Str: PChar): PChar;</PROTO>
   <DESCRIPTION>AnsiStrUpper converts all characters in the given string to upper case. The conversion uses the current Windows locale. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Appends a maximum of characters from a pchar to another pchar</TEXT>
   <PROTO>function StrLCat(Dest, Source: PChar; MaxLen: Cardinal): PChar;</PROTO>
   <DESCRIPTION>StrLCat appends at most MaxLen - StrLen(Dest) characters from Source to the end of Dest, and returns Dest. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Appends a pchar to another pchar</TEXT>
   <PROTO>function StrCat(Dest, Source: PChar): PChar;</PROTO>
   <DESCRIPTION>StrCat appends a Copy of Source to the end of Dest and returns Dest. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Compare two pchars with a maximum length and with case sensitivity</TEXT>
   <PROTO>function StrLComp(Str1, Str2: PChar; MaxLen: Cardinal): integer;</PROTO>
   <DESCRIPTION>StrLComp compares Str1 to Str2, for a maximum length of MaxLen characters. The return Value is the same as StrComp. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Compare two pchars with a maximum length and without case sensitivity</TEXT>
   <PROTO>function StrLIComp(Str1, Str2: PChar; MaxLen: Cardinal): integer;</PROTO>
   <DESCRIPTION>StrLIComp compares Str1 to Str2, for a maximum length of MaxLen characters, without case sensitivity. The return Value is the same as StrComp. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Compare two pchars with case sensitivity</TEXT>
   <PROTO>function StrComp(Str1, Str2: PChar): integer;</PROTO>
   <DESCRIPTION>StrComp compares Str1 to Str2. The return Value is less than 0 if Str1 &lt; Str2, 0 if Str1 = Str2, or greater than 0 if Str1 &gt; Str2. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Compare two pchars without case sensitivity</TEXT>
   <PROTO>function StrIComp(Str1, Str2: PChar): integer;</PROTO>
   <DESCRIPTION>StrIComp compares Str1 to Str2, without case sensitivity. The return Value is the same as StrComp. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert to LowerCase</TEXT>
   <PROTO>function StrLower(Str: PChar): PChar;</PROTO>
   <DESCRIPTION>StrLower converts Str to lower case and returns Str. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert to UpperCase</TEXT>
   <PROTO>function StrUpper(Str: PChar): PChar;</PROTO>
   <DESCRIPTION>StrUpper converts Str to upper case and returns Str. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Copy a string to a pchar with a maximum number of characters</TEXT>
   <PROTO>function StrPLCopy(Dest: PChar; const Source: string;MaxLen: Cardinal): PChar;</PROTO>
   <DESCRIPTION>StrPLCopy copies at most MaxLen characters from the Pascal style string Source into Dest and returns Dest. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Copy a string to a pchar</TEXT>
   <PROTO>function StrPCopy(Dest: PChar; const Source: string): PChar;</PROTO>
   <DESCRIPTION>StrPCopy copies the Pascal style string Source into Dest and returns Dest. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Copy to another pchar</TEXT>
   <PROTO>function StrCopy(Dest, Source: PChar): PChar;</PROTO>
   <DESCRIPTION>StrCopy copies Source to Dest and returns Dest. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Copy to another pchar a maximum of characters and returns the end</TEXT>
   <PROTO>function StrLCopy(Dest, Source: PChar; MaxLen: Cardinal): PChar;</PROTO>
   <DESCRIPTION>StrLCopy copies at most MaxLen characters from Source to Dest and returns Dest. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Copy to another pchar and returns the end</TEXT>
   <PROTO>function StrECopy(Dest, Source: PChar): PChar;</PROTO>
   <DESCRIPTION>StrECopy copies Source to Dest and returns StrEnd(Dest). </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Create a new pchar</TEXT>
   <PROTO>function StrNew(Str: PChar): PChar;</PROTO>
   <DESCRIPTION>StrNew allocates a Copy of Str on the heap. if Str is NIL, StrNew returns NIL and doesn't allocate any heap space. Otherwise, StrNew makes a duplicate of Str, obtaining space with a call to the StrAlloc function, and returns a pointer to the duplicated string. To dispose the string, use StrDispose. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Extract quoted pchar</TEXT>
   <PROTO>function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;</PROTO>
   <DESCRIPTION>AnsiExtractQuotedStr removes the Quote characters from the beginning and end of a quoted string, and reduces pairs of Quote characters within the quoted string to a single character. if the first character in Src is not the Quote character, the function returns an empty string.  The function copies characters from the Src to the result string until the second solitary Quote character or the first null character in Src. The Src parameter is updated to point to the first character following the quoted string.  If the Src string does not contain a matching end Quote character, the Src parameter is updated to point to the terminating null character in Src. This function supports multibyte character strings (MBCS). </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Free the space allocated for the pchar</TEXT>
   <PROTO>procedure StrDispose(Str: PChar);</PROTO>
   <DESCRIPTION>StrDispose disposes a string that was previously allocated with StrAlloc or StrNew. if Str is NIL, StrDispose does nothing. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get a pointer to the last char</TEXT>
   <PROTO>function AnsiStrLastChar(P: PChar): PChar;</PROTO>
   <DESCRIPTION>AnsiStrLastChar returns a pointer to the last full character in the string. This function supports multibyte characters. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the last character</TEXT>
   <PROTO>function StrEnd(Str: PChar): PChar;</PROTO>
   <DESCRIPTION>Strend returns a pointer to the null character that terminates Str. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the length</TEXT>
   <PROTO>function StrLen(Str: PChar): Cardinal;</PROTO>
   <DESCRIPTION>StrLen returns the number of characters in Str, not counting the null terminator. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Get the reserved size for the pchar</TEXT>
   <PROTO>function StrBufSize(Str: PChar): Cardinal;</PROTO>
   <DESCRIPTION>StrBufSize returns the allocated size of the given buffer, not including the two byte header. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Move characters</TEXT>
   <PROTO>function StrMove(Dest, Source: PChar; Count: Cardinal): PChar;</PROTO>
   <DESCRIPTION>StrMove copies exactly Count characters from Source to Dest and returns Dest. Source and Dest may overlap. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Scan for the first occurence of a char in a pchar</TEXT>
   <PROTO>function StrScan(Str: PChar; Chr: Char): PChar;</PROTO>
   <DESCRIPTION>StrScan returns a pointer to the first occurrence of Chr in Str. if Chr does not occur in Str, StrScan returns NIL. The null terminator is considered to be part of the string. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Scan for the last occurence of a char in a pchar</TEXT>
   <PROTO>function StrRScan(Str: PChar; Chr: Char): PChar;</PROTO>
   <DESCRIPTION>StrRScan returns a pointer to the last occurrence of Chr in Str. if Chr does not occur in Str, StrRScan returns NIL. The null terminator is considered to be part of the string. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Scan for a substring in a pchar</TEXT>
   <PROTO>function StrPos(Str1, Str2: PChar): PChar;</PROTO>
   <DESCRIPTION>StrPos returns a pointer to the first occurrence of Str2 in Str1. If Str2 does not occur in Str1, StrPos returns NIL. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>string formatting</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Format</TEXT>
   <PROTO>function Format(const Format: string; const Args: array of const): string;</PROTO>
   <DESCRIPTION>The Format routine formats the argument list given by the Args parameter using the format string given by the Format parameter.  
Format strings contain two types of objects--plain characters and format specifiers. Plain characters are copied verbatim to the resulting string. Format specifiers fetch arguments from the argument list and apply formatting to them.  
Format specifiers have the following form:  
  "%" [index ":"] ["-"] [width] ["." prec] type  
A format specifier begins with a % character. After the % come the following, in this order:  
-  an optional argument index specifier, [index ":"] -  an optional left-justification indicator, ["-"] -  an optional width specifier, [width] -  an optional precision specifier, ["." prec] -  the conversion type character, type  
The following conversion characters are supported:  
d  Decimal. The argument must be an integer Value. The Value is converted    to a string of decimal digits. if the format string contains a precision    specifier, it indicates that the resulting string must contain at least    the specified number of digits; if the Value has less digits, the    resulting string is left-padded with zeros.  
u  Unsigned decimal.  Similar to 'd' but no sign is output.  
e  Scientific. The argument must be a floating-point Value. The Value is    converted to a string of the form "-d.ddd...E+ddd". The resulting    string starts with a minus sign if the number is negative, and one digit    always precedes the decimal point. The total number of digits in the    resulting string (including the one before the decimal point) is given    by the precision specifer in the format string--a default precision of    15 is assumed if no precision specifer is present. The "E" exponent    character in the resulting string is always followed by a plus or minus    sign and at least three digits.  
f  Fixed. The argument must be a floating-point Value. The Value is    converted to a string of the form "-ddd.ddd...". The resulting string    starts with a minus sign if the number is negative. The number of digits    after the decimal point is given by the precision specifier in the    format string--a default of 2 decimal digits is assumed if no precision    specifier is present.  
g  General. The argument must be a floating-point Value. The Value is    converted to the shortest possible decimal string using fixed or    scientific format. The number of significant digits in the resulting    string is given by the precision specifier in the format string--a    default precision of 15 is assumed if no precision specifier is present.    Trailing zeros are removed from the resulting string, and a decimal    point appears only if necessary. The resulting string uses fixed point    format if the number of digits to the left of the decimal point in the    Value is less than or equal to the specified precision, and if the    Value is greater than or equal to 0.00001. Otherwise the resulting    string uses scientific format.  
n  Number. The argument must be a floating-point Value. The Value is    converted to a string of the form "-d,ddd,ddd.ddd...". The "n" format    corresponds to the "f" format, except that the resulting string             contains thousand separators.  
m  Money. The argument must be a floating-point Value. The Value is    converted to a string that represents a currency amount. The conversion    is controlled by the Currencystring, CurrencyFormat, NegCurrFormat,    ThousandSeparator, DecimalSeparator, and CurrencyDecimals global    variables, all of which are initialized from the Currency Format in    the International section of the Windows Control Panel. if the format    string contains a precision specifier, it overrides the Value given    by the CurrencyDecimals global variable.  
p  Pointer. The argument must be a pointer Value. The Value is converted    to a string of the form "XXXX:YYYY" where XXXX and YYYY are the    segment and offset parts of the pointer expressed as four hexadecimal    digits.  
s  string. The argument must be a character, a string, or a PChar Value.    The string or character is inserted in place of the format specifier.    The precision specifier, if present in the format string, specifies the    maximum length of the resulting string. if the argument is a string    that is longer than this maximum, the string is truncated.  
x  Hexadecimal. The argument must be an integer Value. The Value is    converted to a string of hexadecimal digits. if the format string    contains a precision specifier, it indicates that the resulting string    must contain at least the specified number of digits; if the Value has    less digits, the resulting string is left-padded with zeros.  
Conversion characters may be specified in upper case as well as in lower case--both produce the same results.  
For all floating-point formats, the actual characters used as decimal and thousand separators are obtained from the DecimalSeparator and ThousandSeparator global variables.  
Index, width, and precision specifiers can be specified directly using decimal digit string (for example "%10d"), or indirectly using an asterisk character (for example "%*.*f"). When using an asterisk, the next argument in the argument list (which must be an integer Value) becomes the Value that is actually used. For example "Format('%*.*f', [8, 2, 123.456])" is the same as "Format('%8.2f', [123.456])".  
A width specifier sets the minimum field width for a conversion. if the resulting string is shorter than the minimum field width, it is padded with blanks to increase the field width. The default is to right-justify the result by adding blanks in front of the Value, but if the format specifier contains a left-justification indicator (a "-" character preceding the width specifier), the result is left-justified by adding blanks after the Value.  
An index specifier sets the current argument list index to the specified Value. The index of the first argument in the argument list is 0. Using index specifiers, it is possible to format the same argument multiple times. For example "Format('%d %d %0:d %d', [10, 20])" produces the string '10 20 10 20'.  
The Format function can be combined with other formatting functions. For example  
  S := Format('Your total was %s on %s', [     FormatFloat('$#,##0.00;;zero', Total),     FormaTDateTime('mm/dd/yy', Date)]);  
which uses the FormatFloat and FormaTDateTime functions to customize the format beyond what is possible with Format.  </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format to a string</TEXT>
   <PROTO>procedure FmtStr(var Result: string; const Format: string;const Args: array of const);</PROTO>
   <DESCRIPTION>FmtStr formats the argument list given by Args using the format string given by Format into the string variable given by Result. For further details, see the description of the Format function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format to a pchar</TEXT>
   <PROTO>function StrFmt(Buffer, Format: PChar; const Args: array of const): PChar;</PROTO>
   <DESCRIPTION>StrFmt formats the argument list given by Args using the format string given by Format into the buffer given by Buffer. It is up to the caller to ensure that Buffer is large enough for the resulting string. The returned Value is Buffer. For further details, see the description of the Format function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format to a pchar with a maximum length</TEXT>
   <PROTO>function StrLFmt(Buffer: PChar; MaxLen: Cardinal; Format: PChar;  const Args: array of const): PChar;</PROTO>
   <DESCRIPTION>StrFmt formats the argument list given by Args using the format string given by Format into the buffer given by Buffer. The resulting string will contain no more than MaxLen characters, not including the null terminator. The returned Value is Buffer. For further details, see the description of the Format function. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Format to a buffer</TEXT>
   <PROTO>function FormatBuf(var Buffer; BufLen: Cardinal; const Format;FmtLen: Cardinal; const Args: array of const): Cardinal;</PROTO>
   <DESCRIPTION>FormatBuf formats the argument list given by Args using the format string given by Format and FmtLen into the buffer given by Buffer and BufLen. The Format parameter is a reference to a buffer containing FmtLen characters, and the Buffer parameter is a reference to a buffer of BufLen characters. The returned Value is the number of characters actually stored in Buffer. The returned Value is always less than or equal to BufLen. For further details, see the description of the Format function. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>string handling</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Adjust line breaks</TEXT>
   <PROTO>function AdjustLineBreaks(const S: string): string;</PROTO>
   <DESCRIPTION>AdjustLineBreaks adjusts all line breaks in the given string to be true CR/LF sequences. The function changes any CR characters not followed by a LF and any LF characters not preceded by a CR into CR/LF pairs. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Compare with case sensitivity</TEXT>
   <PROTO>function CompareStr(const S1, S2: string): integer;</PROTO>
   <DESCRIPTION>CompareStr compares S1 to S2, with case-sensitivity. The return Value is less than 0 if S1 &lt; S2, 0 if S1 = S2, or greater than 0 if S1 &gt; S2. The compare operation is based on the 8-bit ordinal Value of each character and is not affected by the current Windows locale. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Compare without case sensitivity</TEXT>
   <PROTO>function CompareText(const S1, S2: string): integer;</PROTO>
   <DESCRIPTION>CompareText compares S1 to S2, without case-sensitivity. The return Value is the same as for CompareStr. The compare operation is based on the 8-bit ordinal Value of each character, after converting 'a'..'z' to 'A'..'Z', and is not affected by the current Windows locale. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert to LowerCase</TEXT>
   <PROTO>function LowerCase(const S: string): string;</PROTO>
   <DESCRIPTION>Lowercase converts all ASCII characters in the given string to lower case. The conversion affects only 7-bit ASCII characters between 'A' and 'Z'. To convert 8-bit international characters, use AnsiLowerCase. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Convert to UpperCase</TEXT>
   <PROTO>function UpperCase(const S: string): string;</PROTO>
   <DESCRIPTION>Uppercase converts all ASCII characters in the given string to upper case. The conversion affects only 7-bit ASCII characters between 'A' and 'Z'. To convert 8-bit international characters, use AnsiUpperCase. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Load from resource</TEXT>
   <PROTO>function LoadStr(Ident: integer): string;</PROTO>
   <DESCRIPTION>LoadStr loads the string resource given by Ident from the Application's executable file. if the string resource does not exist, an empty string is returned. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Load from resource and use it for a format</TEXT>
   <PROTO>function FmtLoadStr(Ident: integer; const Args: array of const): string;</PROTO>
   <DESCRIPTION>LoadStr loads the string resource given by Ident from the Application's executable file, and uses it as the format string in a call to the Format function with the given arguments. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Quote</TEXT>
   <PROTO>function QuotedStr(const S: string): string;</PROTO>
   <DESCRIPTION>QuotedStr returns the given string as a quoted string. A single quote character is inserted at the beginning and the end of the string, and for each single quote character in the string, another one is added.  
</DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Remove spaces and control characters</TEXT>
   <PROTO>function Trim(const S: string): string;</PROTO>
   <DESCRIPTION>Trim trims leading and trailing spaces and control characters from the given string. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Remove spaces and control characters on left</TEXT>
   <PROTO>function TrimLeft(const S: string): string;</PROTO>
   <DESCRIPTION>TrimLeft trims leading and trailing spaces and control characters from the given string. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Remove spaces and control characters on right</TEXT>
   <PROTO>function TrimRight(const S: string): string;</PROTO>
   <DESCRIPTION>TrimRight trims leading and trailing spaces and control characters from the given string. </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Valid identifier ?</TEXT>
   <PROTO>function IsValidIdent(const Ident: string): Boolean;</PROTO>
   <DESCRIPTION>IsValidIdent returns true if the given string is a valid identifier. An identifier is defined as a character from the set ['A'..'Z', 'a'..'z', '_']s followed by zero or more characters from the set ['A'..'Z', 'a'..'z', '0..'9', '_']. </DESCRIPTION>
  </NODE>
 </NODE>
 <NODE>
  <TEXT>string utils</TEXT>
  <PROTO></PROTO>
  <DESCRIPTION></DESCRIPTION>
  <NODE>
   <TEXT>Replace substrings</TEXT>
   <PROTO>function stringReplace(const S, OldPattern, NewPattern: string;Flags: TReplaceFlags): string;</PROTO>
   <DESCRIPTION>stringReplace replaces occurances of &lt;oldpattern&gt; with &lt;newpattern&gt; in a given string.  Assumes the string may contain Multibyte characters  
TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase); </DESCRIPTION>
  </NODE>
  <NODE>
   <TEXT>Wrap text</TEXT>
   <PROTO>function WrapText(const Line, BreakStr: string; BreakChars: TSysCharSet;MaxCol: integer): string;</PROTO>
   <DESCRIPTION>WrapText will scan a string for BreakChars and insert the BreakStr at the last BreakChar position before MaxCol.  Will not insert a break into an embedded quoted string (both ''' and '"' supported)  
</DESCRIPTION>
  </NODE>
 </NODE>
</LEXICAL>
þ  ,   ÿÿ
 J V _ I C O N       0 	                   è     (       @                                             ÀÀÀ    ÿ  ÿ   ÿÿ ÿ   ÿ ÿ ÿÿ  ÿÿÿ                                                                                                                                                                                                                                                                                                                                                                                                                               H  ,   ÿÿ
 J V _ S E P         0 	        {*************************************************************}
{/////////////////////////////////////////////////////////////}
{,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,}
{.............................................................}
{_____________________________________________________________}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||}
{°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°}
//***********************************************************//
//***********************************************************
///////////////////////////////////////////////////////////////
//-----------------------------------------------------------\\
//____________________________________________________________O  ,   ÿÿ
 J V _ T I P S       0 	        To indent your code, you can press ctrl+shift+i or ctrl+shift+u
For a quick access in the component palette, go to component/configure palette and add a && like in a menu.
To edit a TColor, TCursor, TFont property, just double click on the property in the object inspector
To Create a multiline hint, double click the hint property in the object inspector, you''ll see the BU Multiline editor

Delphi 4 only : To see a small funny video, go to help/about and press ctrl+chuck
Delphi 4 only : To see the list of developers working on delphi, go to help/about and press ctrl+developers
Delphi 4 only : To see the team working on delphi, go to help/about and press ctrl+team

When you are in the code editor, press ctrl+e to make a quick search.
To make a rectangular selection in the Delphi Ide, press the ALT key and select your text !.
 Ä  8   ÿÿ
 J V _ E X C E P T I O N S       0 	        EAbort
EAbstractError
EAccessViolation
EAssertionFailed
EControlC
EConvertError
EDivByZero
EExternal
EExternalException
EHeapException
EInOutError
EIntError
EIntfCastError
EIntOverflow
EInvalidCast
EInvalidContainer
EInvalidInsert
EInvalidOp
EInvalidPointer
EMathError
EOutOfMemory
EOverflow
EPackageError
EPrivilege
EPropReadOnly
EPropWriteOnly
ERangeError
EStackOverflow
EUnderflow
EVariantError
EWin32Error
EZeroDivide