unit testfile_EnumIndent;

interface

type
  // Simple enum on one line
  TSimpleEnum = (seOne
               , seTwo
               , seThree
                );

  // Enum already on multiple lines with trailing commas
  TMultiLineEnum = (mlOne
                  , mlTwo
                  , mlThree
                   );

  // Enum with line comments
  TLineCommentEnum = (lcOne // first
                    , lcTwo // second
                    , lcThree // third
                     );

  // Enum with curly brace comments
  TCurlyCommentEnum = (ccOne { first }
                     , ccTwo { second }
                     , ccThree { third }
                      );

  // Enum with old-style comments
  TOldStyleCommentEnum = (ocOne (* first *)
                        , ocTwo (* second *)
                        , ocThree (* third *)
                         );

  // Mixed comment styles
  TMixedCommentEnum = (mcOne // line comment
                     , mcTwo { curly comment }
                     , mcThree (* old-style comment *)
                      );

  // Single member enum
  TSingleEnum = (seOnly);

  // Two member enum
  TTwoMemberEnum = (tmFirst
                  , tmSecond
                   );

  // Enum with values
  TValueEnum = (veOne = 0
              , veTwo = 1
              , veThree = 2
               );

  // Enum with values and comments
  TValueCommentEnum = (vcOne = 0 // first value
                     , vcTwo = 1 { second value }
                     , vcThree = 2 (* third value *)
                      );

  // Enum with leading commas (the target format)
  TLeadingCommaEnum = (lcaOne // first
                     , lcaTwo // second
                     , lcaThree // third
                      );

  // Enum inside a class
  TMyClass = class
    type
      TInnerEnum = (ieOne
                  , ieTwo
                  , ieThree
                   );
  end;

  // Enum with long identifiers
  TLongIdentEnum = (lieSomeVeryLongIdentifierOne
                  , lieSomeVeryLongIdentifierTwo
                  , lieSomeVeryLongIdentifierThree
                   );

  // Enum with multi-line curly brace comment
  TMultiLineCurlyEnum = (mlcOne
                       , { this is a multi-line comment }mlcTwo
                       , mlcThree
                        );

  // Enum with multi-line old-style comment
  TMultiLineOldEnum = (mloOne
                     , (* this is a multi-line comment *)mloTwo
                     , mloThree
                      );

  // Consecutive enum declarations
  TFirstEnum = (feA
              , feB
              , feC
               );
  TSecondEnum = (seA
               , seB
               , seC
                );

  // Enum followed by set type
  TColorEnum = (clRed
              , clGreen
              , clBlue
               );
  TColorSet = set of TColorEnum;

implementation

end.
