unit testfile_VariantRecordLinebreak;

interface

type
  TEmployee = record
    FirstName, LastName: string[40];
    BirthDate: TDate;
    case Salaried: Boolean of
      True: (AnnualSalary: Currency);
      False:
      (HourlyWage: Currency);
  end;

implementation

end.
