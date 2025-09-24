*>*******************************************
*>                                          *
*>  Record Definition For The System File   *
*>                                          *
*>*******************************************
*>
*>   TEMP usage only.
*>
*>  THIS IS VERSION 1.2 of the system file. NOT CURRENT.
*>
*>  file size 1024 with fillers
*> 01/02/09 vbc - Repacked 2 reduce slack
*> 05/04/09 vbc - Light clean up
*> 07/04/09 vbc - Remove op-gen to filler (general)
*> 22/04/09 vbc - Stock control data added.
*> 29/05/09 vbc - Added 'system wide' Print-Lines for all ledgers.
*>  1/06/09 vbc - Added Stock link for PL and SL.
*> 14/09/10 vbc - Added Print-Spool-Name.
*> 15/09/10 vbc - Need to increase rec size by 128 bytes in system-data and more in the others
*>                to bring it up to 1024, the other 2 rec types will also increase to same.
*>                Epos remarked out for Open source versions.
*> 07/11/10 vbc - New fields added as above including vers & sub vers for file for auto
*>                updating of changed file layouts by system
*> 16/11/11 vbc - Added extra 88 in op-system
*> 11/12/11 vbc - Added Date-Form for all of ACAS and removed stk-Date-Form
*> 04/03/12 vbc - Added File-Duplicates-In-Use & FS-Duplicate-Processing + support for MS SQL server.
*> 09/04/12 vbc - Added Needed RDB data, DB Name, User Name and password requred for connecting
*>                 to Database tables.
*> 15/05/13 vbc - Added SL-Stock-Audit to invoicing replacing a filler. Needs adding to rdbms layouts!!!
*> 19/05/13 vbc - Added 4 fields at end of SL block for company name/address headings in
*>                Inv, Stat, Pick, Letters, Vat-prints And VAT registration number in system block
*>                  - NEEDS adding to in RDBMS layouts.
*> 04/06/13 vbc - Added fields Print-Spool-Name2 & Print-Spool-Name3 in filler areas but really need to be moved
*>                to system data block AND moving around some other fields. File size NOT changed
*> 12/06/13 vbc - Added IRS fields to main system file - Starter for 10.
*> 22/09/15 vbc - Added Param-Restrict (Access) within a current filler.
*>                Set this will stop display of option Z in sub-system menus.
*> 25/06/16 vbc - Added RDBMS-Port & needs Host, Socket just a Q of where in the file.
*>                Will increase System-data to 512 and remove Order-Entry Block as unused.
*> 15/05/23 vbc - Updated to sort of match wssystem.cob - but it is still not the same.
*>                           DO TO USE THIS for v3.02 etc.
*> 13/03/24 vbc - In fillers added fields SL-BO-Flag and Stk-BO-Active,
*>
 01  System-Record.
*>******************
*>   System Data   *
*>******************
     03  System-Data-Block.                                  *>   384 changed to 512 bytes (25/06/16)
         05  System-Record-Version-Prime      binary-char.   *>  1
         05  System-Record-Version-Secondary  binary-char.   *>  4  Updated 15/01/18
         05  Vat-Rates                    comp.
             07 Vat-Rate-1   pic 99v99.                      *> Standard rate
             07 Vat-Rate-2   pic 99v99.                      *> Reduced rate
             07 Vat-Rate-3   pic 99v99.                      *> Minimal or exempt
             07 Vat-Rate-4   pic 99v99.   *> 2b used for local sales tax   Not UK
             07 Vat-Rate-5   pic 99v99.   *> 2b used for local sales tax   Not UK
         05  Vat-Rate redefines Vat-Rates pic 99v99 comp occurs 5.
         05  Cyclea          binary-char.  *> 99.
         05  Scycle Redefines cyclea  binary-char.
         05  Period          binary-char.  *> 99.
         05  Page-Lines      binary-char  unsigned. *> 999.
         05  Next-Invoice    binary-long. *> 9(8) comp.
         05  Run-Date        binary-long. *> 9(8) comp.
         05  Start-Date      binary-long. *> 9(8) comp.
         05  End-Date        binary-long. *> 9(8) comp.
         05  Suser.                       *> IRS
             07  Usera       pic x(32).
         05  User-Code       pic x(32).   *> encrypted username not used on OS versions so also b 4 client?
         05  Address-1       pic x(24).
         05  Address-2       pic x(24).
         05  Address-3       pic x(24).
         05  Address-4       pic x(24).
         05  Post-Code       pic x(12).   *> or ZipCode size should cover all countries
         05  Country         pic x(24).
         05  Print-Spool-Name pic x(48).
      *>         05  File-Statuses.
      *>             07  File-Status pic 9          occurs 32.
         05  filler          pic x(32).
         05  Pass-Value      pic 9.
         05  Level.
             07  Level-1     pic 9.
                 88  G-L                    value 1.   *> General (Nominal) ledger
             07  Level-2     pic 9.
                 88  B-L                    value 1.   *> Purchase (Payables) ledger
             07  Level-3     pic 9.
                 88  S-L                    value 1.   *> Sales (Receivables) ledger
             07  Level-4     pic 9.
                 88  Stock                  value 1.   *> Stock Control (Inventory)
             07  Level-5     pic 9.
                 88  O-E                    value 1.   *> Order Entry
             07  Level-6     pic 9.
                 88  Payroll                value 1.   *> Payroll
         05  Pass-Word       pic x(4).                 *>
         05  Host            pic 9.
             88  Multi-User                 value 1.
         05  Op-System       pic 9.
             88  Dos                        value 1.
             88  Windows                    value 2.
             88  Mac                        value 3.
             88  Os2                        value 4.
             88  Unix                       value 5.
             88  Linux                      value 6.
             88  OS-Single                  values 1 2 4.
         05  Current-Quarter pic 9.
         05  RDBMS-Flat-Statuses.
             07  File-System-Used  pic 9.
                 88  FS-Cobol-Files-Used        value zero.
                 88  FS-RDBMS-Used              value 1.
*>                 88  FS-Oracle-Used             value 1.  *> THESE NOT IN USE
*>                 88  FS-MySql-Used              value 2.  *> ditto
*>                 88  FS-Postgres-Used           value 3.  *> ditto
*>                 88  FS-DB2-Used                value 4.  *> ditto
*>                 88  FS-MS-SQL-Used             value 5.  *> ditto
                 88  FS-Valid-Options           values 0 thru 1.    *> 5. (not in use unless 1-5)
             07  File-Duplicates-In-Use pic 9.
                 88  FS-Duplicate-Processing    value 1.
         05  Maps-Ser.      *> Not needed in OpenSource version, = 9999 (No Maintainence Contract]
             07  Maps-Ser-xx pic xx.        *> Allows for 36^2 * 100 customers
             07  Maps-Ser-nn binary-short.  *>       =  129600 - 2
         05  Date-Form       pic 9.
             88  Date-UK                    value 1.  		*> dd/mm/yyyy
             88  Date-USA                   value 2.  		*> mm/dd/yyyy
             88  Date-Intl                  value 3.  		*> yyyy/mm/dd
             88  Date-Valid-Formats         values 1 2 3.
         05  Data-Capture-Used pic 9.
             88  DC-Cobol-Standard          value zero.
             88  DC-GUI                     value 1.
             88  DC-Widget                  value 2.
         05  RDBMS-DB-Name   pic x(12)      value "ACASDB".     *> change in setup
         05  RDBMS-User      pic x(12)      value "ACAS-User".  *> change in setup
         05  RDBMS-Passwd    pic x(12)      value "PaSsWoRd".   *> change in setup
         05  VAT-Reg-Number  pic x(11)      value spaces.
         05  Param-Restrict  pic x.                             *> Only via ACAS?
         05  RDBMS-Port      pic x(5)       value "3306".       *> change in setup
         05  RDBMS-Host      pic x(32)      value spaces.       *> change in setup
         05  RDBMS-Socket    pic x(64)      value spaces.       *> change in setup
         05  Stats-Date-Period pic 9(4).                      *> added 15/01/18.
         05  filler          pic x(30).                         *> for expansion v3
*>***************
*>   G/L Data   *
*>***************
     03  General-Ledger-Block.               *> 80 bytes
         05  P-C             pic x.
             88  Profit-Centres             value "P".
             88  Branches                   value "B".
         05  P-C-Grouped     pic x.
             88  Grouped                    value "Y".
         05  P-C-Level       pic x.
             88  Revenue-Only               value "R".
         05  Comps           pic x.
             88  Comparatives               value "Y".
         05  Comps-Active    pic x.
             88  Comparatives-Active        vaLUE "Y".
         05  M-V             pic x.
             88  Minimum-Validation         vaLUE "Y".
         05  Arch            pic x.
             88  Archiving                  value "Y".
         05  Trans-Print     pic x.
             88  Mandatory                  value "Y".
         05  Trans-Printed   pic x.
             88  Trans-Done                 value "Y".
         05  Header-Level    pic 9.
         05  Sales-Range     pic 9.
         05  Purchase-Range  pic 9.
         05  Vat             pic x.
             88  Auto-Vat                   value "Y".
         05  Batch-Id        pic x.
             88  Preserve-Batch             value "Y".
         05  Ledger-2nd-Index pic x.                     	*> But file uses SINGLE INDEX only ???
             88  Index-2                    value "Y".
         05  Irs-Instead     pic x.
             88  Irs-Used                   value "Y".
         05  Ledger-Sec      binary-short.  *> 9(4) comp
         05  Updates         binary-short.  *> 9(4) comp
         05  Postings        binary-short.  *> 9(4) comp
         05  Next-Batch      binary-short.  *> 9(4) comp  should be unsigned used for all ledgers
         05  Extra-Charge-Ac binary-long.   *> 9(8) comp
         05  Vat-Ac          binary-long.   *> 9(8) comp
         05  Print-Spool-Name2 pic x(48).
*>******************
*>   P(B)/L Data   *
*>******************
     03  Purchase-Ledger-Block.             *> 88 bytes
         05  Next-Folio      binary-long.   *> 9(8) comp
         05  BL-Pay-Ac       binary-long.   *> 9(8) comp
         05  P-Creditors     binary-long.   *> 9(8) comp
         05  BL-Purch-Ac     binary-long.   *> 9(8) comp
         05  BL-End-Cycle-Date binary-long. *> 9(8) comp
         05  BL-Next-Batch   binary-short.  *> 9(4) comp  should be unsigned - unused ?
         05  Age-To-Pay      binary-char.   *> 9(4) comp should be unsigned
         05  Purchase-Ledger pic x.
             88  P-L-Exists                 value "Y".
         05  PL-Delim        pic x.
         05  Entry-Level     pic 9.
         05  P-Flag-A        pic 9.
         05  P-Flag-I        pic 9.
         05  P-Flag-P        pic 9.
         05  PL-Stock-Link   pic x.
         05  Print-Spool-Name3 pic x(48).
         05  PL-Autogen      pic x          value space.        *> added 14/04/23 NOT USED YET
         05  PL-Next-Rec     binary-short unsigned.            *> 2 bytes 0 - 65k
         05  filler          pic x(7).
*>***************
*>   S/L Data   *
*>***************
     03  Sales-Ledger-Block.                *> 128 bytes
         05  Sales-Ledger    pic x.
             88  S-L-Exists                 value "Y".
         05  SL-Delim        pic x.
         05  Oi-3-Flag       pic x.         *> 'Y' used in sl060 why?
         05  Cust-Flag       pic x.
         05  Oi-5-Flag       pic x.
         05  S-Flag-Oi-3     pic x.         *> 'z' when otm3 created, used in sl060 why? NO LONGER USED
         05  Full-Invoicing  pic 9.
         05  S-Flag-A        pic 9.         *> '1' used in sl060 why?
         05  S-Flag-I        pic 9.         *> '2' used in sl060 why?
         05  S-Flag-P        pic 9.
         05  SL-Dunning      pic 9.
         05  SL-Charges      pic 9.
         05  Sl-Own-Nos      pic x.
         05  SL-Stats-Run    pic 9.
         05  Sl-Day-Book     pic 9.
         05  invoicer        pic 9.
             88  I-Level-0                  value 0.  *> show totals only (no net & vat) not used?
             88  I-Level-1                  value 1.  *> Show net, vat
             88  I-Level-2                  value 2.  *> show Details + vat etc looks wrong in sl910 totals only (no net & vat)
             88  Not-Invoicing              value 9.  *> show totals only (no net & vat) but not found yet nor level 3 (see sl900)
         05  Extra-Desc      pic x(14).
         05  Extra-Type      pic x.
             88  Discount                   value "D".
             88  Charge                     value "C".
         05  Extra-Print     pic x.
         05  SL-Stock-Link   pic x.
         05  SL-Stock-Audit  pic x.
             88  Stock-Audit-On             value "Y".   *> Invoicing will create an audit record (15/05/13)
         05  SL-Late-Per     pic 99v99    comp.
         05  SL-Disc         pic 99v99    comp.
         05  Extra-Rate      pic 99v99    comp.
         05  SL-Days-1       binary-char.    *> 999  comp.
         05  SL-Days-2       binary-char.    *> 999  comp.
         05  SL-Days-3       binary-char.    *> 999  comp.
         05  SL-Credit       binary-char.    *> 999  comp.
         05  filler          binary-short.   *> No longer used.
         05  SL-Min          binary-short.   *> 9999  comp.
         05  SL-Max          binary-short.   *> 9999  comp.
         05  PF-Retention    binary-short.   *> 9999  comp.
         05  First-Sl-Batch  binary-short.   *> 9999  comp.   *>unused ?
         05  First-Sl-Inv    binary-long.    *> 9(8) comp.
         05  SL-Limit        binary-long.    *> 9(8) comp.
         05  SL-Pay-Ac       binary-long.    *> 9(8) comp.
         05  S-Debtors       binary-long.    *> 9(8) comp.
         05  SL-Sales-Ac     binary-long.    *> 9(8) comp.
         05  S-End-Cycle-Date binary-long.   *> 9(8) comp.
         05  SL-Comp-Head-Pick Pic x.
             88  SL-Comp-Pick               value "Y".
         05  SL-Comp-Head-Inv  pic x.
             88  SL-Comp-Inv                value "Y".
         05  SL-Comp-Head-Stat pic x.
             88  SL-Comp-Stat               value "Y".
         05  SL-Comp-Head-Lets pic x.
             88  SL-Comp-Lets               value "Y".
         05  SL-VAT-Printed  pic x.
             88  SL-VAT-Prints              value "Y".
         05  SL-Invoice-Lines pic 99.
         05  SL-Autogen      pic x          value space.        *> added 14/04/23
         05  SL-Next-Rec     binary-short unsigned.             *> 2 bytes 0 - 65k
         05  SL-BO-Flag      pic x          value space.      *> support for Back Ordering - may be = Y for true.
         05  filler          pic X(15).                       *> was x (16)
*> GL overflow
         05  GL-BL-Pay-Ac    binary-long.    *> 9(8) comp    THESE 6 ADDED 06/06/18 to poss. support GL and IRS
         05  GL-P-Creditors  binary-long.    *> 9(8) comp    RUNNING at same time.
         05  GL-BL-Purch-Ac  binary-long.    *> 9(8) comp
         05  GL-SL-Pay-Ac    binary-long.    *> 9(8) comp.
         05  GL-S-Debtors    binary-long.    *> 9(8) comp.
         05  GL-SL-Sales-Ac  binary-long.    *> 9(8) comp.
*>***************
*> Stock Data   *
*>***************
*>
     03  Stock-Control-Block.                *> 88 bytes
         05  Stk-Abrev-Ref   pic x(6).
         05  Stk-Debug       pic 9.          *> T/F (1/0).
         05  Stk-Manu-Used   pic 9.          *> T/F (Bomp/Wip)
         05  Stk-OE-Used     pic 9.          *> T/F.
         05  Stk-Audit-Used  pic 9.          *> T/F.
         05  Stk-Mov-Audit   pic 9.          *> T/F.
         05  Stk-Period-Cur  pic x.          *> M=Monthly, Q=Quarterly, Y=Yearly
         05  Stk-Period-dat  pic x.          *>  --  ditto  --
         05  filler          pic x.    	     *> was stk-date-form
         05  Stock-Control   pic x.
             88  Stock-Control-Exists   value "Y".
         05  Stk-Averaging   pic 9.          *> T/F.
             88  Stock-Averaging        value 1.
         05  Stk-Activity-Rep-Run pic 9.     *> T/F.  =17 bytes 0=no, 1=add, 2=del, 3=both
         05  Stk-BO-Active   pic x.          *> was filler 13/03/24
         05  Stk-Page-Lines  binary-char unsigned.  *> 9999 comp. Taken from Print-Lines
         05  Stk-Audit-No    binary-char unsigned.  *> 9999 comp.
         05  filler          pic x(68).             *> 64    (just in case)
     03  IRS-Entry-Block.			*> NEW 12/06/13
         05  Client             pic x(24). 	*> 		24
         05  Next-Post          pic 9(5).  	*> 		77
         05  Vat-Rates2.
             07  vat1           pic 99v99. 	*> 		81   *> Standard  changed from vat (11/06/13)
             07  vat2           pic 99v99. 	*> 		85   *> reduced 1 [not yet used]
             07  vat3           pic 99v99. 	*> 		89   *> reduced 2 [not yet used]
         05  Vat-Group redefines Vat-Rates2.
             07  Vat-Psent      pic 99v99    occurs 3.
         05  IRS-Pass-Value     pic 9.	 	 *>		90  (Was Pass-Value in IRS system file)
         05  Save-Sequ          pic 9.     	 *> 		91
         05  system-work-group  pic x(18).	 *> 		109
         05  PL-App-Created     pic x.    	 *> 		62
         05  PL-Approp-AC6      pic 9(6). 	 *> 		68   changed for GL support if needed ?. Both needed in RDB
         05  filler redefines PL-Approp-AC6.
             07  filler         pic 9.           *>                   loose leading char for IRS
             07  PL-Approp-AC   pic 9(5).        *>                   For IRS
         05  1st-Time-Flag      pic 9.    	 *> 		69    (was First-Time-Flag in IRS system file)      N  32
         05  filler             pic x(59).       *>             128  Old fn-1 to 5 files
     03  IRS-Data-Block redefines IRS-Entry-Block.
         05  filler-dummy4   pic x(128).
*>
*>         05  filler             pic x(12).     *> when vat rates killed and the main ones used in System-data-block
*>     03  Payroll-Data-Block.                        *> 128 bytes
*>         05  filler-dummy2   pic x(128).		*> Content Removed
*>     03  Epos-Data-Block.
*>         05  filler-dummy3   pic x(128).		*> Content Removed
