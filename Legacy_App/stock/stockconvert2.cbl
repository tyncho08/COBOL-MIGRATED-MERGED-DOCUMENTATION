       >>source free
  *>
  *> 30 March 2024.
  *>
  *> Updates EXISTING Stock FILE records for new field setting it to zero:
  *>   stock-Arrival-Date.
  *>
  *> Compile with cobc -x stockconvert2.cbl
  *>  then do cp stockconvert2 ~/bin  or to the ACAS data folder.
  *>
  *> There will be a copy of stockconvert2.so in ~/bin if running comp-stock*.sh
  *>  you can delete or just ignore it as it will not be used.
  *>
  *>  YOU MUST RUN THIS PROGRAM IN THE Data Folders containing the files :
  *>    stockctl.dat**  for each folder that you use i.e.,  INCLUDING any
  *>      for warehouses if used, and do so one by one, i.e.,
  *>
  *>      cd ACAS
  *>      stockconvert2
  *>      cd whse-1
  *>      stockconvert2
  *>      cd whse-2
  *>      stockconvert2
  *>
  *>       etc, etc.
  *>
 identification division.
 program-id.                 stockconvert2.
*>Author.                    Vincent B Coen, FBCS (ret). 30/03/2024.
*>
*>Security.                  Copyright (C) 2024 & later, Vincent Bryan Coen.
*>                           Distributed under the GNU General Public License.
*>                           See the file COPYING for details.
*>
*>Purpose.                   Convert file for Stock for new field addition.
*>
*> Updates.
*> 13/12/24 vbc - Add close all open files if any error occurs before a STOP RUN
*>                Add instruction to compile it as the scripts will create a .so
*>                file that is not called.
*> 04/02/25 vbc - Add Location as alternative key with dups.
*>
*>*************************************************************************
*>
*>  From copyright.cob.
*>
*> Copyright Notice.
*> ****************
*>
*> This notice supersedes all prior copyright notices & was updated 2024-04-16.
*>
*> These files and programs are part of the Applewood Computers Accounting
*> System and is Copyright (c) Vincent B Coen. 1976-2025 and later.
*>
*> This program is now free software; you can redistribute it and/or modify it
*> under the terms listed here and of the GNU General Public License as
*> published by the Free Software Foundation; version 3 and later as revised
*> for PERSONAL USAGE ONLY and that includes for use within a business but
*> EXCLUDES repackaging or for Resale, Rental or Hire in ANY way.
*>
*> Persons interested in repackaging, redevelopment for the purpose of resale or
*> distribution in a rental or hire mode must get in touch with the copyright
*> holder with your commercial plans and proposals.
*>
*> ACAS is distributed in the hope that it will be useful, but WITHOUT
*> ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
*> FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
*> for more details. If it breaks, you own both pieces but I will endeavour
*> to fix it, providing you tell me about the problem.
*>
*> You should have received a copy of the GNU General Public License along
*> with ACAS; see the file COPYING.  If not, write to the Free Software
*> Foundation, 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*>
*> *>*************************************************************************
*>
 environment division.
 configuration section.
 source-computer.        Linux.
 object-computer.        Linux.
 input-output section.
 file-control.
*>
     select  Old-Stock-File  assign               "stockctl.dat"    *> File-11
                             access               dynamic
                             organization         indexed
                             status               Fs-Reply
                             record key           Stock-Key in Old-Stock-Record.
*>
     select Temp-Stock-File  assign               "Temp-stock.dat"
                             organization         sequential.
*>
     select  Stock-File  assign                   "stockctl.dat"  *>  File-11
                             access               dynamic
                             organization         indexed
                             status               Fs-Reply
                             record key           Stock-Key in Stock-Record
                             alternate record key Stock-Abrev-Key in Stock-Record
                             alternate record key Stock-Desc      in Stock-Record with duplicates
                             alternate record key Stock-Location  in Stock-Record with duplicates. *> New 04/2/25.
*>
 data division.
 file section.
*>
 copy "fdstock.cob".
 copy "fdstock.cob"  replacing ==Stock-File== by ==Temp-Stock-File==
                               ==Stock-Record== by ==Temp-Stock-Record==.
 fd  Old-Stock-File.
*>
 01  Old-Stock-Record.
     03  Stock-Key                pic x(13).
     03  Stock-Abrev-Key          pic x(7).
     03  Stock-Suppliers-Group.
         05  Stock-Supplier-P1    pic x(7).                          *> Primary   Supplier
         05  Stock-Supplier-P2    pic x(7).                          *> Secondary Supplier
         05  Stock-Supplier-P3    pic x(7).                          *> Back Up   Supplier
     03  filler redefines Stock-Suppliers-Group.
         05  Stock-Suppliers      pic x(7)     occurs 3.  *> 41
     03  Stock-Desc               pic x(32).              *> 73
     03  Stock-Construct-Item     pic x(13).              *> 86
     03  Stock-Location           pic x(10).
     03  Stock-PA-Code.
         05  Stock-pa-System      pic x.
         05  Stock-pa-Group.
             07  Stock-pa-First   pic x.
             07  Stock-pa-Second  pic x.
     03  Stock-SA-Code.
         05  Stock-sa-System      pic x.
         05  Stock-sa-Group.
             07  Stock-sa-First   pic x.
             07  Stock-sa-Second  pic x.
     03  Stock-Services-Flag      pic x.                  *> 103  flag 4 services, not product (Y/N)
     03  Stock-Last-Actual-Cost   pic 9(7)v99     comp-3. *> (5)
     03  filler                   pic x(8).
     03  Stock-Construct-Bundle   pic 9(6)        comp.
     03  Stock-Under-Construction pic 9(6)        comp.
     03  Stock-Work-in-Progress   pic 9(6)        comp.
     03  Stock-ReOrder-Pnt        pic 9(6)        comp.
     03  Stock-Std-ReOrder        pic 9(6)        comp.
     03  Stock-Back-Ordered       pic 9(6)        comp.
     03  Stock-On-Order           pic 9(6)        comp.
     03  Stock-Held               pic 9(6)        comp.
     03  Stock-Pre-Sales          pic 9(6)        comp.   *> 36 = 152
     03  Stock-Committed redefines Stock-Pre-Sales
                                  pic 9(6)       comp.
     03  Stock-Retail             pic 9(7)v99     comp-3. *> 5    *> This 3 increased by 1 leading digit
     03  Stock-Cost               pic 9(7)v9999   comp-3. *> 6          Based on last Order Only
     03  Stock-Value              pic 9(9)v99     comp-3. *> 6 = 169
     03  Stock-Order-Due          pic 9(8)    comp.
     03  Stock-Order-Date         pic 9(8)    comp.   *> 177
     03  Stock-Mthly-Running-Totals.                 *> 16:    cleared at EOY cycle
         05  Stock-Adds           pic 9(8)    comp.  *> binary-long.
         05  Stock-Deducts        pic 9(8)    comp.  *> binary-long.
         05  Stock-Wip-Adds       pic 9(8)    comp.  *> binary-long.
         05  Stock-Wip-Deds       pic 9(8)    comp.  *> binary-long.
     03  Stock-History.
         05  Stock-History-Data.    *> 193:       zeroed for new year
             07  Stock-TD-Adds     pic 9(8)   comp  occurs 12.
             07  Stock-TD-Deds     pic 9(8)   comp  occurs 12.
             07  Stock-TD-Wip-Adds pic 9(8)   comp  occurs 12.
             07  Stock-TD-Wip-Deds pic 9(8)   comp  occurs 12.
     03  filler                   pic x(15).                           *> 400  expansion

*>
 working-storage section.
 77  prog-name           pic x(20)  value "Stockconv2 (1.00.00)".
 01  fs-reply            pic 99.
 01  rec-count           pic 9(4)   value zero.
 01  In-Rec-Count        pic 9(4)   value zero.
 01  Out-Rec-Count       pic 9(4)   value zero.
*>
 procedure division.
 AA000-Start.
     open     input Old-Stock-File.
     if       fs-reply not = "00"
              display "Error on opening input Stock file " fs-reply
              close Old-Stock-File
              stop run.
     open     output Temp-Stock-File.
     if       fs-reply  not = "00"
              display  "Failure to Open output Temp Stock File" fs-reply
              close Temp-Stock-File
              close Old-Stock-File
              stop run.
     Display  prog-name " Starts".
*>
 AA010-Read.
     read     Old-Stock-File  next record at end
              close Old-Stock-File Temp-Stock-File
              display " "
              display "Converting Stock recs 1/2 "
                  In-Rec-Count " Read "   rec-count " written - Pass 1 of 2"
*>             Display  prog-name " Ends"
*>             stop run.
              go to AA020-Process-2.
     if       fs-reply not = zero
              display "Error on reading OLD stock recs = " fs-reply
              close Old-Stock-File Temp-Stock-File
              stop run.
     display  "*" no advancing.
*>
     add      1 to In-Rec-Count.
     initialise Temp-Stock-Record with filler.
     move     CORRESPONDING Old-Stock-Record to Temp-Stock-Record.
     write    Temp-Stock-Record.
     if       fs-reply not = zero
              display "Error writing Temp stock record = " fs-reply
              close Old-Stock-File Temp-Stock-File
              stop run.
     add      1 to rec-count.
     go       to AA010-Read.
*>
 AA020-Process-2.
     move     zero to In-Rec-Count Rec-Count.
     open     input Temp-Stock-File.
     if       fs-reply  not = "00"
              display  "Failure to Open input Temp Stock File" fs-reply
              close Temp-Stock-File
              stop run.
     open     output Stock-File.
     if       fs-reply not = "00"
              display "Error on opening output NEW Stock file " fs-reply
              close Stock-File
              close Temp-Stock-File
              stop run.
     display " Phase 2 starts".
*>
 AA020-Read.
     read     Temp-Stock-File  next record at end
              close Stock-File Temp-Stock-File
              display " "
              display "Converting Stock recs 2/2 "
                  In-Rec-Count " Read "   rec-count " written - Pass 2 of 2"
              Display  prog-name " Ends"
              stop run.
     if       fs-reply not = zero
              display "Error on reading Temp stock recs = " fs-reply
              close Stock-File Temp-Stock-File
              stop run.
     display  "*" no advancing.
*>
     add      1 to In-Rec-Count.
     move     CORRESPONDING Temp-Stock-Record to Stock-Record.
     write    Stock-Record.
     if       fs-reply not = zero
              display "Error writing NEW stock record = " fs-reply
              close Stock-File Temp-Stock-File
              stop run.
     add      1 to rec-count.
     go       to AA020-Read.
*>
