       >>source free
*>****************************************************************
*>                                                               *
*>                Date Validation & Conversion                   *
*>                                                               *
*>****************************************************************
*>
 identification   division.
*>========================
*>
*>**
      Program-Id.         maps04.
*>**
*>    Author.             V B Coen FBCS, FIDM, FIDPM, 31/10/1982
*>                        For Applewood Computers.
*>**
*>    Security.           Copyright (C) 1976-2025, Vincent Bryan Coen.
*>                        Distributed under the GNU General Public License.
*>                        See the file COPYING for details.
*>**
*>    Remarks.            Date Validation / Conversion.
*>                        Converts and checks dates in 10 chars to/from
*>                        9(8) bin-long in form dd/mm/ccyy.
*>
*>                        Needs to look at all code using this to see
*>                        if conversion is needed instead of just storing
*>                         as CCYYMMDD.
*>**
*>    Version.            1.04 of 03/02/02 21:00.
*>                        1.11 of 12/03/09.
*>****
*>
*> changes:
*> 05/02/02 vbc - Converted to year 2k using dd/mm/yyyy.
*> 29/01/09 vbc - Migration to GNU Cobol & using intrinsic functions
*>                to do most of the work as v1.10 for MAPS04, to help
*>                reduce risk of format change problems in old programs.
*>
*> 19/10/16 vbc - THIS uses binary dates from 31/12/1600 so is NOT usable
*>                 within IRS as is, but in any event uses dates with CC
*>                 e.g., dd/mm/ccyy where as IRS uses dd/mm/yy.
*>                 but fixable within IRS itself.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*>
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
*>*************************************************************************
*>
 environment      division.
*>========================
*>
 copy  "envdiv.cob".
 input-output     section.
*>-----------------------
*>
 data             division.
*>========================
 working-storage  section.
*>-----------------------
*>
 01  date-fields.
     03  z                  pic 99 binary.
     03  test-date.
         05  td-ccyy.
             07  td-cc      pic 99.
             07  td-yy      pic 99.
         05  td-mm          pic 99.
         05  td-dd          pic 99.
     03  test-date9 redefines test-date pic 9(8).
*>
 linkage          section.
*>-----------------------
*>
*>*********
*> maps04 *
*>*********
*>
 01  mapa03-ws.
     03  a-date             pic x(10).
     03  filler  redefines  a-date.
       05  a-days           pic 99.
       05  filler           pic x.
       05  a-month          pic 99.
       05  filler           pic x.
       05  a-ccyy           pic 9(4).
       05  filler redefines a-ccyy.
           07  a-cc         pic 99.
           07  a-year       pic 99.
     03  a-bin              binary-long.
*>
 procedure        division using  mapa03-ws.
*>=========================================
*>
 main.
*>---
*>
*> if dd/mm/ccyy is bad a-bin = zero,
*>   if entry a-bin not zero then covert to dd/mm/ccyy
*>
     if       a-bin  >  zero
              go to  ws-unpack.
*>
     move     zero    to  z.
     inspect  a-date replacing all "." by "/".
     inspect  a-date replacing all "," by "/".
     inspect  a-date replacing all "-" by "/".
     inspect  a-date tallying z for all "/".
*>
*>  Very basic testing here as function test-date checks for
*>           February and leap years
*>
     if       z not = 2 or
              a-days not numeric or
              a-month not numeric or
              a-cc   not numeric or
              a-days < 01 or > 31 or
              a-month < 01 or > 12
              go to main-exit.
*>
     move     a-cc    to td-cc.
     move     a-year  to td-yy.
     move     a-month to td-mm.
     move     a-days  to td-dd.
*>
     if       function test-date-yyyymmdd (test-date9) not = zero
              go to main-exit.
*>
*>********************************************
*>       Date Validation & Conversion        *
*>       ============================        *
*>                                           *
*>  Requires date input in a-date as         *
*>  dd.mm.yy or dd.mm.ccyy & returns date as *
*>      ccyymmdd in  a-bin                   *
*>  Date errors returned as a-bin equal zero *
*>                                           *
*>********************************************
*>
     move     function integer-of-date (test-date9) to a-bin.
     go       to main-exit.
*>
*>
*>*************************************
*>   Binary Date Conversion Routine   *
*>   ==============================   *
*>                                    *
*>  Requires ccyymmdd input in a-bin  *
*>  &  returns date  in a-date        *
*>  This way dates can be compared    *
*>    as is                           *
*>*************************************
*>
 ws-unpack.
*>========
*>
     move     "00/00/0000" to a-date.
     move     function date-of-integer (a-bin) to test-date.  *> ccyymmdd
     move     td-ccyy to a-ccyy.
     move     td-mm   to a-month.
     move     td-dd   to a-days.          *> Now UK date
*>
 main-exit.
     exit     program.
