       >>source free
 identification          division.
*>===============================
*>
*>**
     Program-Id.         xl160.
*>**
*>   Author.             Vincent B Coen FBCS, FIDM, FIDPM,
*>                       started 09/02/2023
*>                       For Applewood Computers.
*>   WRITTEN.            9th February 2023.
*>**
*>   Remarks.            Back all ISAM files to sequential and/or reload all
*>                       restore them from Seq to ISAM.
*>                       This program is only for Cobol ISAM files, it will check
*>                       that RDB processing is not set in the system parameter
*>                       file record type 1.
*>
*>**
*>   Security.           Copyright (C) 1976-2025 and later, Vincent Bryan Coen.
*>                       Distributed under the GNU General Public License.
*>                       See the file COPYING for details.
*>**
*>   Version.            See Prog-Name In Ws.
*>**
*>   Called Modules.       ?????????????
*>
*>**
*>   Temporary files.
*>                       None.
*>**
*>   Error messages used.
*>**
*>   Error Codes.
*>                       ????
*>  Changes.
*> 09/02/23 vbc - 1.0 First written
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*>
*> ********************************************************************
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
 environment             division.
*>===============================
*>
 copy "envdiv.cob".
 input-output            Section.
*>------------------------------
*>
 file-control.
*>-----------
*> Common - System
*>
 copy "selsys.cob". *> acas000
*>  copy "selsys2.cob".
 copy "selanal.cob". *> acas015
 copy "selpost-irs.cob".  *> acas008
 copy "seldel.cob". *> acas014


 copy "selval.cob". *> acas013

*>
*>  General Ledger
*>
 copy "selbatch.cob".  *> acas007
 copy "seledger.cob".  *> acas005
 copy "selpost.cob".  *> acas006


*>
*>  IRS
*>
 *> acasirsub5
     select Final-File       assign file-37
                             organization line sequential,
                             access sequential,
                             status fs-reply.
*>   irsub4
 *>    select  Posting-File    assign   File-36      *>  "post.dat"
 *>                            organization indexed
 *>                            access   dynamic
 *>                            record   key-4
 *>                            status   fs-reply.
*>  irsub3
     select Default-File     assign file-35
                             organization line sequential,
                             access sequential,
                             status fs-reply.
*> irsub1
     select nominal-ledger  assign       File-34   *> nl-file
                            organization indexed
                            access       dynamic
                            record       key-1
                            status       fs-reply.

*>
*>  Stock
*>
 copy "selstock.cob". *> acas011
 xopy "selaud.cob".  *> acas010

*>
*>  Sales Ledger
*>
 copy "selsl.cob".  *> acas012
 copy "seldnos.cob". *> acas017
 copy "slselinv.cob". *> acas016

*> copy "selinv.cob". *> ????
 copy "seloi2.cob". *> SL
 copy "slseloi3.cob".  *> SL acas017



*>
*> Purchase Ledger
*>
 copy "plselpinv.cob". *> acas026
 copy "selpl.cob". *> acac022
 copy "selpay.cob". *> acas032
 copy "selpdnos.cob" replacing leading ==File-17== by ==File-23==. *> acas023

 copy "seloi4.cob".  *> PL

 copy "plseloi5.cob". *> PL acas029


*>
 data                    division.
*>===============================
*>
 file Section.
*>-----------
*>
*>  System wide - common
 copy "fdsys.cob". *> acas000
 copy "fdanal.cob". *> acas015
 copy "fdval.cob". *> acas013
 copy "fdpost-irs.cob".  *> acas008
 copy "fddel.cob". *> acas014


*> General
 copy "fdbatch.cob".  *> acas007
 copy "fdledger.cob".  *> acas005
 copy "fdpost.cob".   *> acas006


*>  IRS
 fd  Final-File.  *> acasirsub5
*>
 01  Record-5            pic x(655).
*>
 fd  Posting-File.  *> irsub4
*>
 01  Record-4.
     03  Key-4.
         05  Key-Number   pic 9(5).
     03  Post4-Code       pic xx.
     03  Post4-Date       pic x(8).
     03  Post4-DR         pic 9(5).
     03  Post4-CR         pic 9(5).
     03  Post4-Amount     pic s9(7)v99   sign is leading.
     03  Post4-Legend     pic x(32).
     03  Vat-AC-Def4      pic 99.
     03  Post4-Vat-Side   pic xx.
     03  Vat-Amount4      pic s9(7)v99   sign is leading.
*>
 fd  Default-File.
*>
 01  Record-3            pic x(264).
*>
 fd  nominal-ledger.
 copy "irsfdwsnl.cob".
*>



*> Stock
 copy "fdaudit.cob".   *> acas010
 copy "fdstock.cob". *> acas011

*>  Sales
 copy "fdsl.cob".  *> acas012
 copy "fddnos.cob". *> acas017
 copy "slfdinv.cob". *> acas016
 copy "slfdoi3.cob".   *> acas019    *> & oi2  Sales



*> Purchase
 copy "plfdpinv.cob". *> acas026
 copy "fdpay.cob". *> acas032
 copy "fdpl.cob". *> acac022
 copy "plfdois.cob". *> ??
 copy "fddnos.cob". *> acas023
 copy "plfdoi5.cob".  *> acas029

*> ?????
 copy "fdoi2.cob".
 copy "fdoi4.cob".

 working-storage section.
 procedure division.




