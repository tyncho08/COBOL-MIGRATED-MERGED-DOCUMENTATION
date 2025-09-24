OE  Readme Notes
----------------

> vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv <
> CHECK LAST FEW LINES OF THIS FILE TO SEE IF THERE HAS BEEN UPDATES MADE.  <
> IF SO READ ALL OF IT AS CHANGES CAN BE ANY WHERE INCLUDING PREVIOUS TEXT. <
> ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ <
> Latest addditions are at the end of this document                         <
>^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^<
Read all README type notes.

REMEMBER to Read the Changelog file for a list of changes.

This may also include info regarding testing procedures etc.

All program source files with the extension of .cbl have a list of changes made
for any reason including bugs found and fixed and other coding changes such as
for the migration to using the GnuCobol compiler.
For detailed instructions about getting and installing the compiler see the
ACAS manual within folder ACAS-Manuals --

ACAS - Building the ACAS System.pdf

and set up processes and procedures for normal ACAS system usage.

ACAS - System Set Up Procedures.pdf

Although may need more work on it.


Do this before ANY testing.
^^^^^^^^^^^^^^^^^^^^^^^^^^

Created as I think of them so the order of them may not be logical as well as
duplicated details. Latest notes are always last in this document.

Reminder to also read the file changelog for detailed modifications to sources.

OE - Order Entry.

This system comes from sources made available on a Tandy Archive.
This was written by one company and provided to another who provided it to
Tandy and all three are no longer in business so all original copyright has
terminated.

Judging by the total lack of documentation and evidence of linking to other
accounting products such as A/R (Sales), A/P (Purchase), Inventory (Stock) and
Payroll, I suspect it was never issued/released by programming team or Tandy as
I am dubious if it was ever fully tested for production - hence (may be), no
user manuals created - or none (so far) found.

In fact there appears to be no OE manuals from any supplier even from the very
early Tandy models in the TRS80 range etc.

It should be noted that most of the various accounting products were written in
Basic where often the Basic source code was provided (in the manuals) for which
many companies including book publishers, offered, for use with Microsoft
Basic interpreter and for some the QBasic compiler.

There is however, a series of Tandy accounting products that appear to be
written in Cobol but there is no reference to the OE system anywhere so no
linkage between systems and that would be required for Sales Ledger, IRS/General
and Stock Control or in American terms - A/R (Accounts Receivables), Nominal /
General Ledgers, Inventory.

Some of these seem to indicate an old version such as
V1 (version 1) and some later than V2 so it is very hard to tie in any
relationship between any and that of OE and there is very little within the OE
source code.

Also on the Tandy historic archives is a Tandy Xenix system and emulator which
includes a accounting system including AR, AP, General, Payroll, Stock?,
also includes OE at version 2 but I have not looked at it in details - just so
many hours a day, etc.

Back to the OE source code here, remember OE is at version 1, plus my amendments
for debugging and the needed extra programs to :
1. Get it to work with GnuCobol and
2. Fix bugs as they have appeared & cosmetics in screen and report layouts etc

Note that the programs now differ in some ways from the original specifications
that is supplied with these sources in screen and report layouts.

The one found within TRS80 simulator and Xenix code is actually version 2,
again NO documentation but this does seem to directly link with A/R
and General but requires additional programs to process some of the transfers.

There does not appear to be a manual for it other than the System Specification
document as  a pdf file.  See file :
Source_Code_for_Order_Entry_(26-4607)_700-5012.pdf
I will make an attempt to import this into LibreOffice writer or MS word so that
the content can be updated and also be used as the basis of a users manual -
This has very provisionally been done but as it used automatic conversion s/w it
looks a bit of a mess to say nothing of the use of hundreds of style sheets for
both page and paragraph formatting etc so all needs a clean up before adding in
all the changes to program specifications, file screen and report layouts.

This document is subject to changes due to the compiler migration and that of
linking any data files used with that of ACAS.

Done, see document OE User Manual.pdf with source file as a .odt.

It should be noted that during migration to GnuCobol file, screen and reports
layouts have had to be changed to support larger field sizes such as addresses,
i.e., five lines for address plus a field for Post / Zip codes, phone numbers,
G/L account numbers, Invoice, Purchase Order numbers and stock item number/code.

There are others and when the OE system is integrated over into ACAS there will
be more changes to accommodate the ACAS file and data systems in use & this will
no doubt mean that the Item (Stock), A/R (Sales) Customer and General files will
be replaced with that as used in ACAS and likewise using the same fields within
each of the files in place of the existing OE ones. This will involve a number
of man hours to do. It should be noted that there are fields used in OE which
are NOT in ACAS such as for the Item file Price-2 and -3, salesman commissions
as these are not used at all and there is no need, any way as there
is no Payroll system used within ACAS as this is very country specific but there
is a Payroll product included although it is written in CBASIC so will require a
migration process to change the code to say the FBC (Free Basic Compiler) that
supports running on different platforms such as Linux, Windows etc. This product
uses tables for up to ten deductions and ten additions groups so should cover
any changes made for taxation, although for reporting will need to be changed
to match new standards per country etc. You are welcome to use it as needed for
your own use but for resale get in touch with your business proposals.
Any and all will be looked at.

The following OE main changes have occurred so far:

The three primary and three direct secondary menu programs have been replaced
with just the one, effected was oe1 calling oemenu1, oe2 calling oemenu2 and oe3
calling oemenu3. There are other menu modules that relate to operational needs
that has not been changed, but hopefully there is no need to do so.

The OE system makes use of four modules that handle all screen input and output,
and all printed output for both printing on paper of 80, 132 columns and one to
reduce the size of fonts used for 85 columns wide. plus screen handling for
input and display to screen, i.e., print80, printout. print85 and screen ].

The module screen has had to be changed to support the new Cobol compiler but
very much so for handling many/most numeric fields and only by testing the
system will problems be found including from migrating the system.

In any event there are issues with the way gnuCobol does NOT handle accepting
numeric data such as money and fractions etc so another area that may need code
changes but where ever possible it is now using an experimental C routine for
most numeric data input replacing the calls to the screen module that handled
numeric data where they use decimal point values such as money, percentages,
discounts etc.

In other cases, instead of making calls to screen to handle display and accept,
this is done directly but is being minimised as it takes up a lot of time on my
part.

The print modules have been changed to support output to one file - prt-1.
Code is now present to send the contents to a printer in the correct orientation
i.e., portrait for 80 column or landscape for 132 column paper.
This has been tested using a HP 7305 Ink Tank printer that supports double sided
printing but set to single sided for Invoices and Packing / Picking notes.

This uses a Cups style spooler name (*nix) but can also be the Windows version
of this as well as the Windows internal method see the Windows system parameters
etc.

To find this code, search for SYSTEM to find the call "SYSTEM" statement then
use *> at start of line/s to omit the code (include the IF statement as well).
Hopefully no changes are needed.

The OE system parameter file as Company File uses two printer fields to take a
standard printer set for double sided printing and the other for single sided.

I have been changing the code to support the following :

1. Used the free to obtain and use GnuCobol compiler
   from www.sourceforge.net/p/gnucobol

2. Change various field sizes to match up with the one's used in ACAS so that
   at some point integration with ACAS is possible.
   That said the common files namely :
   Item (stock)
   Invoice

   Have fields that are not present on the ACAS system notably
   three levels of pricing where there is only one for ACAS.
   The prices are presumably price 1 - Retail, 2 & 3 discounts for various
   volumes.

   Files that are not used in ACAS - Salesman, Commission Codes (for sales
   people etc). AS these will need a hook into a Payroll system that does not
   exist it may well be removed - Thoughts on this ?

   It produces data transfer files to A/R (Sales Ledger), GL for the General
   ledger but there is no programs that actually reads these and passes the
   data to the related systems - so these would need to be created.

   Although it creates invoice file holding orders. invoices and credit
   notes / memo's for A/R accounts it does not have any programs for dealing
   with payments.

   It does not place active invoice on an Open Item file so that payments can
   be made to update such and remove fully paid invoices from the system after
   suitable reports issued and yes this is done in ACAS.

   It does not support any form of credit control including restricting an
   order for a customer with a bad payment history etc, which ACAS does although
   it does have fields for credit rating and amount but there is no processing
   of such.

   Any such prepayments must be handled outside the system such as credit or
   debit cards but the same applies to ACAS as well, such as software made
   available via your bankers or other third party providers. This is the normal
   process.

   This would be a good reason to pass on the invoicing file to ACAS directly
   in real time so that history and payments can be correctly processed but uses
   extra processing steps to -
   1. Print a Invoice Proof Report if needed and
   2. Print the Packing (or Picking) notes and Invoices.
   3. Post the Invoices to the OTM (Open Item File).

   The item (Stock) file should be changed / replaced to directly support the
   ACAS stock system.

   To this end, some fields used within OE had to have their sizes changed such
   as :

   Stock location that was set for 5 chars is now 13.
   Invoice number from 6 to 8 chars.
   Order number from 6 to 8 or 10 (used for non account customers i.e., prepaid.
   Supplier (Vendor) number from 6 to 7 where 7th is a check digit in ACAS but
   not used in OE.

   Dates :

   All Dates used which currently only use the USA style of dates and this has
   been changed to also support UK and (world / International formats).
   i.e., mm/dd/ccyy,  dd/mm/ccyy and ccyy/mm/dd.
   This is set using a new company file field for it as a single character.

   Also changing the dates from 6 digits that is not using century so non
   millennium compliant (year 2000) to full 8 digits as any such dates will not
   support year 2000 onwards. All dates stored in files are held as yyyymmdd as
   binary-long - here avoiding any other problems with dates such as one
   possibly occurring in 2038 when using days since 1601 but ACAS and OE should
   not use 16 bit values for this  as far as I remember.
   - BUT THIS WILL NEED TO BE CHECKED. -

   Note that some elements of ACAS stored binary dates since 01/01/1601 just
   might have an issue - this will be looked at, to check as I hope the storage
   of such, is longer than 16 bits - just another issue to look for :(
   I do not think this is an issue but still need to check it.
   Note ACAS was checked for year 2000 issues in the 90's and any needed
   changes made.

   Now you might think that 2038 is a long time for such software to be used so
   did the programmers when ACAS was written around 1967+ when the century was
   not used - the year 1999 soon changed that thinking, so ACAS supports date
   well beyond - very well beyond.

   These OE field changes are still in progress - Nope, OE fields changes are
   complete but any migration of OE to ACAS will require more changes where OE
   field name have to be changed and this will effect almost all programs but by
   changing the copy book names used to that used in ACAS will help.
   The files used in ACAS as they use a File Handler to process via a CALL again
   this should also help to keep changes smaller.

   To this end, any testing of the system at any point attention must be made
   to all printed output and to the screen when entering or displaying data to
   confirm that all fields shown on both are CORRECT in both format and size.
   Likewise that any headings also match up with the data fields below them.
   That means ALL reports must be looked at CLOSELY.

   I will try and remember to update this document as time moves forwards along
   with any changes to the source code to OE.

05/11/23 -
    New program slcdsmnt for Salesman and AR code records maintenance.
    This program also includes a experimental routine for numeric input but only
    currently used for Term record and discount percentage it will also display
    the results of the routine to the right of the screen. Now also used in the
    Billing programs.

17/01/24 -
    Many of the OE menu options have now passed basic** testing namely options :

    A B C D E F
    H  J

    M N O

    R S X

    For options G, I, K, L Data does not match process criteria.

    Likewise P & Q (Period and Year end).

    I have not done any processing that requires BO (Back Order) processing but
    you might want to try that by adding some stock items that :
    (1) have zero stock,
    (2) have stock but an order is placed that exceeds current stock.
    If nothing else, to see what happens

    I suspect the only support is for reports by item and customer for Back
    ordered stock and possibly to select (or un-select) invoices for printing.

    Once you have items that are on back order then to add to items fresh stock
    and see if the invoicing or BO elements then create a new order / invoice
    etc.  I have not seen any code that supports these functions but I may well
    have missed it.

    I have made a change to the Invoice print program - it asks a question to
    NOT do rewrites to the files and while testing the invoice print program.
    I suggest the response is Y.  If nothing else it will save you having to
    create new orders for all such prints - HOWEVER it will not pass the orders
    to the AR file.  For production the answer to this question is always N.
    Secondary testing will use N and at some point I will remove this option.

    I may have removed this test - forgotten.

    If I start migrating OE into ACAS I will change the posting program to post
    to the ACAS OTM (Open Item File) which stores all invoices that have been,
    packed, invoiced and posted. For this I will make use of the ACAS program
    that deals with this.
    It is the OTM file that is used to post payments and yes OE does not do that
    as it expects AR to do so and that does not exist in any form.

    There is more than likely a requirement for fine tuning, but the basics are
    all there.

    Note that all testing has been conducted using ink jet & ink tank printers
    on plain paper so the setup for Company has the setting Print Company
    details ON and not having this set, some of the reports will look odd such
    as Invoices.
    Of course if you are lucky to run a matrix or line printer and use
    preprinted continuous stationery for the purpose then leaving this setting
    off is the right procedure but only after you have tested the OE system to
    your satisfaction.

    Another option for production is to use preprinted paper for Invoices OR if
    using a good (Laser) printer that supports it a template sheet is preloaded
    to the printer before printing Invoice which contains a Invoice image or
    just a company logo and address information, in which case the invoice
    program (oeinvc.cbl) will need changing to match up with the paper layout.

    It is further recommended that when testing you should automatically assume
    that there will be bugs not limited to screen and report layouts including
    incorrect positioning of data and headings.
    Further, there could still be issues regarding the entry of numeric data and
    for that matter alphanumeric data as well.
    Do NOT take anything for granted!
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Reminder:- The OE system has NOT been merged into ACAS in any way at this
    time. The decision has not been made but your input regarding this will be
    valuable.

    The OE system would be used in place only of Sales Ledger Invoicing or in
    addition to it. The payments system will need to use Sales Ledger Payment
    input processes.

    ** Basic testing only consists of a few records for customer, Items (stock)
    and stock volumes.  This really comes under the banner of functional testing
    and system testing will still be required with larger volumes of suitable
    data.  Note that within the ACAS nightly build there is a lot more data for
    Sales Customers, Purchase suppliers - (1,000+ records), Stock depending on
    version of the data - 10 - 100 stock items. Complete set of Accounts in IRS.
    Predefined basic data for Sales and Purchase Analysis codes. To name  but as
    a few.

    OE will not be merged into ACAS as the one missing funtion BO processing has
    now been included within ACAS Sales and Stock Control. All work on OE has
    now been terminated.


15/04/2024 --

OE -----
There was a very limited number of functions / features that are in OE that are
NOT in ACAS - namely:

1. Support for Back Ordering when an order has been placed but
the stock quantity is not enough to fulfill the order in part of all of it.

2. Salesmen and their commissions.

3. Support for three differing prices in the Stock item file.

4. Support for discounts in the Stock item file.

In ACAS item 1 has been added over the last few weeks but is subject to testing.

Likewise recurring Sales invoicing which can be yearly, quarterly or monthly
although Daily is currently present but that is only to help testing and will
be removed after.
This would mostly if not for all be for service items only such as rentals,
service charging, and the like e.g., you rent out specific software on a monthly
or quarterly basis - ACAS was one that we rented / hire out on a monthly basis
paid in advance as well as selling it or charging a yearly fee for usage.

There is also Recurring purchasing added as well, although I am not so sure it
is needed at all and might well be removed after Sales testing. In any event
more code might be needed in Purchasing for this option.
For example would a business on receiving say recurring charges for software
rentals want to issue a recurring Purchase order against it ?

Item 2 will not be supported as 1) there is no payroll system to transfer such
data to and does any one still give staff a commission for taking an order as it
should be part of their job description ?.

3 & 4. ACAS allows for discounts by item and by invoice prior to add in delivery
charges and their ilk, so this covers any such including discounts for bulk
purchases.

Work on OE has been dropped as it is not needed not any fuctionality within as
Back Orders is not included within ACAS - subject to system testing and manual
updates.

WARNING: The current version of ACAS built to the nightly archives is subject
to testing for all the new features listed above as well as some not so listed.
Including while testing to update the manuals to reflect current usage and
screen lay outs etc.

Offers to help in this endeavour would be gratefully received, and that can
include help on Testing, Document writing and software updates and programming
new features if and as required.



26/09/2023 Created Vincent Coen.
12/10/2023 Updated Vincent Coen.
27/10/2023 Updated Vincent Coen.
05/11/2023 Updated Vincent Coen.
17/12/2023 Updated Vincent Coen.
27/12/2023 Updated Vincent Coen.
14/01/2024 Updated Vincent Coen.
18/01/2024 Updated Vincent Coen.
28/02/2024 Updated Vincent Coen.
15/04/2024 Updated Vincent Coen.
15/12/2024 Updated Vincent Coen.
14/04/2025 Updated Vincent Coen.
