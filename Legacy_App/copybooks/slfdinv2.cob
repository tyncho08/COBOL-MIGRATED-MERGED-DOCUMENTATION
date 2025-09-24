*>*******************************************
*>             Sales                        *
*>  File Definition For The Invoice File    *
*>                                          *
*>*******************************************
*> record size 129 bytes 09/03/09
*>        size 134 bytes 17/05/13 to match wsinv
*>        size 137??? bytes 25/01/17
*>
*> 26/01/17 - Taken from the SL copy in that src dir and prefixed by 'sl'.
*>   Removed lowercase values so just using UC - remembering to the
*>   Removed all references to invoice-letter as redundant.
*>    same to all programs.
*>
*> 29/01/17 replaced item-nos from bin-char to pic 99.
*>
*>  NEED TO DO A SIZING CHECK for all INVOICE copybooks.
*>
 fd  Invoice-File.
*>
 01  Invoice-Record.                        *> 137
     03  Invoice-Key.
         05  Invoice-Nos    pic 9(8).
         05  Item-Nos       pic 99.          *> was binary-char.
     03  Invoice-Customer   pic x(7).
     03  Invoice-Date       binary-long.
     03  Filler             pic x(10).
     03  Invoice-Type       pic 9.
     03  filler             pic x(10).
     03  filler             pic x(95).
*>
 01  Invoice-Header.       *> 137 ??? bytes
    02  ih-prime.          *> 42 ??? bytes
     03  ih-invoice         pic 9(8).
     03  ih-test            pic 99.          *> was binary-char.
     03  ih-customer.
         05  ih-nos         pic x(6).
         05  ih-check       pic 9.
     03  ih-date            binary-long.
     03  ih-order           pic x(10).
     03  ih-type            pic 9.
     03  ih-ref             pic x(10).
   02 ih-sub-prime.        *> 95 ??? bytes
     03  ih-description     pic x(32).
     03  ih-fig                             comp-3.
         05  ih-p-c         pic s9(7)v99.
         05  ih-net         pic s9(7)v99.
         05  ih-extra       pic s9(7)v99.
         05  ih-carriage    pic s9(7)v99.
         05  ih-vat         pic s9(7)v99.
         05  ih-discount    pic s9(7)v99.
         05  ih-e-vat       pic s9(7)v99.
         05  ih-c-vat       pic s9(7)v99.
     03  ih-status          pic x.
         88  pending                            value "P".
         88  invoiced                           value "I".
         88  applied                            value "Z".
     03  ih-status-P        pic x.         *> Pick list Printed           space or P
     03  ih-status-L        pic x.         *> Invoice Printed             space or L
     03  ih-status-C        pic x.         *> Invoice Cleared             space or C   paid or credited or cleared/cancelled etc
     03  ih-status-A        pic x.         *> Invoice Applied to a/c      space or A
     03  ih-status-I        pic x.         *> Invoice Item lines deleted  space or D
     03  ih-lines           binary-char.
     03  ih-deduct-days     binary-char.
     03  ih-deduct-amt      pic 999v99    comp.
     03  ih-deduct-vat      pic 999v99    comp.
     03  ih-days            binary-char.
     03  ih-cr              binary-long.
     03  ih-day-book-flag   pic x.
         88  day-booked                         value "B".
     03  ih-update          pic x.
         88  ih-analyised                       value "Z".
*>  removed filler pic x.
*>
 01  Invoice-Line.    *> 80
       05  il-invoice       pic 9(8).
       05  il-line          pic 99.          *> was binary-char.
       05  il-product       pic x(13).   *> +1 17/5/13
       05  il-pa            pic xx.
       05  il-qty           binary-short.
       05  il-type          pic x.
       05  il-description   pic x(32).
       05  il-net           pic s9(7)v99   comp-3.
       05  il-unit          pic s9(7)v99   comp-3.
       05  il-discount      pic 99v99      comp.
       05  il-vat           pic s9(7)v99   comp-3.
       05  il-vat-code      pic 9.
       05  il-update        pic x.
           88 il-analyised                      value "Z".
       05  filler           pic x.           *> was pic xx.
*>

