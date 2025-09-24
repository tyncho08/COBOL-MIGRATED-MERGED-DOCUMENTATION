*>*****************************************************
*>                                                    *
*>  File Definition For The Deleted Invoice Nos File  *
*>                                                    *
*>*****************************************************
*> 19 bytes 04/04/09
 fd  Del-Inv-nos-file.
*>
 01  Del-Inv-nos-record.
     03  Del-Inv-Nos        pic 9(8).
     03  Del-Inv-Dat        binary-long.
     03  Del-Inv-Cus        pic x(7).
*>
