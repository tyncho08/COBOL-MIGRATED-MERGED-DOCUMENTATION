*>
*> 28/10/16 vbc - .00 Common mapser coding for all of ACAS
*>                    common copybook to ease maintenance
*>                     mapser-RDB used instead.
*>
 mapser section.
*>*************
*>
*> Encrypt/Decrypt coding and other security code removed for
*>    Open Source release
*>
     display  prog-name at 0101 with foreground-color 2 erase eos.
     display  "ACAS System Setup Routine - Level 1" at 0122 with foreground-color 2.
*>
*> Now Open System File for Output (overwriting existing contents)
*>
     open     output system-file.
     if       fs-reply not = zero
              display "Problem with opening system file. Hit return to quit"
                         at 1101 with foreground-color 4
              accept ws-reply at 1157
              stop run
     end-if
     initialize system-record with filler.
     move     ws-Sys-Record-Ver-Prime      to System-Record-Version-Prime
     move     ws-Sys-Record-Ver-Secondary  to System-Record-Version-Secondary
     move     1 to date-form.                                                *> default UK format
*>
 Capture-Data.
     display  "Enter the Company Name :- [" at 1101 with foreground-color 2.
     display  "]" at 1160 with foreground-color 2.
*>
     accept   usera at 1128 with foreground-color 3 update.
*>
     display  "Please verify that name is correct (Y/N) :- [ ]"               at 1301 with foreground-color 2.
     move     "Y"  to  ws-reply.
     accept   ws-reply  at 1346 with foreground-color 6 update.
*>
     if       ws-reply not = "Y" and not = "y"
              go to Capture-Data
     end-if
     move     "N" to encode.
     move     usera to pass-name.
     call     "maps01" using maps01-ws.
     move     pass-name to user-code.
     move     "P" to encode.
     move     "pass" to pass-word of maps01-ws
                        pass-word of system-record.
     call     "maps01" using maps01-ws.
     move     pass-word of maps01-ws  to
              pass-word of system-record.
*>
     move     "N" to sl-own-nos.
     move     function current-date to wse-date-block.
     move     "00/00/0000" to u-date.
     move     wse-year  to u-year.
     move     wse-month to u-month.
     move     wse-days  to u-days.
     move     u-date    to to-day.
     move     zero      to u-bin.
     call     "maps04" using maps03-ws.
     move     u-bin  to run-date.
*>
     perform  level-setup.
*>
     move     1  to  op-system.
     move     1  to  sales-range.
     move     2  to  purchase-range.
     move     1  to  next-batch.
     move     1  to sl-dunning sl-charges.
     move     30 to sl-credit.
     move     2  to Invoicer.
     move     "!" to sl-delim.
*>
     move     1  to  rrn.
     write    system-record.
*>
     initialize default-record.
     move     2     to  rrn.
     write    system-record  from  default-record.  *> G/L Only
*>
     initialize final-record.
     move     3       to  rrn.
     write    system-record  from  final-record.    *> G/L only
*>
     initialize system-record-4 with filler.
     move     4 to rrn.
     write    system-record from system-record-4.
*>
     close    system-file.
*>
 main-exit.
     exit section.
*>
 level-setup  section.
*>*******************
*>
     move     "mp9999" to wsmaps-ser.           *> Open Source version
     move     wsmaps-ser-xx to maps-ser-xx.
     move     wsmaps-ser-nn to maps-ser-nn.
*>
     display  "Using General  Ledger (Y/N) ? :- [ ]" at 1901 with foreground-color 2.
     display  "Using Purchase Ledger (Y/N) ? :- [ ]" at 2001 with foreground-color 2.
     display  "Using Sales    Ledger (Y/N) ? :- [ ]" at 2101 with foreground-color 2.
     display  "Using Invoicing       (Y/N) ? :- [ ]" at 1941 with foreground-color 2.
     display  "Using Stock Control   (Y/N) ? :- [ ]" at 2041 with foreground-color 2.
     display  "Using IRS             (Y/N) ? :- [ ]" at 2141 with foreground-color 2.
*>
     accept   ws-reply at 1935 with foreground-color 6 update.
     move     function upper-case (ws-reply) to ws-reply.
     if       ws-reply = "Y"
              move  1  to  level-1    *> General
     else
              move zero to level-1
     end-if
     accept   ws-reply at 2035 with foreground-color 6 update.
     move     function upper-case (ws-reply) to ws-reply.
     if       ws-reply = "Y"
              move  1  to  level-2    *> Purchase
     else
              move zero to level-2
     end-if
     accept   ws-reply at 2135 with foreground-color 6 update.
     move     function upper-case (ws-reply) to ws-reply.
     if       ws-reply = "Y"
              move  1  to  level-3    *> Sales
     else
              move zero to level-3
     end-if
     accept   ws-reply at 1975 with foreground-color 6 update.
     move     function upper-case (ws-reply) to ws-reply.
     if       ws-reply = "Y"
              move  1  to  full-invoicing      *> Invoicing
     else
              move zero to full-invoicing
     end-if
     accept   ws-reply at 2075 with foreground-color 6 update.
     if       ws-reply = "Y" or "y"
              move  1  to  Level-4    *> Stock Control
     else
              move zero to Level-4
     end-if
     accept   ws-reply at 2175 with foreground-color 6 update.
     if       ws-reply = "Y" or "y"
              move  1  to  Level-5     *> IRS
     else
              move zero to Level-5
     end-if
     display  "Please confirm (Y/N) :- [ ] " at 2301 with foreground-color 2.
*>
     move     spaces  to  option-list.
     move     1  to  a.
*>
     if       G-L
              string "General " delimited by size into option-list with pointer a.
*>
     if       B-L  and  G-L
              string "/ " delimited by size into option-list  with pointer a.
*>
     if       B-L
              string "Purchase " delimited by size into option-list with pointer a.
*>
     if       S-L  and  G-L
        or    S-L  and  B-L
              string "/ " delimited by size into option-list with pointer a.
*>
     if       S-L
              string "Sales " delimited by size into option-list with pointer a.
*>
     if       S-L  and  full-invoicing = 1
              string  "/ Invoicing" delimited by size into option-list with  pointer  a.
*>
     if       Stock
              string "/ Stock " delimited by size into option-list pointer a.
*>
     if       IRS
              string "/ IRS" delimited by size into option-list   pointer a.
*>
*>  option-list now max 60 chars used
*>
     display  option-list at 2401 with foreground-color 2.
     accept   ws-reply at 2326 with foreground-color 6.
     move     function upper-case (ws-reply) to ws-reply.
     if       ws-reply not = "Y"
              go to level-setup.
*>
 main-exit.
     exit     section.
*>
