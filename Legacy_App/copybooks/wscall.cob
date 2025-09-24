*> 14/03/18 vbc - 1.01   WS-CD-Args for passing extra info to called process
*>                        that will help in a cron call by time via menu
*>                        program. picked by position within WS-Args.
*>
 01  WS-Calling-Data.
     03  WS-Called       pic x(8).
     03  WS-Caller       pic x(8).
     03  WS-Del-Link     pic x(8).
     03  WS-Term-Code    pic 9.
*>                                 new 18/5/13
     03  WS-Process-Func pic 9.
     03  WS-Sub-Function pic 9.
     03  WS-CD-Args      pic x(13).    *> Changed / Added 14/03/18
*>
