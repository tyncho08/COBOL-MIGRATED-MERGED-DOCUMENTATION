       IDENTIFICATION DIVISION.
       PROGRAM-ID. create-system-dat.
      *
      * Creates a minimal system.dat file for ACAS initial setup
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT system-file ASSIGN TO "system.dat"
                  ORGANIZATION IS RELATIVE
                  ACCESS MODE IS SEQUENTIAL
                  RELATIVE KEY IS rel-key
                  FILE STATUS IS fs-status.

       DATA DIVISION.
       FILE SECTION.
       FD  system-file.
       01  system-record            PIC X(2048).

       WORKING-STORAGE SECTION.
       01  fs-status               PIC XX.
       01  rel-key                 PIC 9(4).
       01  ws-system-rec.
           05  filler              PIC X(2048) VALUE ALL SPACES.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Creating minimal system.dat file..."
           
           OPEN OUTPUT system-file
           
           IF fs-status NOT = "00"
              DISPLAY "Error opening file: " fs-status
              STOP RUN
           END-IF
           
      * Write 4 empty records as ACAS expects
           PERFORM 4 TIMES
              WRITE system-record FROM ws-system-rec
           END-PERFORM
           
           CLOSE system-file
           
           IF fs-status = "00"
              DISPLAY "system.dat created successfully with 4 records"
           ELSE
              DISPLAY "Error closing file: " fs-status
           END-IF
           
           STOP RUN.