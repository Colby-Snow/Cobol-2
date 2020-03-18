       IDENTIFICATION DIVISION.
       program-id. COBCMSCP01.
       DATE-WRITTEN. 03/04/2020.
       AUTHOR. COLBY SNOW.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RESERVEMASTER
           ASSIGN TO 
           "C:\IHCC\COBOLSP20\PROJECTS\CP01\COBCMSCP01\RESERVE.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
           ASSIGN TO 
           "C:\IHCC\COBOLSP20\PROJECTS\CP01\COBCMSCP01\CAMPRES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROUT
           ASSIGN TO 
           "C:\IHCC\COBOLSP20\PROJECTS\CP01\COBCMSCP01\ERR.PRT"
           ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD RESERVEMASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-BOAT-REC
           RECORD CONTAINS 42 CHARACTERS.

       COPY 'C:/IHCC/COBCMSCP01/TRAN.CPY'.
       
       FD PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       COPY '~/VAL.CPY'.

       FD ERROUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS ERRLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       
       01 ERRLINE PIC X(132).

       
       
       WORKING-STORAGE SECTION.

       01 WORKSPACE.
           05 MORE-RECS        PIC XXX     VALUE "YES".
           05 FIRST-ERR-SW     PIC X.
           05 AMT-SW           PIC X.
           05 C-AMT            PIC S9(3)V99 VALUE ZERO.
           05 C-REC-ERR-CTR    PIC 9(3)    VALUE ZERO.
           05 C-TOT-ERR-CTR    PIC 9(3)    VALUE ZERO.
           05 C-DATE-TYPE      PIC X       VALUE SPACES.
           05 C-LEFT-OVER      PIC 9       VALUE ZERO.
           05 C-END-DATE.
               10 C-END-DD     PIC 99      VALUE ZERO.
               10 C-END-MM     PIC 99      VALUE ZERO.
               10 C-END-YY     PIC 99      VALUE ZERO.
           05 C-END-DATE-NUM REDEFINES C-END-DATE PIC 9(8).
           05 C-ERR-PCTR       PIC 99      VALUE ZEROS.
           05 C-PCTR           PIC 99      VALUE ZEROS.

       COPY 'C:/IHCC/COBCMSCP01/ERROR.CPY'.

       01 CURRENT-DATE-AND-TIME.
         05 CUR-DATE.
           10 CUR-YY PIC 9(4).
           10 CUR-MM PIC 99.
           10 CUR-DD PIC 99.
         05 I-TIME PIC X(11).

       01 CUR-DATE-AND-TIME-NUM REDEFINES CURRENT-DATE-AND-TIME.
           05 CUR-DATE-NUM         PIC 9(8).
           05 CUR-TIME             PIC X(11).

       01 TITLE-LINE.
         05 FILLER PIC X(6) VALUE "DATE: ".
         05 O-CUR-MM PIC 99.
         05 FILLER PIC X VALUE "/".
         05 O-CUR-DD PIC 99.
         05 FILLER PIC X VALUE "/".
         05 O-CUR-YY PIC 9999.
         05 FILLER PIC X(44) VALUE SPACES.
         05 FILLER PIC X(28) VALUE "RESERVATIONS".
         05 FILLER PIC X(36) VALUE SPACES.
         05 FILLER PIC X(6) VALUE "PAGE: ".
         05 O-PCTR PIC X(2).

       01 WK-DATE.
         05 WK-YYYY PIC 9(4).
         05 WK-MM PIC 99.
           88 VALID-MM VALUE 1 THRU 12.
           88 VALID-30-MM VALUE 4, 6, 9, 11.
           88 VALID-31-MM VALUE 1, 3, 5, 7, 8, 10, 12.
         05 WK-DD PIC 99.
           88 VAL-WK-DD-31 VALUE 1 THRU 31.
           88 VAL-WK-DD-30 VALUE 1 THRU 30.
           88 VAL-WK-DD-28 VALUE 1 THRU 28.
           88 VAL-WK-DD-29 VALUE 1 THRU 29.
       01 WK-DATE-NUM REDEFINES WK-DATE PIC 9(8).

       01 WK-DATE-NUMERIC-SW       PIC X   VALUE 'Y'.

       01 O-ERR-LINE.
           05 O-ERR        PIC X(100).
           05 FILLER       PIC X(32)       VALUE SPACES.
           
       01 O-REC-LINE.
           05 O-REC-DUMP   PIC X(115).
           05 FILLER       PIC X(17)       VALUE SPACES.

       01 SITE-AREA.
           05 FILLER                   PIC X(3)    VALUE 'A10'.
           05 FILLER                   PIC X(3)    VALUE 'B10'.
           05 FILLER                   PIC X(3)    VALUE 'C10'.
           05 FILLER                   PIC X(3)    VALUE 'D12'.
           05 FILLER                   PIC X(3)    VALUE 'E12'.
           05 FILLER                   PIC X(3)    VALUE 'F12'.
           05 FILLER                   PIC X(3)    VALUE 'G12'.
           05 FILLER                   PIC X(3)    VALUE 'H12'.
           05 FILLER                   PIC X(3)    VALUE 'I12'.
           05 FILLER                   PIC X(3)    VALUE 'J12'.
           05 FILLER                   PIC X(3)    VALUE 'K12'.
           05 FILLER                   PIC X(3)    VALUE 'L12'.
           05 FILLER                   PIC X(3)    VALUE 'M12'.
           05 FILLER                   PIC X(3)    VALUE 'N14'.
           05 FILLER                   PIC X(3)    VALUE 'O14'.
           05 FILLER                   PIC X(3)    VALUE 'O14'.
           05 FILLER                   PIC X(3)    VALUE 'P14'.
           05 FILLER                   PIC X(3)    VALUE 'Q14'.
           05 FILLER                   PIC X(3)    VALUE 'R14'.
           05 FILLER                   PIC X(3)    VALUE 'S14'.
           05 FILLER                   PIC X(3)    VALUE 'T14'.
           05 FILLER                   PIC X(3)    VALUE 'U14'.
           05 FILLER                   PIC X(3)    VALUE 'V14'.
           05 FILLER                   PIC X(3)    VALUE 'W14'.
           05 FILLER                   PIC X(3)    VALUE 'X14'.
           05 FILLER                   PIC X(3)    VALUE 'Y14'.
           05 FILLER                   PIC X(3)    VALUE 'Z14'.
       
       01 SITE-TBL REDEFINES SITE-AREA.
           05 TABLE-SITE OCCURS 26 TIMES INDEXED BY SITE-INDEX.
               10  TBL-SITE            PIC X.
               10  TBL-DAY-AMT        PIC 99.

       01 ERR-TOTAL-LINE.
         05 FILLER             PIC X(26)       VALUE 
         "TOTAL RECORDS WITH ERRORS ".
         05 O-REC-ERR-CTR      PIC ZZ9.
         05 FILLER             PIC X(27)       VALUE 
         " TOTAL NUMBER OF ERRORS ".
         05 O-TOT-ERR-CTR      PIC ZZ9.
         05 FILLER             PIC X(79)       VALUE SPACES.

       01 ERR-COLUMN-HEADINGS.
           05 FILLER                       PIC X(12)   VALUE 
                                                       "ERROR RECORD".
           05 FILLER                       PIC X(120)   VALUE SPACES.

       01 DIVISION-LINE.
           05 FILLER                       PIC X(8)    VALUE "COBCMS05".
           05 FILLER                       PIC X(51)   VALUE SPACES.
           05 FILLER                       PIC X(4)    VALUE "SNOW".
           05 FILLER                       PIC X(9)    VALUE 
                                                       " DIVISION".
           05 FILLER                       PIC X(60)   VALUE SPACES.

       01 ERR-REPORT-LINE.
           05 FILLER                       PIC X(60)   VALUE SPACES.
           05 FILLER                       PIC X(12)   VALUE 
                                                       "ERROR REPORT".
           05 FILLER                       PIC X(60)   VALUE SPACES.

       01 BLANK-LINE       PIC X(132)      VALUE SPACES.

       PROCEDURE DIVISION.
       L1-MAIN.
       	   PERFORM L2-INIT.
       	   PERFORM L2-MAINLINE
               UNTIL MORE-RECS = "NO".
       	   PERFORM L2-CLOSING.
           STOP RUN.

       L2-INIT.
           OPEN INPUT RESERVEMASTER.
           OPEN OUTPUT PRTOUT.
           OPEN OUTPUT ERROUT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CUR-YY TO O-CUR-YY.
           MOVE CUR-MM TO O-CUR-MM.
           MOVE CUR-DD TO O-CUR-DD.
           PERFORM L3-READ.
           PERFORM L3-ERR-HEADINGS.

       L2-MAINLINE.
           PERFORM L3-VALIDATION THRU L3-VALIDATION-EXIT.
           IF O-ERR = SPACES
               PERFORM L3-CALCS
               PERFORM L3-OUTPUT.
           PERFORM L3-READ.

       L3-VALIDATION.
           MOVE "Y" TO FIRST-ERR-SW
           MOVE "N" TO AMT-SW
           MOVE SPACES TO O-ERR
           IF NOT VAL-CAMP
               MOVE ERR-MSG(1) TO O-ERR
               PERFORM L4-ERROR
               IF I-SITE-L NOT ALPHABETIC
                   MOVE ERR-MSG(2) TO O-ERR
                   PERFORM L4-ERROR.
           IF I-SITE-N NOT NUMERIC
               MOVE ERR-MSG(3) TO O-ERR
               PERFORM L4-ERROR
           ELSE
               IF I-SITE-N <= 0
                   MOVE ERR-MSG(4) TO O-ERR
                   PERFORM L4-ERROR.
*      MOVES RESERVATION DATE TO WORK DATE AND VALIDATES
               MOVE I-DATE TO WK-DATE
           MOVE 'R' TO C-DATE-TYPE.
           PERFORM L4-DATE-ROUT.
           IF I-LEN-STAY NOT NUMERIC
               MOVE "Y" TO AMT-SW
               MOVE ERR-MSG(12) TO O-ERR
               PERFORM L4-ERROR
           ELSE
               IF I-LEN-STAY < 1 OR I-LEN-STAY > 12
                   MOVE ERR-MSG(13) TO O-ERR
                   PERFORM L4-ERROR.
           IF I-FNAME = SPACES
               MOVE ERR-MSG(14) TO O-ERR
               PERFORM L4-ERROR.
           IF I-LNAME = SPACES
               MOVE ERR-MSG(15) TO O-ERR
               PERFORM L4-ERROR.
           IF I-AMT NOT NUMERIC
               MOVE ERR-MSG(16) TO O-ERR
               PERFORM L4-ERROR
           ELSE
               IF AMT-SW = 'Y'
                   NEXT SENTENCE
               ELSE
                   SET SITE-INDEX TO 1
                   SEARCH TABLE-SITE
                       AT END
                           MOVE 'SITE NOT FOUND' TO O-ERR
                           PERFORM L4-ERROR
                       WHEN I-SITE = TABLE-SITE(SITE-INDEX)
                           COMPUTE C-AMT = I-LEN-STAY *
                             TBL-DAY-AMT(SITE-INDEX)
                           IF I-AMT NOT EQUAL C-AMT
                               MOVE ERR-MSG(17) TO O-ERR
                               PERFORM L4-ERROR.
           IF NOT VAL-CCTYPE
               MOVE ERR-MSG(18) TO O-ERR
               PERFORM L4-ERROR.
           IF I-CCNUM NOT NUMERIC
               MOVE ERR-MSG(19) TO O-ERR
               PERFORM L4-ERROR.
           MOVE 'C' TO C-DATE-TYPE.
           MOVE I-CCEXP TO WK-DATE.
           PERFORM L4-DATE-ROUT.
           IF O-ERR = SPACES
               MOVE "N" TO FIRST-ERR-SW.

       L3-VALIDATION-EXIT.
           EXIT.

       L3-CALCS.
           MOVE I-DATE TO WK-DATE.
           ADD WK-DATE-NUM TO I-LEN-STAY GIVING C-END-DATE-NUM.
           IF CC-MM = 2
               DIVIDE CC-YY BY 4 GIVING C-LEFT-OVER REMAINDER
               C-LEFT-OVER
               IF C-LEFT-OVER = 0
                   IF VAL-CC-29-DD
                       ADD 1 TO C-END-MM
                       SUBTRACT 29 FROM C-END-DD
                  ELSE
                       NEXT SENTENCE
               ELSE
                   IF VAL-CC-28-DD
                       ADD 1 TO C-END-MM
                       SUBTRACT 28 FROM C-END-DD.
               IF VAL-CC-30-MM
                   IF VAL-CC-30-DD
                       ADD 1 TO C-END-MM
                       SUBTRACT 30 FROM C-END-DD
               IF VAL-CC-31-MM
                   IF VAL-CC-31-DD
                       ADD 1 TO C-END-MM
                       SUBTRACT 31 FROM C-END-DD.
           IF VAL-CC-MM
           ADD 1 TO C-END-YY
           SUBTRACT 12 FROM C-END-MM.

       L3-OUTPUT.
           STRING I-LNAME DELIMITED BY " " ', '
             DELIMITED BY SIZE I-FNAME DELIMITED BY " " INTO O-NAME.

           EVALUATE I-CCTYPE
               WHEN 'V'
                   MOVE 'VISA' TO O-CCTYPE
               WHEN 'M'
                   MOVE 'MASTER CARD' TO O-CCTYPE
               WHEN 'A'
                   MOVE 'AMERICAN EXPRESS' TO O-CCTYPE.



           MOVE I-CAMPGROUND TO O-CAMPGROUND.
           MOVE I-SITE TO O-SITE.
           MOVE I-DATE TO O-DATE.
           MOVE C-END-DATE TO O-END-DATE.
           MOVE I-LEN-STAY TO O-LEN-STAY.
           MOVE I-AMT TO O-AMT.
           MOVE I-CCNUM TO O-CCNUM.
           MOVE I-CCEXP TO O-CCEXP.
       	   WRITE VAL-REC.

       L4-ERROR.
           IF FIRST-ERR-SW = 'Y'
               MOVE 'N' TO FIRST-ERR-SW
               ADD 1 TO C-REC-ERR-CTR
               MOVE I-TRAN-REC TO O-REC-DUMP
               WRITE ERRLINE FROM BLANK-LINE
               WRITE ERRLINE FROM O-REC-LINE
                   AFTER ADVANCING 1 LINE
               WRITE ERRLINE FROM O-ERR-LINE
                   AFTER ADVANCING 2 LINES
           ELSE
               WRITE ERRLINE FROM O-ERR-LINE.
           ADD 1 TO C-TOT-ERR-CTR.

       L4-DATE-ROUT.
           MOVE 'Y' TO WK-DATE-NUMERIC-SW.
           IF WK-DATE NOT NUMERIC
               MOVE 'N' TO WK-DATE-NUMERIC-SW
               IF C-DATE-TYPE = 'R'
                   MOVE ERR-MSG(5) TO O-ERR
                   PERFORM L4-ERROR
               ELSE
                   IF C-DATE-TYPE = 'C'
                       MOVE ERR-MSG(20) TO O-ERR
                       PERFORM L4-ERROR.
           IF WK-DATE-NUMERIC-SW = 'Y'
               IF NOT VALID-MM
                   IF C-DATE-TYPE = 'R'
                       MOVE ERR-MSG(6) TO O-ERR
                       PERFORM L4-ERROR
                   ELSE
                       IF C-DATE-TYPE = 'C'
                           MOVE ERR-MSG(21) TO O-ERR
                           PERFORM L4-ERROR.
           IF WK-DATE-NUMERIC-SW = 'Y'
               IF VALID-30-MM AND NOT VAL-WK-DD-30
                   IF C-DATE-TYPE = 'R'
                       MOVE ERR-MSG(7) TO O-ERR
                       PERFORM L4-ERROR
                   ELSE
                       IF C-DATE-TYPE = 'C'
                           MOVE ERR-MSG(22) TO O-ERR
                           PERFORM L4-ERROR.
           IF WK-DATE-NUMERIC-SW = 'Y'
               IF VALID-31-MM AND NOT VAL-WK-DD-31
                   IF C-DATE-TYPE = 'R'
                       MOVE ERR-MSG(8) TO O-ERR
                       PERFORM L4-ERROR
                   ELSE
                       IF C-DATE-TYPE = 'C'
                           MOVE ERR-MSG(23) TO O-ERR
                           PERFORM L4-ERROR.
           IF WK-DATE-NUMERIC-SW = 'Y'
               IF WK-MM = 2
                   DIVIDE WK-YYYY BY 4 GIVING C-LEFT-OVER REMAINDER 
                   C-LEFT-OVER
                  IF C-LEFT-OVER = 0
                       IF NOT VAL-WK-DD-29
                           IF C-DATE-TYPE = 'R'
                               MOVE ERR-MSG(9) TO O-ERR
                               PERFORM L4-ERROR
                           ELSE
                               IF C-DATE-TYPE = 'C'
                                   MOVE ERR-MSG(24) TO O-ERR
                                   PERFORM L4-ERROR
                               END-IF
                       ELSE
                           NEXT SENTENCE
                  ELSE
                       IF NOT VAL-WK-DD-28
                           IF C-DATE-TYPE = 'R'
                               MOVE ERR-MSG(10) TO O-ERR
                               PERFORM L4-ERROR
                           ELSE
                               IF C-DATE-TYPE = 'C'
                                   MOVE ERR-MSG(25) TO O-ERR
                                   PERFORM L4-ERROR.
           IF WK-DATE-NUMERIC-SW = 'Y'
               IF WK-DATE-NUM <= CUR-DATE-NUM
                   IF C-DATE-TYPE = 'R'
                       MOVE ERR-MSG(11) TO O-ERR
                       PERFORM L4-ERROR
                   ELSE
                       IF C-DATE-TYPE = 'C'
                           MOVE ERR-MSG(26) TO O-ERR
                           PERFORM L4-ERROR.

       L2-CLOSING.
       	   PERFORM L4-GRANDTOTALS.
           CLOSE RESERVEMASTER.
           CLOSE PRTOUT.
           CLOSE ERROUT.

       L4-GRANDTOTALS.
           MOVE C-TOT-ERR-CTR TO O-TOT-ERR-CTR.
           MOVE C-REC-ERR-CTR TO O-REC-ERR-CTR.
       	   WRITE ERRLINE FROM ERR-TOTAL-LINE
               AFTER ADVANCING 2 LINES.

       L3-READ.
           READ RESERVEMASTER
               AT END
                   MOVE "NO" TO MORE-RECS.

       L3-ERR-HEADINGS.
           ADD 1 TO C-ERR-PCTR.
           MOVE C-ERR-PCTR TO O-PCTR.
           WRITE ERRLINE FROM TITLE-LINE
             AFTER ADVANCING PAGE.
           WRITE ERRLINE FROM DIVISION-LINE
             AFTER ADVANCING 1 LINE.
           WRITE ERRLINE FROM ERR-REPORT-LINE
             AFTER ADVANCING 1 LINE.
           WRITE ERRLINE FROM BLANK-LINE
             AFTER ADVANCING 1 LINE.
           WRITE ERRLINE FROM ERR-COLUMN-HEADINGS
             AFTER ADVANCING 1 LINE.