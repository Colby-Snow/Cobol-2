       IDENTIFICATION DIVISION.
       program-id. COBCMSCP01.
       DATE-WRITTEN. 03/04/2020.
       AUTHOR. COLBY SNOW.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOATMASTER
           ASSIGN TO "C:\IHCC\COBOL\COBCMS01\RESERVE.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
           ASSIGN TO "C:\IHCC\COBOL\COBCMS01\CAMPRES.DAT"
           ORGANIZATION IS RECORD SEQUENTIAL.
           SELECT ERROUT
           ASSIGN TO "C:\IHCC\COBOL\COBCMS01\ERR.PRT"
           ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD BOATMASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-BOAT-REC
           RECORD CONTAINS 42 CHARACTERS.

       01 I-TRAN-REC.
           05 I-CAMPGROUND PIC X(25).
           05 I-SITE           PIC X99.
           05 I-DATE           PIC 9(8).
           05 I-LEN-STAY       PIC 99.
           05 I-LNAME		   PIC X(20).
           05 I-FNAME          PIC X(20).
           05 I-AMT            PIC S9(3)V99.
           05 I-CCTYPE         PIC X.
           05 I-CCNUM          PIC 9(16).
       	   05 I-CCEXP          PIC 9(8).

       FD PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01 PRTLINE PIC X(132).

       FD ERROUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS ERRLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       
       01 ERRLINE PIC X(132).

       COPY 'C:/IHCC/COBCMSCP01/ERROR.CPY'.
       COPY 'C:/IHCC/COBCMSCP01/TRAN.CPY'.
       COPY '~/VAL.CPY'.
       
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       L1-MAIN.
       	   PERFORM L2-INIT.
       	   PERFORM L2-MAINLINE
               UNTIL MORE-RECS = "YES".
       		  
       	   PERFORM L2-CLOSING.
           STOP RUN.

       L2-INIT.
           OPEN INPUT FILE.
           OPEN OUTPUT FILE.
             REQUEST SYSTEM DATE
             GET I-DATE
             O-DATE = I-DATE
           PERFORM L3-READ.
           PERFORM HEADINGS.
           PERFORM ERR-HEADINGS.

       L2-MAINLINE.
           PERFORM L3-VALIDATION THRU L3-VALIDATION-EXIT.
           PERFORM L3-CALCS.
           PERFORM L3-OUTPUT.
           PERFORM L3-READ..

       L3-VALIDATION.
           MOVE "Y" TO FIRST-ERR-SW
           MOVE "N" TO AMT-SW
           MOVE SPACES TO O-ERR
           IF VAL-CAMP
               MOVE TBL-ERR(1) TO O-ERR
               PERFORM L4-ERROR
               IF I-SITE-L NOT = ALPHABETIC
                   MOVE TBL-ERR(2) TO O-ERR
                   PERFORM L4-ERROR.
           IF I-SITE-N NOT = NUMERIC
               MOVE TBL-ERR(3) TO O-ERR
               PERFORM L4-ERROR.
           IF I-SITE-N < 0
               MOVE TBL-ERR(4) TO O-ERR
               PERFORM L4-ERROR.
           IF I-DATE NOT = NUMERIC
               MOVE TBL-ERR(5) TO O-ERR
               PERFORM L4-ERROR.
           IF VAL-MONTH
               MOVE TBL-ERR(6) TO O-ERR
               PERFORM L4-ERROR.
           IF I-DATE > I-CURDATE
               MOVE TBL-ERR(7) TO O-ERR
               PERFORM L4-ERROR.
           IF VAL-31MONTH
               IF VAL-31-DAY
                   MOVE TBL-ERR(8) TO O-ERR
                   PERFORM L4-ERROR
                   IF VAL-30MONTH
                       IF VAL-30-DAY
                           MOVE TBL-ERR(9) TO O-ERR
                           PERFORM L4-ERROR
                           IF VAL-LEAP-MONTH
                               IF I-YY % 4 > 0
                                 LEAP-SW = 'N'
                               ELSE
                                   IF I-YY % 100 = 0
                                       IF I-YY % 400 = 0
                                         LEAP-SW = 'Y'
                                       ELSE
                                         LEAP-SW = 'N'
                                   ELSE
                                         LEAP-SW = 'Y'
                                       IF LEAP-SW = 'Y'
                                           IF VAL-29-DAY
                                               MOVE TBL-ERR(10) TO O-ERR
                                               PERFORM L4-ERROR
                                           ELSE
                                               CONTINUE
                                       ELSE
                                           IF VAL-28-DAY
                                               MOVE TBL-ERR(11) TO O-ERR
                                               PERFORM L4-ERROR.
           IF I-LEN-STAY NOT = NUMERIC
               MOVE "Y" TO AMT-SW
               MOVE TBL-ERR(12) TO O-ERR
               PERFORM L4-ERROR.
           IF I-LEN-STAY < 1 OR I-LEN-STAY > 12
               MOVE TBL-ERR(13) TO O-ERR
               PERFORM L4-ERROR.
           IF I-FNAME = SPACES
               MOVE TBL-ERR(14) TO O-ERR
               PERFORM L4-ERROR.
           IF I-LNAME = SPACES
               MOVE TBL-ERR(15) TO O-ERR
               PERFORM L4-ERROR.
           IF I-AMT NOT = NUMERIC
               MOVE TBL-ERR(16) TO O-ERR
               PERFORM L4-ERROR
           ELSE
               IF AMT-SW = 'Y'
                   NEXT SENTENCE
               ELSE
                   PERFORM
                     VARYING I FROM 1 BY 1
                     UNTIL TBL-SITE(I) = I-SITE-L
                       COMPUTE C-AMT = TBL-DAY-AMT(I) * I-LEN-STAY
                       IF C-AMT NOT = I-AMT
                           MOVE TBL-ERR(17) TO O-ERR
                           PERFORM L4-ERROR.
           IF VAL-CCTYPE
               MOVE TBL-ERR(18) TO O-ERR
               PERFORM L4-ERROR.
           IF I-CCNUM NOT = NUMERIC
               MOVE TBL-ERR(19) TO O-ERR
               PERFORM L4-ERROR.
           IF I-DATE NOT = NUMERIC
               MOVE TBL-ERR(20) TO O-ERR
               PERFORM L4-ERROR.
           IF VAL-MONTH
               MOVE TBL-ERR(21) TO O-ERR
               PERFORM L4-ERROR.
           IF I-CCEXP NOT = NUMERIC
               MOVE TBL-ERR(22) TO O-ERR
               PERFORM L4-ERROR.
           IF I-CCEXP > I-CURDATE
               MOVE TBL-ERR(23) TO O-ERR
               PERFORM L4-ERROR.
           IF VALCC-31MONTH
               IF VALCC-31-DAY
                   MOVE TBL-ERR(24) TO O-ERR
                   PERFORM L4-ERROR
                   IF VALCC-30MONTH
                       IF VALCC-30-DAY
                           MOVE TBL-ERR(25) TO O-ERR
                           PERFORM L4-ERROR
                           IF VALCC-LEAP-MONTH
                               IF I-CCYY % 4 > 0
                                 LEAP-SW = 'N'
                               ELSE
                                   IF I-CCYY % 100 = 0
                                       IF I-CCYY % 400 = 0
                                         LEAP-SW = 'Y'
                                       ELSE
                                         LEAP-SW = 'N'
                                   ELSE
                                         LEAP-SW = 'Y'
                                       IF LEAP-SW = 'Y'
                                           IF VALCC-29-DAY
                                               MOVE TBL-ERR(26) TO O-ERR
                                               PERFORM L4-ERROR
                                           ELSE
                                               CONTINUE
                                       ELSE
                                           IF VALCC-28-DAY
                                               MOVE TBL-ERR(27) TO O-ERR
                                               PERFORM L4-ERROR.
           IF O-ERR = SPACES
               MOVE "N" TO FIRST-ERR-SW.

       L3-VALIDATION-EXIT.
           EXIT.

       CALCS.
           IF VALCC-LEAP-MONTH
               IF I-CCYY % 4 > 0
                 LEAP-SW = 'N'
               ELSE
                   IF I-CCYY % 100 = 0
                       IF I-CCYY % 400 = 0
                         LEAP-SW = 'Y'
                       ELSE
                         LEAP-SW = 'N'
                   ELSE
                         LEAP-SW = 'Y'
                       IF LEAP-SW = 'Y'
                           IF VALCC-29-DAY
                               ADD 1 TO MONTH
                               SUBTRACT 29 FROM DAY
                           ELSE
                               CONTINUE
                       ELSE
                           IF VALCC-28-DAY
                               ADD 1 TO MONTH
                               SUBTRACT 28 FROM DAY
                               IF VAL-30-MONTH
                                   ADD I-DATE TO I-LEN-STAY GIVING
                                     END-DATE
                                   IF VAL-30-DAY
                                       ADD 1 TO MONTH
                                       SUBTRACT 30 FROM DAY

                                       IF VAL-31-MONTH
                                           ADD I-DATE TO I-LEN-STAY
                                             GIVING END-DATE
                                           IF VAL-31-DAY
                                               ADD 1 TO MONTH
                                               SUBTRACT 31 FROM DAY
                                               IF VAL-MONTH
                                                   ADD 1 TO YEAR
                                                   SUBTRACT 12 FROM
                                                     MONTH.

       OUTPUT.
       	   PRINT DETAIL LINE
       	   	   AT EOP
               PERFORM HEADINGS.
       	   PRINT BLANK LINE.

       ERR-ROUT.
           IF FIRST-ERR-SW = 'Y'
               MOVE 'N' TO FIRST-ERR-SW
               ADD 1 TO REC-ERR-CTR.
           MOVE TRAN-REC TO O-REC-DUMP
           WRITE ERRLINE FROM O-REC-LINE
             AFTER ADVANCING 1 LINE
           WRITE ERRLINE FROM O-ERR-LINE
             AFTER ADVANCING 1 LINE
             ELSE
           WRITE ERRLINE FROM O-ERR-LINE
             AFTER ADVANCING 1 LINE.
           ADD 1 TO TOT-ERR-CTR.

       L3-CLOSING.
       	   L4-GRANDTOTALS
           CLOSE INPUT FILE
           CLOSE OUTPUT FILE

       L4-GRANDTOTALS.
           

           PERFORM HEADINGS
             AFTER ADVANCING 1 PAGE.
       	   PRINT GRAND TOTAL LINE.

       	   PRINT ERROR-TOTAL LINE O-ERR-COUNT.

       L3-READ.
           READ RECORD
           IF END OF FILE
               MORE-RECTS = "NO".