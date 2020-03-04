       IDENTIFICATION DIVISION.
       program-id. COBCMSCP01.
       DATE-WRITTEN. 03/04/2020.
       AUTHOR. COLBY SNOW.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOATMASTER
           ASSIGN TO "C:\IHCC\COBOL\COBCMS03\CBLBOAT1.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
           ASSIGN TO "C:\IHCC\COBOL\COBCMS03\CBLBOAT.PRT"
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

       COPY 'C:/IHCC/COBCMSCP01/ERROR.CPY'.
       COPY 'C:/IHCC/COBCMSCP01/TRAN.CPY'.
       COPY '~/VAL.CPY'.

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.

           goback.
           
       end program COBCMSCP01.