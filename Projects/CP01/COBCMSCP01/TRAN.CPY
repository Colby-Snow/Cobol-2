       01 I-TRAN-REC.
         05 I-CAMPGROUND PIC X(25).
           88 VAL-CAMP VALUE 'BUCK CREEK', 'HONEY CREEK', 
           'ISLAND VIEW'.
         05 I-SITE.
           10 I-SITE-L         PIC X.
           10 I-SITE-N         PIC 99.
         05 I-DATE.
           10 I-YY     PIC 9(4).
           10 I-MM     PIC 99.
           10 I-DD     PIC 99.
         05 I-LEN-STAY         PIC 99.
         05 I-LNAME            PIC X(20).
         05 I-FNAME            PIC X(20).
         05 I-AMT              PIC S9(3)V99.
         05 I-CCTYPE           PIC X.
           88 VAL-CCTYPE VALUE 'V', 'M', 'A'.
         05 I-CCNUM            PIC 9(16).
         05 I-CCEXP.
           10 CC-YY     PIC 9(4).
           10 CC-MM     PIC 99.
             88 VAL-CC-MM VALUE 1 THRU 12.
             88 VAL-CC-30-MM VALUE 4, 6, 9, 11.
             88 VAL-CC-31-MM VALUE 1, 3, 5, 7, 8, 10, 12.
           10 CC-DD     PIC 99.
             88 VAL-CC-31-DD VALUE 1 THRU 31.
             88 VAL-CC-30-DD VALUE 1 THRU 30.
             88 VAL-CC-28-DD VALUE 1 THRU 28.
             88 VAL-CC-29-DD VALUE 1 THRU 29.