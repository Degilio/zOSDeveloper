       CBL SOURCE,XREF,LIB,APOST
       CBL CICS('COBOL3')
       CBL SQL('APOSTSQL')
      *****************************************************************
      *                                                               *
      *  MODULE NAME = CUSTOMER                                       *
      *                                                               *
      *  DESCRIPTIVE NAME = Simple CRUD application to                *
      *                     perform actions on the CUSTOM table       *
      *                                                               *
      *                                                               *
      *                                                               *
      *      (C) Copyright IBM Corp. 2017"                            *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *  STATUS = 1.0.0                                               *
      *                                                               *
      *  TRANSACTION NAME = n/a                                       *
      *                                                               *
      *  FUNCTION =                                                   *
      *  Table maintenance for CUSTTB table.                          *
      *       Insert row                                              *
      *       Retrieve row                                            *
      *       Update row                                              *
      *       Delete row                                              *
      *                                                               *
      *-------------------------------------------------------------  *
      *                                                               *
      *                                                               *
      *-------------------------------------------------------------  *
      *                                                               *
      *  CHANGE ACTIVITY :                                            *
      *                                                               *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      * Common defintions                                             *
      *---------------------------------------------------------------*

      * Working variables
       01 WORKING-VARIABLES.
           03 WS-RETURN-CODE           PIC S9(8) COMP.
           03 WS-RESP1                 PIC S9(8) COMP.
           03 WS-RESP2                 PIC S9(8) COMP.


      * Variables for columns in the CUSTTB table
       01 Customer-Table.
           03  Customer-Number          PIC S9(9) COMP-4.
           03  Customer-First-Name      PIC X(100).
           03  Customer-Last-Name       PIC X(100).
           03  Customer-Address1        PIC X(100).
           03  Customer-Address2        PIC X(100).
           03  Customer-City             PIC X(100).
           03  Customer-State-Code      PIC XX.
           03  Customer-Postal-Code     PIC X(10).
           03  Customer-Home-Phone      PIC X(20).
           03  Customer-Mobile-Phone    PIC X(20).
           03  Customer-Gender-Code     PIC X(4).
           03  Customer-FICA-Score      PIC S9(9) COMP-4.
           03  Customer-Spending-Limit  PIC S9(9) COMP-4.
           03  Customer-Rewards-Number  PIC S9(9) COMP-4.

      *---------------------------------------------------------------*

      ***********************************
      ***   DB2 RELATED
      *** FOLLOWING DB2 DATA AREAS ARE FOR SB EBCDIC ENCODED STRINGS
      *****************************************************************
      *****************************************************************
      ***   DB2 STORAGE AREA FOR CUSTOMER TABLE
      *****************************************************************
           EXEC SQL DECLARE ZOSCONN.CUSTOM TABLE
           ( CUSTOMERNUMBER                 INTEGER,
             FIRSTNAME                      CHAR(100),
             LASTNAME                       CHAR(100),
             ADDRESS1                       CHAR(100),
             ADDRESS2                       CHAR(100),
             CITYCODE                       CHAR(100),
             STATECODE                      CHAR(2),
             POSTCODE                       CHAR(10),
             PHONEHOME                      CHAR(20),
             PHONEMOBILE                    CHAR(20),
             GENDERCODE                     CHAR(4),
             FICACODE                       INTEGER,
             SPENDINGCODE                   INTEGER,
             REWARDSCODE                    INTEGER
           ) END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.
      *---------------------------------------------------------------*

      *****************************************************************
      *    L I N K A G E   S E C T I O N
      *****************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05  Action                        PIC X.
          05  ErrorCode                     PIC X(4).
          05  TheCustomerNumber             PIC S9(9) COMP-5.
          05  FirstName                     PIC X(100).
          05  LastName                      PIC X(100).
          05  Address1                      PIC X(100).
          05  Address2                      PIC X(100).
          05  CityCode                      PIC X(100).
          05  StateCode                     PIC X(2).
          05  PostCode                      PIC X(10).
          05  PhoneHome                     PIC X(20).
          05  PhoneMobile                   PIC X(20).
          05  GenderCode                    PIC X(4).
          05  FICACode                      PIC S9(9) COMP-5.
          05  SpendingCode                  PIC S9(9) COMP-5.
          05  RewardsCode                   PIC S9(9) COMP-5.

      *****************************************************************
      *    P R O C E D U R E S
      *****************************************************************
       PROCEDURE DIVISION.

      *---------------------------------------------------------------*
       MAINLINE SECTION.

      *---------------------------------------------------------------*
      * Common code                                                   *
      *---------------------------------------------------------------*
      * initialize working storage variables
           DISPLAY 'Starting Program'.
           MOVE 00 TO WS-RETURN-CODE
           INITIALIZE Customer-Table.

      *----------------------------------------------------------------*
      * Check COMMAREA length and proceed
      *----------------------------------------------------------------*

           IF EIBCALEN = LENGTH OF DFHCOMMAREA
               PERFORM Customer-Table-Action
           ELSE
               MOVE 999 TO WS-RETURN-CODE
               DISPLAY 'Wrong-length Commarea received'
               EXEC CICS ABEND ABCODE('CALN')
               END-EXEC
           END-IF.

      * Return to caller
           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.
      *---------------------------------------------------------------*

       CUSTOMER-TABLE-ACTION.

           EVALUATE Action
               WHEN 'A'
                   PERFORM INSERT-ROW-INTO-CUSTOMER-TABLE
               WHEN 'C'
                   PERFORM INSERT-ROW-INTO-CUSTOMER-TABLE
               WHEN 'R'
                   PERFORM SELECT-ROW-FROM-CUSTOMER-TABLE
               WHEN 'U'
                   PERFORM UPDATE-ROW-IN-CUSTOMER-TABLE
               WHEN 'D'
                   PERFORM DELETE-ROW-FROM-CUSTOMER-TABLE
               WHEN OTHER
                   MOVE 999 TO WS-RETURN-CODE
                   MOVE 'CACT' TO ErrorCode
                   DISPLAY 'Invalid Action Code received'
                   EXEC CICS ABEND ABCODE('CACT')
                   END-EXEC
           END-EVALUATE.

      *===============================================================*
      * Procedure to select a row in the CUSTOM table.                *
      *===============================================================*

       SELECT-ROW-FROM-CUSTOMER-TABLE.

      * Perform the SQL SELECT command
           EXEC SQL
              SELECT CUSTOMERNUMBER,
                     FIRSTNAME,
                     LASTNAME,
                     ADDRESS1,
                     ADDRESS2,
                     CITYCODE,
                     STATECODE,
                     POSTCODE,
                     PHONEHOME,
                     PHONEMOBILE,
                     GENDERCODE,
                     FICACODE,
                     SPENDINGCODE,
                     REWARDSCODE
              INTO  :Customer-Number,
                    :Customer-First-Name,
                    :Customer-Last-Name,
                    :Customer-Address1,
                    :Customer-Address2,
                    :Customer-City,
                    :Customer-State-Code,
                    :Customer-Postal-Code,
                    :Customer-Home-Phone,
                    :Customer-Mobile-Phone,
                    :Customer-Gender-Code,
                    :Customer-FICA-Score,
                    :Customer-Spending-Limit,
                    :Customer-Rewards-Number
              FROM ZOSCONN.CUSTOM
              WHERE CUSTOMERNUMBER = :TheCustomerNumber
           END-EXEC.

           if SQLCODE = 0
              display 'SELECT CUSTOM '
                      'Customer Number ' TheCustomerNumber
                      'SLQCODE = ' SQLCODE
              perform MOVE-READ-DATA
           else
              display 'ABENDING SELECT FOR CUSTOM '
                      'Customer Number' TheCustomerNumber
                      'SQLCODE = ' SQLCODE
              move 'ERR' to ErrorCode
           end-if.
       SELECT-CUSTOM-ROW-EXIT.
           EXIT.

      *===============================================================*
      * Procedure to insert a new row into the Customer table.       *
      *===============================================================*

       INSERT-ROW-INTO-CUSTOMER-TABLE.

           DISPLAY 'Starting insert routine'.
      * Perform the SQL INSERT command
           EXEC SQL
              INSERT INTO ZOSCONN.CUSTOM
                    (
                     CUSTOMERNUMBER,
                     FIRSTNAME,
                     LASTNAME,
                     ADDRESS1,
                     ADDRESS2,
                     CITYCODE,
                     STATECODE,
                     POSTCODE,
                     PHONEHOME,
                     PHONEMOBILE,
                     GENDERCODE,
                     FICACODE,
                     SPENDINGCODE,
                     REWARDSCODE
                    )
              VALUES
                    (
                     :TheCustomerNumber,
                     :FirstName,
                     :LastName,
                     :Address1,
                     :Address2,
                     :CityCode,
                     :StateCode,
                     :PostCode,
                     :PhoneHome,
                     :PhoneMobile,
                     :GenderCode,
                     :FICACode,
                     :SpendingCode,
                     :RewardsCode
                    )
           END-EXEC.

           MOVE SQLCODE TO WS-RETURN-CODE
           if SQLCODE = 0
              display 'INSERT CUSTOM '
                      'Customer Number ' TheCustomerNumber
                      'SLQCODE = ' SQLCODE
           else
              display 'ABENDING INSERT FOR CUSTOM '
                      'Customer Number' TheCustomerNumber
                      'SQLCODE = ' SQLCODE
              move 'ERR' to ErrorCode
           end-if.
       INSERT-CUSTOM-ROW-EXIT.
           EXIT.






      *================================================================*
      * Procedure to update a row in the CUSTTB table.                 *
      *================================================================*
       UPDATE-ROW-IN-CUSTOMER-TABLE.

      * Perform the SQL UPDATE command
           EXEC SQL
              UPDATE ZOSCONN.CUSTOM
              SET FIRSTNAME      = :FirstName,
                  LASTNAME       = :LastName,
                  ADDRESS1       = :Address1,
                  ADDRESS2       = :Address2,
                  CITYCODE       = :CityCode,
                  STATECODE      = :StateCode,
                  POSTCODE       = :PostCode,
                  PHONEHOME      = :PhoneHome,
                  PHONEMOBILE    = :PhoneMobile,
                  GENDERCODE     = :GenderCode,
                  FICACODE       = :FICACode,
                  SPENDINGCODE   = :SpendingCode,
                  REWARDSCODE    = :RewardsCode
              WHERE CUSTOMERNUMBER = :TheCustomerNumber
           END-EXEC.

           if SQLCODE = 0
              display 'UPDATE CUSTOM '
                      'Customer Number ' TheCustomerNumber
                      'SLQCODE = ' SQLCODE
           else
              display 'ABENDING UPDATE FOR CUSTOM '
                      'Customer Number' TheCustomerNumber
                      'SQLCODE = ' SQLCODE
              move 'ERR' to ErrorCode
           end-if.
       UPDATE-CUSTTB-ROW-EXIT.
           EXIT.

      *================================================================*
      * Procedure to delete a row in the CUSTTB table.                 *
      *================================================================*
       DELETE-ROW-FROM-CUSTOMER-TABLE.

      * Perform the SQL DELETE command

           EXEC SQL
              DELETE FROM ZOSCONN.CUSTOM
              WHERE CUSTOMERNUMBER = :TheCustomerNumber
           END-EXEC.

           if SQLCODE = 0
              display 'DELETE CUSTOM '
                      'Customer Number ' TheCustomerNumber
                      'SLQCODE = ' SQLCODE
           else
              display 'ABENDING DELETE FOR CUSTOM '
                      'Customer Number' TheCustomerNumber
                      'SQLCODE = ' SQLCODE
              move 'ERR' to ErrorCode
           end-if.
       DELETE-CUSTTB-ROW-EXIT.
           EXIT.

       MOVE-READ-DATA.
           MOVE Customer-Number to         TheCustomerNumber
           MOVE Customer-First-Name to     FirstName
           MOVE Customer-Last-Name to      LastName
           MOVE Customer-Address1 to       Address1
           MOVE Customer-Address2 to       Address2
           MOVE Customer-City to            CityCode
           MOVE Customer-State-Code to     StateCode
           MOVE Customer-Postal-Code to    PostCode
           MOVE Customer-Home-Phone to     PhoneHome
           MOVE Customer-Mobile-Phone to   PhoneMobile
           MOVE Customer-Gender-Code to    GenderCode
           MOVE Customer-FICA-Score to     FICACode
           MOVE Customer-Spending-Limit to SpendingCode
           MOVE Customer-Rewards-Number to RewardsCode.
       MOVE-READ-DATA-EXIT.
           EXIT.
