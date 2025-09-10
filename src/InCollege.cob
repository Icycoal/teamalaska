      IDENTIFICATION DIVISION.
      PROGRAM-ID. InCollege.


      ENVIRONMENT DIVISION.
      INPUT-OUTPUT SECTION.
      FILE-CONTROL.
          SELECT INPUT-FILE ASSIGN TO "./src/InCollege-Test.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
          SELECT OUTPUT-FILE ASSIGN TO "./src/InCollege-Output.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
          SELECT ACCOUNT-FILE ASSIGN TO "./src/InCollege-Accounts.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS acct-file-status.


      DATA DIVISION.
      FILE SECTION.
      FD INPUT-FILE.
      01 IN-REC PIC X(50).


      FD OUTPUT-FILE.
      01 OUT-REC PIC X(100).


      FD ACCOUNT-FILE.
      01 ACC-REC-FILE PIC X(40).


      WORKING-STORAGE SECTION.
      01 mainChoice        PIC 9.
      01 subChoice         PIC 9.
      01 loginOk           PIC X VALUE "N".
      01 doneFlag          PIC X VALUE "N".
      01 msgBuffer         PIC X(100).
      01 userName          PIC X(20).
      01 userPass          PIC X(20).
      01 accountCount      PIC 9 VALUE 0.
      01 foundFlag         PIC X VALUE "N".
      01 idx               PIC 9.
      01 charPos           PIC 99.
      01 char              PIC X.
      01 passLength        PIC 99.
      01 hasUpper          PIC X.
      01 hasDigit          PIC X.
      01 hasSpecial        PIC X.
      01 acct-file-status  PIC XX.
      01 trimmedUser       PIC X(20).
      01 trimmedPass       PIC X(20).


      01 accounts.
          05 account-user OCCURS 5 TIMES PIC X(20) VALUE SPACES.
          05 account-pass OCCURS 5 TIMES PIC X(20) VALUE SPACES.


      01 skillList.
          05 skillName OCCURS 5 TIMES PIC X(20) VALUE SPACES.


      PROCEDURE DIVISION.
      START-PROGRAM.
          OPEN INPUT INPUT-FILE
          OPEN OUTPUT OUTPUT-FILE
          OPEN INPUT ACCOUNT-FILE
          IF acct-file-status = "35"
              MOVE 0 TO accountCount
          ELSE
              PERFORM LOAD-ACCOUNTS
          END-IF


          PERFORM SETUP-SKILLS
          PERFORM WELCOME-SCREEN


          PERFORM MAIN-MENU UNTIL doneFlag = "Y"


          PERFORM SAVE-ACCOUNTS


          CLOSE INPUT-FILE
          CLOSE OUTPUT-FILE
          CLOSE ACCOUNT-FILE
          STOP RUN.


      LOAD-ACCOUNTS.
          MOVE 0 TO accountCount
          PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > 5
              READ ACCOUNT-FILE
                  AT END EXIT PERFORM
                  NOT AT END
                      ADD 1 TO accountCount
                      MOVE ACC-REC-FILE(1:20) TO account-user(idx)
                      MOVE ACC-REC-FILE(21:20) TO account-pass(idx)
              END-READ
          END-PERFORM.


      SAVE-ACCOUNTS.
          OPEN OUTPUT ACCOUNT-FILE
          PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > accountCount
              MOVE account-user(idx) TO ACC-REC-FILE(1:20)
              MOVE account-pass(idx) TO ACC-REC-FILE(21:20)
              WRITE ACC-REC-FILE
          END-PERFORM
          CLOSE ACCOUNT-FILE.


      SETUP-SKILLS.
          MOVE "Skill1" TO skillName(1)
          MOVE "Skill2" TO skillName(2)
          MOVE "Skill3" TO skillName(3)
          MOVE "Skill4" TO skillName(4)
          MOVE "Skill5" TO skillName(5).


      WELCOME-SCREEN.
          MOVE "Welcome to InCollege!" TO msgBuffer
          PERFORM DISPLAY-MSG
          MOVE "1. Log In" TO msgBuffer
          PERFORM DISPLAY-MSG
          MOVE "2. Create New Account" TO msgBuffer
          PERFORM DISPLAY-MSG
          MOVE "Enter your choice:" TO msgBuffer
          PERFORM DISPLAY-MSG.


      MAIN-MENU.
          READ INPUT-FILE
              AT END MOVE "1" TO IN-REC
          END-READ
          MOVE FUNCTION NUMVAL(IN-REC) TO mainChoice


          EVALUATE mainChoice
              WHEN 1
                  PERFORM LOGIN
              WHEN 2
                  PERFORM CREATE-ACCOUNT
          END-EVALUATE
          PERFORM WELCOME-SCREEN.


      CREATE-ACCOUNT.
          IF accountCount >= 5
              MOVE "All permitted accounts have been created, please come back later" TO msgBuffer
              PERFORM DISPLAY-MSG
              EXIT PARAGRAPH
          END-IF


          MOVE "Enter new username:" TO msgBuffer
          PERFORM DISPLAY-MSG
          READ INPUT-FILE
              AT END MOVE SPACES TO userName
              NOT AT END MOVE IN-REC TO userName
          END-READ


          MOVE "Enter new password:" TO msgBuffer
          PERFORM DISPLAY-MSG
          READ INPUT-FILE
              AT END MOVE SPACES TO userPass
              NOT AT END MOVE IN-REC TO userPass
          END-READ


          MOVE 0 TO passLength
          PERFORM VARYING charPos FROM 1 BY 1 UNTIL charPos > 20
              IF userPass(charPos:1) = SPACE
                  EXIT PERFORM
              ELSE
                  ADD 1 TO passLength
              END-IF
          END-PERFORM


          IF passLength < 8 OR passLength > 12
              MOVE "Password does not meet requirements." TO msgBuffer
              PERFORM DISPLAY-MSG
              EXIT PARAGRAPH
          END-IF


          MOVE "N" TO hasUpper
          MOVE "N" TO hasDigit
          MOVE "N" TO hasSpecial


          PERFORM VARYING charPos FROM 1 BY 1 UNTIL charPos > passLength
              MOVE userPass(charPos:1) TO char
              IF char >= "A" AND char <= "Z"
                  MOVE "Y" TO hasUpper
              ELSE IF char >= "0" AND char <= "9"
                  MOVE "Y" TO hasDigit
              ELSE IF char = "!" OR char = "@" OR char = "#" OR char = "$" OR char = "%" OR char = "^" OR char = "&" OR char = "*"
                  MOVE "Y" TO hasSpecial
              END-IF
          END-PERFORM


          IF hasUpper = "N" OR hasDigit = "N" OR hasSpecial = "N"
              MOVE "Password does not meet requirements." TO msgBuffer
              PERFORM DISPLAY-MSG
              EXIT PARAGRAPH
          END-IF


          ADD 1 TO accountCount
          MOVE userName TO account-user(accountCount)
          MOVE userPass TO account-pass(accountCount)
          MOVE "Account successfully created!" TO msgBuffer
          PERFORM DISPLAY-MSG.


      LOGIN.
          MOVE "N" TO loginOk
          PERFORM UNTIL loginOk = "Y"
              MOVE "Please enter your username:" TO msgBuffer
              PERFORM DISPLAY-MSG
              READ INPUT-FILE
                  AT END MOVE SPACES TO userName
                  NOT AT END MOVE IN-REC TO userName
              END-READ


              MOVE "Please enter your password:" TO msgBuffer
              PERFORM DISPLAY-MSG
              READ INPUT-FILE
                  AT END MOVE SPACES TO userPass
                  NOT AT END MOVE IN-REC TO userPass
              END-READ


              MOVE "N" TO foundFlag
              PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > accountCount
                  MOVE FUNCTION TRIM(account-user(idx)) TO trimmedUser
                  MOVE FUNCTION TRIM(account-pass(idx)) TO trimmedPass
                  IF trimmedUser = FUNCTION TRIM(userName)
                     AND trimmedPass = FUNCTION TRIM(userPass)
                      MOVE "Y" TO foundFlag
                  END-IF
              END-PERFORM


              IF foundFlag = "Y"
                  MOVE "You have successfully logged in." TO msgBuffer
                  PERFORM DISPLAY-MSG
                  MOVE "Welcome, " TO msgBuffer
                  STRING msgBuffer DELIMITED BY SIZE
                         userName DELIMITED BY SIZE
                         INTO msgBuffer
                  END-STRING
                  PERFORM DISPLAY-MSG
                  MOVE "Y" TO loginOk
                  PERFORM POST-LOGIN
              ELSE
                  MOVE "Incorrect username/password, please try again." TO msgBuffer
                  PERFORM DISPLAY-MSG
              END-IF
          END-PERFORM.


      POST-LOGIN.
          MOVE "N" TO doneFlag
          PERFORM UNTIL doneFlag = "Y"
              MOVE "1. Search for a job" TO msgBuffer
              PERFORM DISPLAY-MSG
              MOVE "2. Find someone you know" TO msgBuffer
              PERFORM DISPLAY-MSG
              MOVE "3. Learn a new skill" TO msgBuffer
              PERFORM DISPLAY-MSG
              MOVE "Enter your choice:" TO msgBuffer
              PERFORM DISPLAY-MSG


              READ INPUT-FILE
                  AT END MOVE "3" TO IN-REC
              END-READ
              MOVE FUNCTION NUMVAL(IN-REC) TO mainChoice


              EVALUATE mainChoice
                  WHEN 1
                      MOVE "Job search/internship is under construction." TO msgBuffer
                      PERFORM DISPLAY-MSG
                  WHEN 2
                      MOVE "Find someone you know is under construction." TO msgBuffer
                      PERFORM DISPLAY-MSG
                  WHEN 3
                      PERFORM SKILL-MENU
              END-EVALUATE
          END-PERFORM.


      SKILL-MENU.
          MOVE "N" TO doneFlag
          PERFORM UNTIL doneFlag = "Y"
              MOVE "Learn a New Skill:" TO msgBuffer
              PERFORM DISPLAY-MSG
              PERFORM VARYING subChoice FROM 1 BY 1 UNTIL subChoice > 5
                  MOVE skillName(subChoice) TO msgBuffer
                  PERFORM DISPLAY-MSG
              END-PERFORM
              MOVE "6. Go Back" TO msgBuffer
              PERFORM DISPLAY-MSG
              MOVE "Enter your choice:" TO msgBuffer
              PERFORM DISPLAY-MSG


              READ INPUT-FILE
                  AT END MOVE "6" TO IN-REC
              END-READ
              MOVE FUNCTION NUMVAL(IN-REC) TO subChoice


              IF subChoice >= 1 AND subChoice <= 5
                  MOVE "This skill is under construction." TO msgBuffer
                  PERFORM DISPLAY-MSG
              ELSE
                  MOVE "Y" TO doneFlag
              END-IF
          END-PERFORM.


      DISPLAY-MSG.
          DISPLAY msgBuffer
          MOVE msgBuffer TO OUT-REC
          WRITE OUT-REC.



