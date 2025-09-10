       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollegeApp.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "./src/InCollege-Test.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "./src/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC        PIC X(50).

       FD OUTPUT-FILE.
       01 OUT-REC       PIC X(100).

       WORKING-STORAGE SECTION.
       01 mainChoice        PIC 9.
       01 subChoice         PIC 9.
       01 userName          PIC X(20) VALUE SPACES.
       01 userPass          PIC X(20) VALUE SPACES.
       01 msgBuffer         PIC X(100).
       01 doneFlag          PIC X VALUE "N".
       01 loginOk           PIC X VALUE "N".
       01 validPass         PIC X VALUE "N".
       01 hasUpper          PIC 9 VALUE 0.
       01 hasDigit          PIC 9 VALUE 0.
       01 hasSpecial        PIC 9 VALUE 0.
       01 idx               PIC 9.
       01 userCount         PIC 9 VALUE 0.
       01 maxUsers          PIC 9 VALUE 5.
       01 accName           PIC X(20) OCCURS 5 TIMES.
       01 accPass           PIC X(20) OCCURS 5 TIMES.
       01 passLength        PIC 9.
       01 skillList.
           05 skillName OCCURS 5 TIMES PIC X(20).

       PROCEDURE DIVISION.
       START-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM SETUP-SKILLS
           PERFORM WELCOME-SCREEN

           PERFORM GET-LOGIN UNTIL loginOk = "Y"

           PERFORM MAIN-MENU

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       SETUP-SKILLS.
           MOVE "Networking" TO skillName(1)
           MOVE "Programming" TO skillName(2)
           MOVE "Design" TO skillName(3)
           MOVE "Project Mgmt" TO skillName(4)
           MOVE "Data Analysis" TO skillName(5)
           .

       WELCOME-SCREEN.
           MOVE "Welcome to InCollege!" TO msgBuffer
           PERFORM DISPLAY-MSG
           MOVE "1. Log In" TO msgBuffer
           PERFORM DISPLAY-MSG
           MOVE "2. Create Account" TO msgBuffer
           PERFORM DISPLAY-MSG
           MOVE "Please choose an option:" TO msgBuffer
           PERFORM DISPLAY-MSG
           .

       GET-LOGIN.
           READ INPUT-FILE
               AT END MOVE "1" TO IN-REC
           END-READ
           MOVE FUNCTION NUMVAL(IN-REC) TO mainChoice

           IF mainChoice = 2
               PERFORM CREATE-ACCOUNT
           ELSE
               PERFORM LOGIN-PROCESS
           END-IF
           .

       LOGIN-PROCESS.
           MOVE "Enter username:" TO msgBuffer
           PERFORM DISPLAY-MSG
           READ INPUT-FILE
               AT END MOVE "Guest" TO IN-REC
           END-READ
           MOVE IN-REC TO userName

           MOVE "Enter password:" TO msgBuffer
           PERFORM DISPLAY-MSG
           READ INPUT-FILE
               AT END MOVE "password123!" TO IN-REC
           END-READ
           MOVE IN-REC TO userPass

           IF userCount = 0
               MOVE "Please create an account first." TO msgBuffer
               PERFORM DISPLAY-MSG
               PERFORM CREATE-ACCOUNT
           ELSE
               PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > userCount
                   IF userName = accName(idx)
                      IF userPass = accPass(idx)
                           MOVE "Login successful!" TO msgBuffer
                           PERFORM DISPLAY-MSG
                           MOVE "Hello, " TO msgBuffer
                           STRING msgBuffer DELIMITED BY SIZE
                                  userName DELIMITED BY SIZE
                                  INTO msgBuffer
                           END-STRING
                           PERFORM DISPLAY-MSG
                           MOVE "Y" TO loginOk
                           EXIT PERFORM
                      END-IF
                   END-IF
               END-PERFORM
               IF loginOk = "N"
                   MOVE "Invalid credentials. Try again." TO msgBuffer
                   PERFORM DISPLAY-MSG
               END-IF
           END-IF
           .

       CREATE-ACCOUNT.
           IF userCount >= maxUsers
               MOVE "All permitted accounts created." TO msgBuffer
               PERFORM DISPLAY-MSG
               STOP RUN
           END-IF

           MOVE "Enter new username:" TO msgBuffer
           PERFORM DISPLAY-MSG
           READ INPUT-FILE
               AT END MOVE "DefaultUser" TO IN-REC
           END-READ
           MOVE IN-REC TO userName
           PERFORM CHECK-USERNAME-EXISTS

           MOVE "Enter password:" TO msgBuffer
           PERFORM DISPLAY-MSG
           READ INPUT-FILE
               AT END MOVE "DefaultPass1!" TO IN-REC
           END-READ
           MOVE IN-REC TO userPass
           PERFORM VALIDATE-PASSWORD

           IF validPass = "N"
               MOVE "Invalid password. Try again." TO msgBuffer
               PERFORM DISPLAY-MSG
               PERFORM CREATE-ACCOUNT
           END-IF

           ADD 1 TO userCount
           MOVE userName TO accName(userCount)
           MOVE userPass TO accPass(userCount)

           MOVE "Account created successfully!" TO msgBuffer
           PERFORM DISPLAY-MSG
           MOVE "Y" TO loginOk
           .

       CHECK-USERNAME-EXISTS.
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > userCount
               IF userName = accName(idx)
                   MOVE "Username already exists." TO msgBuffer
                   PERFORM DISPLAY-MSG
                   PERFORM CREATE-ACCOUNT
               END-IF
           END-PERFORM
           .

       VALIDATE-PASSWORD.
           MOVE 0 TO hasUpper hasDigit hasSpecial
           MOVE FUNCTION LENGTH(userPass) TO passLength

           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > passLength
               IF userPass(idx:1) >= "A" AND userPass(idx:1) <= "Z"
                   MOVE 1 TO hasUpper
               END-IF
               IF userPass(idx:1) >= "0" AND userPass(idx:1) <= "9"
                   MOVE 1 TO hasDigit
               END-IF
               IF userPass(idx:1) = "!" OR
                  userPass(idx:1) = "@" OR
                  userPass(idx:1) = "#" OR
                  userPass(idx:1) = "$" OR
                  userPass(idx:1) = "%" OR
                  userPass(idx:1) = "&" OR
                  userPass(idx:1) = "*"
                   MOVE 1 TO hasSpecial
               END-IF
           END-PERFORM

           IF passLength >= 8 AND passLength <= 12 AND
              hasUpper = 1 AND hasDigit = 1 AND hasSpecial = 1
               MOVE "Y" TO validPass
           ELSE
               MOVE "N" TO validPass
           END-IF
           .

       MAIN-MENU.
           MOVE "N" TO doneFlag
           PERFORM UNTIL doneFlag = "Y"
               PERFORM SHOW-MAIN-MENU
               READ INPUT-FILE
                   AT END MOVE "6" TO IN-REC
               END-READ
               MOVE FUNCTION NUMVAL(IN-REC) TO mainChoice

               EVALUATE mainChoice
                   WHEN 1
                       MOVE "Job search coming soon." TO msgBuffer
                       PERFORM DISPLAY-MSG
                   WHEN 2
                       MOVE "Find a friend coming soon." TO msgBuffer
                       PERFORM DISPLAY-MSG
                   WHEN 3
                       PERFORM SKILL-MENU
                   WHEN OTHER
                       MOVE "Y" TO doneFlag
               END-EVALUATE
           END-PERFORM
           .

       SHOW-MAIN-MENU.
           MOVE "1. Search for a job" TO msgBuffer
           PERFORM DISPLAY-MSG
           MOVE "2. Find someone you know" TO msgBuffer
           PERFORM DISPLAY-MSG
           MOVE "3. Learn a skill" TO msgBuffer
           PERFORM DISPLAY-MSG
           MOVE "Enter your choice:" TO msgBuffer
           PERFORM DISPLAY-MSG
           .

       SKILL-MENU.
           MOVE "N" TO doneFlag
           PERFORM UNTIL doneFlag = "Y"
               PERFORM SHOW-SKILLS
               READ INPUT-FILE
                   AT END MOVE "6" TO IN-REC
               END-READ
               MOVE FUNCTION NUMVAL(IN-REC) TO subChoice

               IF subChoice >= 1 AND subChoice <= 5
                   MOVE "Skill under construction." TO msgBuffer
                   PERFORM DISPLAY-MSG
               ELSE
                   MOVE "Y" TO doneFlag
               END-IF
           END-PERFORM
           .

       SHOW-SKILLS.
           PERFORM VARYING subChoice FROM 1 BY 1 UNTIL subChoice > 5
               MOVE skillName(subChoice) TO msgBuffer
               PERFORM DISPLAY-MSG
           END-PERFORM
           MOVE "Go Back" TO msgBuffer
           PERFORM DISPLAY-MSG
           MOVE "Enter your selection:" TO msgBuffer
           PERFORM DISPLAY-MSG
           .

       DISPLAY-MSG.
           DISPLAY msgBuffer
           MOVE msgBuffer TO OUT-REC
           WRITE OUT-REC
           .
