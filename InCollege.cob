IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT ACCOUNT-FILE ASSIGN TO "InCollege-Accounts.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS acct-file-status.
    SELECT PROFILE-FILE ASSIGN TO "InCollege-Profiles.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS profile-file-status.
    SELECT CONNECTION-FILE ASSIGN TO "InCollege-Connections.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS conn-file-status.
    SELECT REQUEST-FILE ASSIGN TO "InCollege-Requests.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS request-file-status.
    SELECT JOBS-FILE ASSIGN TO "InCollege-Jobs.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS jobs-file-status.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 IN-REC PIC X(201).

FD OUTPUT-FILE.
01 OUT-REC PIC X(250).

FD ACCOUNT-FILE.
01 ACC-REC-FILE PIC X(40).

FD PROFILE-FILE.
01 PROF-REC-FILE PIC X(1550).

FD CONNECTION-FILE.
01 CONN-REC-FILE.
    05 CONN-USER1-FILE PIC X(20).
    05 CONN-USER2-FILE PIC X(20).
    05 CONN-STATUS-FILE PIC X.

FD REQUEST-FILE.
01 REQ-REC-FILE.
    05 REQ-SENDER-FILE   PIC X(20).
    05 REQ-RECEIVER-FILE PIC X(20).

FD JOBS-FILE.
01 JOB-REC-FILE PIC X(400).

WORKING-STORAGE SECTION.
01 TARGET-USER   PIC X(20).
01 USER-FULLNAME PIC X(250).
01 mainChoice PIC 9.
01 subChoice PIC 9.
01 loginOk PIC X VALUE "N".
01 programDoneFlag    PIC X VALUE "N".
01 postLoginDoneFlag  PIC X VALUE "N".
01 msgBuffer PIC X(250).
01 userName PIC X(20).
01 userPass PIC X(20).
01 accountCount PIC 9 VALUE 0.
01 foundFlag PIC X VALUE "N".
01 idx PIC 9.
01 charPos PIC 99.
01 char PIC X.
01 passLength PIC 99.
01 hasUpper PIC X.
01 hasDigit PIC X.
01 hasSpecial PIC X.
01 acct-file-status PIC XX.
01 profile-file-status PIC XX.
01 conn-file-status PIC XX.
01 request-file-status PIC XX.
01 trimmedUser PIC X(20).
01 trimmedPass PIC X(20).
01 loggedInUser PIC 9.
01 temp-year PIC 9(4).
01 trimmed-input PIC X(50).
01 short-trimmed PIC X(4).
01 graduation-year-str PIC X(4).
01 exp-idx PIC 9.
01 edu-idx PIC 9.
01 EOF-INPUT-FILE PIC X VALUE "N".
01 ws-search-name       PIC X(41).
01 ws-full-name         PIC X(41).
01 ws-display-idx       PIC 9.
01 search-idx           PIC 9.
01 search-found-flag    PIC X.
01 debug-input PIC X(201).
01 conn-idx PIC 99.
01 conn-check-idx PIC 99.
01 user1-id PIC 9.
01 user2-id PIC 9.
01 conn-status-check PIC X.
01 can-send-request PIC X.
01 pending-count PIC 99.

01 jobs-file-status PIC XX.
01 jobMenuDoneFlag PIC X.
01 job-title     PIC X(50).
01 job-desc      PIC X(200).
01 job-employer  PIC X(50).
01 job-location  PIC X(50).
01 job-salary    PIC X(30).

01 accounts.
    05 account-user OCCURS 5 TIMES PIC X(20) VALUE SPACES.
    05 account-pass OCCURS 5 TIMES PIC X(20) VALUE SPACES.

01 user-profiles.
    05 user-profile OCCURS 5 TIMES.
        10 first-name PIC X(20).
        10 last-name PIC X(20).
        10 university PIC X(50).
        10 major PIC X(50).
        10 graduation-year PIC 9(4).
        10 about-me PIC X(200).
        10 experience-table OCCURS 3 TIMES.
            15 exp-title PIC X(50).
            15 exp-company PIC X(50).
            15 exp-dates PIC X(50).
            15 exp-desc PIC X(100).
        10 education-table OCCURS 3 TIMES.
            15 edu-degree PIC X(50).
            15 edu-university PIC X(50).
            15 edu-years PIC X(50).

01 connection-data.
    05 connection-record OCCURS 25 TIMES.
        10 conn-user1 PIC X(20).
        10 conn-user2 PIC X(20).
        10 conn-status PIC X.
           88 conn-pending VALUE "P".
           88 conn-accepted VALUE "A".

01 connection-count PIC 99 VALUE 0.

01 request-data.
    05 request-record OCCURS 25 TIMES.
        10 req-sender   PIC X(20).
        10 req-receiver PIC X(20).

01 request-count PIC 99 VALUE 0.

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
    OPEN INPUT PROFILE-FILE
    IF profile-file-status = "35"
        PERFORM INITIALIZE-PROFILES
    ELSE
        PERFORM LOAD-PROFILES
    END-IF
    OPEN INPUT CONNECTION-FILE
    IF conn-file-status = "35"
        MOVE 0 TO connection-count
    ELSE
        PERFORM LOAD-CONNECTIONS
    END-IF
    OPEN INPUT REQUEST-FILE
    IF request-file-status = "35"
        MOVE 0 TO request-count
    ELSE
        PERFORM LOAD-REQUESTS
    END-IF

    PERFORM SETUP-SKILLS
    PERFORM WELCOME-SCREEN
    MOVE "N" TO programDoneFlag
    PERFORM MAIN-MENU UNTIL programDoneFlag = "Y"
    PERFORM SAVE-ACCOUNTS
    PERFORM SAVE-PROFILES
    PERFORM SAVE-CONNECTIONS
    PERFORM SAVE-REQUESTS
    CLOSE INPUT-FILE
    CLOSE OUTPUT-FILE
    CLOSE ACCOUNT-FILE
    CLOSE PROFILE-FILE
    CLOSE CONNECTION-FILE
    CLOSE REQUEST-FILE
    STOP RUN.

INITIALIZE-PROFILES.
    MOVE ALL SPACES TO user-profiles.

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

LOAD-PROFILES.
    PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > 5
        READ PROFILE-FILE
            AT END EXIT PERFORM
            NOT AT END
                MOVE PROF-REC-FILE TO user-profile(idx)
        END-READ
    END-PERFORM.

LOAD-CONNECTIONS.
    MOVE 0 TO connection-count
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > 25
        READ CONNECTION-FILE
            AT END EXIT PERFORM
            NOT AT END
                ADD 1 TO connection-count
                MOVE CONN-USER1-FILE  TO conn-user1(conn-idx)
                MOVE CONN-USER2-FILE  TO conn-user2(conn-idx)
                MOVE CONN-STATUS-FILE TO conn-status(conn-idx)
        END-READ
    END-PERFORM.

LOAD-REQUESTS.
    MOVE 0 TO request-count
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > 25
        READ REQUEST-FILE
            AT END EXIT PERFORM
            NOT AT END
                ADD 1 TO request-count
                MOVE REQ-SENDER-FILE   TO req-sender(conn-idx)
                MOVE REQ-RECEIVER-FILE TO req-receiver(conn-idx)
        END-READ
    END-PERFORM.

SAVE-ACCOUNTS.
    CLOSE ACCOUNT-FILE
    OPEN OUTPUT ACCOUNT-FILE
    PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > accountCount
        MOVE account-user(idx) TO ACC-REC-FILE(1:20)
        MOVE account-pass(idx) TO ACC-REC-FILE(21:20)
        WRITE ACC-REC-FILE
    END-PERFORM
    CLOSE ACCOUNT-FILE
    OPEN INPUT ACCOUNT-FILE.

SAVE-PROFILES.
    CLOSE PROFILE-FILE
    OPEN OUTPUT PROFILE-FILE
    PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > accountCount
        MOVE user-profile(idx) TO PROF-REC-FILE
        WRITE PROF-REC-FILE
    END-PERFORM
    CLOSE PROFILE-FILE
    OPEN INPUT PROFILE-FILE.

SAVE-CONNECTIONS.
    CLOSE CONNECTION-FILE
    OPEN OUTPUT CONNECTION-FILE
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > connection-count
        MOVE conn-user1(conn-idx) TO CONN-USER1-FILE
        MOVE conn-user2(conn-idx) TO CONN-USER2-FILE
        MOVE conn-status(conn-idx) TO CONN-STATUS-FILE
        WRITE CONN-REC-FILE
    END-PERFORM
    CLOSE CONNECTION-FILE
    OPEN INPUT CONNECTION-FILE.

SAVE-REQUESTS.
    CLOSE REQUEST-FILE
    OPEN OUTPUT REQUEST-FILE
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > request-count
        MOVE req-sender(conn-idx)   TO REQ-SENDER-FILE
        MOVE req-receiver(conn-idx) TO REQ-RECEIVER-FILE
        WRITE REQ-REC-FILE
    END-PERFORM
    CLOSE REQUEST-FILE
    OPEN INPUT REQUEST-FILE.

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

READ-INPUT-SAFELY.
    READ INPUT-FILE
        AT END
            MOVE "Y" TO EOF-INPUT-FILE
            MOVE "0" TO IN-REC
        NOT AT END
            MOVE IN-REC TO debug-input
    END-READ.

MAIN-MENU.
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION NUMVAL(IN-REC) TO mainChoice
    EVALUATE mainChoice
        WHEN 1
            PERFORM LOGIN
        WHEN 2
            PERFORM CREATE-ACCOUNT
        WHEN OTHER
            MOVE "Y" TO programDoneFlag
    END-EVALUATE

    IF programDoneFlag NOT = "Y"
        PERFORM WELCOME-SCREEN
    END-IF.

CREATE-ACCOUNT.
    IF accountCount >= 5
        MOVE "All permitted accounts have been created, please come back later" TO msgBuffer
        PERFORM DISPLAY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE "Enter new username:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE IN-REC TO userName
    MOVE "Enter new password:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE IN-REC TO userPass
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
    PERFORM DISPLAY-MSG
    PERFORM SAVE-ACCOUNTS.

LOGIN.
    MOVE "N" TO loginOk
    PERFORM UNTIL loginOk = "Y"
        MOVE "Please enter your username:" TO msgBuffer
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        MOVE IN-REC TO userName
        MOVE "Please enter your password:" TO msgBuffer
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        MOVE IN-REC TO userPass
        MOVE "N" TO foundFlag
        PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > accountCount
            MOVE FUNCTION TRIM(account-user(idx)) TO trimmedUser
            MOVE FUNCTION TRIM(account-pass(idx)) TO trimmedPass
            IF trimmedUser = FUNCTION TRIM(userName)
                AND trimmedPass = FUNCTION TRIM(userPass)
                MOVE "Y" TO foundFlag
                MOVE idx TO loggedInUser
            END-IF
        END-PERFORM
        IF foundFlag = "Y"
            MOVE "You have successfully logged in." TO msgBuffer
            PERFORM DISPLAY-MSG
            MOVE SPACES TO msgBuffer
            STRING "Welcome, " DELIMITED BY SIZE
                   FUNCTION TRIM(userName) DELIMITED BY SIZE
                   "!" DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG
            MOVE "Y" TO loginOk
            PERFORM POST-LOGIN-MENU
        ELSE
            MOVE "Incorrect username/password, please try again." TO msgBuffer
            PERFORM DISPLAY-MSG
        END-IF
    END-PERFORM.

POST-LOGIN-MENU.
    MOVE "N" TO postLoginDoneFlag
    PERFORM UNTIL postLoginDoneFlag = "Y"
        PERFORM CHECK-PENDING-REQUESTS

        MOVE "1. Create/Edit My Profile" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "2. View My Profile" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "3. Search for User" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "4. View My Pending Connection Requests" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "5. Learn a New Skill" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "6. View My Network" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "7. Go Back" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "8. Job Search/Internship" TO msgBuffer
        PERFORM DISPLAY-MSG

        PERFORM READ-INPUT-SAFELY
        MOVE FUNCTION NUMVAL(IN-REC) TO mainChoice

        EVALUATE mainChoice
            WHEN 1
                PERFORM CREATE-EDIT-PROFILE
            WHEN 2
                PERFORM VIEW-PROFILE
            WHEN 3
                PERFORM SEARCH-USER
            WHEN 4
                PERFORM VIEW-PENDING-REQUESTS
            WHEN 5
                PERFORM SKILL-MENU
            WHEN 6
                PERFORM VIEW-MY-NETWORK
            WHEN 7
                MOVE "Y" TO postLoginDoneFlag
            WHEN 8
                PERFORM JOB-MENU
        END-EVALUATE
    END-PERFORM.

CHECK-PENDING-REQUESTS.
    MOVE 0 TO pending-count
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > request-count
        IF FUNCTION TRIM(req-receiver(conn-idx)) = FUNCTION TRIM(account-user(loggedInUser))
            ADD 1 TO pending-count
        END-IF
    END-PERFORM

    IF pending-count > 0
        MOVE "========================================" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE SPACES TO msgBuffer
        STRING "You have " DELIMITED BY SIZE
               pending-count DELIMITED BY SIZE
               " pending connection request(s)" DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        MOVE "========================================" TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF.

VIEW-PENDING-REQUESTS.
    MOVE "--- My Pending Connection Requests ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE 0 TO pending-count
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > request-count
        IF req-receiver(conn-idx) = account-user(loggedInUser)
            ADD 1 TO pending-count
            MOVE SPACES TO msgBuffer
            STRING pending-count DELIMITED BY SIZE
                   ". From: " DELIMITED BY SIZE
                   FUNCTION TRIM(req-sender(conn-idx)) DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG
        END-IF
    END-PERFORM

    PERFORM PROCESS-PENDING-REQUESTS
    MOVE "--------------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG.

SKILL-MENU.
    MOVE "N" TO postLoginDoneFlag
    PERFORM UNTIL postLoginDoneFlag = "Y"
        MOVE "Learn a New Skill:" TO msgBuffer
        PERFORM DISPLAY-MSG
        PERFORM VARYING subChoice FROM 1 BY 1 UNTIL subChoice > 5
            MOVE skillName(subChoice) TO msgBuffer
            PERFORM DISPLAY-MSG
        END-PERFORM
        MOVE "7. Go Back" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "Enter your choice:" TO msgBuffer
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        MOVE FUNCTION NUMVAL(IN-REC) TO subChoice
        IF subChoice >= 1 AND subChoice <= 5
            MOVE "This skill is under construction." TO msgBuffer
            PERFORM DISPLAY-MSG
        ELSE
            MOVE "Y" TO postLoginDoneFlag
        END-IF
    END-PERFORM.

JOB-MENU.
    MOVE "N" TO jobMenuDoneFlag
    PERFORM UNTIL jobMenuDoneFlag = "Y"
        MOVE "--- Job Search/Internship Menu ---" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "1. Post a Job/Internship" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "2. Browse Jobs/Internships" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "3. Back to Main Menu" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "Enter your choice:" TO msgBuffer
        PERFORM DISPLAY-MSG

        PERFORM READ-INPUT-SAFELY
        MOVE FUNCTION NUMVAL(IN-REC) TO ws-display-idx

        EVALUATE ws-display-idx
            WHEN 1
                PERFORM POST-JOB
            WHEN 2
                MOVE "This feature is under construction." TO msgBuffer
                PERFORM DISPLAY-MSG
            WHEN 3
                MOVE "Y" TO jobMenuDoneFlag
            WHEN OTHER
                MOVE "Invalid choice." TO msgBuffer
                PERFORM DISPLAY-MSG
        END-EVALUATE
    END-PERFORM.


POST-JOB.
    MOVE "--- Post a New Job/Internship ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    *> Collect required fields exactly once
    MOVE "Enter Job Title:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO job-title

    MOVE "Enter Description (max 200 chars):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO job-desc

    MOVE "Enter Employer Name:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO job-employer

    MOVE "Enter Location:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO job-location

    *> Validate required fields BEFORE asking for salary
    IF FUNCTION TRIM(job-title)    = SPACES
       OR FUNCTION TRIM(job-desc)  = SPACES
       OR FUNCTION TRIM(job-employer) = SPACES
       OR FUNCTION TRIM(job-location) = SPACES
        MOVE "Error: All required fields must be filled in." TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "Job not posted." TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "----------------------------------" TO msgBuffer
        PERFORM DISPLAY-MSG
        EXIT PARAGRAPH
    END-IF

    *> Salary optional
    MOVE "Enter Salary (optional, enter 'NONE' to skip):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO job-salary
    IF FUNCTION TRIM(job-salary) = "NONE"
        MOVE SPACES TO job-salary
    END-IF

    *> Persist only when validation passed
    OPEN EXTEND JOBS-FILE
    IF jobs-file-status = "35"
        OPEN OUTPUT JOBS-FILE
        CLOSE JOBS-FILE
        OPEN EXTEND JOBS-FILE
    END-IF

    MOVE SPACES TO JOB-REC-FILE
    STRING
        FUNCTION TRIM(job-title)    DELIMITED BY SIZE
        "|"                          DELIMITED BY SIZE
        FUNCTION TRIM(job-desc)     DELIMITED BY SIZE
        "|"                          DELIMITED BY SIZE
        FUNCTION TRIM(job-employer) DELIMITED BY SIZE
        "|"                          DELIMITED BY SIZE
        FUNCTION TRIM(job-location) DELIMITED BY SIZE
        "|"                          DELIMITED BY SIZE
        FUNCTION TRIM(job-salary)   DELIMITED BY SIZE
        INTO JOB-REC-FILE
    END-STRING
    WRITE JOB-REC-FILE
    CLOSE JOBS-FILE

    MOVE "Job posted successfully!" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE "----------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG.


DISPLAY-MSG.
    DISPLAY msgBuffer
    MOVE msgBuffer TO OUT-REC
    WRITE OUT-REC.

CREATE-EDIT-PROFILE.
    MOVE "--- Create/Edit Profile ---" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE "Enter First Name:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO first-name(loggedInUser)
    MOVE "Enter Last Name:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO last-name(loggedInUser)
    MOVE "Enter University/College Attended:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO university(loggedInUser)
    MOVE "Enter Major:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO major(loggedInUser)
    MOVE "Enter Graduation Year (YYYY):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO trimmed-input
    MOVE trimmed-input(1:4) TO short-trimmed
    IF short-trimmed IS NUMERIC AND FUNCTION LENGTH(FUNCTION TRIM(short-trimmed)) = 4
        MOVE FUNCTION NUMVAL(short-trimmed) TO graduation-year(loggedInUser)
    ELSE
        MOVE "Invalid year entered." TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE 0 TO graduation-year(loggedInUser)
    END-IF
    MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE FUNCTION TRIM(IN-REC) TO about-me(loggedInUser)
    MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM VARYING exp-idx FROM 1 BY 1 UNTIL exp-idx > 3
        MOVE SPACES TO msgBuffer
        STRING "Experience #" DELIMITED BY SIZE
               exp-idx DELIMITED BY SIZE
               " - Title:" DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        IF FUNCTION TRIM(IN-REC) = "DONE"
            MOVE SPACES TO exp-title(loggedInUser, exp-idx)
            EXIT PERFORM
        END-IF
        MOVE FUNCTION TRIM(IN-REC) TO exp-title(loggedInUser, exp-idx)
        MOVE SPACES TO msgBuffer
        STRING "Experience #" DELIMITED BY SIZE
               exp-idx DELIMITED BY SIZE
               " - Company/Organization:" DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        MOVE FUNCTION TRIM(IN-REC) TO exp-company(loggedInUser, exp-idx)
        MOVE SPACES TO msgBuffer
        STRING "Experience #" DELIMITED BY SIZE
               exp-idx DELIMITED BY SIZE
               " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        MOVE FUNCTION TRIM(IN-REC) TO exp-dates(loggedInUser, exp-idx)
        MOVE SPACES TO msgBuffer
        STRING "Experience #" DELIMITED BY SIZE
               exp-idx DELIMITED BY SIZE
               " - Description (optional, max 100 chars, blank to skip):" DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        MOVE FUNCTION TRIM(IN-REC) TO exp-desc(loggedInUser, exp-idx)
    END-PERFORM
    MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM VARYING edu-idx FROM 1 BY 1 UNTIL edu-idx > 3
        MOVE SPACES TO msgBuffer
        STRING "Education #" DELIMITED BY SIZE
               edu-idx DELIMITED BY SIZE
               " - Degree:" DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        IF FUNCTION TRIM(IN-REC) = "DONE"
            MOVE SPACES TO edu-degree(loggedInUser, edu-idx)
            EXIT PERFORM
        END-IF
        MOVE FUNCTION TRIM(IN-REC) TO edu-degree(loggedInUser, edu-idx)
        MOVE SPACES TO msgBuffer
        STRING "Education #" DELIMITED BY SIZE
               edu-idx DELIMITED BY SIZE
               " - University/College:" DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        MOVE FUNCTION TRIM(IN-REC) TO edu-university(loggedInUser, edu-idx)
        MOVE SPACES TO msgBuffer
        STRING "Education #" DELIMITED BY SIZE
               edu-idx DELIMITED BY SIZE
               " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        MOVE FUNCTION TRIM(IN-REC) TO edu-years(loggedInUser, edu-idx)
    END-PERFORM
    MOVE "Profile saved successfully!" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM SAVE-PROFILES.

VIEW-PROFILE.
    MOVE "--- Your Profile ---" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE loggedInUser TO ws-display-idx
    PERFORM DISPLAY-GENERIC-PROFILE.

SEARCH-USER.
    MOVE "--- Find Someone You Know ---" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE "Enter the full name of the person you are looking for:"
        TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    MOVE IN-REC TO ws-search-name
    MOVE "N" TO search-found-flag
    MOVE 0 TO ws-display-idx
    PERFORM VARYING search-idx FROM 1 BY 1
        UNTIL search-idx > accountCount OR search-found-flag = "Y"
        MOVE SPACES TO ws-full-name
        STRING FUNCTION TRIM(first-name(search-idx)) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(last-name(search-idx)) DELIMITED BY SIZE
               INTO ws-full-name
        END-STRING
        IF FUNCTION TRIM(ws-search-name) = FUNCTION TRIM(ws-full-name)
            MOVE "Y" TO search-found-flag
            MOVE search-idx TO ws-display-idx
        END-IF
    END-PERFORM
    IF search-found-flag = "Y"
        PERFORM DISPLAY-GENERIC-PROFILE

        IF ws-display-idx NOT = loggedInUser
            PERFORM OFFER-CONNECTION-REQUEST
        END-IF
    ELSE
        MOVE "No one by that name could be found." TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF.

OFFER-CONNECTION-REQUEST.
    MOVE "Would you like to send a connection request? (Y/N):" TO msgBuffer
    PERFORM DISPLAY-MSG

    PERFORM READ-INPUT-SAFELY
    IF FUNCTION UPPER-CASE(FUNCTION TRIM(IN-REC)) = "Y"
        PERFORM SEND-CONNECTION-REQUEST
    END-IF.

SEND-CONNECTION-REQUEST.
    MOVE "Y" TO can-send-request

    *> Check in requests file for pending requests
    PERFORM VARYING conn-check-idx FROM 1 BY 1
        UNTIL conn-check-idx > request-count

        *> Check if already sent request
        IF req-sender(conn-check-idx) = account-user(loggedInUser) AND
           req-receiver(conn-check-idx) = account-user(ws-display-idx)
            MOVE "You have already sent a connection request to this user." TO msgBuffer
            PERFORM DISPLAY-MSG
            MOVE "N" TO can-send-request
            EXIT PERFORM
        END-IF

        *> Check if other user already sent request
        IF req-sender(conn-check-idx) = account-user(ws-display-idx) AND
           req-receiver(conn-check-idx) = account-user(loggedInUser)
            MOVE "This user has already sent you a connection request." TO msgBuffer
            PERFORM DISPLAY-MSG
            MOVE "N" TO can-send-request
            EXIT PERFORM
        END-IF
    END-PERFORM

    *> Check in connections file for accepted connections
    IF can-send-request = "Y"
        PERFORM VARYING conn-check-idx FROM 1 BY 1
            UNTIL conn-check-idx > connection-count

            IF (conn-user1(conn-check-idx) = account-user(loggedInUser) AND
                conn-user2(conn-check-idx) = account-user(ws-display-idx) AND
                conn-status(conn-check-idx) = "A") OR
               (conn-user1(conn-check-idx) = account-user(ws-display-idx) AND
                conn-user2(conn-check-idx) = account-user(loggedInUser) AND
                conn-status(conn-check-idx) = "A")
                MOVE "You are already connected with this user." TO msgBuffer
                PERFORM DISPLAY-MSG
                MOVE "N" TO can-send-request
                EXIT PERFORM
            END-IF
        END-PERFORM
    END-IF

    IF can-send-request = "Y"
        IF request-count < 25
            ADD 1 TO request-count
            MOVE account-user(loggedInUser) TO req-sender(request-count)
            MOVE account-user(ws-display-idx) TO req-receiver(request-count)

            MOVE "Connection request sent successfully!" TO msgBuffer
            PERFORM DISPLAY-MSG
            PERFORM SAVE-REQUESTS
        ELSE
            MOVE "Request limit reached. Cannot send more requests." TO msgBuffer
            PERFORM DISPLAY-MSG
        END-IF
    END-IF.

DISPLAY-GENERIC-PROFILE.
    IF FUNCTION TRIM(first-name(ws-display-idx)) NOT = SPACES
        MOVE SPACES TO msgBuffer
        STRING "Name: " DELIMITED BY SIZE
               FUNCTION TRIM(first-name(ws-display-idx)) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(last-name(ws-display-idx)) DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
    END-IF
    IF FUNCTION TRIM(university(ws-display-idx)) NOT = SPACES
        MOVE SPACES TO msgBuffer
        STRING "University: " DELIMITED BY SIZE
               FUNCTION TRIM(university(ws-display-idx)) DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
    END-IF
    IF FUNCTION TRIM(major(ws-display-idx)) NOT = SPACES
        MOVE SPACES TO msgBuffer
        STRING "Major: " DELIMITED BY SIZE
               FUNCTION TRIM(major(ws-display-idx)) DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
    END-IF
    IF graduation-year(ws-display-idx) > 0
        MOVE graduation-year(ws-display-idx) TO graduation-year-str
        MOVE SPACES TO msgBuffer
        STRING "Graduation Year: " DELIMITED BY SIZE
               graduation-year-str DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
    END-IF
    MOVE "About Me:" TO msgBuffer
    PERFORM DISPLAY-MSG
    IF FUNCTION TRIM(about-me(ws-display-idx)) NOT = SPACES
        MOVE about-me(ws-display-idx) TO msgBuffer
        PERFORM DISPLAY-MSG
    ELSE
        MOVE "(Not provided)" TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF
    MOVE "Experience:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM VARYING exp-idx FROM 1 BY 1 UNTIL exp-idx > 3
        IF FUNCTION TRIM(exp-title(ws-display-idx, exp-idx)) NOT = SPACES
            MOVE SPACES TO msgBuffer
            STRING "  Title: " DELIMITED BY SIZE
                   FUNCTION TRIM(exp-title(ws-display-idx, exp-idx)) DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG
            IF FUNCTION TRIM(exp-company(ws-display-idx, exp-idx)) NOT = SPACES
                MOVE SPACES TO msgBuffer
                STRING "  Company: " DELIMITED BY SIZE
                       FUNCTION TRIM(exp-company(ws-display-idx, exp-idx)) DELIMITED BY SIZE
                       INTO msgBuffer
                END-STRING
                PERFORM DISPLAY-MSG
            END-IF
            IF FUNCTION TRIM(exp-dates(ws-display-idx, exp-idx)) NOT = SPACES
                MOVE SPACES TO msgBuffer
                STRING "  Dates: " DELIMITED BY SIZE
                       FUNCTION TRIM(exp-dates(ws-display-idx, exp-idx)) DELIMITED BY SIZE
                       INTO msgBuffer
                END-STRING
                PERFORM DISPLAY-MSG
            END-IF
            IF FUNCTION TRIM(exp-desc(ws-display-idx, exp-idx)) NOT = SPACES
                MOVE SPACES TO msgBuffer
                STRING "  Description: " DELIMITED BY SIZE
                       FUNCTION TRIM(exp-desc(ws-display-idx, exp-idx)) DELIMITED BY SIZE
                       INTO msgBuffer
                END-STRING
                PERFORM DISPLAY-MSG
            END-IF

            MOVE " " TO msgBuffer
            PERFORM DISPLAY-MSG
        END-IF
    END-PERFORM
    MOVE "Education:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM VARYING edu-idx FROM 1 BY 1 UNTIL edu-idx > 3
        IF FUNCTION TRIM(edu-degree(ws-display-idx, edu-idx)) NOT = SPACES
            MOVE SPACES TO msgBuffer
            STRING "  Degree: " DELIMITED BY SIZE
                   FUNCTION TRIM(edu-degree(ws-display-idx, edu-idx)) DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG
            IF FUNCTION TRIM(edu-university(ws-display-idx, edu-idx)) NOT = SPACES
                MOVE SPACES TO msgBuffer
                STRING "  University: " DELIMITED BY SIZE
                       FUNCTION TRIM(edu-university(ws-display-idx, edu-idx)) DELIMITED BY SIZE
                       INTO msgBuffer
                END-STRING
                PERFORM DISPLAY-MSG
            END-IF
            IF FUNCTION TRIM(edu-years(ws-display-idx, edu-idx)) NOT = SPACES
                MOVE SPACES TO msgBuffer
                STRING "  Years: " DELIMITED BY SIZE
                       FUNCTION TRIM(edu-years(ws-display-idx, edu-idx)) DELIMITED BY SIZE
                       INTO msgBuffer
                END-STRING
                PERFORM DISPLAY-MSG
            END-IF

            MOVE " " TO msgBuffer
            PERFORM DISPLAY-MSG
        END-IF
    END-PERFORM

    MOVE "------------------" TO msgBuffer
    PERFORM DISPLAY-MSG.

VIEW-MY-NETWORK.
      MOVE "--- Your Network ---" TO msgBuffer
      PERFORM DISPLAY-MSG

      MOVE 0 TO ws-display-idx
      MOVE 0 TO pending-count

      PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx> connection-count
          IF conn-status(conn-idx) = "A"
              MOVE SPACES TO TARGET-USER
              IF FUNCTION TRIM(conn-user1(conn-idx)) = FUNCTION TRIM(account-user(loggedInUser))
                MOVE conn-user2(conn-idx) TO TARGET-USER
            ELSE
                IF FUNCTION TRIM(conn-user2(conn-idx)) = FUNCTION TRIM(account-user(loggedInUser))
                    MOVE conn-user1(conn-idx) TO TARGET-USER
                END-IF
            END-IF

            IF TARGET-USER NOT = SPACES
                PERFORM FETCH-USER-NAME
                ADD 1 TO pending-count
                MOVE SPACES TO msgBuffer
                STRING "Connected with: " DELIMITED BY SIZE
                       FUNCTION TRIM(USER-FULLNAME) DELIMITED BY SIZE
                       INTO msgBuffer
                END-STRING
                PERFORM DISPLAY-MSG
            END-IF
        END-IF
    END-PERFORM

    IF pending-count = 0
        MOVE "You currently have no established connections." TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF

    MOVE "--------------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG.

FETCH-USER-NAME.
    MOVE SPACES TO USER-FULLNAME
    PERFORM VARYING ws-display-idx FROM 1 BY 1 UNTIL ws-display-idx > accountCount
        IF FUNCTION TRIM(account-user(ws-display-idx)) = FUNCTION TRIM(TARGET-USER)
            MOVE SPACES TO msgBuffer
            STRING
                FUNCTION TRIM(first-name(ws-display-idx)) DELIMITED BY SIZE
                " " DELIMITED BY SIZE
                FUNCTION TRIM(last-name(ws-display-idx)) DELIMITED BY SIZE
                " (University: " DELIMITED BY SIZE
                FUNCTION TRIM(university(ws-display-idx)) DELIMITED BY SIZE
                ", Major: " DELIMITED BY SIZE
                FUNCTION TRIM(major(ws-display-idx)) DELIMITED BY SIZE
                ")" DELIMITED BY SIZE
                INTO USER-FULLNAME
            END-STRING
            EXIT PERFORM
        END-IF
    END-PERFORM.

PROCESS-PENDING-REQUESTS.
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > request-count
        IF FUNCTION TRIM(req-receiver(conn-idx)) = FUNCTION TRIM(account-user(loggedInUser))
            MOVE SPACES TO msgBuffer
            STRING "Request from: " DELIMITED BY SIZE
                   FUNCTION TRIM(req-sender(conn-idx)) DELIMITED BY SIZE
                   " | 1=Accept  2=Reject  3=Skip" DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG

            PERFORM READ-INPUT-SAFELY
            MOVE FUNCTION TRIM(IN-REC) TO trimmed-input
            EVALUATE trimmed-input
                WHEN "1"
                    PERFORM ACCEPT-THIS-REQUEST
                WHEN "2"
                    PERFORM REJECT-THIS-REQUEST
                WHEN OTHER
                    CONTINUE
            END-EVALUATE
        END-IF
    END-PERFORM

    *> Persist updates
    PERFORM SAVE-REQUESTS
    PERFORM SAVE-CONNECTIONS.

ACCEPT-THIS-REQUEST.
    MOVE "N" TO foundFlag
    PERFORM VARYING conn-check-idx FROM 1 BY 1 UNTIL conn-check-idx > connection-count OR foundFlag = "Y"
        IF (FUNCTION TRIM(conn-user1(conn-check-idx)) = FUNCTION TRIM(account-user(loggedInUser)) AND
            FUNCTION TRIM(conn-user2(conn-check-idx)) = FUNCTION TRIM(req-sender(conn-idx))) OR
           (FUNCTION TRIM(conn-user2(conn-check-idx)) = FUNCTION TRIM(account-user(loggedInUser)) AND
            FUNCTION TRIM(conn-user1(conn-check-idx)) = FUNCTION TRIM(req-sender(conn-idx)))
            MOVE "Y" TO foundFlag
        END-IF
    END-PERFORM

    IF foundFlag = "N"
        IF connection-count < 25
            ADD 1 TO connection-count
            MOVE account-user(loggedInUser) TO conn-user1(connection-count)
            MOVE req-sender(conn-idx)       TO conn-user2(connection-count)
            MOVE "A"                        TO conn-status(connection-count)
        END-IF
    END-IF

    MOVE SPACES TO msgBuffer
    STRING "Connection request from " DELIMITED BY SIZE
           FUNCTION TRIM(req-sender(conn-idx)) DELIMITED BY SIZE
           " accepted!" DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    PERFORM DELETE-REQUEST-AT-IDX.

REJECT-THIS-REQUEST.
    MOVE SPACES TO msgBuffer
    STRING "Connection request from " DELIMITED BY SIZE
           FUNCTION TRIM(req-sender(conn-idx)) DELIMITED BY SIZE
           " rejected." DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG
    PERFORM DELETE-REQUEST-AT-IDX.

DELETE-REQUEST-AT-IDX.
    PERFORM VARYING ws-display-idx FROM conn-idx BY 1 UNTIL ws-display-idx >= request-count
        ADD 1 TO temp-year
        MOVE req-sender(ws-display-idx + 1)   TO req-sender(ws-display-idx)
        MOVE req-receiver(ws-display-idx + 1) TO req-receiver(ws-display-idx)
    END-PERFORM
    SUBTRACT 1 FROM request-count
    SUBTRACT 1 FROM conn-idx.
    