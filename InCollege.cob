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
    SELECT APPLICATIONS-FILE ASSIGN TO "InCollege-Applications.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS apps-file-status.

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

FD APPLICATIONS-FILE.
01 APP-REC-FILE.
    05 APP-USER-FILE    PIC X(20).
    05 APP-JOB-TITLE    PIC X(50).
    05 APP-EMPLOYER     PIC X(50).
    05 APP-LOCATION     PIC X(50).

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
01 apps-file-status PIC XX.
01 jobMenuDoneFlag PIC X.
01 job-title     PIC X(50).
01 job-desc      PIC X(200).
01 job-employer  PIC X(50).
01 job-location  PIC X(50).
01 job-salary    PIC X(30).

01 job-count PIC 99 VALUE 0.
01 job-idx PIC 99.
01 job-selection PIC 99.
01 job-details-flag PIC X.
01 job-apply-flag PIC X.

01 application-count PIC 99 VALUE 0.
01 app-idx PIC 99.
01 app-selection PIC 99.

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

01 job-data.
    05 job-record OCCURS 10 TIMES.
        10 job-title-record    PIC X(50).
        10 job-desc-record     PIC X(200).
        10 job-employer-record PIC X(50).
        10 job-location-record PIC X(50).
        10 job-salary-record   PIC X(30).

01 application-data.
    05 application-record OCCURS 25 TIMES.
        10 app-user-record    PIC X(20).
        10 app-job-title-record PIC X(50).
        10 app-employer-record  PIC X(50).
        10 app-location-record  PIC X(50).

01 input-status PIC XX.
01 temp-input PIC X(201).

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
    OPEN INPUT JOBS-FILE
    IF jobs-file-status = "35"
        MOVE 0 TO job-count
    ELSE
        PERFORM LOAD-JOBS
    END-IF
    OPEN INPUT APPLICATIONS-FILE
    IF apps-file-status = "35"
        MOVE 0 TO application-count
    ELSE
        PERFORM LOAD-APPLICATIONS
    END-IF

    PERFORM SETUP-SKILLS
    PERFORM WELCOME-SCREEN
    MOVE "N" TO programDoneFlag
    PERFORM MAIN-MENU UNTIL programDoneFlag = "Y"
    PERFORM SAVE-ACCOUNTS
    PERFORM SAVE-PROFILES
    PERFORM SAVE-CONNECTIONS
    PERFORM SAVE-REQUESTS
    PERFORM SAVE-JOBS
    PERFORM SAVE-APPLICATIONS
    CLOSE INPUT-FILE
    CLOSE OUTPUT-FILE
    CLOSE ACCOUNT-FILE
    CLOSE PROFILE-FILE
    CLOSE CONNECTION-FILE
    CLOSE REQUEST-FILE
    CLOSE JOBS-FILE
    CLOSE APPLICATIONS-FILE
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

LOAD-JOBS.
    MOVE 0 TO job-count
    PERFORM VARYING job-idx FROM 1 BY 1 UNTIL job-idx > 10
        READ JOBS-FILE
            AT END EXIT PERFORM
            NOT AT END
                ADD 1 TO job-count
                MOVE JOB-REC-FILE TO job-record(job-idx)
        END-READ
    END-PERFORM.

LOAD-APPLICATIONS.
    MOVE 0 TO application-count
    PERFORM VARYING app-idx FROM 1 BY 1 UNTIL app-idx > 25
        READ APPLICATIONS-FILE
            AT END EXIT PERFORM
            NOT AT END
                ADD 1 TO application-count
                MOVE APP-USER-FILE TO app-user-record(app-idx)
                MOVE APP-JOB-TITLE TO app-job-title-record(app-idx)
                MOVE APP-EMPLOYER TO app-employer-record(app-idx)
                MOVE APP-LOCATION TO app-location-record(app-idx)
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

SAVE-JOBS.
    CLOSE JOBS-FILE
    OPEN OUTPUT JOBS-FILE
    PERFORM VARYING job-idx FROM 1 BY 1 UNTIL job-idx > job-count
        MOVE job-record(job-idx) TO JOB-REC-FILE
        WRITE JOB-REC-FILE
    END-PERFORM
    CLOSE JOBS-FILE
    OPEN INPUT JOBS-FILE.

SAVE-APPLICATIONS.
    CLOSE APPLICATIONS-FILE
    OPEN OUTPUT APPLICATIONS-FILE
    PERFORM VARYING app-idx FROM 1 BY 1 UNTIL app-idx > application-count
        MOVE app-user-record(app-idx) TO APP-USER-FILE
        MOVE app-job-title-record(app-idx) TO APP-JOB-TITLE
        MOVE app-employer-record(app-idx) TO APP-EMPLOYER
        MOVE app-location-record(app-idx) TO APP-LOCATION
        WRITE APP-REC-FILE
    END-PERFORM
    CLOSE APPLICATIONS-FILE
    OPEN INPUT APPLICATIONS-FILE.

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
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    READ INPUT-FILE
        AT END
            MOVE "Y" TO EOF-INPUT-FILE
            MOVE SPACES TO IN-REC
        NOT AT END
            CONTINUE
    END-READ.

MAIN-MENU.
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        MOVE "Y" TO programDoneFlag
    ELSE
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
        END-IF
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
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE IN-REC TO userName
    MOVE "Enter new password:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
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
        IF EOF-INPUT-FILE = "Y"
            EXIT PERFORM
        END-IF
        MOVE IN-REC TO userName
        MOVE "Please enter your password:" TO msgBuffer
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        IF EOF-INPUT-FILE = "Y"
            EXIT PERFORM
        END-IF
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
        IF EOF-INPUT-FILE = "Y"
            MOVE "Y" TO postLoginDoneFlag
        ELSE
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
        END-IF
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
        IF EOF-INPUT-FILE = "Y"
            MOVE "Y" TO postLoginDoneFlag
        ELSE
            MOVE FUNCTION NUMVAL(IN-REC) TO subChoice
            IF subChoice >= 1 AND subChoice <= 5
                MOVE "This skill is under construction." TO msgBuffer
                PERFORM DISPLAY-MSG
            ELSE
                MOVE "Y" TO postLoginDoneFlag
            END-IF
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
        MOVE "3. View My Applications" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "4. Back to Main Menu" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "Enter your choice:" TO msgBuffer
        PERFORM DISPLAY-MSG

        PERFORM READ-INPUT-SAFELY
        IF EOF-INPUT-FILE = "Y"
            MOVE "Y" TO jobMenuDoneFlag
        ELSE
            MOVE FUNCTION NUMVAL(IN-REC) TO ws-display-idx

            EVALUATE ws-display-idx
                WHEN 1
                    PERFORM POST-JOB
                WHEN 2
                    PERFORM BROWSE-JOBS
                WHEN 3
                    PERFORM VIEW-MY-APPLICATIONS
                WHEN 4
                    MOVE "Y" TO jobMenuDoneFlag
                WHEN OTHER
                    MOVE "Invalid choice." TO msgBuffer
                    PERFORM DISPLAY-MSG
            END-EVALUATE
        END-IF
    END-PERFORM.

POST-JOB.
    MOVE "--- Post a New Job/Internship ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    *> Collect required fields exactly once
    MOVE "Enter Job Title:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE FUNCTION TRIM(IN-REC) TO job-title

    MOVE "Enter Description (max 200 chars):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE FUNCTION TRIM(IN-REC) TO job-desc

    MOVE "Enter Employer Name:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE FUNCTION TRIM(IN-REC) TO job-employer

    MOVE "Enter Location:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
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
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE FUNCTION TRIM(IN-REC) TO job-salary
    IF FUNCTION TRIM(job-salary) = "NONE"
        MOVE SPACES TO job-salary
    END-IF

    *> Add job to in-memory array
    IF job-count < 10
        ADD 1 TO job-count
        MOVE job-title TO job-title-record(job-count)
        MOVE job-desc TO job-desc-record(job-count)
        MOVE job-employer TO job-employer-record(job-count)
        MOVE job-location TO job-location-record(job-count)
        MOVE job-salary TO job-salary-record(job-count)
    ELSE
        MOVE "Maximum job limit reached." TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "----------------------------------" TO msgBuffer
        PERFORM DISPLAY-MSG
        EXIT PARAGRAPH
    END-IF

    *> Persist to file
    PERFORM SAVE-JOBS

    MOVE "Job posted successfully!" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE "----------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG.

BROWSE-JOBS.
    MOVE "--- Available Job Listings ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    IF job-count = 0
        MOVE "No job listings available." TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "----------------------------------" TO msgBuffer
        PERFORM DISPLAY-MSG
        EXIT PARAGRAPH
    END-IF

    *> Display job summaries
    PERFORM VARYING job-idx FROM 1 BY 1 UNTIL job-idx > job-count
        MOVE SPACES TO msgBuffer
        STRING
            job-idx DELIMITED BY SIZE
            ". " DELIMITED BY SIZE
            FUNCTION TRIM(job-title-record(job-idx)) DELIMITED BY SIZE
            " at " DELIMITED BY SIZE
            FUNCTION TRIM(job-employer-record(job-idx)) DELIMITED BY SIZE
            " (" DELIMITED BY SIZE
            FUNCTION TRIM(job-location-record(job-idx)) DELIMITED BY SIZE
            ")" DELIMITED BY SIZE
            INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
    END-PERFORM

    MOVE "----------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE "Enter job number to view details, or 0 to go back:" TO msgBuffer
    PERFORM DISPLAY-MSG

    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE FUNCTION NUMVAL(IN-REC) TO job-selection

    IF job-selection = 0
        MOVE "Returning to job menu." TO msgBuffer
        PERFORM DISPLAY-MSG
    ELSE IF job-selection > 0 AND job-selection <= job-count
        PERFORM DISPLAY-JOB-DETAILS
    ELSE
        MOVE "Error: Job number does not exist. Please select a valid job number." TO msgBuffer
        PERFORM DISPLAY-MSG
        PERFORM BROWSE-JOBS
    END-IF.

DISPLAY-JOB-DETAILS.
    MOVE "--- Job Details ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "Title: " DELIMITED BY SIZE
           FUNCTION TRIM(job-title-record(job-selection)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "Description: " DELIMITED BY SIZE
           FUNCTION TRIM(job-desc-record(job-selection)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "Employer: " DELIMITED BY SIZE
           FUNCTION TRIM(job-employer-record(job-selection)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "Location: " DELIMITED BY SIZE
           FUNCTION TRIM(job-location-record(job-selection)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    IF FUNCTION TRIM(job-salary-record(job-selection)) NOT = SPACES
        MOVE SPACES TO msgBuffer
        STRING "Salary: " DELIMITED BY SIZE
               FUNCTION TRIM(job-salary-record(job-selection)) DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
    END-IF

    MOVE "----------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE "1. Apply for this Job" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE "2. Back to Job List" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE "Enter your choice:" TO msgBuffer
    PERFORM DISPLAY-MSG

    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE FUNCTION NUMVAL(IN-REC) TO subChoice

    EVALUATE subChoice
        WHEN 1
            PERFORM APPLY-FOR-JOB
        WHEN 2
            PERFORM BROWSE-JOBS
        WHEN OTHER
            MOVE "Invalid choice, returning to job list." TO msgBuffer
            PERFORM DISPLAY-MSG
            PERFORM BROWSE-JOBS
    END-EVALUATE.

APPLY-FOR-JOB.
    *> Check if user already applied for this job
    MOVE "N" TO foundFlag
    PERFORM VARYING app-idx FROM 1 BY 1 UNTIL app-idx > application-count
        IF app-user-record(app-idx) = account-user(loggedInUser) AND
           app-job-title-record(app-idx) = job-title-record(job-selection) AND
           app-employer-record(app-idx) = job-employer-record(job-selection)
            MOVE "Y" TO foundFlag
        END-IF
    END-PERFORM

    IF foundFlag = "Y"
        MOVE "You have already applied for this job." TO msgBuffer
        PERFORM DISPLAY-MSG
    ELSE
        *> Add application
        IF application-count < 25
            ADD 1 TO application-count
            MOVE account-user(loggedInUser) TO app-user-record(application-count)
            MOVE job-title-record(job-selection) TO app-job-title-record(application-count)
            MOVE job-employer-record(job-selection) TO app-employer-record(application-count)
            MOVE job-location-record(job-selection) TO app-location-record(application-count)

            *> Persist to file
            PERFORM SAVE-APPLICATIONS

            MOVE SPACES TO msgBuffer
            STRING "Your application for " DELIMITED BY SIZE
                   FUNCTION TRIM(job-title-record(job-selection)) DELIMITED BY SIZE
                   " at " DELIMITED BY SIZE
                   FUNCTION TRIM(job-employer-record(job-selection)) DELIMITED BY SIZE
                   " has been submitted." DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG
        ELSE
            MOVE "Application limit reached. Cannot apply for more jobs." TO msgBuffer
            PERFORM DISPLAY-MSG
        END-IF
    END-IF.

VIEW-MY-APPLICATIONS.
    MOVE "--- Your Job Applications ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "Application Summary for " DELIMITED BY SIZE
           FUNCTION TRIM(account-user(loggedInUser)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE "----------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE 0 TO pending-count
    PERFORM VARYING app-idx FROM 1 BY 1 UNTIL app-idx > application-count
        IF app-user-record(app-idx) = account-user(loggedInUser)
            ADD 1 TO pending-count

            MOVE SPACES TO msgBuffer
            STRING "Job Title: " DELIMITED BY SIZE
                   FUNCTION TRIM(app-job-title-record(app-idx)) DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG

            MOVE SPACES TO msgBuffer
            STRING "Employer: " DELIMITED BY SIZE
                   FUNCTION TRIM(app-employer-record(app-idx)) DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG

            MOVE SPACES TO msgBuffer
            STRING "Location: " DELIMITED BY SIZE
                   FUNCTION TRIM(app-location-record(app-idx)) DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG

            MOVE "----------------------------------" TO msgBuffer
            PERFORM DISPLAY-MSG
        END-IF
    END-PERFORM

    IF pending-count = 0
        MOVE "You have not applied for any jobs yet." TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "----------------------------------" TO msgBuffer
        PERFORM DISPLAY-MSG
    ELSE
        MOVE SPACES TO msgBuffer
        STRING "Total applications: " DELIMITED BY SIZE
               pending-count DELIMITED BY SIZE
               INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        MOVE "----------------------------------" TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF.

CREATE-EDIT-PROFILE.
    MOVE "--- Create/Edit Profile ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE "Enter First Name:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE IN-REC TO first-name(loggedInUser)

    MOVE "Enter Last Name:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE IN-REC TO last-name(loggedInUser)

    MOVE "Enter University:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE IN-REC TO university(loggedInUser)

    MOVE "Enter Major:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE IN-REC TO major(loggedInUser)

    MOVE "Enter Graduation Year (YYYY):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE IN-REC TO graduation-year-str
    MOVE FUNCTION NUMVAL(graduation-year-str) TO graduation-year(loggedInUser)

    MOVE "Enter About Me (max 200 chars):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE IN-REC TO about-me(loggedInUser)

    MOVE "Profile updated successfully!" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM SAVE-PROFILES.

VIEW-PROFILE.
    MOVE "--- My Profile ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "Name: " DELIMITED BY SIZE
           FUNCTION TRIM(first-name(loggedInUser)) DELIMITED BY SIZE
           " " DELIMITED BY SIZE
           FUNCTION TRIM(last-name(loggedInUser)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "University: " DELIMITED BY SIZE
           FUNCTION TRIM(university(loggedInUser)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "Major: " DELIMITED BY SIZE
           FUNCTION TRIM(major(loggedInUser)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "Graduation Year: " DELIMITED BY SIZE
           graduation-year(loggedInUser) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE SPACES TO msgBuffer
    STRING "About Me: " DELIMITED BY SIZE
           FUNCTION TRIM(about-me(loggedInUser)) DELIMITED BY SIZE
           INTO msgBuffer
    END-STRING
    PERFORM DISPLAY-MSG

    MOVE "----------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG.

SEARCH-USER.
    MOVE "Enter the first and last name to search:" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM READ-INPUT-SAFELY
    IF EOF-INPUT-FILE = "Y"
        EXIT PARAGRAPH
    END-IF
    MOVE IN-REC TO ws-search-name

    MOVE "N" TO search-found-flag
    PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > accountCount
        IF idx = loggedInUser
            EXIT PERFORM CYCLE
        END-IF

        STRING FUNCTION TRIM(first-name(idx)) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(last-name(idx)) DELIMITED BY SIZE
               INTO ws-full-name
        END-STRING

        IF FUNCTION TRIM(ws-full-name) = FUNCTION TRIM(ws-search-name)
            MOVE "Y" TO search-found-flag
            MOVE SPACES TO msgBuffer
            STRING "User found: " DELIMITED BY SIZE
                   FUNCTION TRIM(ws-full-name) DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG
            PERFORM CHECK-CONNECTION-STATUS
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF search-found-flag = "N"
        MOVE "User not found." TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF.

CHECK-CONNECTION-STATUS.
    MOVE "N" TO can-send-request
    MOVE 0 TO conn-idx
    PERFORM VARYING conn-check-idx FROM 1 BY 1 UNTIL conn-check-idx > connection-count
        IF (conn-user1(conn-check-idx) = account-user(loggedInUser) AND
            conn-user2(conn-check-idx) = account-user(idx)) OR
           (conn-user2(conn-check-idx) = account-user(loggedInUser) AND
            conn-user1(conn-check-idx) = account-user(idx))
            MOVE conn-check-idx TO conn-idx
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF conn-idx > 0
        IF conn-pending(conn-idx)
            MOVE "Connection request is pending." TO msgBuffer
            PERFORM DISPLAY-MSG
        ELSE IF conn-accepted(conn-idx)
            MOVE "You are already connected." TO msgBuffer
            PERFORM DISPLAY-MSG
        END-IF
    ELSE
        *> Check if there's already a pending request
        MOVE "N" TO foundFlag
        PERFORM VARYING conn-check-idx FROM 1 BY 1 UNTIL conn-check-idx > request-count
            IF req-sender(conn-check-idx) = account-user(loggedInUser) AND
               req-receiver(conn-check-idx) = account-user(idx)
                MOVE "Y" TO foundFlag
                EXIT PERFORM
            END-IF
        END-PERFORM

        IF foundFlag = "Y"
            MOVE "Connection request already sent." TO msgBuffer
            PERFORM DISPLAY-MSG
        ELSE
            MOVE "Would you like to send a connection request? (Y/N)" TO msgBuffer
            PERFORM DISPLAY-MSG
            PERFORM READ-INPUT-SAFELY
            IF EOF-INPUT-FILE = "Y"
                EXIT PARAGRAPH
            END-IF
            IF IN-REC = "Y" OR IN-REC = "y"
                PERFORM SEND-CONNECTION-REQUEST
            END-IF
        END-IF
    END-IF.

SEND-CONNECTION-REQUEST.
    IF request-count < 25
        ADD 1 TO request-count
        MOVE account-user(loggedInUser) TO req-sender(request-count)
        MOVE account-user(idx) TO req-receiver(request-count)
        PERFORM SAVE-REQUESTS
        MOVE "Connection request sent!" TO msgBuffer
        PERFORM DISPLAY-MSG
    ELSE
        MOVE "Maximum request limit reached." TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF.

PROCESS-PENDING-REQUESTS.
    IF pending-count > 0
        MOVE "Enter request number to accept/reject, or 0 to skip:" TO msgBuffer
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        IF EOF-INPUT-FILE = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION NUMVAL(IN-REC) TO subChoice

        IF subChoice > 0 AND subChoice <= pending-count
            PERFORM FIND-AND-PROCESS-REQUEST
        END-IF
    END-IF.

FIND-AND-PROCESS-REQUEST.
    MOVE 0 TO conn-idx
    MOVE 0 TO pending-count
    PERFORM VARYING conn-check-idx FROM 1 BY 1 UNTIL conn-check-idx > request-count
        IF req-receiver(conn-check-idx) = account-user(loggedInUser)
            ADD 1 TO pending-count
            IF pending-count = subChoice
                MOVE conn-check-idx TO conn-idx
                EXIT PERFORM
            END-IF
        END-IF
    END-PERFORM

    IF conn-idx > 0
        MOVE "Accept this connection request? (Y/N)" TO msgBuffer
        PERFORM DISPLAY-MSG
        PERFORM READ-INPUT-SAFELY
        IF EOF-INPUT-FILE = "Y"
            EXIT PARAGRAPH
        END-IF
        IF IN-REC = "Y" OR IN-REC = "y"
            PERFORM ACCEPT-CONNECTION-REQUEST
        ELSE
            PERFORM REJECT-CONNECTION-REQUEST
        END-IF
    END-IF.

ACCEPT-CONNECTION-REQUEST.
    IF connection-count < 25
        ADD 1 TO connection-count
        MOVE req-sender(conn-idx) TO conn-user1(connection-count)
        MOVE req-receiver(conn-idx) TO conn-user2(connection-count)
        MOVE "A" TO conn-status(connection-count)
        PERFORM SAVE-CONNECTIONS
    END-IF
    PERFORM REMOVE-REQUEST
    MOVE "Connection request accepted!" TO msgBuffer
    PERFORM DISPLAY-MSG.

REJECT-CONNECTION-REQUEST.
    PERFORM REMOVE-REQUEST
    MOVE "Connection request rejected." TO msgBuffer
    PERFORM DISPLAY-MSG.

REMOVE-REQUEST.
    PERFORM VARYING conn-check-idx FROM conn-idx BY 1 UNTIL conn-check-idx >= request-count
        MOVE req-sender(conn-check-idx + 1) TO req-sender(conn-check-idx)
        MOVE req-receiver(conn-check-idx + 1) TO req-receiver(conn-check-idx)
    END-PERFORM
    SUBTRACT 1 FROM request-count
    PERFORM SAVE-REQUESTS.

VIEW-MY-NETWORK.
    MOVE "--- My Network ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE 0 TO pending-count
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > connection-count
        IF conn-accepted(conn-idx)
            IF conn-user1(conn-idx) = account-user(loggedInUser)
                ADD 1 TO pending-count
                MOVE SPACES TO msgBuffer
                STRING pending-count DELIMITED BY SIZE
                       ". " DELIMITED BY SIZE
                       FUNCTION TRIM(conn-user2(conn-idx)) DELIMITED BY SIZE
                       INTO msgBuffer
                END-STRING
                PERFORM DISPLAY-MSG
            ELSE IF conn-user2(conn-idx) = account-user(loggedInUser)
                ADD 1 TO pending-count
                MOVE SPACES TO msgBuffer
                STRING pending-count DELIMITED BY SIZE
                       ". " DELIMITED BY SIZE
                       FUNCTION TRIM(conn-user1(conn-idx)) DELIMITED BY SIZE
                       INTO msgBuffer
                END-STRING
                PERFORM DISPLAY-MSG
            END-IF
        END-IF
    END-PERFORM

    IF pending-count = 0
        MOVE "You have no connections yet." TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF

    MOVE "----------------------------------" TO msgBuffer
    PERFORM DISPLAY-MSG.

DISPLAY-MSG.
    DISPLAY msgBuffer
    WRITE OUT-REC FROM msgBuffer.
