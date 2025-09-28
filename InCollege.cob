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
01 CONN-REC-FILE PIC X(60).

WORKING-STORAGE SECTION.
01 mainChoice PIC 9.
01 subChoice PIC 9.
01 loginOk PIC X VALUE "N".
01 doneFlag PIC X VALUE "N".
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
01 connectionCount PIC 99 VALUE 0.
01 conn-idx PIC 99.
01 conn-sender PIC X(20).
01 conn-recipient PIC X(20).
01 conn-status PIC X(10).
01 request-exists-flag PIC X.
01 already-connected-flag PIC X.
01 send-request-choice PIC 9.
01 pending-count PIC 99.

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

01 skillList.
    05 skillName OCCURS 5 TIMES PIC X(20) VALUE SPACES.

01 connection-requests.
    05 connection-entry OCCURS 50 TIMES.
        10 sender-username PIC X(20).
        10 recipient-username PIC X(20).
        10 request-status PIC X(10).

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
        MOVE 0 TO connectionCount
        PERFORM INITIALIZE-CONNECTIONS
    ELSE
        PERFORM LOAD-CONNECTIONS
    END-IF

    PERFORM SETUP-SKILLS
    PERFORM WELCOME-SCREEN

    PERFORM MAIN-MENU UNTIL doneFlag = "Y"

    PERFORM SAVE-ACCOUNTS
    PERFORM SAVE-PROFILES
    PERFORM SAVE-CONNECTIONS

    CLOSE INPUT-FILE
    CLOSE OUTPUT-FILE
    CLOSE ACCOUNT-FILE
    CLOSE PROFILE-FILE
    CLOSE CONNECTION-FILE
    STOP RUN.

INITIALIZE-PROFILES.
    MOVE ALL SPACES TO user-profiles.

INITIALIZE-CONNECTIONS.
    MOVE ALL SPACES TO connection-requests.

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
    MOVE 0 TO connectionCount
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > 50
        READ CONNECTION-FILE
            AT END EXIT PERFORM
            NOT AT END
                ADD 1 TO connectionCount
                MOVE CONN-REC-FILE(1:20) TO sender-username(conn-idx)
                MOVE CONN-REC-FILE(21:20) TO recipient-username(conn-idx)
                MOVE CONN-REC-FILE(41:10) TO request-status(conn-idx)
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

SAVE-PROFILES.
    OPEN OUTPUT PROFILE-FILE
    PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > accountCount
        MOVE user-profile(idx) TO PROF-REC-FILE
        WRITE PROF-REC-FILE
    END-PERFORM
    CLOSE PROFILE-FILE.

SAVE-CONNECTIONS.
    OPEN OUTPUT CONNECTION-FILE
    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > connectionCount
        MOVE sender-username(conn-idx) TO CONN-REC-FILE(1:20)
        MOVE recipient-username(conn-idx) TO CONN-REC-FILE(21:20)
        MOVE request-status(conn-idx) TO CONN-REC-FILE(41:10)
        WRITE CONN-REC-FILE
    END-PERFORM
    CLOSE CONNECTION-FILE.

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
        MOVE SPACES TO IN-REC
    ELSE
        READ INPUT-FILE
            AT END
                MOVE "Y" TO EOF-INPUT-FILE
                MOVE SPACES TO IN-REC
            NOT AT END
                MOVE IN-REC TO debug-input
                *> Uncomment next line for debugging
                *> DISPLAY "DEBUG: Read input: [" debug-input "]"
        END-READ
    END-IF.

MAIN-MENU.
    PERFORM READ-INPUT-SAFELY
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
    PERFORM DISPLAY-MSG.
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
    MOVE "N" TO doneFlag
    PERFORM UNTIL doneFlag = "Y"
        MOVE "1. Create/Edit My Profile" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "2. View My Profile" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "3. Search for User" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "4. Learn a New Skill" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "5. View My Pending Connection Requests" TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE "6. Go Back" TO msgBuffer
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
                PERFORM SKILL-MENU
            WHEN 5
                PERFORM VIEW-PENDING-REQUESTS
            WHEN 6
                MOVE "Y" TO doneFlag
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

        PERFORM READ-INPUT-SAFELY
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
    PERFORM DISPLAY-MSG.
    PERFORM SAVE-PROFILES.

VIEW-PROFILE.
    MOVE "--- Your Profile ---" TO msgBuffer
    PERFORM DISPLAY-MSG
    MOVE loggedInUser TO ws-display-idx
    PERFORM DISPLAY-GENERIC-PROFILE.

SEARCH-USER.
    MOVE "--- Find Someone You Know ---" 
        TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE "Enter the full name of the person you are looking for:" 
        TO msgBuffer
    PERFORM DISPLAY-MSG

    PERFORM READ-INPUT-SAFELY
    MOVE IN-REC TO ws-search-name

    MOVE "N" TO search-found-flag
    MOVE 0 TO ws-display-idx

    PERFORM VARYING search-idx FROM 1 BY 1
        UNTIL search-idx > accountCount 
           OR search-found-flag = "Y"

    
        MOVE SPACES TO ws-full-name
        STRING FUNCTION TRIM(first-name(search-idx)) DELIMITED BY SIZE
               " "                              DELIMITED BY SIZE
               FUNCTION TRIM(last-name(search-idx)) DELIMITED BY SIZE
               INTO ws-full-name
        END-STRING
        
        IF FUNCTION TRIM(ws-search-name) = FUNCTION TRIM(ws-full-name)
            MOVE "Y" TO search-found-flag
            MOVE search-idx TO ws-display-idx
        END-IF
    END-PERFORM.

    EVALUATE TRUE
        WHEN search-found-flag = "Y"
            PERFORM DISPLAY-GENERIC-PROFILE
            *> Only offer to send connection request if not viewing own profile
            IF ws-display-idx NOT = loggedInUser
                PERFORM OFFER-CONNECTION-REQUEST
            END-IF
        WHEN OTHER
            MOVE "No one by that name could be found." TO msgBuffer
            PERFORM DISPLAY-MSG
    END-EVALUATE.

OFFER-CONNECTION-REQUEST.
    *> Check if already connected or request pending
    PERFORM CHECK-CONNECTION-STATUS

    EVALUATE TRUE
        WHEN already-connected-flag = "Y"
            MOVE "You are already connected with this user." TO msgBuffer
            PERFORM DISPLAY-MSG
        WHEN request-exists-flag = "Y"
            MOVE "This user has already sent you a connection request." TO msgBuffer
            PERFORM DISPLAY-MSG
        WHEN OTHER
            MOVE "1. Send Connection Request" TO msgBuffer
            PERFORM DISPLAY-MSG
            MOVE "2. Go Back" TO msgBuffer
            PERFORM DISPLAY-MSG
            MOVE "Enter your choice:" TO msgBuffer
            PERFORM DISPLAY-MSG

            PERFORM READ-INPUT-SAFELY
            MOVE FUNCTION NUMVAL(IN-REC) TO send-request-choice

            IF send-request-choice = 1
                PERFORM SEND-CONNECTION-REQUEST
            END-IF
    END-EVALUATE.

CHECK-CONNECTION-STATUS.
    MOVE "N" TO already-connected-flag
    MOVE "N" TO request-exists-flag

    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > connectionCount
        *> Check if already connected (request-status = "CONNECTED")
        IF (sender-username(conn-idx) = FUNCTION TRIM(account-user(loggedInUser))
            AND recipient-username(conn-idx) = FUNCTION TRIM(account-user(ws-display-idx))
            AND request-status(conn-idx) = "CONNECTED")
        OR (sender-username(conn-idx) = FUNCTION TRIM(account-user(ws-display-idx))
            AND recipient-username(conn-idx) = FUNCTION TRIM(account-user(loggedInUser))
            AND request-status(conn-idx) = "CONNECTED")
            MOVE "Y" TO already-connected-flag
        END-IF

        *> Check if pending request exists from the target user to logged in user
        IF sender-username(conn-idx) = FUNCTION TRIM(account-user(ws-display-idx))
            AND recipient-username(conn-idx) = FUNCTION TRIM(account-user(loggedInUser))
            AND request-status(conn-idx) = "PENDING"
            MOVE "Y" TO request-exists-flag
        END-IF

        *> Check if logged in user already sent request to target user
        IF sender-username(conn-idx) = FUNCTION TRIM(account-user(loggedInUser))
            AND recipient-username(conn-idx) = FUNCTION TRIM(account-user(ws-display-idx))
            AND request-status(conn-idx) = "PENDING"
            MOVE "Y" TO already-connected-flag  *> Treat as "already connected" for UI purposes
        END-IF
    END-PERFORM.

SEND-CONNECTION-REQUEST.
    ADD 1 TO connectionCount
    MOVE FUNCTION TRIM(account-user(loggedInUser)) TO sender-username(connectionCount)
    MOVE FUNCTION TRIM(account-user(ws-display-idx)) TO recipient-username(connectionCount)
    MOVE "PENDING" TO request-status(connectionCount)

    MOVE "Connection request sent successfully!" TO msgBuffer
    PERFORM DISPLAY-MSG
    
    PERFORM SAVE-CONNECTIONS.

VIEW-PENDING-REQUESTS.
    MOVE "--- Your Pending Connection Requests ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE 0 TO pending-count

    PERFORM VARYING conn-idx FROM 1 BY 1 UNTIL conn-idx > connectionCount
        IF recipient-username(conn-idx) = FUNCTION TRIM(account-user(loggedInUser))
            AND request-status(conn-idx) = "PENDING"
            ADD 1 TO pending-count
            MOVE SPACES TO msgBuffer
            STRING "Connection request from: " DELIMITED BY SIZE
                   FUNCTION TRIM(sender-username(conn-idx)) DELIMITED BY SIZE
                   INTO msgBuffer
            END-STRING
            PERFORM DISPLAY-MSG
        END-IF
    END-PERFORM

    IF pending-count = 0
        MOVE "You have no pending connection requests." TO msgBuffer
        PERFORM DISPLAY-MSG
    END-IF

    MOVE "------------------" TO msgBuffer
    PERFORM DISPLAY-MSG.

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
    