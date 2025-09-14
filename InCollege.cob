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

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 IN-REC PIC X(50).

FD OUTPUT-FILE.
01 OUT-REC PIC X(100).

FD ACCOUNT-FILE.
01 ACC-REC-FILE PIC X(40).

FD PROFILE-FILE.
01 PROF-REC-FILE PIC X(1550).

WORKING-STORAGE SECTION.
01 mainChoice PIC 9.
01 subChoice PIC 9.
01 loginOk PIC X VALUE "N".
01 doneFlag PIC X VALUE "N".
01 msgBuffer PIC X(100).
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
01 trimmedUser PIC X(20).
01 trimmedPass PIC X(20).
01 loggedInUser PIC 9.
01 temp-year PIC 9(4).
01 trimmed-input PIC X(50).
01 exp-idx PIC 9.
01 edu-idx PIC 9.

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

    PERFORM SETUP-SKILLS
    PERFORM WELCOME-SCREEN

    PERFORM MAIN-MENU UNTIL doneFlag = "Y"

    PERFORM SAVE-ACCOUNTS
    PERFORM SAVE-PROFILES

    CLOSE INPUT-FILE
    CLOSE OUTPUT-FILE
    CLOSE ACCOUNT-FILE
    CLOSE PROFILE-FILE
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
                MOVE idx TO loggedInUser
            END-IF
        END-PERFORM

        IF foundFlag = "Y"
            MOVE "You have successfully logged in." TO msgBuffer
            PERFORM DISPLAY-MSG
            MOVE "Welcome, " TO msgBuffer
            STRING msgBuffer DELIMITED BY SIZE
                   FUNCTION TRIM(userName) DELIMITED BY SIZE
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
        MOVE "5. Go Back" TO msgBuffer
        PERFORM DISPLAY-MSG

        READ INPUT-FILE
            AT END MOVE "5" TO IN-REC
        END-READ
        MOVE FUNCTION NUMVAL(IN-REC) TO mainChoice

        EVALUATE mainChoice
            WHEN 1
                PERFORM CREATE-EDIT-PROFILE
            WHEN 2
                PERFORM VIEW-PROFILE
            WHEN 3
                MOVE "Search for User is under construction." TO msgBuffer
                PERFORM DISPLAY-MSG
            WHEN 4
                PERFORM SKILL-MENU
            WHEN 5
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

CREATE-EDIT-PROFILE.
    MOVE "--- Create/Edit Profile ---" TO msgBuffer
    PERFORM DISPLAY-MSG

    MOVE "Enter First Name:" TO msgBuffer
    PERFORM DISPLAY-MSG
    READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
    MOVE FUNCTION TRIM(IN-REC) TO first-name(loggedInUser)

    MOVE "Enter Last Name:" TO msgBuffer
    PERFORM DISPLAY-MSG
    READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
    MOVE FUNCTION TRIM(IN-REC) TO last-name(loggedInUser)

    MOVE "Enter University/College Attended:" TO msgBuffer
    PERFORM DISPLAY-MSG
    READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
    MOVE FUNCTION TRIM(IN-REC) TO university(loggedInUser)

    MOVE "Enter Major:" TO msgBuffer
    PERFORM DISPLAY-MSG
    READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
    MOVE FUNCTION TRIM(IN-REC) TO major(loggedInUser)

    MOVE "Enter Graduation Year (YYYY):" TO msgBuffer
    PERFORM DISPLAY-MSG
    READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
    IF IN-REC IS NUMERIC AND FUNCTION LENGTH(FUNCTION TRIM(IN-REC)) = 4
        MOVE FUNCTION NUMVAL(IN-REC) TO graduation-year(loggedInUser)
    ELSE
        MOVE "Invalid year entered." TO msgBuffer
        PERFORM DISPLAY-MSG
        MOVE 0 TO graduation-year(loggedInUser)
    END-IF

    MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):" TO msgBuffer
    PERFORM DISPLAY-MSG
    READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
    MOVE FUNCTION TRIM(IN-REC) TO about-me(loggedInUser)

    MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM VARYING exp-idx FROM 1 BY 1 UNTIL exp-idx > 3
        MOVE SPACES TO msgBuffer
        STRING "Experience #" DELIMITED BY SIZE
               exp-idx DELIMITED BY SIZE
               " - Title:" DELIMITED BY SIZE INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        READ INPUT-FILE AT END MOVE "DONE" TO IN-REC END-READ
        IF FUNCTION TRIM(IN-REC) = "DONE"
            MOVE SPACES TO exp-title(loggedInUser, exp-idx)
            EXIT PERFORM
        END-IF
        MOVE FUNCTION TRIM(IN-REC) TO exp-title(loggedInUser, exp-idx)


        MOVE SPACES TO msgBuffer
        STRING "Experience #" DELIMITED BY SIZE
               exp-idx DELIMITED BY SIZE
               " - Description (optional, max 100 chars, blank to skip):" DELIMITED BY SIZE INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
        MOVE FUNCTION TRIM(IN-REC) TO exp-desc(loggedInUser, exp-idx)
    END-PERFORM

    MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO msgBuffer
    PERFORM DISPLAY-MSG
    PERFORM VARYING edu-idx FROM 1 BY 1 UNTIL edu-idx > 3
        MOVE SPACES TO msgBuffer
        STRING "Education #" DELIMITED BY SIZE
               edu-idx DELIMITED BY SIZE
               " - Degree:" DELIMITED BY SIZE INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        READ INPUT-FILE AT END MOVE "DONE" TO IN-REC END-READ
        IF FUNCTION TRIM(IN-REC) = "DONE"
            MOVE SPACES TO edu-degree(loggedInUser, edu-idx)
            EXIT PERFORM
        END-IF
        MOVE FUNCTION TRIM(IN-REC) TO edu-degree(loggedInUser, edu-idx)

        MOVE SPACES TO msgBuffer
        STRING "Education #" DELIMITED BY SIZE
               edu-idx DELIMITED BY SIZE
               " - University/College:" DELIMITED BY SIZE INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
        MOVE FUNCTION TRIM(IN-REC) TO edu-university(loggedInUser, edu-idx)

        MOVE SPACES TO msgBuffer
        STRING "Education #" DELIMITED BY SIZE
               edu-idx DELIMITED BY SIZE
               " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE INTO msgBuffer
        END-STRING
        PERFORM DISPLAY-MSG
        READ INPUT-FILE AT END MOVE SPACES TO IN-REC END-READ
        MOVE FUNCTION TRIM(IN-REC) TO edu-years(loggedInUser, edu-idx)
    END-PERFORM

    MOVE "Profile saved successfully!" TO msgBuffer
    PERFORM DISPLAY-MSG.

VIEW-PROFILE.
    MOVE "View Profile is under construction." TO msgBuffer
    PERFORM DISPLAY-MSG.
    