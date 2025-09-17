# InCollege - User Authentication System (COBOL)

## Overview
This is the **InCollege Project: Week 3 Deliverable - Log In, Part 1**, a console-based COBOL application that simulates a basic **user authentication system**. Users can create new accounts, log in, and navigate initial post-login options.  

The project demonstrates:
- File-based input/output handling
- Account persistence
- Password validation
- Menu-driven user interface

---

## Features

### 1. User Registration
- Supports up to **5 unique student accounts**
- Password requirements:
  - Minimum 8 characters, maximum 12 characters
  - At least **one uppercase letter**
  - At least **one digit**
  - At least **one special character** (`! @ # $ % ^ & *`)
- Shows message if account creation fails or account limit is reached

### 2. Login Functionality
- Unlimited login attempts
- Messages for **successful login** or **incorrect credentials**

### 3. Post-Login Navigation
- **Search for a job** → under construction
- **Find someone you know** → under construction
- **Learn a new skill**
  - List of 5 skills
  - Selecting a skill → under construction
  - Option to go back to main menu

---

## Files in Repository

| File | Description |
|------|-------------|
| `InCollege.cob` | Main COBOL program |
| `InCollege-Test.txt` | Test input file covering positive, negative, and edge cases |
| `InCollege-Input.txt` | Sample input file for standard program execution |
| `InCollege-Output.txt` | Actual program output generated during test execution |
| `InCollege-Accounts.txt` | Sequential file storing user accounts |
| `Roles.txt` | List of team members and their roles |
| `Jira.jpg` | Screenshot of Jira board with user stories and tasks |

---

## How to Compile and Run

1. Make sure you have a COBOL compiler installed (e.g., **GnuCOBOL**).  

2. Compile the program:

```bash
cobc -x -free InCollege.cob -o InCollege
```

2. Run program:

```bash
./InCollege   
```     
                                              
