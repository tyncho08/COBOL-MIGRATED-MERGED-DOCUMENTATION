# ACAS system.dat File Creation Investigation

## Problem Summary
ACAS is not automatically creating the system.dat file on first run, resulting in file status 35 (file not found) errors.

## Root Cause Analysis

### 1. File Opening Logic in sys002.cbl
The program sys002.cbl is responsible for system file maintenance. When it starts:

1. At line 1076, it performs `acas000-Open-Input` to open system.dat as input
2. If this fails (fs-reply not = zero), it performs `ba000-mapser`
3. The `ba000-mapser` routine (in copybooks/Proc-ACAS-Mapser-RDB.cob) is responsible for initial system setup

### 2. The acas000.cbl File Handler
The file handler `acas000.cbl` manages all I/O operations for the system file:

- When opening as INPUT (line 401-407), if the file doesn't exist, it returns fs-reply = 35
- When opening as I-O (line 409-416), it attempts to:
  1. Open I-O
  2. If that fails, close the file
  3. Open as OUTPUT (which creates the file)
  4. Close the file
  5. Re-open as I-O

### 3. The Issue
The problem occurs in the initial open sequence:

1. `acas000-Open-Input` is called first (sys002.cbl line 1076)
2. This fails with status 35 because the file doesn't exist
3. However, in acas000.cbl lines 403-407, when opening as INPUT fails:
   - It sets fs-Reply to 35
   - It immediately goes to `aa999-Main-Exit`
   - It does NOT attempt to create the file

### 4. The ba000-mapser Section
When the initial open fails, `ba000-mapser` is called, which:

1. Displays the system setup screen
2. Performs `acas000-Open-Output` (line 35) to create the file
3. Initializes all four system records
4. Writes them to the file

However, this only happens if the initial open fails AND the program continues to execute ba000-mapser.

## Why It's Not Working

The issue appears to be that when `acas000-Open-Input` fails:

1. The error handling may be preventing the flow from reaching `ba000-mapser`
2. Or there might be additional error checking that's stopping execution
3. The disk-error-display routine (called at line 1085 in sys002.cbl) might be terminating the program

## Recommendations

### Immediate Workaround
Create an empty system.dat file manually before running ACAS:
```bash
touch ~/ACAS/system.dat
```

### Proper Fix Options

1. **Modify acas000.cbl**: Change the Open-Input logic to automatically create the file if it doesn't exist:
   - When opening as INPUT fails with status 35
   - Automatically switch to OUTPUT mode to create the file
   - Then close and re-open as INPUT

2. **Modify sys002.cbl**: Add explicit file creation logic before the first open:
   - Check if file exists
   - If not, create it using OUTPUT mode
   - Then proceed with normal processing

3. **Review Error Handling**: The disk-error-display routine might be too aggressive in handling the "expected" error 35 on first run.

## Additional Notes

- The system expects to run `ba000-mapser` only once when creating a new system
- The file uses relative organization with 4 records (system, default, final, and sys4)
- The system uses environment variables ACAS_LEDGERS (default ~/ACAS) for file locations
- There's no sample system.dat file in the distribution
- The installation scripts don't pre-create the system.dat file