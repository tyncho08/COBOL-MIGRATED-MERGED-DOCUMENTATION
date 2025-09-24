# How to Run ACAS (Applewood Computers Accounting System)

## Prerequisites
- macOS with GnuCOBOL 3.2.0 installed ✅
- ACAS compiled and installed ✅
- Data directory created at `~/ACAS` ✅

## Running ACAS

### Option 1: Using the Wrapper Script (Recommended)
```bash
~/bin/acas-run.sh
```

### Option 2: Running Directly with Environment Variables
```bash
ACAS_LEDGERS="/Users/MartinGonella/ACAS" \
ACAS_BIN="/Users/MartinGonella/bin" \
COB_LIBRARY_PATH="/Users/MartinGonella/Desktop/Demos/ACAS-Nightly/common:/Users/MartinGonella/bin" \
/Users/MartinGonella/bin/ACAS
```

## Available Modules

The system includes several modules that can be run independently:

| Command | Description |
|---------|-------------|
| `ACAS` | Main system menu |
| `irs` | Internal Revenue System (IRS) - General Ledger |
| `sales` | Sales Ledger / Accounts Receivable |
| `purchase` | Purchase Ledger / Accounts Payable |
| `stock` | Stock Control / Inventory Management |
| `general` | General Ledger (alternative) |

To run a specific module, edit the `acas-run.sh` script and change the last line.

## First-Time Setup

When running ACAS for the first time:

1. **Company Name**: The system will prompt you to enter your company name
2. **System Parameters**: The `system.dat` file will be created automatically
3. **Chart of Accounts**: You can import the default chart of accounts
4. **Ledger Configuration**: Configure the different accounting ledgers

## Directory Structure

```
~/ACAS/                 # Data directory
├── system.dat          # System parameters file
├── archives/           # Archived backups
└── temp-backups/       # Temporary backups

~/bin/                  # Executables
├── ACAS               # Main program
├── irs                # IRS module
├── sales              # Sales module
├── purchase           # Purchase module
├── stock              # Inventory module
├── general            # General ledger
└── acas-run.sh        # Wrapper script
```

## System Navigation

- Use arrow keys to move between fields
- `Enter` to confirm
- `Esc` to cancel or go back
- Menus use numbers or letters to select options

## Troubleshooting

### Error: "SY009 Environment variables not yet set up"
Environment variables are not configured. Use the `acas-run.sh` script or set the variables manually.

### Error: "module 'acas000' not found"
The COBOL library path is not configured. Ensure `COB_LIBRARY_PATH` includes the correct directories.

### Program not responding
- Press `Ctrl+C` to exit
- Verify the terminal has at least 80x24 characters

### Error: "SY102 Read Err 1 = 23"
This error occurs when system.dat is missing or corrupted. To fix:

**Option 1 (Quick Fix):**
```bash
cd ~/ACAS
rm -f system.dat
cobc -x /Users/MartinGonella/Desktop/Demos/ACAS-Nightly/create-system-dat.cbl -o create-system-dat
./create-system-dat
```

**Option 2 (Manual):**
1. Delete the empty file: `rm ~/ACAS/system.dat`
2. The create-system-dat.cbl program creates a minimal valid system.dat file that ACAS can then initialize properly

## Current System Status

✅ **ACAS IS FULLY INSTALLED AND FUNCTIONAL**

The system starts correctly and displays the initial configuration screen when run for the first time.

### IMPORTANT: Interactive Execution
ACAS requires an interactive terminal to function properly. **It cannot be run in batch mode or from automated scripts** because it needs user input.

## Important Notes

- ACAS is a complete accounting system developed since 1976
- Uses COBOL indexed files (ISAM) to store data
- Supports multiple companies and accounting periods
- Includes complete audit trail of all transactions
- Requires interactive terminal for data entry

## Compilation (if needed)

To recompile the system:
```bash
cd /Users/MartinGonella/Desktop/Demos/ACAS-Nightly
./comp-all-no-rdbms.sh
./install-ACAS-preinstalled.sh
```