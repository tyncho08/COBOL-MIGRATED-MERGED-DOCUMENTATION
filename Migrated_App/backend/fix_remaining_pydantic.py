#!/usr/bin/env python3
"""
Fix remaining Pydantic v2 issues in all schema files
"""
import os
import re

schema_dir = "/Users/MartinGonella/Desktop/Demos/COBOL-MIGRATED-MERGED-DOCUMENTATION/Migrated_App/backend/app/schemas"

def fix_file(file_path):
    """Fix Pydantic v2 issues in a single file"""
    print(f"Processing {file_path}")
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    original_content = content
    
    # Fix regex -> pattern
    content = re.sub(r'regex=', 'pattern=', content)
    
    # Fix orm_mode -> from_attributes in Config class
    content = re.sub(
        r'(\s+)class Config:\s*\n(\s+)orm_mode = True',
        r'\1model_config = ConfigDict(from_attributes=True)',
        content
    )
    
    # Add ConfigDict import if needed and not already there
    if 'model_config = ConfigDict(from_attributes=True)' in content:
        if 'from pydantic import' in content and 'ConfigDict' not in content:
            # Add ConfigDict to existing import
            content = re.sub(
                r'from pydantic import ([^\\n]+)',
                lambda m: f"from pydantic import {m.group(1)}, ConfigDict" if 'ConfigDict' not in m.group(1) else m.group(0),
                content
            )
    
    # Write back if changed
    if content != original_content:
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"  ✓ Fixed {file_path}")
        return True
    else:
        print(f"  - No changes needed in {file_path}")
        return False

def main():
    """Process all schema files"""
    files_fixed = 0
    
    for filename in os.listdir(schema_dir):
        if filename.endswith('.py') and filename != '__init__.py':
            file_path = os.path.join(schema_dir, filename)
            if fix_file(file_path):
                files_fixed += 1
    
    print(f"\n✓ Fixed {files_fixed} files")

if __name__ == "__main__":
    main()