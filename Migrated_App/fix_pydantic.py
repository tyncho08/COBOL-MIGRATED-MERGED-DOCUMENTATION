#!/usr/bin/env python3
"""
Fix Pydantic v2 compatibility issues across all schema files
"""
import os
import re
from pathlib import Path

def fix_decimal_places_in_file(file_path):
    """Fix decimal_places constraints in a single file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    original_content = content
    
    # Remove decimal_places constraints from Field() calls
    # Pattern: Field(..., decimal_places=N, ...)
    content = re.sub(r',\s*decimal_places=\d+', '', content)
    content = re.sub(r'decimal_places=\d+,\s*', '', content)
    content = re.sub(r'decimal_places=\d+', '', content)
    
    # Fix validator imports
    content = content.replace('from pydantic import BaseModel, Field, validator', 
                             'from pydantic import BaseModel, Field, field_validator')
    content = content.replace('from pydantic import validator', 
                             'from pydantic import field_validator')
    
    # Fix validator decorators
    content = re.sub(r'@validator\(([^)]+)\)', r'@field_validator(\1)\n    @classmethod', content)
    
    # Fix Config classes to model_config
    content = re.sub(r'class Config:\s*\n\s*orm_mode = True\s*\n\s*schema_extra = ({[^}]+})', 
                     r'model_config = {\n        "from_attributes": True,\n        "json_schema_extra": \1\n    }', content)
    
    content = re.sub(r'class Config:\s*\n\s*schema_extra = ({[^}]+})', 
                     r'model_config = {\n        "json_schema_extra": \1\n    }', content)
    
    if content != original_content:
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Fixed: {file_path}")
        return True
    return False

def main():
    """Fix all schema files"""
    schemas_dir = Path("/Users/MartinGonella/Desktop/Demos/COBOL-MIGRATED-MERGED-DOCUMENTATION/Migrated_App/backend/app/schemas")
    
    fixed_files = 0
    for schema_file in schemas_dir.glob("*.py"):
        if schema_file.name == "__init__.py":
            continue
        
        if fix_decimal_places_in_file(schema_file):
            fixed_files += 1
    
    print(f"Fixed {fixed_files} schema files")

if __name__ == "__main__":
    main()