#!/usr/bin/env python3
"""
Script to analyze ALL imports in the backend and find missing classes/modules
"""
import os
import re
import ast
from pathlib import Path
from typing import Set, Dict, List, Tuple

def get_python_files(directory: str) -> List[Path]:
    """Get all Python files in directory, excluding venv"""
    python_files = []
    for root, dirs, files in os.walk(directory):
        if 'venv' in dirs:
            dirs.remove('venv')
        if '__pycache__' in dirs:
            dirs.remove('__pycache__')
        for file in files:
            if file.endswith('.py'):
                python_files.append(Path(root) / file)
    return python_files

def extract_imports(file_path: Path) -> List[Dict]:
    """Extract all import statements from a Python file"""
    imports = []
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Parse AST
        tree = ast.parse(content)
        
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom):
                module = node.module or ''
                for alias in node.names:
                    imports.append({
                        'file': str(file_path),
                        'line': node.lineno,
                        'module': module,
                        'name': alias.name,
                        'as': alias.asname,
                        'type': 'from'
                    })
            elif isinstance(node, ast.Import):
                for alias in node.names:
                    imports.append({
                        'file': str(file_path),
                        'line': node.lineno,
                        'module': alias.name,
                        'name': None,
                        'as': alias.asname,
                        'type': 'import'
                    })
    except Exception as e:
        print(f"Error parsing {file_path}: {e}")
    
    return imports

def check_module_exists(module_path: str, base_dir: Path) -> bool:
    """Check if a module exists"""
    if module_path.startswith('app.'):
        # Convert module path to file path
        parts = module_path.split('.')
        file_path = base_dir / '/'.join(parts[1:]) / '__init__.py'
        module_file = base_dir / ('/'.join(parts[1:]) + '.py')
        
        return file_path.exists() or module_file.exists()
    return True  # Assume external modules exist

def check_class_exists(module_path: str, class_name: str, base_dir: Path) -> bool:
    """Check if a class exists in a module"""
    if not module_path.startswith('app.'):
        return True  # Assume external classes exist
    
    # Convert module path to file path
    parts = module_path.split('.')
    module_file = base_dir / ('/'.join(parts[1:]) + '.py')
    
    if not module_file.exists():
        return False
    
    try:
        with open(module_file, 'r') as f:
            content = f.read()
        
        # Simple check for class definition
        class_pattern = rf'^\s*class\s+{re.escape(class_name)}\s*[\(\:]'
        if re.search(class_pattern, content, re.MULTILINE):
            return True
        
        # Check for assignments (aliases)
        alias_pattern = rf'^\s*{re.escape(class_name)}\s*='
        if re.search(alias_pattern, content, re.MULTILINE):
            return True
            
    except Exception as e:
        print(f"Error checking {module_file}: {e}")
    
    return False

def main():
    base_dir = Path('app')
    python_files = get_python_files('app')
    
    all_imports = []
    for file_path in python_files:
        imports = extract_imports(file_path)
        all_imports.extend(imports)
    
    # Analyze imports
    missing_modules = []
    missing_classes = []
    
    for imp in all_imports:
        if imp['type'] == 'from' and imp['module'].startswith('app.'):
            # Check if module exists
            if not check_module_exists(imp['module'], base_dir):
                missing_modules.append({
                    'file': imp['file'],
                    'line': imp['line'],
                    'module': imp['module']
                })
            elif imp['name'] and imp['name'] != '*':
                # Check if class/function exists
                if not check_class_exists(imp['module'], imp['name'], base_dir):
                    missing_classes.append({
                        'file': imp['file'],
                        'line': imp['line'],
                        'module': imp['module'],
                        'name': imp['name']
                    })
    
    # Report results
    print("=" * 80)
    print("IMPORT ANALYSIS REPORT")
    print("=" * 80)
    print()
    
    if missing_modules:
        print("MISSING MODULES:")
        print("-" * 40)
        for item in sorted(missing_modules, key=lambda x: x['file']):
            print(f"File: {item['file']}")
            print(f"Line: {item['line']}")
            print(f"Module: {item['module']}")
            print()
    
    if missing_classes:
        print("MISSING CLASSES/FUNCTIONS:")
        print("-" * 40)
        # Group by module
        by_module = {}
        for item in missing_classes:
            key = (item['module'], item['file'])
            if key not in by_module:
                by_module[key] = []
            by_module[key].append((item['name'], item['line']))
        
        for (module, file), items in sorted(by_module.items()):
            print(f"\nFile: {file}")
            print(f"Module: {module}")
            print("Missing items:")
            for name, line in sorted(items):
                print(f"  - {name} (line {line})")
    
    print("\n" + "=" * 80)
    print(f"Total missing modules: {len(missing_modules)}")
    print(f"Total missing classes/functions: {len(missing_classes)}")
    print("=" * 80)

if __name__ == "__main__":
    main()