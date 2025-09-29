#!/usr/bin/env python3
"""
Script to analyze ALL imports in the frontend and find missing files/modules
"""
import os
import re
from pathlib import Path
from typing import Set, Dict, List, Tuple

def get_ts_tsx_files(directory: str) -> List[Path]:
    """Get all TypeScript files in directory"""
    files = []
    for root, dirs, filenames in os.walk(directory):
        if 'node_modules' in dirs:
            dirs.remove('node_modules')
        if '.next' in dirs:
            dirs.remove('.next')
        for file in filenames:
            if file.endswith(('.ts', '.tsx', '.js', '.jsx')):
                files.append(Path(root) / file)
    return files

def extract_imports(file_path: Path) -> List[Dict]:
    """Extract all import statements from a TypeScript file"""
    imports = []
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Match various import patterns
        patterns = [
            # import ... from '...'
            r'import\s+(?:(?:\{[^}]*\})|(?:\*\s+as\s+\w+)|(?:\w+))\s+from\s+[\'"]([^\'"]+)[\'"]',
            # import '...'
            r'import\s+[\'"]([^\'"]+)[\'"]',
            # require('...')
            r'require\s*\(\s*[\'"]([^\'"]+)[\'"]\s*\)',
            # dynamic import
            r'import\s*\(\s*[\'"]([^\'"]+)[\'"]\s*\)'
        ]
        
        line_num = 0
        for line in content.split('\n'):
            line_num += 1
            for pattern in patterns:
                matches = re.findall(pattern, line)
                for match in matches:
                    imports.append({
                        'file': str(file_path),
                        'line': line_num,
                        'import_path': match
                    })
    except Exception as e:
        print(f"Error parsing {file_path}: {e}")
    
    return imports

def resolve_import_path(import_path: str, file_path: Path, base_dir: Path) -> Path:
    """Resolve import path to actual file path"""
    if import_path.startswith('.'):
        # Relative import
        resolved = (file_path.parent / import_path).resolve()
    elif import_path.startswith('@/'):
        # Alias import
        resolved = base_dir / import_path[2:]
    elif import_path.startswith('/'):
        # Absolute import from root
        resolved = base_dir / import_path[1:]
    else:
        # Node module or external package
        return None
    
    # Try different extensions
    extensions = ['', '.ts', '.tsx', '.js', '.jsx', '/index.ts', '/index.tsx', '/index.js', '/index.jsx']
    for ext in extensions:
        test_path = Path(str(resolved) + ext)
        if test_path.exists():
            return test_path
    
    return resolved

def main():
    frontend_dir = Path('../frontend')
    src_dir = frontend_dir / 'src'
    
    if not src_dir.exists():
        print(f"Frontend src directory not found at: {src_dir}")
        return
    
    ts_files = get_ts_tsx_files(str(src_dir))
    
    all_imports = []
    for file_path in ts_files:
        imports = extract_imports(file_path)
        all_imports.extend(imports)
    
    # Analyze imports
    missing_imports = []
    
    for imp in all_imports:
        import_path = imp['import_path']
        
        # Skip external packages
        if not (import_path.startswith('.') or import_path.startswith('@/') or import_path.startswith('/')):
            if not import_path.startswith(('react', 'next', '@types', 'axios', 'lucide-react')):
                # Check if it might be a custom module
                resolved = resolve_import_path(import_path, Path(imp['file']), src_dir)
                if resolved and not resolved.exists():
                    missing_imports.append({
                        'file': imp['file'],
                        'line': imp['line'],
                        'import': import_path,
                        'resolved': str(resolved) if resolved else 'N/A'
                    })
        else:
            # Check local imports
            resolved = resolve_import_path(import_path, Path(imp['file']), src_dir)
            if resolved and not resolved.exists():
                missing_imports.append({
                    'file': imp['file'],
                    'line': imp['line'],
                    'import': import_path,
                    'resolved': str(resolved) if resolved else 'N/A'
                })
    
    # Report results
    print("=" * 80)
    print("FRONTEND IMPORT ANALYSIS REPORT")
    print("=" * 80)
    print()
    
    if missing_imports:
        print("MISSING IMPORTS:")
        print("-" * 40)
        # Group by file
        by_file = {}
        for item in missing_imports:
            file_key = item['file']
            if file_key not in by_file:
                by_file[file_key] = []
            by_file[file_key].append(item)
        
        for file_path, items in sorted(by_file.items()):
            print(f"\nFile: {file_path}")
            for item in sorted(items, key=lambda x: x['line']):
                print(f"  Line {item['line']}: import '{item['import']}'")
                if item['resolved'] != 'N/A':
                    print(f"    Expected at: {item['resolved']}")
    else:
        print("No missing imports found!")
    
    print("\n" + "=" * 80)
    print(f"Total missing imports: {len(missing_imports)}")
    print("=" * 80)

if __name__ == "__main__":
    main()