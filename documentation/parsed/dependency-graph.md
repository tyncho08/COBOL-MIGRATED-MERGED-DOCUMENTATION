# Dependency Graph Data

## Statistics
- Total Programs: 278
- Total Copybooks: 185
- Total CALL relationships: 509
- Total COPY relationships: 408

## Graph Structure

### Nodes (463)
```json
[
  {
    "id": "sl910",
    "file": "sales/sl910.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 555,
      "lines": 2312
    }
  },
  {
    "id": "xl150",
    "file": "common/xl150.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 520,
      "lines": 1461
    }
  },
  {
    "id": "st030",
    "file": "stock/st030.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 433,
      "lines": 1765
    }
  },
  {
    "id": "sl920",
    "file": "sales/sl920.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 420,
      "lines": 1665
    }
  },
  {
    "id": "gl030",
    "file": "general/gl030.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 377,
      "lines": 1835
    }
  },
  {
    "id": "sys002",
    "file": "common/sys002.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 370,
      "lines": 2132
    }
  },
  {
    "id": "st020",
    "file": "stock/st020.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 351,
      "lines": 1622
    }
  },
  {
    "id": "st010",
    "file": "stock/st010.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 342,
      "lines": 1639
    }
  },
  {
    "id": "sl810",
    "file": "sales/sl810.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 308,
      "lines": 1314
    }
  },
  {
    "id": "sl810",
    "file": "purchase/sl810.cbl",
    "type": "sub",
    "metrics": {
      "complexity": 308,
      "lines": 1314
    }
  }
]
... (truncated)
```

### Edges (917)
```json
[
  {
    "from": "sl910",
    "to": "sl930",
    "type": "CALL",
    "line": 1819
  },
  {
    "from": "sl910",
    "to": "sl960",
    "type": "CALL",
    "line": 2229
  },
  {
    "from": "sl910",
    "to": "sl070",
    "type": "CALL",
    "line": 2388
  },
  {
    "from": "sl910",
    "to": "maps04",
    "type": "CALL",
    "line": 3367
  },
  {
    "from": "sl910",
    "to": "block",
    "type": "COPY",
    "line": 177
  },
  {
    "from": "sl910",
    "to": "of",
    "type": "COPY",
    "line": 185
  },
  {
    "from": "sl910",
    "to": "of",
    "type": "COPY",
    "line": 293
  },
  {
    "from": "sl910",
    "to": "books",
    "type": "COPY",
    "line": 622
  },
  {
    "from": "xl150",
    "to": "maps04",
    "type": "CALL",
    "line": 644
  },
  {
    "from": "xl150",
    "to": "of",
    "type": "COPY",
    "line": 323
  },
  {
    "from": "xl150",
    "to": "to",
    "type": "COPY",
    "line": 1808
  },
  {
    "from": "xl150",
    "to": "to",
    "type": "COPY",
    "line": 2046
  },
  {
    "from": "st030",
    "to": "SYSTEM",
    "type": "CALL",
    "line": 834
  },
  {
    "from": "st030",
    "to": "maps04",
    "type": "CALL",
    "line": 839
  },
  {
    "from": "st030",
    "to": "of",
    "type": "COPY",
    "line": 135
  },
  {
    "from": "sl920",
    "to": "sl070",
    "type": "CALL",
    "line": 2230
  },
  {
    "from": "sl920",
    "to": "maps04",
    "type": "CALL",
    "line": 2448
  },
  {
    "from": "sl920",
    "to": "of",
    "type": "COPY",
    "line": 131
  },
  {
    "from": "sl920",
    "to": "of",
    "type": "COPY",
    "line": 192
  },
  {
    "from": "gl030",
    "to": "SYSTEM",
    "type": "CALL",
    "line": 1679
  }
]
... (truncated)
```

## Visualization Instructions
To visualize this dependency graph:
1. Use the data above to create a directed graph
2. Nodes represent programs/copybooks
3. Edges represent dependencies (CALL or COPY)
4. Node size can represent complexity or lines of code
5. Edge color can differentiate CALL (blue) from COPY (green)
