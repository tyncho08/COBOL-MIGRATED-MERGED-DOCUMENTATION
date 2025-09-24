-- Most complex programs
SELECT program_id, complexity, source_path
FROM programs
ORDER BY complexity DESC
LIMIT 20;

-- Circular dependencies
WITH RECURSIVE dep_chain AS (
    SELECT caller_program, called_program, 
           caller_program || ' -> ' || called_program AS chain
    FROM dependencies
    UNION ALL
    SELECT d.caller_program, d.called_program,
           dc.chain || ' -> ' || d.called_program
    FROM dependencies d
    JOIN dep_chain dc ON d.caller_program = dc.called_program
    WHERE dc.chain NOT LIKE '%' || d.called_program || '%'
)
SELECT chain FROM dep_chain
WHERE chain LIKE '%' || SUBSTR(chain, 1, INSTR(chain, ' ') - 1) || '%';

-- Programs without callers (potential dead code)
SELECT p.program_id, p.source_path
FROM programs p
LEFT JOIN dependencies d ON p.program_id = d.called_program
WHERE d.called_program IS NULL
  AND p.program_type != 'main';

-- COPYBOOK impact analysis
SELECT c.copybook_name, COUNT(*) as usage_count,
       GROUP_CONCAT(p.program_id) as used_by
FROM copybook_usage cu
JOIN programs p ON cu.program_id = p.program_id
JOIN copybooks c ON cu.copybook_name = c.copybook_name
GROUP BY c.copybook_name
ORDER BY usage_count DESC;