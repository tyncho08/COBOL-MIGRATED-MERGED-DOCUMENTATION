-- Programs table
CREATE TABLE programs (
    program_id VARCHAR(30) PRIMARY KEY,
    source_path VARCHAR(255) NOT NULL,
    program_type VARCHAR(20),
    module VARCHAR(20),
    lines_of_code INTEGER,
    complexity INTEGER,
    last_modified TIMESTAMP
);

-- Dependencies table
CREATE TABLE dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    caller_program VARCHAR(30),
    called_program VARCHAR(30),
    call_type VARCHAR(20),
    line_number INTEGER,
    FOREIGN KEY (caller_program) REFERENCES programs(program_id),
    FOREIGN KEY (called_program) REFERENCES programs(program_id)
);

-- Copybooks table
CREATE TABLE copybooks (
    copybook_name VARCHAR(50) PRIMARY KEY,
    source_path VARCHAR(255),
    data_elements INTEGER,
    used_by_count INTEGER
);

-- Metrics table
CREATE TABLE metrics (
    program_id VARCHAR(30),
    metric_type VARCHAR(50),
    metric_value REAL,
    calculated_at TIMESTAMP,
    FOREIGN KEY (program_id) REFERENCES programs(program_id)
);

-- File usage table
CREATE TABLE file_usage (
    program_id VARCHAR(30),
    file_name VARCHAR(50),
    operation VARCHAR(10),
    access_mode VARCHAR(20),
    FOREIGN KEY (program_id) REFERENCES programs(program_id)
);

-- Copybook usage table
CREATE TABLE copybook_usage (
    program_id VARCHAR(30),
    copybook_name VARCHAR(50),
    line_number INTEGER,
    division VARCHAR(20),
    section VARCHAR(50),
    FOREIGN KEY (program_id) REFERENCES programs(program_id),
    FOREIGN KEY (copybook_name) REFERENCES copybooks(copybook_name)
);