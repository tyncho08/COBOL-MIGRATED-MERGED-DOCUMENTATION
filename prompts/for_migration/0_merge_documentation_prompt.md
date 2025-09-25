# COMPREHENSIVE COBOL ANALYSIS — ACAS SYSTEM (Improved Claude Prompt)

## Overview
You are an expert COBOL analyst, legacy-systems architect, and technical writer with deep experience extracting business logic from large procedural codebases. Your job is to read, cross-check, consolidate and produce a single authoritative Markdown documentation file for the legacy ACAS (Applewood Computers Accounting System) codebase and the AI-generated analyses already produced.

## Objective
Using the provided AI-generated analyses + the original legacy code under Legacy_App/, produce a single clean, exhaustive, validated Markdown document that will serve as the canonical documentation of the original COBOL project. Remove duplicates and discrepancies, reconcile conflicts, preserve every business rule, and clearly flag any uncertainty or decisions made.

## Inputs
* Original legacy source code (canonical): path Legacy_App/ (use as absolute source of truth for any ambiguity).
* AI-generated analyses (multiple folders):    
    * documentation/ken/ — per-file LLM-generated internal process notes (THIS IS MEGA IMPORTANT INFORMATION!)
    * documentation/parsed/ — parser outputs & JSON representations
    * documentation/parsed/parser_analysis/ — advanced metrics & visualizations
    * documentation/functional/ — business-process documentation
    * documentation/subsystems/ — subsystem identification & architecture

## Core rules / constraints
1. Legacy_App is the single source of truth. When documentation conflicts with code, prefer the code unless code is ambiguous — in which case record the ambiguity and the rationale for the chosen interpretation.
2. You MUST analyze and use EVERY file of the AI-generated analyses on documentation folder. I need 100% coverage here!
2. Produce a single Markdown file as the final canonical doc, final_documentation/merged_documentation.md

## Deliverables
1. final_documentation/merged_documentation.md — the canonical Markdown doc.