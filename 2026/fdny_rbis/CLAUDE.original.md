# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

NYC Council Data Team R project analyzing RBIS (racial/business impact) equality. Outputs reports/visuals for Council hearings.

## Running Code

Scripts are numbered and must run in order. Start with dependencies:

```r
# In RStudio or Rscript
source("code/00_load_dependencies.R")
```

Run subsequent scripts in numbered order. `councilverse` installs from GitHub (`newyorkcitycouncil/councilverse`) — requires `remotes` package and internet access.

## Architecture

- `code/` — numbered R scripts (`00_`, `01_`, etc.), run sequentially
- `data/input/` — raw source data (never modify)
- `data/output/` — cleaned/processed outputs from scripts
- `visuals/` — generated charts and maps
- `assets/` — CSS (`style.css`), LaTeX template (`template.tex`), logos for Rmd reports

Scripts follow single-responsibility: one script = one task. Rmd/notebook files orchestrate and call prior scripts for final output.

## Conventions (from Documentation.md)

**File names:** `dataset-name_time-granularity_grouping_year.extension`  
Example: `acs_unemployment_by-cd_2018.csv`

**Variable names:** `lower_case_underscore`, no dots (Python-compatibility).

**Script numbering:** Files that depend on others get a numeric prefix (`00`–`10`). Functions and sourced scripts go at the top of every file.

Avoid hard-coding values. Repeated code → parameterized function.
