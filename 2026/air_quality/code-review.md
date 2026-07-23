---
name: Code Review
about: Issue template to review project across 4 main areas
title: Code Review
labels: ''
assignees: ''

---

If there any of the following:

## File organization improvements
- none

## Analysis/Methods Clarifications
- Specified rate per 10k children in 01_eda.R and 02_asthma_corr.R
- Specified PM 2.5 mcg/m3 in 02_pm_corr.R
- We report SNAP recipiency as a significant predictor of childhood asthma hospitalization rate, but this is only true when we control for HMCV. Maybe we could look into a broader poverty metric, e.g. <200% FPL?

## Code Efficiency Suggestions/Alternatives
- Changed the way that inf/nan values are set to NA in 00_clean_data.R
- 02_asthma_corr.R has repetitive code that could be compressed into a function, but the current layout isn't inefficient.

## Runtime Errors
- none
