# Healthcare Length-of-Stay (LoS) Prediction

**Analytics II (BC2407) group project — NTU Business Analytics**

## Problem
Singapore hospitals face overcrowding driven by unpredictable patient Length of Stay (LoS). Unplanned long stays strain bed capacity and scheduling. The goal: predict whether an admitted patient will have a short or long LoS so capacity can be planned proactively.

## Data
- Source: HDHI admission dataset (`HDHI_Admission_data.csv`, ~2.6 MB, real admission records)
- Preparation: cleaning, feature grouping, exploratory data analysis (see report Appendix A–C)

## Method
Built and compared three advanced predictive models:
- **Random Forest** — best overall: highest accuracy across all 3 evaluation metrics
- **XGBoost**
- **Neural Network**

Model selection favoured Random Forest for its reliability and for **minimising false positives** (predicting a short stay that becomes a long stay — the costly error for bed/scheduling planning).

## Deliverables
- Project report (Word) with executive summary for non-technical management
- Analysis scripts (Python/R)
- **Live Power BI dashboard** (not static screenshot) for presentation

## Tools
Python · R · Random Forest / XGBoost / Neural Network · Power BI · scikit-learn

## Files in repo
- `BC2407 S02 - Group 7 Project Report.docx` — full report
- `Code/` — model code
- `Dashboard - Power BI/` — dashboard files
- `HDHI_Admission_data.csv` — dataset

*Note: group project; my contribution = [INSERT YOUR PART — e.g. data prep, RF modelling, dashboard].*
