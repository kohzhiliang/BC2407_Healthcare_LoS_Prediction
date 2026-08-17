# Healthcare Length-of-Stay (LoS) Prediction

**Analytics II (BC2407) — NTU Business Analytics · group project**

## Overview
A predictive-analytics solution for optimising Singapore's healthcare capacity.
Hospital overcrowding is driven largely by unpredictable patient Length of Stay;
predicting whether an admission will be short or long lets management plan beds and
scheduling proactively rather than reactively.

## Problem
- Business problem: unplanned long stays strain bed capacity, staffing and scheduling.
- Success measure: a model that reliably flags long-stay risk early, with special
  care to avoid **false positives** (predicting a short stay that becomes a long stay —
  the costly error for bed/scheduling planning).

## Data
- `HDHI_Admission_data.csv` (~2.6 MB) — real hospital admission records.
- Steps: dataset sourcing, data preparation & cleaning, exploratory data analysis (EDA),
  feature/variable grouping (see report Appendices A–C).

## Method
Three advanced predictive models built and compared:
- **Random Forest** — selected as best overall
- **XGBoost**
- **Neural Network**

Model selection favoured Random Forest because it showed the highest predictive
accuracy across all three evaluation metrics and was the most reliable model for
minimising false positives in a practical bed-planning context.

## Deliverables
- Full project report (Word) written for a non-technical senior-management audience,
  with a one-page executive summary.
- Analysis scripts (Python / R).
- A **live Power BI dashboard** (not a static screenshot) presented in class.

## Tools & Techniques
Python · R · Random Forest · XGBoost · Neural Network · Power BI · scikit-learn ·
Exploratory Data Analysis · Feature Engineering

## Repository Structure
- `BC2407 S02 - Group 7 Project Report.docx` — full write-up (executive summary → conclusion)
- `Code/` — model code (Python/R)
- `Dashboard - Power BI/` — Power BI dashboard files
- `HDHI_Admission_data.csv` — dataset

## How to Explore
1. Open the project report for the business framing and key findings.
2. Run the scripts in `Code/` against `HDHI_Admission_data.csv` to reproduce results.
3. Open the Power BI file to interact with the live dashboard.

*Group project — NTU Business Analytics (BC2407), S02 Group 7.*
