Messy Data Challenge (ASRR 2025)

Research Question:
Is oxygen saturation (O₂Sat) at the time of ICU admission associated with a subsequent diagnosis of sepsis?

Objective:
To determine whether O₂Sat measured at ICU admission predicts sepsis during the ICU stay, and how this association changes after adjusting for demographic and clinical variables.


---

Methods

Data Source: Electronic health records (EHR) from ICU patients across two U.S. hospitals.

Data Cleaning:

Inspected and visualized missingness; dropped temperature due to >60% missing data.
Imputed missing values (O₂Sat, HR, SBP, DBP, Resp, MAP) using time-aware single imputation.
Transformed data from long to wide format; created a binary sepsislabel outcome variable.


Feature Selection: Included age, gender, HR, SBP, and respiratory rate; excluded identifiers and highly correlated variables.

Modeling: Logistic regression assessing association between O₂Sat and sepsis.
Model 1: O₂Sat only
Model 2: Adjusted for confounders

Sensitivity Models: Excluded early sepsis cases, applied weights, and removed outliers.

Diagnostics: Checked for leverage, residuals, and influential points; refit models using bias-reduced logistic regression.
