library(tidyverse)
library(haven)
library(pROC)
library(caret)
library(car)


icu_data <- read_dta("icu_data.dta")
head(icu_data)
summary(icu_data)


icu_data %>%
  select(age, gender, iculos, hr, temp, sbp, dbp, resp, o2sat, map, sepsislabel) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(outlier.alpha = 0.1, fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplots of ICU Variables", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Convert categorical variables to factors
icu_data$hospid <- as.factor(icu_data$hospid)
icu_data$gender <- as.factor(icu_data$gender)

# Count missing values per variable
missing_counts <- colSums(is.na(icu_data))
print(missing_counts)




cat("===== Assessing Missingness Patterns using Logistic Regression =====\n")

icu_data_for_missingness_analysis <- icu_data

# Heart Rate (hr) missingness
cat("\n\n----- Logistic Regression for Missingness of: hr -----\n")
icu_data_for_missingness_analysis$is_missing_hr <- as.factor(as.integer(is.na(icu_data_for_missingness_analysis$hr)))

model_hr_pre <- glm(is_missing_hr ~ age + gender + iculos + hospid + sepsislabel + temp + sbp + dbp + o2sat + map + resp, 
                    data = icu_data_for_missingness_analysis, family = binomial(link = "logit"))
print(summary(model_hr_pre))

# Temperature (temp) missingness
cat("\n\n----- Logistic Regression for Missingness of: temp -----\n")
icu_data_for_missingness_analysis$is_missing_temp <- as.factor(as.integer(is.na(icu_data_for_missingness_analysis$temp)))

model_temp_pre <- glm(is_missing_temp ~ age + gender + iculos + hospid + sepsislabel + hr + sbp + dbp + o2sat + map + resp, 
                      data = icu_data_for_missingness_analysis, family = binomial(link = "logit"))
print(summary(model_temp_pre))

# Systolic Blood Pressure (sbp) missingness
cat("\n\n----- Logistic Regression for Missingness of: sbp -----\n")
icu_data_for_missingness_analysis$is_missing_sbp <- as.factor(as.integer(is.na(icu_data_for_missingness_analysis$sbp)))

model_sbp_pre <- glm(is_missing_sbp ~ age + gender + iculos + hospid + sepsislabel + hr + temp + dbp + o2sat + map + resp, 
                     data = icu_data_for_missingness_analysis, family = binomial(link = "logit"))
print(summary(model_sbp_pre))

# Diastolic Blood Pressure (dbp) missingness
cat("\n\n----- Logistic Regression for Missingness of: dbp -----\n")
icu_data_for_missingness_analysis$is_missing_dbp <- as.factor(as.integer(is.na(icu_data_for_missingness_analysis$dbp)))

model_dbp_pre <- glm(is_missing_dbp ~ age + gender + iculos + hospid + sepsislabel + hr + temp + sbp + o2sat + map + resp, 
                     data = icu_data_for_missingness_analysis, family = binomial(link = "logit"))
print(summary(model_dbp_pre))

# Oxygen Saturation (o2sat) missingness
cat("\n\n----- Logistic Regression for Missingness of: o2sat -----\n")
icu_data_for_missingness_analysis$is_missing_o2sat <- as.factor(as.integer(is.na(icu_data_for_missingness_analysis$o2sat)))

model_o2sat_pre <- glm(is_missing_o2sat ~ age + gender + iculos + hospid + sepsislabel + hr + temp + sbp + dbp + map + resp, 
                       data = icu_data_for_missingness_analysis, family = binomial(link = "logit"))
print(summary(model_o2sat_pre))

# Mean Arterial Pressure (map) missingness
cat("\n\n----- Logistic Regression for Missingness of: map -----\n")
icu_data_for_missingness_analysis$is_missing_map <- as.factor(as.integer(is.na(icu_data_for_missingness_analysis$map)))

model_map_pre <- glm(is_missing_map ~ age + gender + iculos + hospid + sepsislabel + hr + temp + sbp + dbp + o2sat + resp, 
                     data = icu_data_for_missingness_analysis, family = binomial(link = "logit"))
print(summary(model_map_pre))

# Respiratory Rate (resp) missingness
cat("\n\n----- Logistic Regression for Missingness of: resp -----\n")
icu_data_for_missingness_analysis$is_missing_resp <- as.factor(as.integer(is.na(icu_data_for_missingness_analysis$resp)))

model_resp_pre <- glm(is_missing_resp ~ age + gender + iculos + hospid + sepsislabel + hr + temp + sbp + dbp + o2sat + map, 
                      data = icu_data_for_missingness_analysis, family = binomial(link = "logit"))
print(summary(model_resp_pre))




# Impute missing values within each patient by carrying values forward and backward
icu_data_imputed <- icu_data %>%
  group_by(patid) %>%
  arrange(patid, iculos) %>% 
  tidyr::fill(hr, temp, sbp, dbp, o2sat, map, resp, .direction = "down") %>% 
  tidyr::fill(hr, temp, sbp, dbp, o2sat, map, resp, .direction = "up") %>%
  ungroup()
# Flag patients who developed sepsis at any point
icu_data_imputed <- icu_data_imputed %>%
  group_by(patid) %>%
  mutate(any_sepsis = as.numeric(any(sepsislabel == 1))) %>%
  ungroup()



cat("===== Checking Missingness After Imputation =====\n")

missing_counts_after <- colSums(is.na(icu_data_imputed))
cat("\nMissing counts after imputation:\n")
print(missing_counts_after)


cat("\n===== Comparison of Missing Values Before and After Imputation =====\n")
comparison_df <- data.frame(
  Variable = names(missing_counts),
  Before_Imputation = missing_counts,
  After_Imputation = missing_counts_after[names(missing_counts)],
  Reduction = missing_counts - missing_counts_after[names(missing_counts)]
)
print(comparison_df)



cat("\n===== Assessing Remaining Missingness Patterns After Imputation =====\n")

icu_data_for_post_imputation_analysis <- icu_data_imputed

# Heart Rate (hr) missingness
cat("\n\n----- Logistic Regression for Remaining Missingness of: hr -----\n")
icu_data_for_post_imputation_analysis$is_missing_hr_post_imp <- as.factor(as.integer(is.na(icu_data_for_post_imputation_analysis$hr)))

model_hr <- glm(is_missing_hr_post_imp ~ age + gender + iculos + hospid + sepsislabel + temp + sbp + dbp + o2sat + map + resp, 
                data = icu_data_for_post_imputation_analysis, family = binomial(link = "logit"))
print(summary(model_hr))

# Temperature (temp) missingness
cat("\n\n----- Logistic Regression for Remaining Missingness of: temp -----\n")
icu_data_for_post_imputation_analysis$is_missing_temp_post_imp <- as.factor(as.integer(is.na(icu_data_for_post_imputation_analysis$temp)))

model_temp <- glm(is_missing_temp_post_imp ~ age + gender + iculos + hospid + sepsislabel + hr + sbp + dbp + o2sat + map + resp, 
                  data = icu_data_for_post_imputation_analysis, family = binomial(link = "logit"))
print(summary(model_temp))

# Systolic Blood Pressure (sbp) missingness
cat("\n\n----- Logistic Regression for Remaining Missingness of: sbp -----\n")
icu_data_for_post_imputation_analysis$is_missing_sbp_post_imp <- as.factor(as.integer(is.na(icu_data_for_post_imputation_analysis$sbp)))

model_sbp <- glm(is_missing_sbp_post_imp ~ age + gender + iculos + hospid + sepsislabel + hr + temp + dbp + o2sat + map + resp, 
                 data = icu_data_for_post_imputation_analysis, family = binomial(link = "logit"))
print(summary(model_sbp))

# Diastolic Blood Pressure (dbp) missingness
cat("\n\n----- Logistic Regression for Remaining Missingness of: dbp -----\n")
icu_data_for_post_imputation_analysis$is_missing_dbp_post_imp <- as.factor(as.integer(is.na(icu_data_for_post_imputation_analysis$dbp)))

model_dbp <- glm(is_missing_dbp_post_imp ~ age + gender + iculos + hospid + sepsislabel + hr + temp + sbp + o2sat + map + resp, 
                 data = icu_data_for_post_imputation_analysis, family = binomial(link = "logit"))
print(summary(model_dbp))

# Oxygen Saturation (o2sat) missingness
cat("\n\n----- Logistic Regression for Remaining Missingness of: o2sat -----\n")
icu_data_for_post_imputation_analysis$is_missing_o2sat_post_imp <- as.factor(as.integer(is.na(icu_data_for_post_imputation_analysis$o2sat)))

model_o2sat <- glm(is_missing_o2sat_post_imp ~ age + gender + iculos + hospid + sepsislabel + hr + temp + sbp + dbp + map + resp, 
                   data = icu_data_for_post_imputation_analysis, family = binomial(link = "logit"))
print(summary(model_o2sat))

# Mean Arterial Pressure (map) missingness
cat("\n\n----- Logistic Regression for Remaining Missingness of: map -----\n")
icu_data_for_post_imputation_analysis$is_missing_map_post_imp <- as.factor(as.integer(is.na(icu_data_for_post_imputation_analysis$map)))

model_map <- glm(is_missing_map_post_imp ~ age + gender + iculos + hospid + sepsislabel + hr + temp + sbp + dbp + o2sat + resp, 
                 data = icu_data_for_post_imputation_analysis, family = binomial(link = "logit"))
print(summary(model_map))

# Respiratory Rate (resp) missingness
cat("\n\n----- Logistic Regression for Remaining Missingness of: resp -----\n")
icu_data_for_post_imputation_analysis$is_missing_resp_post_imp <- as.factor(as.integer(is.na(icu_data_for_post_imputation_analysis$resp)))

model_resp <- glm(is_missing_resp_post_imp ~ age + gender + iculos + hospid + sepsislabel + hr + temp + sbp + dbp + o2sat + map, 
                  data = icu_data_for_post_imputation_analysis, family = binomial(link = "logit"))
print(summary(model_resp))
Missingness Mechanism Assessment:




Most variables show Missing at Random (MAR) patterns:

Temperature (temp): Significant predictors include age, gender, ICU length of stay, hospital, sepsis status, and other vital signs. The missingness depends on observed covariates.

Diastolic Blood Pressure (dbp): Strong associations with age, gender, ICU stay length, and other vital signs indicate missingness depends on observable factors.

Oxygen Saturation (o2sat): Missingness related to age, gender, hospital, temperature, and respiratory rate - systematic patterns based on observed data.

Mean Arterial Pressure (map): Significant predictors include age, gender, ICU length, and multiple vital signs.

Respiratory Rate (resp): Missingness associated with age, gender, hospital, sepsis status, and other measures.




Heart Rate (hr) and Systolic Blood Pressure (sbp) appear closest to Missing Completely at Random (MCAR) because there are convergence issues (perfect separation) in the logistic reression,
there very few remaining missing values after forward/backward fill.


Key Insights:
Hospital Effects: Strong hospidB coefficients across multiple variables suggest systematic differences in data collection practices between hospitals.

Iculos (ICU length of stay) is significant in many models, indicating missingness patterns change over time of stay, maybe more or less measurements taken as patient gets better/worse.

Clinical decisions may impact measurements taken with multiple measurements taken simultaneously probably for certain decisions.

Age and gender significantly predict missingness, indicating systematic patterns based on patient demographics.

Conclusion:
The missingness is primarily MAR (Missing at Random) rather than MCAR, as it depends on observed covariates like hospital, patient characteristics, and other vital signs.
This supports the use of imputation methods that leverage these relationships. For MAR maybe therefore need to use more sophisticated imputation methods like multiple imputation or regression imputation.
