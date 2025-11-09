# 1. How you will deal with missing data (note: we do not expect you to use multiple imputation)

  #seeing data structure 

glimpse(icu_data)

  #check level of missing data 

colMeans(is.na(icu_data)) * 100

  #imputation 

icu_data <- icu_data %>%
  group_by(patid) %>%
  fill(o2sat, hr, sbp, dbp, resp, map, .direction = "downup") %>%
  ungroup()

  #drop temp 

icu_data <- select(icu_data, -temp)

# 2. What are the likely mechanisms for missing data

  #Based on exploratory checks, the missingness of key vital signs (e.g., O2Sat, SBP, DBP) 
  #appears to be dependent on other observed variables such as gender or ICU length of stay, 
  #rather than missing completely at random. Therefore, 
  #we assume the missing data mechanism is Missing At Random (MAR). Under this assumption, 
  #we applied time-aware imputation within each patient (carrying values forward and backward) 
  #to preserve temporal relationships while minimizing information loss. 
  #Variables with excessive missingness (e.g., temperature) were excluded from the analysis.

# 3. How you will transform multiple rows per patient (long data) into one row of data for modelling (wide data)

  #The dataset is in long format, with multiple time-stamped measurements per patient. 
  #To transform it into a wide format suitable for regression, we selected the first observation for each patient 
  #(i.e., the earliest time point after ICU admission). This reflects the clinical goal of 
  # evaluating whether vital signs at admission (e.g., O2Sat) are predictive of subsequent sepsis diagnosis.

  #Take only the first row per patient based on ICU time order (iculos)
icu_first <- icu_data %>%
  arrange(patid, iculos) %>%      # sort by patient and time
  group_by(patid) %>%
  slice(1) %>%                    # take the first line
  ungroup()

  #distribution of sepsislabel
table(icu_first$sepsislabel)
prop.table(table(icu_first$sepsislabel))

  #re start our model, with patient with all time sepsis 
icu_first2 <- icu_data %>%
  group_by(patid) %>%
  summarise(
    age = first(age),
    gender = first(gender),
    o2sat = first(o2sat),
    hr = first(hr),
    sbp = first(sbp),
    resp = first(resp),
    sepsislabel = max(sepsislabel)  
  ) %>%
  ungroup()

  #distribution of new sepsislabel
table(icu_first2$sepsislabel)
prop.table(table(icu_first2$sepsislabel))


# 4. What type of regression model you will use

# used logistic regression to model the binary outcome sepsislabel. 
#The first model included only o2sat as the predictor. 
#In the second model, we adjusted for additional covariates including 
#age, gender, heart rate, systolic blood pressure, and respiratory rate. 
#The models were fitted using glm() with family = "binomial" in R.

  #simple LM
model1 <- glm(sepsislabel ~ o2sat, data = icu_first2, family = "binomial")
summary(model1)

  #simple LM + control 
model2 <- glm(sepsislabel ~ o2sat + age + gender + hr + sbp + resp, data = icu_first2, family = "binomial")
summary(model2)

  # In the unadjusted model, O2Sat was not significantly associated with sepsis (p = 0.169). 
  # However, after adjusting for age, gender, heart rate, systolic blood pressure, and respiratory rate, 
  # O2Sat became a statistically significant predictor (p < 0.001). All included covariates showed strong and 
  # significant associations with sepsis, suggesting that early physiological measurements can be informative 
  # for identifying patients at higher risk.

# 5. How you will decide which variables to include in your model

  #We selected variables based on their clinical relevance to sepsis and prior knowledge. 
  #O2Sat was our main exposure of interest. We included age, gender, heart rate, and systolic blood pressure 
  #as potential confounders. We excluded variables like diastolic BP and MAP due to their strong correlation 
  #with SBP, to avoid multicollinearity. We considered including respiratory rate in a sensitivity analysis.

# 6. Whether you will exclude any patients from your analysis (and why)

  #We excluded patients with missing values in key variables that were required for modeling. 
  #This included patients who lacked valid values for O2Sat, heart rate, SBP, or respiratory rate at admission. 
  #Patients without any sepsis label over time were also removed. In total, 241 patients were excluded due to 
  # missingness during model fitting, as reported by the glm() function. 
  #These exclusions were necessary to ensure reliable model estimation and interpretation.

# 7. Sensitivity analyses you could perform to test assumptions










#----------- Fig and Pict-----------#

### SLIDE 1

# before imputasi
missing_before <- icu_data %>%
  summarise(across(everything(), ~mean(is.na(.))*100)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_percent")

# Plot
ggplot(missing_before, aes(x = reorder(variable, -missing_percent), y = missing_percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "% Missing per Variable (Before Cleaning)", y = "% Missing", x = "Variable")

# after imputasi
missing_before <- icu_data %>%
  summarise(across(everything(), ~mean(is.na(.))*100)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_percent")

# Plot
ggplot(missing_before, aes(x = reorder(variable, -missing_percent), y = missing_percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "% Missing per Variable (After Cleaning)", y = "% Missing", x = "Variable")





