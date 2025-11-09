library(haven)
library(dplyr)
library(tidyr)
library(broom)

icu_data<-read_dta('icu_data.dta')

icu_data <- icu_data %>%
  group_by(patid) %>%
  fill(o2sat, hr, sbp, dbp, resp, map, .direction = "downup") %>%
  ungroup()

icu_data <- select(icu_data, -temp)

icu_first2 <- icu_data %>%
  arrange(patid,iculos) %>%
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

model2 <- glm(sepsislabel ~ o2sat + age + gender + hr + sbp + resp, data = icu_first2, family = "binomial")
summary(model2)

#Note that all of the following models need to be replaced by the final model
#derived in part 5

#Subgroup Analyses(as a supplementry to the primary analyses)


# By gender
model_gender_0 <- glm(sepsislabel ~ o2sat + age + hr + sbp + resp, data = filter(icu_first2,gender == 0), family = "binomial")
model_gender_1 <- glm(sepsislabel ~ o2sat + age + hr + sbp + resp, data = filter(icu_first2,gender == 1), family = "binomial")
results_gender <- bind_rows(
  tidy(model_gender_0 ) %>% mutate(gender = "Female"),
  tidy(model_gender_1) %>% mutate(gender = "Male")
)

print(results_gender)

# By age group
# Did not balance the number of individuals across age groups:
# as the skewed age distribution in this dataset likely reflects the true age distribution of ICU patients.
# However, note that age < 20 has much smaller samples, therefore, lower statistical power
icu_first2 <- icu_first2 %>%
  mutate(age_group = case_when(
    age < 20 ~ 1,
    age >= 20 & age < 40 ~ 2,
    age >= 40 & age < 60 ~ 3,
    age >= 60 & age < 80 ~ 4,
    age >= 80 ~ 5
  ))
model_age1 <- glm(sepsislabel ~ o2sat + gender + hr + sbp + resp, data = filter(icu_first2,age_group == 1), family = "binomial")
model_age2 <- glm(sepsislabel ~ o2sat + gender + hr + sbp + resp, data = filter(icu_first2,age_group == 2), family = "binomial")
model_age3 <- glm(sepsislabel ~ o2sat + gender + hr + sbp + resp, data = filter(icu_first2,age_group == 3), family = "binomial")
model_age4 <- glm(sepsislabel ~ o2sat + gender + hr + sbp + resp, data = filter(icu_first2,age_group == 4), family = "binomial")
model_age5 <- glm(sepsislabel ~ o2sat + gender + hr + sbp + resp, data = filter(icu_first2,age_group == 5), family = "binomial")

results_age_group <- bind_rows(
  tidy(model_age1 ) %>% mutate(age_group = "< 20"),
  tidy(model_age2 ) %>% mutate(age_group = "20-40"),
  tidy(model_age3 ) %>% mutate(age_group = "40-60"),
  tidy(model_age4 ) %>% mutate(age_group = "60-80"),
  tidy(model_age5 ) %>% mutate(age_group = ">= 80")
)

print(results_age_group,n =Inf)

# by iculos(<= 24 hours and > 24 hours)
# Create another dataset including the total lenth of stay(identified by the last record of iculos)

icu_first3 <- icu_data %>%
  group_by(patid) %>%
  summarise(
    age = first(age),
    gender = first(gender),
    o2sat = first(o2sat),
    hr = first(hr),
    sbp = first(sbp),
    resp = first(resp),
    sepsislabel = max(sepsislabel), 
    iculos = max(iculos)
  ) %>%
  ungroup()

icu_first3 <- icu_first3 %>%
  mutate(iculos_group = case_when(
    iculos <= 24 ~ 1,
    iculos > 24 ~ 2
  ))
model_iculos_1 <- glm(sepsislabel ~ o2sat + age + gender + hr + sbp + resp, data = filter(icu_first3,iculos_group == 1), family = "binomial")
model_iculos_2 <- glm(sepsislabel ~ o2sat + age + gender + hr + sbp + resp, data = filter(icu_first3,iculos_group == 2), family = "binomial")
results_isulos <- bind_rows(
  tidy(model_iculos_1 ) %>% mutate(iculos_group = "<= 24"),
  tidy(model_iculos_2) %>% mutate(iculos_group = "> 24")
)

print(results_isulos)
# 6. Whether you will exclude any patients from your analysis (and why)

  #Excluded patients with missing values in key variables required for modeling after imputation,
  #including: O2Sat, heart rate, SBP, or respiratory rate at admission. 

# 7. Sensitivity analyses you could perform to test assumptions
# Remove patients identified as having sepsis within the first 6 hours from admission to icu
early_sepsis_patients <- icu_data %>%
  filter(sepsislabel == 1, iculos <= 6) %>%
  pull(patid) %>%
  unique()
icu_data_0 <- icu_data %>%
  filter(!patid %in% early_sepsis_patients)

icu_first4 <- icu_data_0 %>%
  arrange(patid,iculos) %>%
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

# re-fit the model and compare with the previous results
model2_1 <- glm(sepsislabel ~ o2sat + age + gender + hr + sbp + resp, data = icu_first4, family = "binomial")


# Adjusting weight(inverse frequency weights) to handle imbalanced data
# NAs removed, as this may affect the calculation of weights
icu_first2 <- na.omit(icu_first2)

class_weights <- icu_first2 %>%
  count(sepsislabel) %>%
  mutate(weight = (1/n) / sum(1/n)) 

model_sens <- glm(
  sepsislabel ~ o2sat + age + gender + hr + sbp + resp,
  data = icu_first2,
  family = binomial(),
  weights = ifelse(sepsislabel == 1, class_weights$weight[2], class_weights$weight[1])
)

summary(model_sens)
or_ci_weighted <- tidy(model_sens, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)
print(or_ci_weighted)


# excluding all outliers in the variables used for modelling
# if the results are similar: our model is robust to outliers
# o.w. model is sensitive to extrme values and requires careful handling of outliers
vars <- c("age", "hr", "sbp", "resp", "o2sat") # need to be replaced by thoes in the final model
clean_data <- icu_data %>%
  filter(if_all(all_of(vars), ~ {
    q1 <- quantile(.x, 0.25, na.rm = TRUE)
    q3 <- quantile(.x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    .x >= (q1 - 1.5 * iqr) & .x <= (q3 + 1.5 * iqr)
  }))
icu_first5 <- clean_data %>%
  group_by(patid) %>%
  summarise(
    age = first(age),
    gender = first(gender),
    o2sat = first(o2sat),
    hr = first(hr),
    sbp = first(sbp),
    resp = first(resp),
    sepsislabel = max(sepsislabel), 
    iculos = max(iculos)
  ) %>%
  ungroup()
# re-fit the model and compare with the previous results
model2 <- glm(sepsislabel ~ o2sat + age + gender + hr + sbp + resp, data = icu_first5, family = "binomial")
summary(model2)

