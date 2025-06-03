# Install required packages 
install.packages(c("haven", "dplyr", "ggplot2", "broom", "car", "tidyr", "gridExtra"), dependencies = TRUE)

# Load libraries
library(haven)
library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(tidyr)
library(gridExtra)

# Load data
icu_data <- read_dta('icu_data.dta')

# Fill missing vitals
icu_data <- icu_data %>%
  group_by(patid) %>%
  fill(o2sat, hr, sbp, resp, map, .direction = "downup") %>%
  ungroup()

# Drop temp
icu_data <- select(icu_data, -temp)

# Get first value per patient
icu_first <- icu_data %>%
  arrange(patid, iculos) %>%
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

# Clean data (remove outliers and NAs)
icu_clean <- icu_first %>%
  filter(
    !is.na(sepsislabel),
    !is.na(o2sat), !is.na(age), !is.na(gender),
    !is.na(hr), !is.na(sbp), !is.na(resp),
    between(o2sat, 60, 100),
    between(hr, 30, 220),
    between(sbp, 50, 250),
    between(resp, 5, 60),
    between(age, 0, 120),
    sepsislabel %in% c(0, 1)
  ) %>%
  mutate(sepsislabel = as.numeric(sepsislabel))

# Fit model
model2 <- glm(sepsislabel ~ o2sat + age + gender + hr + sbp + resp, 
              data = icu_clean, 
              family = binomial())

# Summarize model into a clean table
model_summary <- tidy(model2) %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  )) %>%
  select(term, estimate, std.error, statistic, p.value, significance)

# Create a table image and save
table_plot <- tableGrob(model_summary)
ggsave("model_summary_table.png", table_plot, width = 8, height = 4)

# Model diagnostics
student_resid <- rstudent(model2)
leverage_vals <- hatvalues(model2)
dfb <- dfbetas(model2)

library(dplyr)

icu_diagnostics <- icu_clean %>%
  mutate(
    student_resid = student_resid,
    leverage = leverage_vals,
    dfbeta_o2sat = dfb[, "o2sat"]
  )

# Identify potential outliers
outliers <- icu_diagnostics %>%
  filter(
    abs(student_resid) > 2 |
      leverage > 2 * mean(leverage) |
      abs(dfbeta_o2sat) > 0.2
  )

# View flagged rows
print(outliers)

# Remove outliers before re-running model
icu_no_outliers <- icu_diagnostics %>%
  filter(
    abs(student_resid) <= 2,
    leverage <= 2 * mean(leverage),
    abs(dfbeta_o2sat) <= 0.2
  )

# Install and load detection tool
install.packages("brglm2")
library(brglm2)

# Refit with bias-reduction to handle separation
model3 <- glm(sepsislabel ~ o2sat + age + gender + hr + sbp + resp,
              data = icu_no_outliers,
              family = binomial(),
              method = "brglmFit")
summary(model3)

library(broom)
library(dplyr)
library(gridExtra)

coef_table <- tidy(model3) %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  )) %>%
  select(term, estimate, std.error, statistic, p.value, significance)

# Show table in plot viewer
grid.table(coef_table)

