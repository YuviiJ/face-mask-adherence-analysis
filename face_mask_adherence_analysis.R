
# Load packages
library(tidyverse)
library(inspectdf)
library(tidymodels)
library(caret)
library(ranger)
library(xgboost)
library(vip)
library(yardstick)
library(lubridate)
library(stringr)
library(scales)
library(ggplot2)
set.seed(1933162)

# Read data
aus_raw <- read_csv("australia.csv", show_col_types = FALSE)

dim(aus_raw)
head(aus_raw, 5)

# Select required variables
aus1 <- aus_raw %>%
  select(
    RecordNo, endtime, qweek, weight,
    age, gender, state, household_size, employment_status,
    WCRex2, WCRV_4, cantril_ladder,
    PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4,
    i12_health_1:i12_health_17
  )


# Convert blank strings to NA
aus1 <- aus1 %>%
  mutate(across(where(is.character), ~na_if(str_trim(.x), "")))

inspect_na(aus1)


# Convert date and week
aus1 <- aus1 %>%
  mutate(
    endtime = dmy_hm(endtime),
    qweek_num = as.numeric(str_extract(qweek, "\\d+"))
  )


# Recode categorical variables
aus1 <- aus1 %>%
  mutate(
    gender = as.factor(gender),
    state = as.factor(state)
  )

aus1 <- aus1 %>%
  mutate(
    employment_status = case_when(
      employment_status %in% c(
        "Full time employment",
        "Working full time (35 or more hours per week)"
      ) ~ "Full time",
      employment_status %in% c(
        "Part time employment",
        "Working part time (8-34 hours a week)",
        "Working part time (Less than 8 hours a week)"
      ) ~ "Part time",
      employment_status %in% c(
        "Unemployed",
        "Not working - looking for work"
      ) ~ "Unemployed",
      employment_status %in% c("Retired") ~ "Retired",
      employment_status %in% c(
        "Not working",
        "Not working - not looking for work"
      ) ~ "Not working",
      TRUE ~ employment_status
    ),
    employment_status = as.factor(employment_status)
  )


# Recode household size

aus1 <- aus1 %>%
  mutate(
    household_size = case_when(
      household_size == "8 or more" ~ "8",
      household_size %in% c("Don't know", "Prefer not to say") ~ NA_character_,
      TRUE ~ household_size
    ),
    household_size = as.numeric(household_size)
  )


# Recode trust in government

aus1 <- aus1 %>%
  mutate(
    trust_gov = case_when(
      WCRex2 == "No confidence at all" ~ 1,
      WCRex2 == "Not very much confidence" ~ 2,
      WCRex2 == "A fair amount of confidence" ~ 3,
      WCRex2 == "A lot of confidence" ~ 4,
      WCRex2 == "Don't know" ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


# Recode illness threat

aus1 <- aus1 %>%
  mutate(
    illness_threat = case_when(
      WCRV_4 == "I am not at all scared that I will contract the Coronavirus (COVID-19)" ~ 1,
      WCRV_4 == "I am not very scared that I will contract the Coronavirus (COVID-19)" ~ 2,
      WCRV_4 == "I am fairly scared that I will contract the Coronavirus (COVID-19)" ~ 3,
      WCRV_4 == "I am very scared that I will contract the Coronavirus (COVID-19)" ~ 4,
      WCRV_4 %in% c(
        "Don't know",
        "Not applicable - I have already contracted Coronavirus (COVID-19)"
      ) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


# Recode wellbeing
aus1 <- aus1 %>%
  mutate(
    cantril_ladder = as.numeric(cantril_ladder)
  )


#  Recode PHQ-4 items
aus1 <- aus1 %>%
  mutate(
    PHQ4_1 = case_when(
      PHQ4_1 == "Not at all" ~ 0,
      PHQ4_1 == "Several days" ~ 1,
      PHQ4_1 == "More than half the days" ~ 2,
      PHQ4_1 == "Nearly every day" ~ 3,
      PHQ4_1 == "Prefer not to say" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    PHQ4_2 = case_when(
      PHQ4_2 == "Not at all" ~ 0,
      PHQ4_2 == "Several days" ~ 1,
      PHQ4_2 == "More than half the days" ~ 2,
      PHQ4_2 == "Nearly every day" ~ 3,
      PHQ4_2 == "Prefer not to say" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    PHQ4_3 = case_when(
      PHQ4_3 == "Not at all" ~ 0,
      PHQ4_3 == "Several days" ~ 1,
      PHQ4_3 == "More than half the days" ~ 2,
      PHQ4_3 == "Nearly every day" ~ 3,
      PHQ4_3 == "Prefer not to say" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    PHQ4_4 = case_when(
      PHQ4_4 == "Not at all" ~ 0,
      PHQ4_4 == "Several days" ~ 1,
      PHQ4_4 == "More than half the days" ~ 2,
      PHQ4_4 == "Nearly every day" ~ 3,
      PHQ4_4 == "Prefer not to say" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    phq4_total = PHQ4_1 + PHQ4_2 + PHQ4_3 + PHQ4_4
  )


# Recode behaviour variables
behaviour_cols <- paste0("i12_health_", 1:17)

aus1 <- aus1 %>%
  mutate(
    across(
      all_of(behaviour_cols),
      ~case_when(
        .x == "Not at all" ~ 1,
        .x == "Rarely" ~ 2,
        .x == "Sometimes" ~ 3,
        .x == "Frequently" ~ 4,
        .x == "Always" ~ 5,
        TRUE ~ NA_real_
      )
    )
  )


# Create target variable

mask_cols <- c(
  "i12_health_1",
  "i12_health_9",
  "i12_health_10",
  "i12_health_11"
)

all_behaviour_cols <- paste0("i12_health_", 1:17)
other_behaviour_cols <- setdiff(all_behaviour_cols, mask_cols)

aus1 <- aus1 %>%
  rowwise() %>%
  mutate(
    face_mask_score = median(c_across(all_of(mask_cols)), na.rm = TRUE),
    other_behaviour_score = median(c_across(all_of(other_behaviour_cols)), na.rm = TRUE),
    general_behaviour_score = median(c_across(all_of(all_behaviour_cols)), na.rm = TRUE)
  ) %>%
  ungroup()

aus1 <- aus1 %>%
  mutate(
    face_mask_score = ifelse(is.infinite(face_mask_score), NA, face_mask_score),
    other_behaviour_score = ifelse(is.infinite(other_behaviour_score), NA, other_behaviour_score),
    general_behaviour_score = ifelse(is.infinite(general_behaviour_score), NA, general_behaviour_score)
  )

aus1 <- aus1 %>%
  mutate(
    face_mask_binary = ifelse(face_mask_score >= 4, 1, 0),
    general_behaviour_binary = ifelse(general_behaviour_score >= 4, 1, 0)
  )

aus1 <- aus1 %>%
  mutate(
    face_mask_binary = factor(face_mask_binary, levels = c(0, 1), labels = c("No", "Yes")),
    general_behaviour_binary = factor(general_behaviour_binary, levels = c(0, 1), labels = c("No", "Yes"))
  )


# Create cleaned dataset

aus_clean <- aus1 %>%
  select(
    RecordNo, endtime, qweek, qweek_num, weight,
    age, gender, state, household_size, employment_status,
    trust_gov, illness_threat, cantril_ladder, phq4_total,
    all_of(behaviour_cols),
    face_mask_score, other_behaviour_score, general_behaviour_score,
    face_mask_binary, general_behaviour_binary
  )

aus_model <- aus_clean %>%
  filter(
    !is.na(age) &
      !is.na(gender) &
      !is.na(state) &
      !is.na(household_size) &
      !is.na(employment_status) &
      !is.na(trust_gov) &
      !is.na(illness_threat) &
      !is.na(cantril_ladder) &
      !is.na(phq4_total) &
      !is.na(face_mask_binary)
  )

dim(aus_model)
count(aus_model, face_mask_binary)
inspect_na(aus_model)

write_csv(aus_model, "australia_cleaned_phase1.csv")


# Create final model-ready dataset
# RecordNo and endtime are removed.
# Mask component variables are removed from predictors to reduce target leakage.
# general_behaviour_binary is removed because it is another outcome variable.

aus_model_ready <- aus_model %>%
  select(
    face_mask_binary,
    qweek_num,
    age, gender, state, household_size, employment_status,
    trust_gov, illness_threat, cantril_ladder, phq4_total,
    i12_health_2, i12_health_3, i12_health_4, i12_health_5,
    i12_health_6, i12_health_7, i12_health_8,
    i12_health_12, i12_health_13, i12_health_14,
    i12_health_15, i12_health_16, i12_health_17
  ) %>%
  filter(
    !is.na(qweek_num) &
      !is.na(i12_health_2) &
      !is.na(i12_health_3) &
      !is.na(i12_health_4) &
      !is.na(i12_health_5) &
      !is.na(i12_health_6) &
      !is.na(i12_health_7) &
      !is.na(i12_health_8) &
      !is.na(i12_health_12) &
      !is.na(i12_health_13) &
      !is.na(i12_health_14) &
      !is.na(i12_health_15) &
      !is.na(i12_health_16) &
      !is.na(i12_health_17)
  )

aus_model_ready <- aus_model_ready %>%
  mutate(
    face_mask_binary = factor(face_mask_binary, levels = c("Yes", "No"))
  )

dim(aus_model_ready)
inspect_na(aus_model_ready)
count(aus_model_ready, face_mask_binary)

write_csv(aus_model_ready, "australia_model_ready_final.csv")


# MODELLING WITH 5-FOLD CV AND TUNING

# Train-test split

mask_split <- initial_split(
  aus_model_ready,
  prop = 0.8,
  strata = face_mask_binary
)

mask_train <- training(mask_split)
mask_test  <- testing(mask_split)

count(mask_train, face_mask_binary)
count(mask_test, face_mask_binary)


# 5-fold cross-validation on training data

mask_folds <- vfold_cv(
  mask_train,
  v = 5,
  strata = face_mask_binary
)


# Recipe

mask_recipe <- recipe(face_mask_binary ~ ., data = mask_train) %>%
  step_dummy(all_nominal_predictors())


# Metrics

model_metrics <- metric_set(roc_auc, accuracy)


# RANDOM FOREST TUNING

rf_spec <- rand_forest(
  mode = "classification",
  trees = 500,
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "permutation")

rf_workflow <- workflow() %>%
  add_recipe(mask_recipe) %>%
  add_model(rf_spec)

rf_grid <- grid_regular(
  mtry(range = c(3, 15)),
  min_n(range = c(2, 10)),
  levels = 4
)

rf_tuned <- tune_grid(
  rf_workflow,
  resamples = mask_folds,
  grid = rf_grid,
  metrics = model_metrics
)

rf_cv_metrics <- rf_tuned %>%
  collect_metrics()

rf_cv_metrics

best_rf <- rf_tuned %>%
  select_best(metric = "roc_auc")

best_rf

final_rf_workflow <- rf_workflow %>%
  finalize_workflow(best_rf)

final_rf_fit <- final_rf_workflow %>%
  fit(data = mask_train)

rf_test_results <- mask_test %>%
  bind_cols(predict(final_rf_fit, new_data = mask_test, type = "class")) %>%
  bind_cols(predict(final_rf_fit, new_data = mask_test, type = "prob"))

rf_auc <- rf_test_results %>%
  roc_auc(truth = face_mask_binary, .pred_Yes)

rf_accuracy <- rf_test_results %>%
  accuracy(truth = face_mask_binary, estimate = .pred_class)

rf_confusion <- rf_test_results %>%
  conf_mat(truth = face_mask_binary, estimate = .pred_class)

rf_auc
rf_accuracy
rf_confusion

# XGBOOST TUNING

xgb_spec <- boost_tree(
  mode = "classification",
  trees = 300,
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = tune()
) %>%
  set_engine("xgboost", verbosity = 0)

xgb_workflow <- workflow() %>%
  add_recipe(mask_recipe) %>%
  add_model(xgb_spec)

xgb_grid <- grid_regular(
  tree_depth(range = c(3, 6)),
  learn_rate(range = c(-3, -1)),
  min_n(range = c(2, 6)),
  levels = 2
)

xgb_tuned <- tune_grid(
  xgb_workflow,
  resamples = mask_folds,
  grid = xgb_grid,
  metrics = model_metrics
)

xgb_cv_metrics <- xgb_tuned %>%
  collect_metrics()

xgb_cv_metrics

best_xgb <- xgb_tuned %>%
  select_best(metric = "roc_auc")

best_xgb

final_xgb <- finalize_workflow(xgb_workflow, best_xgb) %>%
  fit(data = mask_train)

xgb_test_results <- mask_test %>%
  bind_cols(predict(final_xgb, mask_test, type = "class")) %>%
  bind_cols(predict(final_xgb, mask_test, type = "prob"))

xgb_auc <- xgb_test_results %>%
  roc_auc(truth = face_mask_binary, .pred_Yes)

xgb_accuracy <- xgb_test_results %>%
  accuracy(truth = face_mask_binary, estimate = .pred_class)

xgb_confusion <- xgb_test_results %>%
  conf_mat(truth = face_mask_binary, estimate = .pred_class)

xgb_auc
xgb_accuracy
xgb_confusion

# PHASE 3: FINAL MODEL COMPARISON

final_model_comparison <- bind_rows(
  rf_auc %>% mutate(model = "Tuned Random Forest"),
  xgb_auc %>% mutate(model = "Tuned XGBoost")
)

final_model_comparison

write_csv(final_model_comparison, "final_model_comparison.csv")
write_csv(rf_test_results, "rf_test_predictions.csv")
write_csv(xgb_test_results, "xgb_test_predictions.csv")


# theme and colours

theme_report <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

mask_cols_plot <- c("Yes" = "#2A9D8F", "No" = "#E76F51")


#  Class distribution

p_class <- ggplot(aus_model_ready, aes(x = face_mask_binary, fill = face_mask_binary)) +
  geom_bar(width = 0.65) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, size = 4) +
  scale_fill_manual(values = mask_cols_plot) +
  labs(
    title = "Distribution of Face Mask Adherence",
    subtitle = "Number of respondents classified as adherent or non-adherent",
    x = "Face mask adherence",
    y = "Count",
    fill = "Adherence"
  ) +
  theme_report

p_class

ggsave("report_01_class_distribution.png", plot = p_class, width = 8, height = 5, dpi = 300)


# Face mask adherence by state

p_state <- aus_model_ready %>%
  count(state, face_mask_binary) %>%
  group_by(state) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = state, y = prop, fill = face_mask_binary)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_fill_manual(values = mask_cols_plot) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Face Mask Adherence by Australian State",
    subtitle = "Proportion of respondents classified as adherent or non-adherent",
    x = "State",
    y = "Proportion of respondents",
    fill = "Adherence"
  ) +
  theme_report

p_state

ggsave("report_02_state_mask.png", plot = p_state, width = 9, height = 6, dpi = 300)


#  Illness threat boxplot

p_threat_box <- ggplot(
  aus_model_ready,
  aes(x = face_mask_binary, y = illness_threat, fill = face_mask_binary)
) +
  geom_boxplot(alpha = 0.85, width = 0.55, outlier.alpha = 0.25) +
  scale_fill_manual(values = mask_cols_plot) +
  labs(
    title = "Perceived Illness Threat by Face Mask Adherence",
    subtitle = "Higher values indicate greater fear of contracting COVID-19",
    x = "Face mask adherence",
    y = "Illness threat level",
    fill = "Adherence"
  ) +
  theme_report

p_threat_box

ggsave("report_03_illness_threat_boxplot.png", plot = p_threat_box, width = 8, height = 5, dpi = 300)

# Face mask adherence by trust in government

p_trust <- aus_model_ready %>%
  count(trust_gov, face_mask_binary) %>%
  group_by(trust_gov) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(trust_gov), y = prop, fill = face_mask_binary)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = mask_cols_plot) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Face Mask Adherence by Trust in Government",
    subtitle = "Proportion of mask-adherent and non-adherent respondents by trust level",
    x = "Trust in Government Level",
    y = "Proportion of respondents",
    fill = "Mask Adherence"
  ) +
  theme_report

p_trust

ggsave("report_04_trust_government_bar.png", plot = p_trust, width = 8, height = 5, dpi = 300)

#  PHQ-4 score boxplot

p_phq_box <- ggplot(
  aus_model_ready,
  aes(x = face_mask_binary, y = phq4_total, fill = face_mask_binary)
) +
  geom_boxplot(alpha = 0.85, width = 0.55, outlier.alpha = 0.25) +
  scale_fill_manual(values = mask_cols_plot) +
  labs(
    title = "PHQ-4 Score by Face Mask Adherence",
    subtitle = "PHQ-4 captures anxiety and depressive symptom burden",
    x = "Face mask adherence",
    y = "PHQ-4 total score",
    fill = "Adherence"
  ) +
  theme_report

p_phq_box

ggsave("report_05_phq4_boxplot.png", plot = p_phq_box, width = 8, height = 5, dpi = 300)

# ROC curve comparison

rf_roc <- rf_test_results %>%
  roc_curve(truth = face_mask_binary, .pred_Yes) %>%
  mutate(model = "Tuned Random Forest")

xgb_roc <- xgb_test_results %>%
  roc_curve(truth = face_mask_binary, .pred_Yes) %>%
  mutate(model = "Tuned XGBoost")

roc_data <- bind_rows(rf_roc, xgb_roc)

p_roc <- ggplot(
  roc_data,
  aes(x = 1 - specificity, y = sensitivity, colour = model)
) +
  geom_line(linewidth = 1.1) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  coord_equal() +
  scale_colour_manual(
    values = c(
      "Tuned Random Forest" = "#264653",
      "Tuned XGBoost" = "#E9C46A"
    )
  ) +
  labs(
    title = "ROC Curve Comparison of Final Tuned Models",
    subtitle = "Model discrimination evaluated on the hold-out test set",
    x = "False positive rate",
    y = "True positive rate",
    colour = "Model"
  ) +
  theme_report

p_roc

ggsave("report_06_roc_curve_comparison.png", plot = p_roc, width = 8, height = 6, dpi = 300)

# Final AUC comparison

final_auc_plot <- final_model_comparison %>%
  ggplot(aes(x = reorder(model, .estimate), y = .estimate, fill = model)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = round(.estimate, 3)), hjust = -0.15, size = 4) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Tuned Random Forest" = "#264653",
      "Tuned XGBoost" = "#E9C46A"
    )
  ) +
  labs(
    title = "Final Tuned Model Comparison",
    subtitle = "Model performance evaluated using test AUC",
    x = "Model",
    y = "Test AUC",
    fill = "Model"
  ) +
  ylim(0, 1) +
  theme_report

final_auc_plot

ggsave("report_07_final_tuned_model_auc.png", plot = final_auc_plot, width = 8, height = 5, dpi = 300)

# Random Forest variable importance

rf_vip_plot <- final_rf_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 15) +
  labs(
    title = "Top Predictors from the Tuned Random Forest Model",
    subtitle = "Permutation importance identifies variables most useful for prediction"
  ) +
  theme_report

rf_vip_plot

ggsave("report_08_rf_variable_importance.png", plot = rf_vip_plot, width = 8, height = 6, dpi = 300)


#  XGBoost variable importance

xgb_vip_plot <- final_xgb %>%
  extract_fit_parsnip() %>%
  vip(num_features = 15) +
  labs(
    title = "Top Predictors from the Tuned XGBoost Model",
    subtitle = "Importance values identify variables most useful for prediction"
  ) +
  theme_report

xgb_vip_plot

ggsave("report_09_xgb_variable_importance.png", plot = xgb_vip_plot, width = 8, height = 6, dpi = 300)


# save key tables

write_csv(rf_cv_metrics, "rf_cross_validation_metrics.csv")
write_csv(xgb_cv_metrics, "xgb_cross_validation_metrics.csv")
write_csv(final_model_comparison, "final_model_comparison.csv")
write_csv(rf_test_results, "rf_test_predictions.csv")
write_csv(xgb_test_results, "xgb_test_predictions.csv")


# saved files


getwd()

list.files(pattern = "report_.*\\.png")
list.files(pattern = ".*\\.csv")