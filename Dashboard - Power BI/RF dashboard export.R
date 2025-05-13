#################################################################
##-------------------------------------------------------------##
##   Course        : BC2407 Analytics II                       ##
##   Group         : 07                                        ##
##   Seminar Group : 2                                         ##
##-------------------------------------------------------------##
#################################################################


#################################################################
#                                                               #
#                                                               #
#       Power BI dashboard - export data for monitoring tab     #
#                                                               #
#                                                               #
#################################################################


## Purpose: For use in Power BI - model monitoring tab, the Random Forest model is trained and evaluated in a separate R script. This avoids merge conflicts that occur when predictions are appended to the compiled model script.


library(dplyr)
library(randomForest)
library(caret)

# --- 1. Load Data -------------------------------------------------------------
df <- read.csv("HDHI_clean.csv")

# --- 2. Group Variables -------------------------------------------------------
lifestyle_vars <- c("SMOKING", "ALCOHOL") 
df$LIFESTYLE_RISK <- ifelse(rowSums(df[lifestyle_vars] == 1, na.rm = TRUE) > 0, "yes", "no")

chronic_cardio_vars <- c("CAD", "STABLE_ANGINA", "VALVULAR", "HFREF", "HFNEF", "HEART_FAILURE", "AF", "CONGENITAL")
df$CHRONIC_CARDIO <- ifelse(rowSums(df[chronic_cardio_vars] == 1, na.rm = TRUE) > 0, "yes", "no")

acute_cardio_vars <- c("ACS", "STEMI", "ATYPICAL_CHEST_PAIN", "RAISED_CARDIAC_ENZYMES", "CARDIOGENIC_SHOCK", "SHOCK", "PULMONARY_EMBOLISM")
df$ACUTE_CARDIO <- ifelse(rowSums(df[acute_cardio_vars] == 1, na.rm = TRUE) > 0, "yes", "no")

arrhythmia_vars <- c("CHB", "SSS", "VT", "PSVT", "NEURO_CARDIOGENIC_SYNCOPE", "ORTHOSTATIC")
df$ARRHYTHMIA_CONDUCTION <- ifelse(rowSums(df[arrhythmia_vars] == 1, na.rm = TRUE) > 0, "yes", "no")

non_cardio_vars <- c("CVA_INFRACT", "CVA_BLEED", "AKI", "ANAEMIA", "SEVERE_ANAEMIA", "UTI", "CHEST_INFECTION", "CKD", "DM")
df$NON_CARDIAC_COMORBID <- ifelse(rowSums(df[non_cardio_vars] == 1, na.rm = TRUE) > 0, "yes", "no")

other_cardio_vars <- c("PRIOR_CMP", "INFECTIVE_ENDOCARDITIS", "DVT", "HTN")
df$OTHER_CARDIO <- ifelse(rowSums(df[other_cardio_vars] == 1, na.rm = TRUE) > 0, "yes", "no")

# --- 3. Remove Redundant Columns ---------------------------------------------
factor_columns <- c(
  "SMOKING", "ALCOHOL", "DM", "HTN", "CAD", "PRIOR_CMP", "CKD",
  "RAISED_CARDIAC_ENZYMES", "SEVERE_ANAEMIA", "ANAEMIA", "STABLE_ANGINA",
  "ACS", "STEMI", "ATYPICAL_CHEST_PAIN", "HEART_FAILURE", "HFREF", "HFNEF",
  "VALVULAR", "CHB", "SSS", "AKI", "CVA_INFRACT", "CVA_BLEED", "AF", "VT",
  "PSVT", "CONGENITAL", "UTI", "NEURO_CARDIOGENIC_SYNCOPE", "ORTHOSTATIC",
  "INFECTIVE_ENDOCARDITIS", "DVT", "CARDIOGENIC_SHOCK", "SHOCK",
  "PULMONARY_EMBOLISM", "CHEST_INFECTION"
)

grouped_columns <- c(
  "LIFESTYLE_RISK", "CHRONIC_CARDIO", "ACUTE_CARDIO",
  "ARRHYTHMIA_CONDUCTION", "NON_CARDIAC_COMORBID", "OTHER_CARDIO"
)

df_grouped <- df[, setdiff(names(df), union(factor_columns, grouped_columns))]
df_grouped <- cbind(df_grouped, df[, grouped_columns])

# --- 4. Create ICU Column and Filter Outcomes -------------------------------
df_grouped$ICU_stay <- ifelse(df_grouped$duration_of_intensive_unit_stay > 0, 1, 0)

df_filtered <- df_grouped %>%
  filter(!(OUTCOME %in% c("DAMA", "EXPIRY")))

# --- 5. Prepare for Modeling ------------------------------------------------
df_model <- df_filtered %>%
  select(-c(MRD_No_, month_year, duration_of_intensive_unit_stay, OUTCOME))

cat_cols <- sapply(df_model, is.character)
df_model[cat_cols] <- lapply(df_model[cat_cols], factor)

median_los <- median(df_model$DURATION_OF_STAY, na.rm = TRUE)
df_model$Stay_Length <- ifelse(df_model$DURATION_OF_STAY <= median_los, "Short", "Long")
df_model$Stay_Length <- factor(df_model$Stay_Length)
df_model <- df_model %>% select(-DURATION_OF_STAY)

# --- 6. Train-Test Split ----------------------------------------------------
set.seed(0)
train_idx <- createDataPartition(df_model$Stay_Length, p = 0.7, list = FALSE)
train_data <- df_model[train_idx, ]
test_data <- df_model[-train_idx, ]

# --- 7. Tune and Train Random Forest ----------------------------------------
set.seed(0)
tune_result <- tuneRF(
  x = train_data %>% select(-Stay_Length),
  y = train_data$Stay_Length,
  ntreeTry = 500,
  stepFactor = 2,
  improve = 0.01,
  trace = TRUE,
  plot = TRUE
)

best_mtry <- tune_result[which.min(tune_result[, 2]), 1]

rf_final <- randomForest(
  Stay_Length ~ .,
  data = train_data,
  ntree = 500,
  mtry = best_mtry,
  nodesize = 5,
  importance = TRUE
)

# --- 8. Evaluation ----------------------------------------------------------
pred <- predict(rf_final, newdata = test_data)
conf_matrix <- confusionMatrix(pred, test_data$Stay_Length)

cat("Accuracy:", round(conf_matrix$overall["Accuracy"], 4), "\n")
print(conf_matrix)

probs <- predict(rf_final, newdata = test_data, type = "prob")[, "Long"]
roc_obj <- pROC::roc(test_data$Stay_Length, probs)
cat("AUC:", pROC::auc(roc_obj), "\n")

varImpPlot(rf_final, type = 1, n.var = 15, main = "Top 15 Variables (Tuned RF)")

# --- 9. Save Feature Importance and Predictions -----------------------------
importance_values <- importance(rf_final, type = 1)[, 1]
rf_feature_importance <- data.frame(
  Feature = names(importance_values),
  Importance = importance_values
) %>%
  arrange(desc(Importance))
write.csv(rf_feature_importance, "R_rf_feature_importance.csv", row.names = FALSE)

test_with_meta <- df_filtered[-train_idx, ]
test_with_meta$Predicted_Stay_Length <- pred
test_with_meta$Stay_Length <- ifelse(test_with_meta$DURATION_OF_STAY <= median_los, "Short", "Long")
test_with_meta$Stay_Length <- factor(test_with_meta$Stay_Length, levels = c("Short", "Long"))

rf_prediction_by_month <- test_with_meta %>%
  select(month_year, Stay_Length, Predicted_Stay_Length)
write.csv(rf_prediction_by_month, "R_rf_prediction_by_month.csv", row.names = FALSE)
