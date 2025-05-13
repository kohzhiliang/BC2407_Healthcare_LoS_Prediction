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
#                 PREDICTIVE ANALYTICS MODELS                   #
#                                                               #
#                                                               #
#################################################################

library(data.table)
library(dplyr)

data <- fread("HDHI_clean.csv", stringsAsFactors = T)


str(data) 
numeric_col <- c("HB", "TLC", "PLATELETS", "GLUCOSE", "UREA", "CREATININE", "EF")
data[, (numeric_col) := lapply(.SD, as.numeric), .SDcols = numeric_col]

binary_col <- names(data)[sapply(data, function(x) 
  length(unique(x)) == 2 & all(unique(x) %in% c(0, 1))
)]
data[, (binary_col) := lapply(.SD, as.factor), .SDcols = binary_col]

summary(data)

# Grouping of variables ========================================================

# Lifestyle Variables
lifestyle_vars <- c("SMOKING", "ALCOHOL") 
data[, LIFESTYLE_RISK := ifelse(rowSums(.SD == 1, na.rm = TRUE) > 0, 
                                "yes", "no"), .SDcols = lifestyle_vars]

# Chronic Cardio Variables
chronic_cardio_vars <- c("CAD", "STABLE_ANGINA", "VALVULAR", "HFREF", "HFNEF", "HEART_FAILURE", "AF", "CONGENITAL")
data[, CHRONIC_CARDIO := ifelse(rowSums(.SD == 1, na.rm = TRUE) > 0, 
                                "yes", "no"), .SDcols = chronic_cardio_vars]

# Acute Cardio Variables
acute_cardio_vars <- c("ACS", "STEMI", "ATYPICAL_CHEST_PAIN", "RAISED_CARDIAC_ENZYMES", "CARDIOGENIC_SHOCK", "SHOCK", "PULMONARY_EMBOLISM")
data[, ACUTE_CARDIO := ifelse(rowSums(.SD == 1, na.rm = TRUE) > 0, 
                              "yes", "no"), .SDcols = acute_cardio_vars]

# Arrythmia Variables
arrhythmia_vars <- c("CHB", "SSS", "VT", "PSVT", "NEURO_CARDIOGENIC_SYNCOPE", "ORTHOSTATIC")
data[, ARRHYTHMIA_CONDUCTION := ifelse(rowSums(.SD == 1, na.rm = TRUE) > 0, 
                                       "yes", "no"), .SDcols = arrhythmia_vars]

# Non-Cardio Variables
non_cardio_vars <- c("CVA_INFRACT", "CVA_BLEED", "AKI", "ANAEMIA", "SEVERE_ANAEMIA", "UTI", "CHEST_INFECTION", "CKD", "DM")
data[, NON_CARDIAC_COMORBID := ifelse(rowSums(.SD == 1, na.rm = TRUE) > 0, 
                                      "yes", "no"), .SDcols = non_cardio_vars]

# Other Cardio Variables
other_cardio_vars <- c("PRIOR_CMP", "INFECTIVE_ENDOCARDITIS", "DVT", "HTN")
data[, OTHER_CARDIO := ifelse(rowSums(.SD == 1, na.rm = TRUE) > 0, 
                              "yes", "no"), .SDcols = other_cardio_vars]

factor_columns <- c("SMOKING", "ALCOHOL", "DM", "HTN", "CAD", "PRIOR_CMP", "CKD", "RAISED_CARDIAC_ENZYMES", "SEVERE_ANAEMIA", "ANAEMIA", "STABLE_ANGINA","ACS", "STEMI", "ATYPICAL_CHEST_PAIN", "HEART_FAILURE", "HFREF", "HFNEF","VALVULAR", "CHB", "SSS", "AKI", "CVA_INFRACT", "CVA_BLEED", "AF", "VT","PSVT", "CONGENITAL", "UTI", "NEURO_CARDIOGENIC_SYNCOPE", "ORTHOSTATIC","INFECTIVE_ENDOCARDITIS", "DVT", "CARDIOGENIC_SHOCK", "SHOCK", "PULMONARY_EMBOLISM", "CHEST_INFECTION"
)

grouped_columns <- c(
  "LIFESTYLE_RISK", "CHRONIC_CARDIO", "ACUTE_CARDIO",
  "ARRHYTHMIA_CONDUCTION", "NON_CARDIAC_COMORBID", "OTHER_CARDIO"
)

data_grouped <- data[, !factor_columns, with = FALSE] 
data_grouped <- data_grouped %>% mutate_if(is.character, as.factor)
summary(data_grouped)


data_grouped[, ICU_stay := ifelse(rowSums(.SD == 1, na.rm = TRUE) > 0,
                        "yes", "no"), .SDcols = c("duration_of_intensive_unit_stay")]
data_grouped$ICU_stay <- as.factor(data_grouped$ICU_stay)

df_filtered <- data_grouped %>%
  filter(!(OUTCOME %in% c("DAMA", "EXPIRY")))
summary(df_filtered)

df_model <- df_filtered %>%
  select(-c(MRD_No_, month_year, duration_of_intensive_unit_stay, OUTCOME))

summary(df_model)

# Categorise Duration of Stay
dos.median <- median(df_model$DURATION_OF_STAY)
df_model[, DOS := ifelse(DURATION_OF_STAY > dos.median, "Long", "Short")] # 1 = Long, 0 = Short
df_model$DOS <- as.factor(df_model$DOS)
df_model <- df_model %>% select(-DURATION_OF_STAY)

## Export CSV for Model --------------------------------------------------------

write.csv(df_model, "model.csv", row.names = FALSE)

library(data.table)
library(dplyr)
library(randomForest)
library(caTools)
library(caret) # F1 Score
library(MLmetrics)
library(pROC) # AUC
library(earth)

# Load Data ------------------------------------------------------------------
data <- fread("model.csv", stringsAsFactors = TRUE)
summary(data)

# Train-Test Split ------------------------------------------------------------
set.seed(0)
split <- sample.split(Y = data$DOS, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Export train and test sets --------------------------------------------------
write.csv(train, "train.csv", row.names = FALSE)
write.csv(test, "test.csv", row.names = FALSE)

# Initialize results data frame -----------------------------------------------
results <- data.frame("Random Forest" = rep(NA, 3), "MARS" = rep(NA, 3), 
                      "XGBoost" = rep(NA, 3))
rownames(results) <- c("Mean Accuracy", "F1 Score", "AUC")

# (1) Random Forest ------------------------------------------------------------
set.seed(0)
tune_result <- tuneRF(
  x = train %>% select(-DOS),
  y = train$DOS,
  ntreeTry = 500,
  stepFactor = 2,
  improve = 0.01,
  trace = TRUE,
  plot = TRUE
)

best_mtry <- tune_result[which.min(tune_result[, 2]), 1]

rf_final <- randomForest(
  DOS ~ .,
  data = train,
  ntree = 500,
  mtry = best_mtry,
  nodesize = 5,
  importance = TRUE
)

pred <- predict(rf_final, newdata = test)
conf_matrix <- confusionMatrix(pred, test$DOS)

results[1,1] <- round(conf_matrix$overall["Accuracy"], 4)
results[2,1] <- F1_Score(y_true = test$DOS, y_pred = pred, positive = "Long")

probs <- predict(rf_final, newdata = test, type = "prob")[, "Long"]
roc_obj <- roc(test$DOS, probs)
results[3,1] <- auc(roc_obj)

varImpPlot(rf_final, type = 1, n.var = 15, main = "Top 15 Variables (Tuned RF)")

# (2) MARS ---------------------------------------------------------------------

train_data <- train
test_data <- test

set.seed(0)
mars_model <- earth(DOS ~ ., data = train_data)

#Lets now see if we should be working with degree 1 or degree 2 
mars_deg1 <- earth(DOS ~ ., data = train_data, degree = 1)
summary(mars_deg1)
mars_deg2 <- earth(DOS ~ ., data = train_data, degree = 2)
summary(mars_deg2)

#Gonna go ahead with our model having a degree of 2

#Lets now fit our final model with 10 fold cv with the degree being 2
mars_final <- earth(DOS ~ ., data = train_data, degree = 2, nfold = 10, pmethod = "cv")

pred <- predict(mars_final, newdata = test_data, type = "class")
pred <- factor(pred, levels = levels(test_data$DOS))

conf_matrix <- confusionMatrix(pred, test_data$DOS)
print(conf_matrix)

results[1,2] <- conf_matrix$overall["Accuracy"]
results[2,2] <- F1_Score(test_data$DOS, pred, positive = "Long")
roc_curve <- roc(test_data$DOS, as.numeric(pred == "Long"))
results[3,2] <- auc(roc_curve)

# (3) XGBoost ------------------------------------------------------------------
library(xgboost)
library(Matrix)

xg_train <- copy(train)
xg_test <- copy(test)

xg_train$DOS <- as.numeric(as.factor(xg_train$DOS)) - 1
xg_test$DOS <- as.numeric(as.factor(xg_test$DOS)) - 1
train_matrix <- sparse.model.matrix(DOS ~ . -1, data = xg_train)
test_matrix <- sparse.model.matrix(DOS ~ . -1, data = xg_test)
train_labels <- xg_train$DOS
test_labels <- xg_test$DOS

set.seed(0)
xgb_model <- xgboost(
  data = train_matrix, 
  label = train_labels, 
  max_depth = 5,
  eta = 0.1,
  nrounds = 100,
  early_stopping_rounds = 10,
  objective = "binary:logistic", 
  eval_metric = "auc",  
  verbose = 1,
  lambda = 10,
  alpha = 0.1,
  min_child_weight = 5,
  colsample_bytree = 0.5,
  gamma = 0
)

xgb_pred_prob <- predict(xgb_model, newdata = test_matrix)
xgb_pred_class <- ifelse(xgb_pred_prob > 0.5, 1, 0)

cm2 <- confusionMatrix(factor(xgb_pred_class), factor(test_labels))

results[1,3] <- mean(test_labels == xgb_pred_class)
results[2,3] <- F1_Score(y_pred = xgb_pred_class, y_true = test_labels, positive = "1")
roc_curve <- roc(test_labels, xgb_pred_prob)
results[3,3] <- auc(roc_curve)

plot(roc_curve)

# Feature Importance ----------------------------------------------------------
feature_names <- colnames(train_matrix)
importance_matrix <- xgb.importance(feature_names = feature_names, model = xgb_model)
importance_df <- as.data.frame(importance_matrix)

ggplot(importance_df, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_bar(stat = "identity") +  
  coord_flip() +
  labs(title = "XGBoost Feature Importance", x = "Features", y = "Importance (Gain)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Final Results ---------------------------------------------------------------
print(results)
