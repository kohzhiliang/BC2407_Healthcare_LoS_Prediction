
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
#                 DATA PREPARATION AND CLEANING                 #
#                                                               #
#                                                               #
#################################################################

library(data.table)
library(ggplot2)
library(dplyr)
library(randomForest)
library(VIM) # kNN imputer

data <- fread("HDHI_Admission_data.csv", stringsAsFactors = TRUE)
summary(data)

# (1) Renaming variables -------------------------------------------------------

# Remove spacing in names
setnames(data, gsub(" ", "_", names(data)))
# Remove other special characters
colnames(data) <- gsub("[./-]", "_", colnames(data))

summary(data)

# (2) Duplicate MRD_No_ with same D_O_A -----------------------------------------

sum(duplicated(data$MRD_No_))

# 1 MRD_No contains value "NILL" --> Unknown person
data <- data %>% filter(MRD_No_ != "NILL")

data_notclean <- copy(data)
data <- data %>%
  group_by(MRD_No_, D_O_A) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  as.data.table()

# (3) Conversion of values to NA -----------------------------------------------

# Blank data
data[data == ""] <- NA
data[data == "EMPTY"] <- NA

# 1 mismatched value for CHEST INFECTION
data$CHEST_INFECTION[data$CHEST_INFECTION == "\\"] <- NA
data$CHEST_INFECTION <- droplevels(data$CHEST_INFECTION)

# Change to numerical
numeric_col <- c("HB", "TLC", "PLATELETS", "GLUCOSE", "UREA", "CREATININE", "EF")
data[, (numeric_col) := lapply(.SD, as.numeric), .SDcols = numeric_col]

summary(data)
sum(is.na(data))

# (4) NA Values ----------------------------------------------------------------

# Check for NA values
for (col in colnames(data)) {
  na_percentage <- sum(is.na(data[[col]])) / nrow(data) * 100
  if (sum(is.na(data[[col]])) != 0) {
    cat(sprintf("%-30s\t%.2f\n", col, na_percentage))
  }
}

# BNP has higher percentage of NA - 53.57%
data[, c("BNP") := NULL]

temp <- data[, .(MRD_No_, month_year, duration_of_intensive_unit_stay, OUTCOME)] # To add back to data after imputation
# Remove irrelevant columns before imputation
data[, c("SNO", "MRD_No_", "D_O_A", "D_O_D", "month_year", 
         "duration_of_intensive_unit_stay", "OUTCOME") := NULL]

# Impute other values
data_imputed <- copy(data)
set.seed(0)
data_imputed <- kNN(data, k = 3, impNA = TRUE, trace = TRUE) # Only impute NAs
data_imputed <- data_imputed %>% select(-contains("_imp")) # remove _imp columns

# Add columns back
data_imputed <- cbind(temp, data_imputed)
summary(data_imputed)

# (5) Change datatypes ---------------------------------------------------------
# (Do this after importing csv in other R files. CSV does not save categories)

str(data_imputed) # Check datatypes

# Numeric variables
numeric_col <- c("HB", "TLC", "PLATELETS", "GLUCOSE", "UREA", "CREATININE", "EF")
data_imputed[, (numeric_col) := lapply(.SD, as.numeric), .SDcols = numeric_col]

# Categorical variables
binary_col <- names(data_imputed)[sapply(data_imputed, function(x) 
  length(unique(x)) == 2 & all(unique(x) %in% c(0, 1))
)]
data_imputed[, (binary_col) := lapply(.SD, as.factor), .SDcols = binary_col]

# (6) Export clean CSV ---------------------------------------------------------

write.csv(data_imputed, "HDHI_clean.csv", row.names = FALSE)


#################################################################
#                                                               #
#                                                               #
#                 EXPLORATORY DATA ANALYSIS                     #
#                                                               #
#                                                               #
#################################################################


library(dplyr)
library(lubridate)
library(arules)
library(arulesViz)
library(data.table)
library(rpart)
library(rpart.plot)   
library(ggplot2)
library(scales)
library(reshape2)
library(caTools)
library(e1071)
library(car)
library(gridExtra)
# Read the cleaned CSV file
df <- fread("HDHI_clean.csv")


# Convert to numeric and check missing values
df$DURATION_OF_STAY <- as.numeric(df$DURATION_OF_STAY)  
summary(df$DURATION_OF_STAY)
sum(is.na(df$DURATION_OF_STAY)) #0



###------(a) General Findings of LoS and admission------###
##Figure 1: Distribution plot
ggplot(df, aes(x = DURATION_OF_STAY)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Fig 1: Distribution of Length of Stay", x = "Length of Stay (Days)", y = "Count")
theme_minimal() +
  ggtitle("Distribution of Length of Stay")


##Figure 2.1: Average Duration of Stay by Month
df$month_year <- as.Date(paste0(df$month_year, "-01"), format = "%b-%y-%d")
df$month <- format(df$month_year, "%b")  
df$month_num <- as.numeric(format(df$month_year, "%m")) 

avg_duration_by_month <- df %>%
  group_by(month, month_num) %>%  
  summarise(avg_duration = mean(DURATION_OF_STAY, na.rm = TRUE), .groups = 'drop') %>%
  arrange(month_num) 

avg_duration_by_month$month <- factor(avg_duration_by_month$month, levels = month.abb)

ggplot(avg_duration_by_month, aes(x = month, y = avg_duration)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Fig 2.1: Average Duration of Stay by Month",
       x = "Month",
       y = "Average Duration of Stay (days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


##Figure 2.2: Total Admissions by Month
admissions_by_month <- df %>%
  group_by(month, month_num) %>% 
  summarise(total_admissions = n_distinct(row_number()), .groups = 'drop') %>%    arrange(as.numeric(month_num))  

admissions_by_month$month <- factor(admissions_by_month$month, levels = month.abb)

ggplot(admissions_by_month, aes(x = month, y = total_admissions)) +
  geom_bar(stat = "identity", fill = "maroon") +
  labs(title = "Fig 2.2: Total Admissions by Month",
       x = "Month",
       y = "Total Admissions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


###------(b) Demographic Analysis------###

##Figure 3: Gender distribution
df$GENDER <- as.factor(df$GENDER)
ggplot(df, aes(x = GENDER, fill = GENDER)) +
  geom_bar() +
  labs(title = "Fig 3: Gender Distribution of Patients", x = "Gender", y = "Count") +
  theme_minimal()


##Figure 4: Age distribution
ggplot(df, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Fig 4: Age Distribution of Patients", x = "Age", y = "Count") +
  theme_minimal()


##Figure 5: Area distribution
ggplot(df, aes(x = RURAL, fill = RURAL)) +
  geom_bar() +
  labs(title = "Fig 5: Distribution of Patients by Area", x = "Area", y = "Count") +
  theme_minimal()


###------(c) Patient's Conditions Analysis------###

##Figure 6: Bar chart - Frequency of Conditions
condition_columns <- c("SMOKING", "ALCOHOL", "DM", "HTN", "CAD", "PRIOR_CMP", "CKD","RAISED_CARDIAC_ENZYMES", "SEVERE_ANAEMIA", "ANAEMIA", "STABLE_ANGINA","ACS", "STEMI", "ATYPICAL_CHEST_PAIN", "HEART_FAILURE", "HFREF", "HFNEF","VALVULAR", "CHB", "SSS", "AKI", "CVA_INFRACT", "CVA_BLEED", "AF", "VT","PSVT", "CONGENITAL", "UTI", "NEURO_CARDIOGENIC_SYNCOPE", "ORTHOSTATIC","INFECTIVE_ENDOCARDITIS", "DVT", "CARDIOGENIC_SHOCK", "SHOCK","PULMONARY_EMBOLISM", "CHEST_INFECTION")

df_condition <- df[, condition_columns, with = FALSE] %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))  
df_condition <- df[, condition_columns, with = FALSE] %>%
  mutate(across(everything(), ~factor(ifelse(is.na(.), 0, .), levels = c(0, 1), labels = c("Absent", "Present")))) 

condition_counts <- colSums(df_condition[, ..condition_columns] == "Present")

summary_condition_df <- data.frame(Condition = names(condition_counts), Count = condition_counts)

ggplot(summary_condition_df, aes(x = reorder(Condition, Count), y = Count, fill = Condition)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Fig 6: Frequency of Conditions", x = "Condition", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


##Appendix B - Figure 1: Association Rules
transactions <- as(df_condition, "transactions")
mean(df$DURATION_OF_STAY) #6.3

df$LoS_Bucket <- cut(df$DURATION_OF_STAY, 
                     breaks = c(-Inf, 5, 7, Inf), 
                     labels = c("Below Average", "Average", "Above Average"))

df_condition$LoS_Bucket <- df$LoS_Bucket

df_trans <- as(df_condition, "transactions")

rules_los <- apriori(df_trans, parameter = list(support = 0.01, confidence = 0.5),
                     appearance = list(rhs = c("LoS_Bucket=Below Average", "LoS_Bucket=Average", "LoS_Bucket=Above Average")))

inspect(head(sort(rules_los, by = "lift"), 30))


###------(d) Patient's Medical Index Analysis------###

##Figure 7,1: Scatterplot - Index vs LoS
numeric_cols <- c("DURATION_OF_STAY","HB", "TLC", "PLATELETS", "GLUCOSE", "UREA", "CREATININE", "EF")  

df[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
numeric_dt <- df[, ..numeric_cols]  

melted_df <- melt(numeric_dt, id.vars = "DURATION_OF_STAY", variable.name = "Factor", value.name = "Value")

ggplot(melted_df, aes(x = Value, y = DURATION_OF_STAY)) +
  geom_point(alpha = 0.5, color = "blue") + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  facet_wrap(~ Factor, scales = "free_x") +  
  labs(title = "Fig 7.1: Scatter Plot of Medical Index vs Length of Stay",
       x = "Index Value",
       y = "Length of Stay (days)") +
  theme_minimal()

##Figure 7.2: Correlation Matrix of Medical Variables
cor(numeric_dt)
cormat <- round(cor(numeric_dt),2)
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

melted_cormat <- melt(get_lower_tri(cormat), na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = 'white') +
  scale_fill_gradient2(
    low = "lightblue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation"
  ) +
  geom_text(aes(label = round(value, 2), vjust = 1)) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_fixed() +
  labs(title = "Fig 7.2: Correlation Matrix of Medical Variables")


###------(e) Mortality rate Analysis------###

##Appendix B - Figure 2: Association Rule (Mortality Rate)
df$Mortality <- ifelse(df$OUTCOME == "EXPIRY", "Yes", "No")

df_condition$Mortality <- df$Mortality

df_trans_mortality <- as(df_condition, "transactions")

rules_mortality <- apriori(df_trans_mortality, 
                           parameter = list(support = 0.01, confidence = 0.5),
                           appearance = list(rhs = c("Mortality=Yes")))

rules_mortality_yes <- subset(rules_mortality, rhs %in% "Mortality=Yes")

inspect(head(sort(rules_mortality_yes, by = "lift"), 30))



###------(f) Readmission rate Analysis------###

##Fig 8.1: Readmission Rate by Outcome 
readmit_counts <- df %>%
  group_by(MRD_No_, month_year) %>%
  summarise(visit_count = n(), .groups = 'drop')

readmit_flags <- readmit_counts %>%
  filter(visit_count > 1) %>%
  mutate(Readmitted = 1)

df <- df %>%
  left_join(readmit_flags %>% select(MRD_No_, month_year, Readmitted),
            by = c("MRD_No_", "month_year")) %>%
  mutate(Readmitted = ifelse(is.na(Readmitted), 0, Readmitted))

outcome_summary <- df %>%
  group_by(OUTCOME) %>%
  summarise(Readmitted_Rate = mean(Readmitted))

outcome_summary$label <- paste0(outcome_summary$OUTCOME, ": ",
                                round(outcome_summary$Readmitted_Rate * 100, 1), "%")

ggplot(outcome_summary, aes(x = "", y = Readmitted_Rate, fill = OUTCOME)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Fig 8.1: Readmission Rate by Outcome", x = NULL, y = NULL) +
  theme_void()

##Fig 8.2: Trends in Readmission rate
df <- df %>%
  mutate(month_date = as.Date(month_year))

monthly_readmit <- df %>%
  group_by(month_date) %>%
  summarise(Readmission_Rate = mean(Readmitted), .groups = 'drop')

ggplot(monthly_readmit, aes(x = month_date, y = Readmission_Rate, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Fig 8.2: Trends in Readmission rate",
       x = "Month-Year", y = "Readmission Rate") +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
