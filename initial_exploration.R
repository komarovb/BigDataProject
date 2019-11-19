library(data.table) # TODO remove because not used!

# Naive Bayes implementation:
library(naivebayes) # https://github.com/majkamichal/naivebayes

# Random forest implementation
library(randomForest)

# Useful aggregation & manipulation functions
library(dplyr)

# student_dt = fread("data/SM_2012_13_20141103_01.csv")
# staff_dt = fread("STT_2012_13_20141103.csv")
# teaching_dt = fread("STA_2012_13_20141103")

# staff_dt[rowSums(is.na(staff_dt[ , 1:34])) == 0, ]

# dt[order(-dt[, STUDENT_AGE_VALUE])[1],]

# Useful functions: 
# write.csv(carSpeeds, file = 'data/car-speeds-cleaned.csv', row.names = FALSE)
# carSpeeds$Color = ifelse(carSpeeds$Color == 'Blue', 'Green', carSpeeds$Color)
# str(df)
# print(factor_data)
# colnames(df)
# names(df)
# rownames(df)
# data[data$Code %in% selected,]
# df[is.na(df$Fare),]
# tmp = rbind(tmp, tmp2)

# String to number!
# as.numeric(s_dt[1, STUDY_GRANT_AMT])

# 'o' %in% missing_values

# WORKING ON MISSING VALUES ------------------------------

# Produce top-2 accuracy
top_n_accuracy <- function(x, top_accuracy) {
  x = sort(x, decreasing=TRUE)
  if(top_accuracy == 2) {
    top_accuracy = array(c(names(x[1]), names(x[2])))
  } else if(top_accuracy == 3) {
    top_accuracy = array(c(names(x[1]), names(x[2]), names(x[3])))
  }
  return(top_accuracy)
}

# Transform grant
transform_grant <- function(x) {
  tmp = strsplit(x, '.', fixed=TRUE)
  tmp = unlist(tmp)
  if(length(tmp) > 2){
    tmp = paste(tmp[1], tmp[2], '.', tmp[3], sep='')
  } else if(length(tmp) > 1){
    tmp = paste(tmp[1], '.', tmp[2], sep='')
  }
  
  return(as.numeric(tmp))
}

# Remove selected columns
remove_columns <- function(df, columns_to_drop) {
  col_names = colnames(df)
  col_names = col_names[! col_names %in% columns_to_drop]
  df = df[, col_names]
  df[] = lapply(df, function(x) if(is.factor(x)) factor(x) else x)  # TODO refactor!
  return(df)
}

# Custom application of the Naive Bayes method
custom_naive_bayes <- function(tmp_df, percentage_train, laplace, n) {
  # REMOVE: STUDENT_ID, HOST_INSTITUTION_CDE, STUDY_START_DATE, ECTS_CREDITS_STUDY_AMT, STUDY_GRANT_AMT
  # Predict: HOST_INSTITUTION_COUNTRY_CDE
  tmp_df = remove_columns(s_df, c('STUDENT_ID', 'HOST_INSTITUTION_CDE', 'NUMB_YRS_HIGHER_EDUCAT_VALUE', 'STUDENT_SUBJECT_AREA_VALUE',
                                  'STUDENT_STUDY_LEVEL_CDE'))
  
  train_data_instances = sample(nrow(tmp_df), nrow(tmp_df)*percentage_train)
  tmp = 1:nrow(tmp_df)
  test_data_instances = setdiff(tmp, train_data_instances)
  
  train_data = tmp_df[train_data_instances,]
  # train_data[] = lapply(train_data, function(x) if(is.factor(x)) factor(x) else x) # TODO refactor
  
  test_data = tmp_df[test_data_instances,]
  # test_data[] = lapply(test_data, function(x) if(is.factor(x)) factor(x) else x) # TODO refactor
  
  nb = naive_bayes(HOST_INSTITUTION_COUNTRY_CDE~., train_data, laplace = laplace)
  
  #tmp = nb %class% test_data # Alternative option
  if(n == 1) {
    tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "class")
    predictions_mlr = data.frame(predicted = tmp, actual = test_data$HOST_INSTITUTION_COUNTRY_CDE)
    correctly_classified = nrow(predictions_mlr[predictions_mlr$predicted == predictions_mlr$actual, ])
    incorrectly_classified = nrow(predictions_mlr[predictions_mlr$predicted != predictions_mlr$actual, ])
  } else if(n == 2) {
    tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "prob")
    tmp = apply(tmp, 1, top_n_accuracy, top_accuracy = n)
    predictions_mlr = data.frame(top_1 = tmp[2,], top_2 = tmp[1, ], actual = test_data$HOST_INSTITUTION_COUNTRY_CDE)
    correctly_classified = nrow(predictions_mlr[predictions_mlr$top_1 == predictions_mlr$actual, ]) +
                                                         nrow(predictions_mlr[predictions_mlr$top_2 == predictions_mlr$actual,])
  }else if(n==3) {
    tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "prob")
    tmp = apply(tmp, 1, top_n_accuracy, top_accuracy = n)
    predictions_mlr = data.frame(top_1 = tmp[3,], top_2 = tmp[2, ], top_3 = tmp[1, ], actual = test_data$HOST_INSTITUTION_COUNTRY_CDE)
    correctly_classified = nrow(predictions_mlr[predictions_mlr$top_1 == predictions_mlr$actual, ]) + 
      nrow(predictions_mlr[predictions_mlr$top_2 == predictions_mlr$actual,]) + 
      nrow(predictions_mlr[predictions_mlr$top_3 == predictions_mlr$actual,])
  } else {
    correctly_classified = 0
  }
  # t2 = colnames(tmp2)[apply(tmp2,1,which.max)]
  accuracy = (correctly_classified / nrow(test_data)) * 100
  return(accuracy)
}

df = read.csv("data/SM_2012_13_20141103_01.csv", header = TRUE, sep = ';') # Reading the whole data


# Filter unnecessary attributes based on the data structure for each type of placement
# Remove for all:
# ID_MOBILITY_CDE, CONSORTIUM_AGREEMENT_NUMBER, SPECIAL_NEEDS_SUPPLEMENT_VALUE, 
# ????? SHORT_DURATION_CDE ????? QUALIFICATION_AT_HOST_CDE ?????
drop_for_all = c('ID_MOBILITY_CDE', 
                 'CONSORTIUM_AGREEMENT_NUMBER', 'SPECIAL_NEEDS_SUPPLEMENT_VALUE', 
                 'SHORT_DURATION_CDE', 'QUALIFICATION_AT_HOST_CDE')

df = remove_columns(df, drop_for_all)

# Split data based on what was the type of placement for further processing
s_df = df[ df$MOBILITY_TYPE_CDE == 'S',]
s_p_df = df[ df$MOBILITY_TYPE_CDE == 'C',]
p_df = df[ df$MOBILITY_TYPE_CDE == 'P',]

# Remove for 'S' placement
# MOBILITY_TYPE_CDE, PLACEMENT_ENTERPRISE_VALUE, PLACEMENT_ENTERPRISE_CTRY_CDE, 
# PLACEMENT_ENTERPRISE_SIZE_CDE, TYPE_PLACEMENT_SECTOR_VALUE", LENGTH_PLACEMENT_VALUE, 
# PLACEMENT_START_DATE, ECTS_CREDITS_PLACEMENT_AMT, PLACEMENT_GRANT_AMT
# ????? TOTAL_ECTS_CREDITS_AMT ?????
drop_for_s = c('MOBILITY_TYPE_CDE', 'PLACEMENT_ENTERPRISE_VALUE', 'PLACEMENT_ENTERPRISE_CTRY_CDE', 
               'PLACEMENT_ENTERPRISE_SIZE_CDE', 'TYPE_PLACEMENT_SECTOR_VALUE', 'LENGTH_PLACEMENT_VALUE', 
               'PLACEMENT_START_DATE', 'ECTS_CREDITS_PLACEMENT_AMT', 'PLACEMENT_GRANT_AMT')
s_df = remove_columns(s_df, drop_for_s)
# Candidates for descriptive analytics:
# TAUGHT_HOST_LANGUAGE_CDE, LANGUAGE_TAUGHT_CDE
# STUDY_GRANT_AMT, PLACEMENT_GRANT_AMT, PREVIOUS_PARTICIPATION_CDE, SPECIAL_NEEDS_SUPPLEMENT_VALUE
# ....

# Check for missing values & Feature selection
missing_values = c('? Unknown ?', '???', '?')

# Character fields: 
# HOME_INSTITUTION_CDE, HOME_INSTITUTION_CTRY_CDE, STUDENT_GENDER_CDE, STUDENT_NATIONALITY_CDE, 
# STUDENT_SUBJECT_AREA_VALUE, STUDENT_STUDY_LEVEL_CDE, HOST_INSTITUTION_CDE, HOST_INSTITUTION_COUNTRY_CDE,
# TAUGHT_HOST_LANGUAGE_CDE, LANGUAGE_TAUGHT_CDE, LINGUISTIC_PREPARATION_CDE, PREVIOUS_PARTICIPATION_CDE,
# STUDY_GRANT_AMT

# DATE: STUDY_START_DATE

s_df[ s_df$STUDENT_GENDER_CDE %in% missing_values, ]
s_df[ s_df$LINGUISTIC_PREPARATION_CDE %in% missing_values, ]


# For 'S' placement
colums_to_check = c('HOME_INSTITUTION_CDE', 'HOME_INSTITUTION_CTRY_CDE',  'STUDENT_GENDER_CDE', 
                    'STUDENT_NATIONALITY_CDE', 'STUDENT_SUBJECT_AREA_VALUE', 'STUDENT_STUDY_LEVEL_CDE',
                    'HOST_INSTITUTION_CDE', 'HOST_INSTITUTION_COUNTRY_CDE', 'TAUGHT_HOST_LANGUAGE_CDE',
                    'LANGUAGE_TAUGHT_CDE', 'LINGUISTIC_PREPARATION_CDE', 'PREVIOUS_PARTICIPATION_CDE', 'STUDY_GRANT_AMT')
missing_values_per_feature = c()
for (i in 1:length(colums_to_check))
{
  col = colums_to_check[i]
  number_of_missing_values = nrow(s_df[s_df[[col]] %in% missing_values, col])
  missing_values_per_feature[i] = number_of_missing_values
}

# Mapping Belgium from 3 into 1 country
s_df$HOME_INSTITUTION_CTRY_CDE = unlist(lapply(s_df$HOME_INSTITUTION_CTRY_CDE, as.character))
s_df$HOME_INSTITUTION_CTRY_CDE[s_df$HOME_INSTITUTION_CTRY_CDE == 'BEDE'] = "BE"
s_df$HOME_INSTITUTION_CTRY_CDE[s_df$HOME_INSTITUTION_CTRY_CDE == 'BENL'] = "BE"
s_df$HOME_INSTITUTION_CTRY_CDE[s_df$HOME_INSTITUTION_CTRY_CDE == 'BEFR'] = "BE"
s_df$HOME_INSTITUTION_CTRY_CDE = unlist(lapply(s_df$HOME_INSTITUTION_CTRY_CDE, as.factor))

s_df$HOST_INSTITUTION_COUNTRY_CDE = unlist(lapply(s_df$HOST_INSTITUTION_COUNTRY_CDE, as.character))
s_df$HOST_INSTITUTION_COUNTRY_CDE[s_df$HOST_INSTITUTION_COUNTRY_CDE == 'BEDE'] = "BE"
s_df$HOST_INSTITUTION_COUNTRY_CDE[s_df$HOST_INSTITUTION_COUNTRY_CDE == 'BENL'] = "BE"
s_df$HOST_INSTITUTION_COUNTRY_CDE[s_df$HOST_INSTITUTION_COUNTRY_CDE == 'BEFR'] = "BE"
s_df$HOST_INSTITUTION_COUNTRY_CDE = unlist(lapply(s_df$HOST_INSTITUTION_COUNTRY_CDE, as.factor))

train_percentage = 0.7

# Naive Bayes
accuracy = custom_naive_bayes(s_df, train_percentage, 0.5, 1)
cat(sprintf("Naive Bayes top-1 accuracy: %f%%\n\n", accuracy))

accuracy = custom_naive_bayes(s_df, train_percentage, 0.5, 2)
cat(sprintf("Naive Bayes top-2 accuracy: %f%%\n\n", accuracy))

accuracy = custom_naive_bayes(s_df, train_percentage, 0.5, 3)
cat(sprintf("Naive Bayes top-3 accuracy: %f%%\n\n", accuracy))

# Update study grant Amt!
s_df$STUDY_GRANT_AMT = as.character(s_df$STUDY_GRANT_AMT)
s_df$STUDY_GRANT_AMT = lapply(s_df$STUDY_GRANT_AMT, transform_grant)
s_df$STUDY_GRANT_AMT = unlist(s_df$STUDY_GRANT_AMT)

implement_rf = FALSE;

if(implement_rf) {
  # Random forest implementation: https://www.stat.berkeley.edu/~breiman/RandomForests/
  tmp_df = remove_columns(s_df, c('STUDENT_ID', 'HOME_INSTITUTION_CDE', 'HOST_INSTITUTION_CDE', 'NUMB_YRS_HIGHER_EDUCAT_VALUE', 'STUDENT_SUBJECT_AREA_VALUE',
                                  'STUDENT_STUDY_LEVEL_CDE', 'LANGUAGE_TAUGHT_CDE'))
  
  train_data_instances = sample(nrow(tmp_df), nrow(tmp_df)*0.6)
  tmp = 1:nrow(tmp_df)
  test_data_instances = setdiff(tmp, train_data_instances)
  train_data = tmp_df[train_data_instances,]
  test_data = tmp_df[test_data_instances,]
  
  
  start_time = Sys.time()
  rf <- randomForest(HOST_INSTITUTION_COUNTRY_CDE~., data = train_data, ntree = 500, mtry = 6, importance = TRUE)
  end_time = Sys.time()
  
  cat(sprintf("Random Forest was built in: %f\n\n", end_time - start_time))
  
  tmp <- predict(rf, test_data, type = "class")
  predictions_mlr = data.frame(predicted = tmp, actual = test_data$HOST_INSTITUTION_COUNTRY_CDE)
  correctly_classified = nrow(predictions_mlr[predictions_mlr$predicted == predictions_mlr$actual, ])
  accuracy = (correctly_classified / nrow(test_data)) * 100
  cat(sprintf("Random Forest accuracy: %f%%\n\n", accuracy))
}

# predictions_mlr[] <- lapply(predictions_mlr, as.character)

# ------------------------------

# TODO before presentation:
# Do basic feature selection! 17.11.2019 +
# Deal with missing values 11.11.2019 - 18.11.2019 +

# Implement multiple scenarious of clustering 18.11.2019
# Implement some kind of classification / regression 18.11.2019 +

# He will ask how we handle missing values
# Overfitting and feature selection

# MENTION OVERFITTING


# Add some descriptive analytics and visualisation to the initial results 19.11.2019
# Make presentaiton 19.11.2019




















