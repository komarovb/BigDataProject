# Big Data project code 
#
# Anna Dieckvoss - 1127324
# Borys Komarov - 0889421

# Features selection
library(CORElearn)

# Naive Bayes implementation:
library(naivebayes) # https://github.com/majkamichal/naivebayes

# Random forest implementation
library(randomForest)

# Useful aggregation & manipulation functions
library(dplyr)

# Useful functions --------------------------------------------

# Taken from: https://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
f1 <- function(df) {
  diag = diag(df)
  rowsums = apply(df, 1, sum)
  colsums = apply(df, 2, sum)
  diag[diag == 0] = 0.00001
  
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  
  return(f1)
}

# Produce top-2 accuracy
top_n_accuracy <- function(x, top_accuracy) {
  x = sort(x, decreasing=TRUE)
  xs = x[1:top_accuracy]
  labels = names(xs)
  top_accuracy = array(labels)
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
  # Predict: HOST_INSTITUTION_COUNTRY_CDE
  # TODO remove after features selection will be implemented
  tmp_df = remove_columns(tmp_df, c('STUDENT_ID', 'HOST_INSTITUTION_CDE', 'NUMB_YRS_HIGHER_EDUCAT_VALUE', 'STUDENT_SUBJECT_AREA_VALUE',
                                  'STUDENT_STUDY_LEVEL_CDE', 
                                  'STUDENT_AGE_VALUE', 'LENGTH_STUDY_PERIOD_VALUE', 'TOTAL_ECTS_CREDITS_AMT', 'STUDENT_GENDER_CDE',
                                  'ECTS_CREDITS_STUDY_AMT', 'PREVIOUS_PARTICIPATION_CDE'))
  
  train_data_instances = sample(nrow(tmp_df), nrow(tmp_df)*percentage_train)
  tmp = 1:nrow(tmp_df)
  test_data_instances = setdiff(tmp, train_data_instances)
  
  train_data = tmp_df[train_data_instances,]
  test_data = tmp_df[test_data_instances,]
  
  nb = naive_bayes(HOST_INSTITUTION_COUNTRY_CDE~., train_data, laplace = laplace)
  
  # home <- as.matrix(table(erasmushome))
  # tmp = nb %class% test_data # Alternative option
  # TODO refactor so that it calculates top - 3 probabilities straight away
  if(n == 1) {
    tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "class", threshold = 0.5)
    f1_score = f1(table(tmp, test_data$HOST_INSTITUTION_COUNTRY_CDE))
    predictions_mlr = data.frame(predicted = tmp, actual = test_data$HOST_INSTITUTION_COUNTRY_CDE)
    correctly_classified = nrow(predictions_mlr[predictions_mlr$predicted == predictions_mlr$actual, ])
    incorrectly_classified = nrow(predictions_mlr[predictions_mlr$predicted != predictions_mlr$actual, ])
  } else if(n == 2) {
    tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "prob", threshold = 0.5)
    tmp = apply(tmp, 1, top_n_accuracy, top_accuracy = n)
    predictions_mlr = data.frame(top_1 = tmp[2,], top_2 = tmp[1, ], actual = test_data$HOST_INSTITUTION_COUNTRY_CDE)
    correctly_classified = nrow(predictions_mlr[predictions_mlr$top_1 == predictions_mlr$actual, ]) +
      nrow(predictions_mlr[predictions_mlr$top_2 == predictions_mlr$actual,])
  }else if(n==3) {
    tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "prob", threshold = 0.5)
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
  
  cat(sprintf("Naive Bayes top-%d accuracy: %f%%\n\n", n, accuracy))
  if(n == 1){
    cat(sprintf("Naive Bayes F1 score: %f\n\n", mean(f1_score)))
  }
  
  # tmp = as.matrix(table(tmp))
  # tmp2 = as.matrix(table(test_data$HOST_INSTITUTION_COUNTRY_CDE))
  # tmp = data.frame(predicted = tmp, actual = tmp2)
  # tmp$actual_predicted = tmp$actual - tmp$predicted
  # tmp[order(tmp$actual_predicted_actual),]
  
  return(accuracy)
}

# Useful functions end --------------------------------------------

# Function used to RUN the program
# Please use it to verify experiment results obtained during the project
# Default values for all parameters are present
# Dataset have to be placed under the following path: "data/SM_2012_13_20141103_01.csv"
#
# We have three primary program modes implemented (value for program_mode parameter):
#   * Classification - 0
#   * Clustering - 1
#   * Descriptive analytics - 2
# For any of the modes work the corresponding file should be present in the same directory as 'main.R'
#
# For classisfication we implemented Naive bayes classifier and Random Forest classifier
# Please set classification_method parameter to one of the following values
#   * 0 - Naive Bayes
#   * 1 - Random Forest
#
# Input parameters:
#   * program_mode - mode in which the program should run
#   * classification_method - (Only if program_mode = 0)
#   * k - hyperparameter used in k-medoids algorithm

run_program <- function(program_mode = 0, classification_method = 0, k = 3) {
  # Main flow of the program --------------------------------------------
  print("----------Project main script execution----------")
  df = read.csv("data/SM_2012_13_20141103_01.csv", header = TRUE, sep = ';') # Reading the whole data
  
  print("----------Starting initial data preprocessing----------")
  # Remove unnecessary attributes
  drop_for_all = c('ID_MOBILITY_CDE', 'STUDENT_ID',
                   'CONSORTIUM_AGREEMENT_NUMBER', 'SPECIAL_NEEDS_SUPPLEMENT_VALUE', 
                   'SHORT_DURATION_CDE', 'QUALIFICATION_AT_HOST_CDE')
  df = remove_columns(df, drop_for_all)
  
  # Filter only data related to study placements
  s_df = df[ df$MOBILITY_TYPE_CDE == 'S' | df$MOBILITY_TYPE_CDE == 'C',]
  
  # Remove attributes realted to the work placement
  drop_for_s = c('MOBILITY_TYPE_CDE', 'PLACEMENT_ENTERPRISE_VALUE', 'PLACEMENT_ENTERPRISE_CTRY_CDE', 
                 'PLACEMENT_ENTERPRISE_SIZE_CDE', 'TYPE_PLACEMENT_SECTOR_VALUE', 'LENGTH_PLACEMENT_VALUE', 
                 'PLACEMENT_START_DATE', 'ECTS_CREDITS_PLACEMENT_AMT', 'PLACEMENT_GRANT_AMT')
  s_df = remove_columns(s_df, drop_for_s)
  
  # Check for missing values
  missing_values = c('? Unknown ?', '???', '?')
  
  colums_to_check = c('HOME_INSTITUTION_CDE', 'HOME_INSTITUTION_CTRY_CDE',  'STUDENT_GENDER_CDE', 
                      'STUDENT_NATIONALITY_CDE', 'STUDENT_SUBJECT_AREA_VALUE', 'STUDENT_STUDY_LEVEL_CDE',
                      'HOST_INSTITUTION_CDE', 'HOST_INSTITUTION_COUNTRY_CDE', 'TAUGHT_HOST_LANGUAGE_CDE',
                      'LANGUAGE_TAUGHT_CDE', 'LINGUISTIC_PREPARATION_CDE', 'PREVIOUS_PARTICIPATION_CDE', 'STUDY_GRANT_AMT')
  missing_values_per_feature = c()
  for (i in 1:length(colums_to_check))
  {
    col = colums_to_check[i]
    number_of_missing_values = nrow(s_df[s_df[[col]] %in% missing_values,])
    missing_values_per_feature[i] = number_of_missing_values
  }
  
  # Report missing values
  if(max(missing_values_per_feature) > 0) {
    cat(sprintf("\nWARNING: Some missing values are present\n"))
    print(missing_values_per_feature)
  }
  
  # Necessary data preprocessing
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
  
  # Mapping Languages properly
  s_df$LANGUAGE_TAUGHT_CDE = unlist(lapply(s_df$LANGUAGE_TAUGHT_CDE, as.character))
  s_df$LANGUAGE_TAUGHT_CDE = toupper(s_df$LANGUAGE_TAUGHT_CDE)
  s_df$LANGUAGE_TAUGHT_CDE = unlist(lapply(s_df$LANGUAGE_TAUGHT_CDE, as.factor))
  
  print("----------Initial data preprocessing is OVER----------\n")
  # Checking program mode:
  #   * Classification - 0
  #   * Clustering - 1
  #   * Descriptive analytics - 2
  if(program_mode == 0) {
    print("----------Starting classification----------")
    if(classification_method == 0) {
      # Classification using Naive Bayes method
    } else if(classification_method == 1) {
      # Classification using Random Forest method
    } else {
      cat(sprintf("\nUnknown value for classification_method parameter: %d\n", classification_method))
    }
    print("----------Classification is over!----------")
  } else if(program_mode == 1) {
    if(k < 1 || k > nrow(s_df)) {
      cat(sprintf("\nValue for k is out of bounds! k = %d\n", k))
      print("----------Starting clustering----------")
      print("----------Clustering is over!----------")
    } else {
      # Performing clustering on data
      # Detailed explanation of clustering strategy is present in the report
    }
  } else if(program_mode == 2) {
    print("----------Starting descriptive analytics----------")
    print("----------Descriptive analytics over!----------")
  } else {
    cat(sprintf("\nUnknown value for program_mode parameter: %d\n", program_mode))
  }
  print("----------Main script execution is over----------")
  # Main flow of the program end --------------------------------------------
}