library(data.table) # TODO remove because not used!

# Features selection
library(CORElearn)

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

# Taken from: https://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
f1 <- function(df) {
  diag = diag(df)
  rowsums = apply(df, 1, sum)
  colsums = apply(df, 2, sum)
  diag[diag == 0] = 0.00001
  rowsums[rowsums == 0] = 0.00001
  colsums[colsums == 0] = 0.00001
  
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

# Functions which finds the accuracies for features for given test and train data
best_feature <- function (test_data, train_data, features, selected_features, class_variable) {
  accuracies = array(0, ncol(train_data) -1)
  # Find the best feature using accuracy measurments - ML algo
  for(i in features) {
    col_name = colnames(train_data)[i]
    tmp_selected_features = c(selected_features, i)
    tmp = colnames(train_data)[tmp_selected_features]
    
    # Trick to make formula more dynamic, inspired by:
    # http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
    resulting_varialbe = colnames(train_data)[class_variable]
    variables = c('.')
    f <- as.formula(paste(resulting_varialbe, paste(variables, collapse = " + "), sep = " ~ "))
    
    nb = naivebayes::naive_bayes(f, train_data[,c(tmp, colnames(train_data[class_variable]))], laplace = 0.5)
    
    predicted = predict(nb, test_data[ , tmp, drop=FALSE], type = "class", threshold = 0.5)
    predictions_mlr = data.frame(predicted = predicted, actual = test_data[[class_variable]])
    correctly_classified = nrow(predictions_mlr[predictions_mlr$predicted == predictions_mlr$actual, ])
    
    accuracies[i] = (correctly_classified / nrow(test_data)) * 100
  }
  
  return(accuracies)
}

# The function takes one argument
# *data - dataset with the last column as a target variable
#
# The function prints to console the set of important features
sfs <- function(data) {
  cat(sprintf("Starting Sequential Forwards Selection algorithm for dataset\n\n"))
  
  df = data
  class_variable = ncol(df)
  current_accuracy = 0
  accuracy_gain = 1
  features = 1:(ncol(df)-1)
  selected_features = c()
  # The algorithm would run until we stop improving the performance
  while (accuracy_gain > 0){
    accuracy_gain = 0
    
    best_features_per_fold = c()
    best_accuracies_per_fold = c()
    # Iterate through splits taking 1 fold as test and remaining as train
    for(k in 1:5) {
      instances_per_fold = ceiling(nrow(df) / 5)
      
      # Randomly permute data
      df<-df[sample(nrow(df)),]
      #Select testing instances
      test_instances = ((k-1)*instances_per_fold):(k*instances_per_fold)
      tmp = 1:nrow(df)
      train_instances = setdiff(tmp, test_instances)
      
      test_data = df[test_instances,]
      train_data = df[train_instances,]
      
      # Get accuracies for current fold
      accuracies = best_feature(test_data, train_data, features, selected_features, class_variable)
      # Select best feature and add to the array
      best_feature = which.max(accuracies)
      best_accuracy = accuracies[best_feature]
      
      best_features_per_fold[k] = best_feature
      best_accuracies_per_fold[k] = best_accuracy
    }
    
    # Select best accuracy
    # best_feature = best_features_per_fold[which.max(best_accuracies_per_fold)]
    best_feature = as.numeric(names(which.max(table(best_features_per_fold))))
    best_accuracy = as.numeric(names(which.max(table(best_accuracies_per_fold))))
    
    selected_features = c(selected_features, best_feature)
    
    # Remove best feature from features
    features = features[!features %in% best_feature]
    
    # Assign new accuracy
    new_accuracy = best_accuracy
    accuracy_gain = new_accuracy - current_accuracy
    current_accuracy = new_accuracy
  }
  cat(sprintf("Best features:\n"))
  print(colnames(df[selected_features]))
  cat(sprintf("\nSequential Forwards Selection execution finished, please see results in console above\n\n"))
  
  return(colnames(df[selected_features]))
}

# Custom application of the Naive Bayes method
# All the data preprocessing should be done beforehand!
custom_naive_bayes <- function(tmp_df, percentage_train, laplace) {
  # Predict: HOST_INSTITUTION_COUNTRY_CDE
  
  train_data_instances = sample(nrow(tmp_df), nrow(tmp_df)*percentage_train)
  tmp = 1:nrow(tmp_df)
  test_data_instances = setdiff(tmp, train_data_instances)
  
  train_data = tmp_df[train_data_instances,]
  test_data = tmp_df[test_data_instances,]
  
  nb = naive_bayes(HOST_INSTITUTION_COUNTRY_CDE~., train_data, laplace = laplace)
  
  # home <- as.matrix(table(erasmushome))
  # tmp = nb %class% test_data # Alternative option
  # TODO refactor so that it calculates top - 3 probabilities straight away
  
  tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "class", threshold = 0.5)
  f1_score = f1(table(tmp, test_data$HOST_INSTITUTION_COUNTRY_CDE))
  predictions_mlr = data.frame(predicted = tmp, actual = test_data$HOST_INSTITUTION_COUNTRY_CDE)
  correctly_classified1 = nrow(predictions_mlr[predictions_mlr$predicted == predictions_mlr$actual, ])
  incorrectly_classified = nrow(predictions_mlr[predictions_mlr$predicted != predictions_mlr$actual, ])

  tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "prob", threshold = 0.5)
  tmp = apply(tmp, 1, top_n_accuracy, top_accuracy = 2)
  predictions_mlr = data.frame(top_1 = tmp[2,], top_2 = tmp[1, ], actual = test_data$HOST_INSTITUTION_COUNTRY_CDE, stringsAsFactors = FALSE)
  correctly_classified2 = nrow(predictions_mlr[predictions_mlr$top_1 == predictions_mlr$actual, ]) +
                                                       nrow(predictions_mlr[predictions_mlr$top_2 == predictions_mlr$actual,])

  tmp = predict(nb, test_data[ , !(colnames(test_data) == 'HOST_INSTITUTION_COUNTRY_CDE')], type = "prob", threshold = 0.5)
  tmp = apply(tmp, 1, top_n_accuracy, top_accuracy = 3)
  predictions_mlr = data.frame(top_1 = tmp[3,], top_2 = tmp[2, ], top_3 = tmp[1, ], actual = test_data$HOST_INSTITUTION_COUNTRY_CDE, stringsAsFactors = FALSE)
  correctly_classified3 = nrow(predictions_mlr[predictions_mlr$top_1 == predictions_mlr$actual, ]) + 
    nrow(predictions_mlr[predictions_mlr$top_2 == predictions_mlr$actual,]) + 
    nrow(predictions_mlr[predictions_mlr$top_3 == predictions_mlr$actual,])
  
  # t2 = colnames(tmp2)[apply(tmp2,1,which.max)]
  accuracy1 = (correctly_classified1 / nrow(test_data)) * 100
  accuracy2 = (correctly_classified2 / nrow(test_data)) * 100
  accuracy3 = (correctly_classified3 / nrow(test_data)) * 100
  
  cat(sprintf("Naive Bayes top-1 accuracy: %f%%\n\n", accuracy1))
  cat(sprintf("Naive Bayes F1 score: %f\n\n", mean(f1_score)))
  cat(sprintf("Naive Bayes top-2 accuracy: %f%%\n\n", accuracy2))
  cat(sprintf("Naive Bayes top-3 accuracy: %f%%\n\n", accuracy3))
  
  return(accuracy)
}

df = read.csv("data/SM_2012_13_20141103_01.csv", header = TRUE, sep = ';') # Reading the whole data


# Filter unnecessary attributes based on the data structure for each type of placement
# Remove for all:
# ID_MOBILITY_CDE, CONSORTIUM_AGREEMENT_NUMBER, SPECIAL_NEEDS_SUPPLEMENT_VALUE, 
drop_for_all = c('ID_MOBILITY_CDE', 'STUDENT_ID',
                 'CONSORTIUM_AGREEMENT_NUMBER', 'SPECIAL_NEEDS_SUPPLEMENT_VALUE', 
                 'SHORT_DURATION_CDE', 'QUALIFICATION_AT_HOST_CDE')

df = remove_columns(df, drop_for_all)

# Split data based on what was the type of placement for further processing
s_df = df[ df$MOBILITY_TYPE_CDE == 'S' | df$MOBILITY_TYPE_CDE == 'C',]
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

s_df[ s_df$HOME_INSTITUTION_COUNTRY_CDE %in% missing_values, ]
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
  number_of_missing_values = nrow(s_df[s_df[[col]] %in% missing_values,])
  missing_values_per_feature[i] = number_of_missing_values
}

if(max(missing_values_per_feature) > 0) {
  cat(sprintf("\nWARNING: Some missing values are present\n"))
  print(missing_values_per_feature)
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

# Mapping Languages properly
s_df$LANGUAGE_TAUGHT_CDE = unlist(lapply(s_df$LANGUAGE_TAUGHT_CDE, as.character))
s_df$LANGUAGE_TAUGHT_CDE = toupper(s_df$LANGUAGE_TAUGHT_CDE)
s_df$LANGUAGE_TAUGHT_CDE = unlist(lapply(s_df$LANGUAGE_TAUGHT_CDE, as.factor))

# Update study grant Amt!
s_df$STUDY_GRANT_AMT = as.character(s_df$STUDY_GRANT_AMT)
s_df$STUDY_GRANT_AMT = lapply(s_df$STUDY_GRANT_AMT, transform_grant)
s_df$STUDY_GRANT_AMT = unlist(s_df$STUDY_GRANT_AMT)


train_percentage = 0.7

# tmp = table(s_df$HOST_INSTITUTION_COUNTRY_CDE)
# tmp[order(names(tmp))]
# INFORMATION GAIN
# feature_gain = attrEval(HOST_INSTITUTION_COUNTRY_CDE~., tmp_df,  estimator = "InfGain")

# Features selection
#target = s_df$HOST_INSTITUTION_COUNTRY_CDE
#s_df = remove_columns(s_df, c('HOST_INSTITUTION_COUNTRY_CDE', 'HOST_INSTITUTION_CDE'))
#s_df$HOST_INSTITUTION_COUNTRY_CDE = target

#selected_features = sfs(s_df)
#s_df = s_df[selected_features]
#s_df$HOST_INSTITUTION_COUNTRY_CDE = target


# Naive Bayes
# accuracy = custom_naive_bayes(s_df, train_percentage, 0.5)

implement_rf = TRUE;

if(implement_rf) {
  # Random forest implementation: https://www.stat.berkeley.edu/~breiman/RandomForests/
  tmp_df = remove_columns(s_df, c('HOME_INSTITUTION_CDE'))
  
  target = tmp_df$HOST_INSTITUTION_COUNTRY_CDE
  tmp_df = remove_columns(tmp_df, c('HOST_INSTITUTION_COUNTRY_CDE', 'HOST_INSTITUTION_CDE'))
  tmp_df$HOST_INSTITUTION_COUNTRY_CDE = target
  
  selected_features = sfs(tmp_df)
  tmp_df = tmp_df[selected_features]
  tmp_df$HOST_INSTITUTION_COUNTRY_CDE = target
  
  train_data_instances = sample(nrow(tmp_df), nrow(tmp_df)*0.7)
  tmp = 1:nrow(tmp_df)
  test_data_instances = setdiff(tmp, train_data_instances)
  train_data = tmp_df[train_data_instances,]
  test_data = tmp_df[test_data_instances,]
  
  
  start_time = Sys.time()
  rf <- randomForest(HOST_INSTITUTION_COUNTRY_CDE~., data = train_data, ntree = 200, mtry = 4, importance = TRUE)
  end_time = Sys.time()
  
  cat(sprintf("Random Forest was built in: %f\n\n", end_time - start_time))
  
  tmp = predict(rf, test_data, type = "class")
  f1_score = f1(table(tmp, test_data$HOST_INSTITUTION_COUNTRY_CDE))
  predictions_mlr = data.frame(predicted = tmp, actual = test_data$HOST_INSTITUTION_COUNTRY_CDE)
  correctly_classified1 = nrow(predictions_mlr[predictions_mlr$predicted == predictions_mlr$actual, ])
  
  tmp = predict(rf, test_data, type = "prob")
  tmp = apply(tmp, 1, top_n_accuracy, top_accuracy = 2)
  predictions_mlr = data.frame(top_1 = tmp[2,], top_2 = tmp[1, ], actual = test_data$HOST_INSTITUTION_COUNTRY_CDE, stringsAsFactors = FALSE)
  correctly_classified2 = nrow(predictions_mlr[predictions_mlr$top_1 == predictions_mlr$actual, ]) +
    nrow(predictions_mlr[predictions_mlr$top_2 == predictions_mlr$actual,])
  
  tmp = predict(rf, test_data, type = "prob")
  tmp = apply(tmp, 1, top_n_accuracy, top_accuracy = 3)
  predictions_mlr = data.frame(top_1 = tmp[3,], top_2 = tmp[2, ], top_3 = tmp[1, ], actual = test_data$HOST_INSTITUTION_COUNTRY_CDE, stringsAsFactors = FALSE)
  correctly_classified3 = nrow(predictions_mlr[predictions_mlr$top_1 == predictions_mlr$actual, ]) + 
    nrow(predictions_mlr[predictions_mlr$top_2 == predictions_mlr$actual,]) + 
    nrow(predictions_mlr[predictions_mlr$top_3 == predictions_mlr$actual,])
  
  accuracy1 = (correctly_classified1 / nrow(test_data)) * 100
  accuracy2 = (correctly_classified2 / nrow(test_data)) * 100
  accuracy3 = (correctly_classified3 / nrow(test_data)) * 100
  
  cat(sprintf("Random Forest top-1 accuracy: %f%%\n\n", accuracy1))
  cat(sprintf("Random Forest F1 score: %f\n\n", mean(f1_score)))
  cat(sprintf("Random Forest top-2 accuracy: %f%%\n\n", accuracy2))
  cat(sprintf("Random Forest top-3 accuracy: %f%%\n\n", accuracy3))
}

# predictions_mlr[] <- lapply(predictions_mlr, as.character)

# ------------------------------

# He will ask how we handle missing values
# Overfitting and feature selection

# MENTION OVERFITTING


# Add some descriptive analytics and visualisation to the initial results 19.11.2019
# Make presentaiton 19.11.2019




# FOR CLUSTERING
# Create a separate table with summary information about Host universities
# Arrtibutes:
#   * Number of universities which have connection to this university
#   * Number of students which came to this university
#   * Average/Median age of student (We can use median to overcome outliers)
#   * Number of programs
#   * Average grant amount
#   * Number of languages
#   * Average number of ECTS points
#   * Average/median (TO DECIDE) length of the program
#   * Percentage of people who failed the program, proportion (failed / total)















