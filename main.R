# Big Data project code 
#
# Anna Dieckvoss - 1127324
# Borys Komarov - 0889421
#
# Please use run_program function to run the program!


# Useful aggregation & manipulation functions
library(dplyr)

# Useful functions --------------------------------------------

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
  df[] = lapply(df, function(x) if(is.factor(x)) factor(x) else x)
  return(df)
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

run_program <- function(program_mode = 0, classification_method = 0, k = 2) {
  # Main flow of the program --------------------------------------------
  print("----------Project main script execution----------")
  df = read.csv("data/SM_2012_13_20141103_01.csv", header = TRUE, sep = ';') # Reading the whole data
  
  print("----------Starting initial data preprocessing----------")
  # Remove unnecessary attributes
  drop_for_all = c('ID_MOBILITY_CDE',
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
  
  # Study grant attribute normalization!
  s_df$STUDY_GRANT_AMT = as.character(s_df$STUDY_GRANT_AMT)
  s_df$STUDY_GRANT_AMT = lapply(s_df$STUDY_GRANT_AMT, transform_grant)
  s_df$STUDY_GRANT_AMT = unlist(s_df$STUDY_GRANT_AMT)
  
  print("----------Initial data preprocessing is OVER----------\n")
  # Checking program mode:
  #   * Classification - 0
  #   * Clustering - 1
  #   * Descriptive analytics - 2
  if(program_mode == 0) {
    print("----------Starting classification----------")
    source('./classification.R')
    
    print("----------Data preprocessing was started----------")
    target = s_df$HOST_INSTITUTION_COUNTRY_CDE
    # Removing HOST_INSTITUTION_CDE attribute which has a direct correlatio with the target variable
    s_df = remove_columns(s_df, c('HOST_INSTITUTION_COUNTRY_CDE', 'HOST_INSTITUTION_CDE', 'STUDENT_ID'))
    s_df$HOST_INSTITUTION_COUNTRY_CDE = target
    
    print("----------Data preprocessing is over----------")
    if(classification_method == 0) {
      # Classification using Naive Bayes method
      laplace = 0.5
      with_balancing = FALSE
      accuracy = custom_naive_bayes(s_df, 0.5, with_balancing = with_balancing)
    } else if(classification_method == 1) {
      s_df = remove_columns(s_df, c('HOME_INSTITUTION_CDE'))
      # Classification using Random Forest method
      number_of_trees = 300
      mtry = 3
      percentage_train = 0.7
      with_balancing = FALSE
      custom_random_forest(s_df, number_of_trees = number_of_trees, mtry = mtry, percentage_train = percentage_train, with_balancing = with_balancing)
    } else {
      cat(sprintf("\nUnknown value for classification_method parameter: %d\n", classification_method))
    }
    print("----------Classification is over!----------")
  } else if(program_mode == 1) {
    if(k < 1 || k > nrow(s_df)) {
      cat(sprintf("\nValue for k is out of bounds! k = %d\n", k))
    } else {
      # Performing clustering on data
      # Detailed explanation of clustering strategy is present in the report
      print("----------Starting clustering----------")
      source('./clustering.R')
      clustering(s_df)
      print("----------Clustering is over!----------")
    }
  } else {
    cat(sprintf("\nUnknown value for program_mode parameter: %d\n", program_mode))
  }
  print("----------Main script execution is over----------")
  # Main flow of the program end --------------------------------------------
}