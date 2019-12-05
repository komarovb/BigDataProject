# Big Data project code 
#
# Anna Dieckvoss - 1127324
# Borys Komarov - 0889421
#
# File containing useful function for the classification task

# Naive Bayes implementation:
library(naivebayes) # https://github.com/majkamichal/naivebayes

# Random forest implementation
library(randomForest)

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

# Function which finds the accuracies for features for given test and train data
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
#   * data - dataset with the last column as a target variable
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

# Function used to slightly balance classes
balance_classes <- function(s_df) {
  cat(sprintf("----------Starting balancing classes!----------\n"))
  before = sort(table(s_df$HOST_INSTITUTION_COUNTRY_CDE))
  bottom10 = before[1:10]
  top10 = before[(length(before)-10):length(before)]
  cat(sprintf("----------Number of instances in top 10 classes: %d----------\n", sum(top10)))
  cat(sprintf("----------Number of instances in bottom 10 classes: %d----------\n", sum(bottom10)))
  
  for(i in 1:length(bottom10)) {
    under_represented_instances = s_df[s_df$HOST_INSTITUTION_COUNTRY_CDE %in% names(bottom10[i]),]
    number_of_instances_to_take = sample(nrow(under_represented_instances), nrow(under_represented_instances)*3, replace = TRUE)
    to_add = under_represented_instances[number_of_instances_to_take,]
    rownames(to_add) = NULL
    s_df = rbind(s_df, to_add)
  }
  
  over_represented_instances = s_df[s_df$HOST_INSTITUTION_COUNTRY_CDE %in% names(top10),]
  number_of_instances_to_take = sample(nrow(over_represented_instances), nrow(over_represented_instances)*0.5)
  to_remove = over_represented_instances[number_of_instances_to_take,]
  to_remove = as.numeric(rownames(to_remove))
  s_df = s_df[-to_remove,]
  
  cat(sprintf("----------Class balancing is over!----------\n"))
  return(s_df)
}

# Custom application of the Naive Bayes method
# All the data preprocessing should be done beforehand!
custom_naive_bayes <- function(tmp_df, percentage_train, laplace, with_balancing=FALSE) {
  # Predict: HOST_INSTITUTION_COUNTRY_CDE
  cat(sprintf("----------Starting Naive Bayes classifier!----------\n"))
  
  # Features selection
  target = tmp_df$HOST_INSTITUTION_COUNTRY_CDE
  selected_features = sfs(tmp_df)
  tmp_df = tmp_df[selected_features]
  tmp_df$HOST_INSTITUTION_COUNTRY_CDE = target
  
  accuracies1 = c()
  accuracies2 = c()
  accuracies3 = c()
  # Randomly permute data
  tmp_df<-tmp_df[sample(nrow(tmp_df)),]
  for(k in 1:5) {
    instances_per_fold = ceiling(nrow(tmp_df) / 5)
    
    #Select testing instances
    test_instances = ((k-1)*instances_per_fold):(k*instances_per_fold)
    tmp = 1:nrow(tmp_df)
    train_instances = setdiff(tmp, test_instances)
    
    test_data = tmp_df[test_instances,]
    train_data = tmp_df[train_instances,]
    if(with_balancing) {
      train_data = balance_classes(train_data)
      train_data = balance_classes(train_data)
      train_data = balance_classes(train_data)
    }
    
    nb = naive_bayes(HOST_INSTITUTION_COUNTRY_CDE~., train_data, laplace = laplace)
    
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
    
    accuracies1[k] = accuracy1
    accuracies2[k] = accuracy2
    accuracies3[k] = accuracy3
  }
  
  cat(sprintf("Naive Bayes top-1 accuracy: %f%%\n\n", mean(accuracies1)))
  cat(sprintf("Naive Bayes F1 score: %f\n\n", mean(f1_score)))
  cat(sprintf("Naive Bayes top-2 accuracy: %f%%\n\n", mean(accuracies2)))
  cat(sprintf("Naive Bayes top-3 accuracy: %f%%\n\n", mean(accuracies3)))
  
  cat(sprintf("----------Naive Bayes classifier work is over!----------\n"))
}

# Function testing Random Forest
custom_random_forest <- function(tmp_df, number_of_trees = 200, mtry = 4, percentage_train = 0.7, with_balancing=FALSE) {
  cat(sprintf("----------Starting Random Forest classifier!----------\n"))
  
  # Features selection
  target = tmp_df$HOST_INSTITUTION_COUNTRY_CDE
  selected_features = sfs(tmp_df)
  tmp_df = tmp_df[selected_features]
  tmp_df$HOST_INSTITUTION_COUNTRY_CDE = target
  
  train_data_instances = sample(nrow(tmp_df), nrow(tmp_df)*percentage_train)
  tmp = 1:nrow(tmp_df)
  test_data_instances = setdiff(tmp, train_data_instances)
  train_data = tmp_df[train_data_instances,]
  test_data = tmp_df[test_data_instances,]
  if(with_balancing) {
    train_data = balance_classes(train_data)
    train_data = balance_classes(train_data)
    train_data = balance_classes(train_data)
  }
  
  start_time = Sys.time()
  rf <- randomForest(HOST_INSTITUTION_COUNTRY_CDE~., data = train_data, ntree = number_of_trees, mtry = mtry, importance = TRUE)
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
  cat(sprintf("----------Random Forest classifier work is over!----------\n"))
}





