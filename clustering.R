# Big Data project code 
#
# Anna Dieckvoss - 1127324
# Borys Komarov - 0889421
#
# File containing useful function for the clustering task

# Useful aggregation & manipulation functions
library(dplyr)
library(cluster)
library(factoextra)

# Function used for data normalisation
normalize <- function(x){
  return((x-min(x))/max(x)-min(x))
}

clustering <- function(s_df, k = 2) {
  cat(sprintf("----------Starting clustering!----------\n"))
  # FOR CLUSTERING
  # Create a separate table with summary information about Host universities
  # Arrtibutes:
  #   * Number of universities which have connection to this university HOME_INSTITUTION_CDE
  tmp = table(s_df$HOST_INSTITUTION_CDE, s_df$HOME_INSTITUTION_CDE)
  connected_universities = rowSums(tmp>0)
  #   * Number of students which came to this university
  students_per_university = table(s_df$HOST_INSTITUTION_CDE)
  #   * Average/Median age of student (We can use median to overcome outliers) per university STUDENT_AGE_VALUE
  median_age = aggregate(STUDENT_AGE_VALUE ~ HOST_INSTITUTION_CDE, s_df, median)
  #   * Number of programs per university STUDENT_SUBJECT_AREA_VALUE
  tmp = table(s_df$HOST_INSTITUTION_CDE, s_df$STUDENT_SUBJECT_AREA_VALUE)
  subjects_per_uni = rowSums(tmp>0)
  #   * Average grant amount per university STUDY_GRANT_AMT
  grant_amount = aggregate(STUDY_GRANT_AMT ~ HOST_INSTITUTION_CDE, s_df, mean)
  #   * Number of languages per university LANGUAGE_TAUGHT_CDE
  tmp = table(s_df$HOST_INSTITUTION_CDE, s_df$LANGUAGE_TAUGHT_CDE)
  languages_per_uni = rowSums(tmp>0)
  #   * Average number of ECTS points for student per university ECTS_CREDITS_STUDY_AMT
  average_ects = aggregate(ECTS_CREDITS_STUDY_AMT ~ HOST_INSTITUTION_CDE, s_df, mean)
  #   * Average/median (TO DECIDE) length of the program per university LENGTH_STUDY_PERIOD_VALUE
  average_length = aggregate(LENGTH_STUDY_PERIOD_VALUE ~ HOST_INSTITUTION_CDE, s_df, mean)
  
  # Packing up descriptive analytics results into a new data frame
  tmp_results = median_age
  tmp_results$MEDIAN_AGE = median_age$STUDENT_AGE_VALUE
  tmp_results$STUDENT_AGE_VALUE = NULL
  tmp_results$AVERAGE_GRANT_AMOUNT = grant_amount$STUDY_GRANT_AMT
  tmp_results$AVERAGE_ECTS = average_ects$ECTS_CREDITS_STUDY_AMT
  tmp_results$STUDY_LENGTH = average_length$LENGTH_STUDY_PERIOD_VALUE
 
  tmp_results$LANGUAGES_TAUGHT = languages_per_uni
  tmp_results$SUBJECTS_TAUGHT = subjects_per_uni
  tmp_results$STUDENTS_ARRIVED = students_per_university
  tmp_results$CONNECTED_UNIVERSITIES = connected_universities
  
  df = tmp_results
  
  # Backing up the resulting dataset
  write.csv(df, file = 'data/clustering_data.csv', row.names = FALSE)
  
  clustering_results_file = "data/clustering_results.csv"
  Data = df
  Data_n=Data[,-1]
  # Normalize the data
  for (i in colnames(Data_n)){
    Data_n[,i]=normalize(Data_n[,i])
  }
  
  Data_n = Data_n[,c("CONNECTED_UNIVERSITIES", "LANGUAGES_TAUGHT", "SUBJECTS_TAUGHT", "STUDENTS_ARRIVED", "STUDY_LENGTH")]
  
  # Finding optimal number of clusters for our problem - calculating silhoutette width
  # The average silhouette measures the quality of a clustering
  fviz_nbclust(Data_n,pam, method = c("silhouette"))
  
  
  
  pam.res <- pam(Data_n, k)
  
  # Add the point classifications to the original data
  clusterdata <- cbind(Data, cluster = pam.res$cluster)
  write.csv(clusterdata, file = clustering_results_file)
  
  cat(sprintf("Clustering results can be found under: %s\n", clustering_results_file))
  
  cat(sprintf("----------Clustering is done!----------\n"))
}