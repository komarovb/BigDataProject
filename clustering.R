# Big Data project code 
#
# Anna Dieckvoss - 1127324
# Borys Komarov - 0889421
#
# File containing useful function for the clustering task

# Useful aggregation & manipulation functions
library(dplyr)

clustering <- function(s_df, k = 3) {
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
  #   * Percentage of people who failed the program, proportion (failed / total) per university
  
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
 
  # Backing up the resulting dataset
  write.csv(tmp_results, file = 'clustering_data.csv', row.names = FALSE)
  
  df = tmp_results
  # Call k-medoids function and perform clustering here
  
  cat(sprintf("----------Clustering is done!----------\n"))
}