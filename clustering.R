# Big Data project code 
#
# Anna Dieckvoss - 1127324
# Borys Komarov - 0889421
#
# File containing useful function for the clustering task

clustering <- function(tmp_df, k = 3) {
  cat(sprintf("----------Starting clustering!----------\n"))
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
  
  
  # Call k-medoids function here
  cat(sprintf("----------Clustering is done!----------\n"))
}