# Big Data project code 
#
# Anna Dieckvoss - 1127324
# Borys Komarov - 0889421

library(data.table)

# Useful functions --------------------------------------------

# Function implementing the Euclidean distance algorithm
euclidean_distance <- function(v1, v2) {
  distance = 0
  if(any(is.na(v1))) {
    distance = abs(v2[1] - v2[2])
  } else if(any(is.na(v2))) {
    distance = abs(v1[1] - v1[2])
  } else {
    distance = sqrt((v1[1] - v1[2])^2 + (v2[1] - v2[2])^2)
  }
  return(distance)
}

# Function which reads the desired dataset
# Takes 1 param corresponding to the dataset:
# 0 - Students mobility dataset
# 1 - Staff mobility dataset
# 2 - Teaching mobility dataset
read_data <- function(dataset = 0) {
  dt = NULL
  if(dataset == 0) {
    dt = fread("data/SM_2012_13_20141103_01.csv")
  } else if(dataset == 1) {
    dt = fread("data/STT_2012_13_20141103.csv")
  } else if(dataset == 2){
    dt = fread("data/STA_2012_13_20141103")
  }
  return(dt)
}

# Useful functions end --------------------------------------------

# Main flow of the program --------------------------------------------
print("Project main script execution")
student_dt = read_data(0)


# Main flow of the program end --------------------------------------------