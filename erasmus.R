
library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

# Remove selected columns
remove_columns <- function(df, columns_to_drop) {
  col_names = colnames(df)
  col_names = col_names[! col_names %in% columns_to_drop]
  df = df[, col_names]
  df[] = lapply(df, function(x) if(is.factor(x)) factor(x) else x)  # TODO refactor!
  return(df)
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
# Update study grant Amt!
s_df$STUDY_GRANT_AMT = as.character(s_df$STUDY_GRANT_AMT)
s_df$STUDY_GRANT_AMT = unlist(lapply(s_df$STUDY_GRANT_AMT, transform_grant))

#lower instances
small_data_instances = sample(nrow(s_df), 100)
small_data = s_df[small_data_instances,]


#language(3)
#'TAUGHT_HOST_LANGUAGE_CDE', 'LANGUAGE_TAUGHT_CDE', 'LINGUISTIC_PREPARATION_CDE',
drop_for_lang = c('LENGTH_STUDY_PERIOD_VALUE', 'STUDY_START_DATE', 'ECTS_CREDITS_STUDY_AMT',
                  'TOTAL_ECTS_CREDITS_AMT','NUMB_YRS_HIGHER_EDUCAT_VALUE' ,'STUDENT_SUBJECT_AREA_VALUE',  
                  'STUDENT_STUDY_LEVEL_CDE','HOST_INSTITUTION_CDE', 'HOST_INSTITUTION_COUNTRY_CD',
                  'HOME_INSTITUTION_CDE', 'HOME_INSTITUTION_CTRY_CDE','STUDENT_AGE_VALUE', 'STUDENT_GENDER_CDE',
                  'STUDENT_NATIONALITY_CDE')
s_dflanguage=remove_columns(s_df, drop_for_lang)

#programm relatet(7)
#'LENGTH_STUDY_PERIOD_VALUE', 'STUDY_START_DATE', 'ECTS_CREDITS_STUDY_AMT',  'TOTAL_ECTS_CREDITS_AMT',
#' 'NUMB_YRS_HIGHER_EDUCAT_VALUE' ,'STUDENT_SUBJECT_AREA_VALUE',  'STUDENT_STUDY_LEVEL_CDE',
drop_for_prog = c('TAUGHT_HOST_LANGUAGE_CDE', 'LANGUAGE_TAUGHT_CDE', 'LINGUISTIC_PREPARATION_CDE',
                  'HOST_INSTITUTION_CDE', 'HOST_INSTITUTION_COUNTRY_CD','HOME_INSTITUTION_CDE', 
                  'HOME_INSTITUTION_CTRY_CDE','STUDENT_AGE_VALUE', 'STUDENT_GENDER_CDE', 'STUDENT_NATIONALITY_CDE')
s_dfprog=remove_columns(s_df, drop_for_prog)

#geographical(4)
#'HOST_INSTITUTION_CDE', 'HOST_INSTITUTION_COUNTRY_CD','HOME_INSTITUTION_CDE', 'HOME_INSTITUTION_CTRY_CDE'
drop_for_geo = c('TAUGHT_HOST_LANGUAGE_CDE', 'LANGUAGE_TAUGHT_CDE', 'LINGUISTIC_PREPARATION_CDE',
                 'LENGTH_STUDY_PERIOD_VALUE', 'STUDY_START_DATE', 'ECTS_CREDITS_STUDY_AMT',  'TOTAL_ECTS_CREDITS_AMT',
                 'NUMB_YRS_HIGHER_EDUCAT_VALUE' ,'STUDENT_SUBJECT_AREA_VALUE',  'STUDENT_STUDY_LEVEL_CDE',
                 'STUDENT_AGE_VALUE', 'STUDENT_GENDER_CDE', 'STUDENT_NATIONALITY_CDE')
s_dfgeo=remove_columns(s_df, drop_for_geo)

#personal(3)
#'STUDENT_AGE_VALUE', 'STUDENT_GENDER_CDE', 'STUDENT_NATIONALITY_CDE')
drop_for_perso = c('TAUGHT_HOST_LANGUAGE_CDE', 'LANGUAGE_TAUGHT_CDE', 'LINGUISTIC_PREPARATION_CDE',
                   'LENGTH_STUDY_PERIOD_VALUE', 'STUDY_START_DATE', 'ECTS_CREDITS_STUDY_AMT',
                   'TOTAL_ECTS_CREDITS_AMT','NUMB_YRS_HIGHER_EDUCAT_VALUE' ,'STUDENT_SUBJECT_AREA_VALUE',  
                   'STUDENT_STUDY_LEVEL_CDE','HOST_INSTITUTION_CDE', 'HOST_INSTITUTION_COUNTRY_CD',
                   'HOME_INSTITUTION_CDE', 'HOME_INSTITUTION_CTRY_CDE')
s_dfperso=remove_columns(s_df, drop_for_perso)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#library(cluster)
#library(factoextra)
#head(s_df, n = 5) 

#The average silhouette measures the quality of a clustering
#fviz_nbclust(s_df,pam, method = c("silhouette"))
#pamk(s_df,krange=1:15,criterion="asw", usepam=TRUE)
#pam.res <- pam(s_df, 2)
#print(pam.res)

small_data_instances = sample(nrow(s_dfprog), 10000)
small_data = s_dfprog[small_data_instances,]

#https://towardsdatascience.com/clustering-on-mixed-type-data-8bbd0a2569c3
#' Compute Gower distance
gower_dist <- daisy(small_data, metric = "gower")
gower_mat <- as.matrix(gower_dist)#' Print most similar clients
#' Print most similar clients
small_data[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
#' Print most dissimilar clients
small_data[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

#k silhoutette width
sil_width <- c(NA)
for(i in 2:10){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:10, sil_width,
      xlab = "Number of clusters",
      ylab = "Silhouette Width")
lines(1:10, sil_width)

#clustering algorithm
k <- 22
# pam_fit <- pam(gower_dist, diss = TRUE, k)
# pam_results <- small_data
#  mutate(cluster = pam_fit$clustering) 
#  group_by(cluster)
#  do(the_summary = summary(.))
# pamres=pam_results$the_summary
# write.csv(pamres, file = "pamresults.csv")

#plot low dimensional embedding of high-dimensional data, distances or similarities
#tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
#tsne_data <- tsne_obj$Y 
#  data.frame()
#  setNames(c("X", "Y")) 
#  mutate(cluster = factor(pam_fit$clustering))
#ggplot(aes(x = X, y = Y), data = tsne_data) +
#  geom_point(aes(color = cluster))







#https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9
#library(cluster)
#gower.dissimilarity.mtrx <- daisy(small_data, metric = c("gower"))
#dissimilarity.mtrx.csv.content = as.matrix(gower.dissimilarity.mtrx)
#write.table(dissimilarity.mtrx.csv.content,
#            'dissimilarity.mtrx.csv', 
#            row.names=FALSE, 
#            col.names=FALSE, 
#            sep=",")
#dist <- gower.dissimilarity.mtrx
#pamx <- pam(dist, k)
#sil = silhouette (pamx$clustering, dist)
#plot(sil)


