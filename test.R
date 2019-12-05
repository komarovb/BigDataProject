#install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

normalize <- function(x){
  return((x-min(x))/max(x)-min(x))
}
Data= read.csv(file = 'clustering_data.csv',TRUE,",")
#remove first row host_institution_C
Data_n=Data[,-1]
#normalize the data
for (i in colnames(Data_n)){
  Data_n[,i]=normalize(Data_n[,i])
}

#optimal number of clusters- calculating silhoutette width
#The average silhouette measures the quality of a clustering
fviz_nbclust(Data_n,pam, method = c("silhouette"))



#pam(x, k, metric = "euclidean", stand = FALSE)
pam.res <- pam(Data_n,2)
#print(pam.res)

#add the point classifications to the original data
clusterdata <- cbind(Data, cluster = pam.res$cluster)
#head(clusterdata, n = 5)
write.csv(clusterdata, file = "clusters_resnorm.csv")


