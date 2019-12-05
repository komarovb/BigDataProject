install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

normalize <- function(x){
  return((x-min(x))/max(x)-min(x))
}
Data= read.csv(file = 'clustering_data.csv',TRUE,",")
Data=Data[,-1]
#normalize the data
for (i in colnames(Data)){
  Data[,i]=normalize(Data[,i])
}

#optimal number of clusters- calculating silhoutette width
#The average silhouette measures the quality of a clustering
fviz_nbclust(Data,pam, method = c("silhouette"))



#pam(x, k, metric = "euclidean", stand = FALSE)
pam.res <- pam(Data, 4)
print(pam.res)

#add the point classifications to the original data
clusterdata <- cbind(Data, cluster = pam.res$cluster)
#head(clusterdata, n = 5)
write.csv(clusterdata, file = "clusters_resnorm.csv")


