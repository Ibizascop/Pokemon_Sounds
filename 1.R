library(ggplot2,quietly = TRUE)
library(dtw)
library(dtwclust)
library(cluster)
library(ggpubr)
library(factoextra)
library(progress)
library()


nb_files = 878
nb_grps = 15
audio_files = list.files('C:/Users/ibiza/OneDrive/Desktop/Cours/Python/Time Series/Son/Data/CSV')

matrix <- matrix(0L, nrow = nb_files, ncol = nb_files)
names = audio_files
for (i in 1:length(names)) {
  name = gsub(".csv","",names[i])
  names[i]=name
}
rownames(matrix) = names
colnames(matrix) = names

pb <- progress_bar$new(total = nrow(matrix))
for (i in 1:nb_files) {
  pb$tick()
  time_serie_1 <- read.csv(audio_files[i])
  for (j in i:nb_files){
    if (i == j) {
      matrix[i,j] = 0
    }
    else { 
      time_serie_2 <- read.csv(audio_files[j])
      if (is.null(time_serie_2$values) == F){
        matrix[j,i] = dtw(time_serie_1$values,time_serie_2$values,keep = T)$normalizedDistance
        matrix[i,j] = matrix[j,i]
      }
    }
  }
}
#Sauvergarder matrice
write.table(matrix,"DTW_matrix.txt",col.names = NA)

#Visualiser Matrice
fviz_dist(as.dist(matrix),gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),show_labels=FALSE)

#CAH
Dendo= agnes(x = matrix , 
                diss = TRUE,
                method = "ward")

#Partitions
inertie <- sort(Dendo$height, decreasing = TRUE)
plot(inertie[1:50], type = "s", xlab = "Nombre de classes", ylab = "Inertie")


#Plot
fviz_dend(Dendo, cex = 0.5, hang=-1,k=15,main = paste("Quality=",round(Dendo$ac,4)), 
          xlab = "",
          ylab = "Height",
          color_labels_by_k = TRUE,
          rect = TRUE,
          rect_border = "black",
          show_labels = FALSE)


#Clusters
clusters = cutree(Dendo,k=15)
res_CAH = matrix(ncol = 2,nrow = length(Dendo$order))
res_CAH[,1] = clusters
res_CAH[,2] = audio_files
colnames(res_CAH) = c("Cluster","Pokemon")

#Extraire les centroides
for (i in 1:15){
  clusters_list <- list()
  print(paste((i/15)*100,"%"))
  for (j in 1:nb_files){
    counter = 1
    if ((clusters == i)[j]){
      time_serie <- read.csv(gsub(" ","",paste("./CSV/",audio_files[j])))
      clusters_list[[counter]] <-time_serie$values
      counter = counter+1
    }
  }
  centroid = shape_extraction(clusters_list,znorm=F)
  plot(centroid,type="lines",main =paste("Class n°",i ," with",sum((clusters == i)),"elements"))
  
  png(file=gsub(" ","",paste("./Centroides/",i,".png")), width=600, height=350)
  plot(centroid,type="lines",main =paste("Class n°",i ," with",sum((clusters == i)),"elements"))
  dev.off()
  write.csv(centroid, file =gsub(" ","",paste("./Centroides/",i,".csv")), row.names=FALSE)
}
write.table(res_CAH,"Clusters.txt")
