install.packages("languageserver")
install.packages(c("factoextra","NbClust"))
library(ggplot2)
library(factoextra)
library(NbClust)
library(cluster)
library(readxl)

#CLUSTERING
#KALIMANTAN BARAT
#A. Input Data
datab = read_excel("IPM Kalimantan 2022.xlsx", sheet = "Kalbar 2022")
datab = as.data.frame(datab)
datab
summary(datab)
str(datab)
#Mengubah kolom Kabupaten/Kota menjadi nama baris
rownames(datab) = datab$'Kabupaten/Kota'
datab
databb = datab[,2:5]
databb
#C. Standardize the data
datab_scaled <- scale(databb)
#D. Optimal Number of Cluster
#Silhouette Method
silhouette_score <- function(k){
  km <- kmeans(datab_scaled, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(datab_scaled))
  mean(ss[, 3])
}
k <- 2:13
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#E. K-Means Clustering dengan k=2
set.seed(26)
kb_2<-kmeans(datab_scaled, centers = 2, nstart = 25)
str(kb_2)
kb_2
data_kb2 = as.data.frame(kb_2$cluster)
data_kb2
colnames(data_kb2)[1] = "Cluster"
data_kb2
#F. PLOT
fviz_cluster(kb_2, data = datab_scaled)
#G. K-Means Clustering dengan k=3
set.seed(88)
kb_3<-kmeans(datab_scaled, centers = 3, nstart = 25)
str(kb_3)
kb_3
data_kb3 = as.data.frame(kb_3$cluster)
data_kb3
colnames(data_kb3)[1] = "Cluster"
data_kb3
#H. PLOT
fviz_cluster(kb_3, data = datab_scaled)


#KALIMANTAN TIMUR

#A. Input Data
datat = read_excel("IPM Kalimantan 2022.xlsx", sheet = "Kaltim 2022")
datat = as.data.frame(datat)
datat
summary(datat)
str(datat)
#Mengubah kolom Kabupaten/Kota menjadi nama baris
rownames(datat) = datat$'Kabupaten/Kota'
datat
datatt = datat[,2:5]
datatt
#C. Standardize the data
datat_scaled <- scale(datatt)
#D. Optimal Number of Cluster
#Silhouette Method
silhouette_score <- function(k){
  km <- kmeans(datat_scaled, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(datat_scaled))
  mean(ss[, 3])
}
k <- 2:8
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#E. K-Means Clustering dengan k=2
set.seed(37)
kt_2<-kmeans(datat_scaled, centers = 2, nstart = 25)
str(kt_2)
kt_2
data_kt2 = as.data.frame(kt_2$cluster)
data_kt2
colnames(data_kt2)[1] = "Cluster"
data_kt2
#F. PLOT
fviz_cluster(kt_2, data = datat_scaled)
#G. K-Means Clustering dengan k=3
set.seed(99)
kt_3<-kmeans(datat_scaled, centers = 3, nstart = 25)
str(kt_3)
kt_3
data_kt3 = as.data.frame(kt_3$cluster)
data_kt3
colnames(data_kt3)[1] = "Cluster"
data_kt3
#H. PLOT
fviz_cluster(kt_3, data = datat_scaled)

#KALIMANTAN UTARA

#A. Input Data
datau = read_excel("IPM Kalimantan 2022.xlsx", sheet = "Kalut 2022")
datau = as.data.frame(datau)
datau
summary(datau)
str(datau)
#Mengubah kolom Kabupaten/Kota menjadi nama baris
rownames(datau) = datau$'Kabupaten/Kota'
datau
datauu = datau[,2:5]
datauu
#C. Standardize the data
datau_scaled <- scale(datauu)
#D. Optimal Number of Cluster
#Silhouette Method
silhouette_score <- function(k){
  km <- kmeans(datau_scaled, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(datau_scaled))
  mean(ss[, 3])
}
k <- 2:4
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#E. K-Means Clustering dengan k=2
set.seed(26)
ku_2<-kmeans(datau_scaled, centers = 2, nstart = 25)
str(ku_2)
ku_2
data_ku2 = as.data.frame(ku_2$cluster)
data_ku2
colnames(data_ku2)[1] = "Cluster"
data_ku2
#F. PLOT
fviz_cluster(ku_2, data = datau_scaled)
#G. K-Means Clustering dengan k=3
set.seed(26)
ku_3<-kmeans(datau_scaled, centers = 3, nstart = 25)
str(ku_3)
ku_3
data_ku3 = as.data.frame(ku_3$cluster)
data_ku3
colnames(data_ku3)[1] = "Cluster"
data_ku3
#H. PLOT
fviz_cluster(ku_3, data = datau_scaled)

#KALIMANTAN SELATAN

#A. Input Data
datas = read_excel("IPM Kalimantan 2022.xlsx", sheet = "Kalsel 2022")
datas = as.data.frame(datas)
datas
summary(datas)
str(datas)
#Mengubah kolom Kabupaten/Kota menjadi nama baris
rownames(datas) = datas$'Kabupaten/Kota'
datas
datass = datas[,2:5]
datass
#C. Standardize the data
datas_scaled <- scale(datass)
#D. Optimal Number of Cluster
#Silhouette Method
silhouette_score <- function(k){
  km <- kmeans(datas_scaled, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(datas_scaled))
  mean(ss[, 3])
}
k <- 2:12
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#E. K-Means Clustering dengan k=2
set.seed(37)
ks_2<-kmeans(datas_scaled, centers = 2, nstart = 25)
str(ks_2)
ks_2
data_ks2 = as.data.frame(ks_2$cluster)
data_ks2
colnames(data_ks2)[1] = "Cluster"
data_ks2
#F. PLOT
fviz_cluster(ks_2, data = datas_scaled)
#G. K-Means Clustering dengan k=3
set.seed(2)
ks_3<-kmeans(datas_scaled, centers = 3, nstart = 25)
str(ks_3)
ks_3
data_ks3 = as.data.frame(ks_3$cluster)
data_ks3
colnames(data_ks3)[1] = "Cluster"
data_ks3
#H. PLOT
fviz_cluster(ks_3, data = datab_scaled)

#KALIMANTAN TENGAH

#A. Input Data
datac = read_excel("IPM Kalimantan 2022.xlsx", sheet = "Kalteng 2022")
datac = as.data.frame(datac)
datac
summary(datac)
str(datac)
#Mengubah kolom Kabupaten/Kota menjadi nama baris
rownames(datac) = datac$'Kabupaten/Kota'
datac
datacc = datac[,2:5]
datacc
#C. Standardize the data
datac_scaled <- scale(datacc)
#D. Optimal Number of Cluster
#Silhouette Method
silhouette_score <- function(k){
  km <- kmeans(datac_scaled, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(datac_scaled))
  mean(ss[, 3])
}
k <- 2:13
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#E. K-Means Clustering dengan k=2
set.seed(4)
kc_2<-kmeans(datac_scaled, centers = 2, nstart = 25)
str(kc_2)
kc_2
data_kc2 = as.data.frame(kc_2$cluster)
data_kc2
colnames(data_kc2)[1] = "Cluster"
data_kc2
#F. PLOT
fviz_cluster(kc_2, data = datac_scaled)
#G. K-Means Clustering dengan k=3
set.seed(1003)
kc_3<-kmeans(datac_scaled, centers = 3, nstart = 25)
str(kc_3)
kc_3
data_kc3 = as.data.frame(kc_3$cluster)
data_kc3
colnames(data_kc3)[1] = "Cluster"
data_kc3
#H. PLOT
fviz_cluster(kc_3, data = datab_scaled)

#CLUSTER KALIMANTAN

#A. Input Data
data = read_excel("IPM Kalimantan 2022.xlsx", sheet = "Kalimantan")
summary(data)
#Mengubah kolom Kabupaten/Kota menjadi nama baris
data = as.data.frame(data)
rownames(data) = data$'Kabupaten/Kota'
dataa = data[,2:5]
dataa
#C. Standardize the data
data_scaled <- scale(dataa)
#D. Optimal Number of Cluster
#Silhouette Method
silhouette_score <- function(k){
  km <- kmeans(data_scaled, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(data_scaled))
  mean(ss[, 3])
}
k <- 2:54
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#E. K-Means Clustering dengan k=2
set.seed(37)
kk_2<-kmeans(data_scaled, centers = 2, nstart = 25)
str(kk_2)
kk_2

data_cluster2 = as.data.frame(kk_2$cluster)
colnames(data_cluster2)[1] = "Cluster"
data_cluster2 = cbind(KabupatenKota = rownames(data_cluster2), data_cluster2)
rownames(data_cluster2) = 1:nrow(data_cluster2)
colnames(data_cluster2)[1] = "Kabupaten/Kota"
data_cluster2 = data_cluster2[order(data_cluster2$Cluster, decreasing = FALSE),]
data_cluster2

#F. Plot
fviz_cluster(kk_2, data = data_scaled)

#G. K-Means Clustering dengan k=4
set.seed(8)
kk_4<-kmeans(data_scaled, centers = 4, nstart = 25)
str(kk_4)
print(kk_4)
data_cluster4 = as.data.frame(kk_4$cluster)
colnames(data_cluster4)[1] = "Cluster"
data_cluster4 = cbind(KabupatenKota = rownames(data_cluster4), data_cluster4)
rownames(data_cluster4) = 1:nrow(data_cluster4)
colnames(data_cluster4)[1] = "Kabupaten/Kota"
data_cluster4 = data_cluster4[order(data_cluster4$Cluster, decreasing = FALSE),]
data_cluster4
#H. Plot
fviz_cluster(kk_4, data = data_scaled)
