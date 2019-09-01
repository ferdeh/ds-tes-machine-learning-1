
# Unsupervised Machine learning With Clustering Method

## Cases Study Online Retail Clean

## Business Understanding
#### Online Retail Clean merupakan sebuah perusahaan jasa retail 
#### data set dari data transaksi berisi data customer ID, Frequency dan Monetary 

## Data Understanding
#### Cutomer ID : adalah ID unik yang dimiliki oleh masing-masing pelanggan
#### Recency : adalah jumlah hari dari hari terakhir customer membeli ( satuan hari)
#### Frequency : adalah jumlah pembelian yang dilakukan oleh customer ( satuan kali)
#### Monetary : adalah total nilai pembelian dari customer ( satuan Dollar)

## Data Preparation

### Import Library
library(dplyr)
library(plotly)
library(factoextra)
library(cluster)
### Read Data
x<-read.csv('https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv')

### Cek data
print(summary(x))


#### 3D Plot Data Sebelum data dibersihkan
#### Gunakan R sudio Desktop dengan OS Windows atau gunakan R Server dengan Google Chrome Browser

p <- plot_ly(x, x = ~recency, y = ~frequency, z = ~monetary) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p

### Clean Invalid data 
#### Dari Summary data dan dari plot data terihat bahwa terdapat data monetary dengan nilai <0
#### secara logika tidak mungkin jumlah pembelian <0 maka data ini di anggap data invalid yang harus dibuang
#### selain data NA juga harus dibuang dari data set

x<-filter(x, monetary >= 0) %>% na.omit(x)
print(summary(x))

### Scale Data
#### Pada metode cluster sangat dipengaruhi jarak antar point pada setiap variable
#### dikarenakan setiap variable memiliki satuan dan skala yang berbeda, maka perlu dilakukan sclaling dari setiap nilai pada variable agar memiliki skala yang sama
#### scale dilakukan dengan menghitung z score dari masing2 nilai variable

x.scale <- as.data.frame(scale(x, scale = TRUE))

head(x.scale)
summary(x.scale)
#### 3D Plot 
p <- plot_ly(x.scale, x = ~recency, y = ~frequency, z = ~monetary) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p

## Built Model
### K-Mean Method

#### Mencari jumlah cluster optimal

#### Elbow method
fviz_nbclust(x.scale[,2:4], kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

##### Silhouette method
fviz_nbclust(x.scale[,2:4], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#### Kedua metode menghasilkan jumlah K optimal k=4

#### Membuat model k-mean
km.out <- kmeans(x.scale[,2:4], center=3, nstart=10)
x.scale$cluster <- km.out$cluster
x$cluster <- km.out$cluster

#### Plot hasil clustering
plot(x.scale[,2:4], col = x.scale$cluster,
     main = "K-MEANS of RFM")

#### 3D plot
p <- plot_ly(x.scale, x = ~recency, y = ~frequency, z = ~monetary, color = ~cluster) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p

### K-MEDOIDS (PAM) Method
### Mencari Jumlah Cluster Optimal
fviz_nbclust(x.scale[,2:4], pam, method = "silhouette")+
  theme_classic()

#### jumlah K Optimal adalah k=2
pam.out<-pam(x.scale[,2:4], 2, metric = "euclidean", stand = FALSE)
x.scale$clusterpam <-pam.out$clustering
head(x.scale)

#### Plot hasil clustering
plot(x.scale[,2:4], col = x.scale$clusterpam,
     main = "K-MEDOIDS of RFM")

#### 3D plot
p <- plot_ly(x.scale, x = ~recency, y = ~frequency, z = ~monetary, color = ~clusterpam) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p


### Hirarchical Method
#### Compute Dissimilarity Matrix

res.dist <- dist(x.scale[,2:4], method = "euclidean")
res.hc <- hclust(d = res.dist, method = "ward.D2")

res.hc
fviz_dend(res.hc, cex = 0.5)

# ## Mencari Center Number Optimal
# 
# wss <- 0
# 
# ### For 1 to 15 cluster centers
# for (i in 1:15) {
#   km.out <- kmeans(x.scale[,2:4], centers = i, nstart = 20)
#   ### Save total within sum of squares to wss variable
#   wss[i] <- km.out$tot.withinss
# }
# 
# 
# ### Plot total within sum of squares vs. number of clusters
# print(plot(1:15, wss, type = "b", 
#      xlab = "Number of Clusters", 
#      ylab = "Within groups sum of squares"))
# 
# km.out <- kmeans(x[,2:4], center=3, nstart=10)
# x$cluster <- km.out$cluster
# plot(x[,2:4], col = x$cluster,
#      main = "Cluster of Customer")
# 
# 
# 
# 
# 
# summary(km.out)
# print(km.out$cluster)
# print(km.out)
# 
# 
# #p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
# #p
# 
# x$cluster <- as.factor(x$cluster)
# 
# 
# p <- plot_ly(x, x = ~recency, y = ~frequency, z = ~monetary, color = ~cluster, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'Recency'),
#                       yaxis = list(title = 'Frequency'),
#                       zaxis = list(title = 'Monetary')))
# 
# p
# 
# 
# 
# 
# 
# 
# plot(x[,2:3], col=x$cluster)
# 
# 
# 
# library("plot3D")
# 
#  
# scatter3D(x$recency, x$frequency, x$monetary, col= x$cluster)
# 
# 
# #library(plotly)
# 
