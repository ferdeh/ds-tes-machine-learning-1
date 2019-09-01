
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
library(clValid)
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
x
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
res.hc <- hclust(d = res.dist, method = "complete")


plot(res.hc)

### Mencari Algoritma Optimal

clmethods <- c("hierarchical","kmeans","pam")
intr <- clValid(x.scale[,2:4], nClust = 2:6, clMethods = clmethods,validation = "internal", maxitems = 2350 ,metric = "euclidean",method = "complete")
summary(intr)
optimalScores(intr)
####Diketahui algoritma optimal adalah Hirarki Cluster dengan jumlah cluster = 2
hc.out <- cutree(res.hc, k=2)
x.scale$clusterhc <- hc.out
head(x.scale)


#### Plot hasil clustering
plot(x.scale[,2:4], col = x.scale$clusterhc,
     main = "HC of RFM")

#### 3D plot
p <- plot_ly(x.scale, x = ~recency, y = ~frequency, z = ~monetary, color = ~clusterhc) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p

x.scale2<-filter(x.scale, monetary <= 16)

intr <- clValid(x.scale2[,2:4], nClust = 2:6, clMethods = clmethods,validation = "internal", maxitems = 2350 ,metric = "euclidean",method = "complete")
summary(intr)
optimalScores(intr)

res.dist <- dist(x.scale2[,2:4], method = "euclidean")
res.hc <- hclust(d = res.dist, method = "complete")

hc.out <- cutree(res.hc, k=3)
x.scale2$clusterhc <- hc.out
head(x.scale2)


#### Plot hasil clustering
plot(x.scale2[,2:4], col = x.scale$clusterhc,
     main = "HC of RFM")

#### 3D plot
p <- plot_ly(x.scale2, x = ~recency, y = ~frequency, z = ~monetary, color = ~clusterhc) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p
