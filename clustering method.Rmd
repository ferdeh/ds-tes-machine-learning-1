---
title: "Unsupervised Machine learning With Clustering Method"
output: html_notebook
author: Ferdiansyah
---

## Cases Study Online Retail Clean

## Business Understanding
Online Retail Clean merupakan sebuah perusahaan jasa retail 
data set dari data transaksi berisi data customer ID, Frequency dan Monetary 

## Data Understanding
Cutomer ID : adalah ID unik yang dimiliki oleh masing-masing pelanggan
Recency : adalah jumlah hari dari hari terakhir customer membeli ( satuan hari) <br/>
Frequency : adalah jumlah pembelian yang dilakukan oleh customer ( satuan kali) <br/>
Monetary : adalah total nilai pembelian dari customer ( satuan Dollar) <br/>

## Data Preparation

### Import Library
```{r}
library(dplyr)
library(plotly)
library(factoextra)
library(cluster)
library(clValid)
```


### Read Data
```{r}
x<-read.csv('https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv')

```


### Cek data
```{r}
print(summary(x))
```


#### 3D Plot Data Sebelum data dibersihkan
Gunakan R sudio Desktop dengan OS Windows atau gunakan R Server dengan Google Chrome Browser
```{r}
p <- plot_ly(x, x = ~recency, y = ~frequency, z = ~monetary) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p
```

### Clean Invalid data 
Dari Summary data dan dari plot data terihat bahwa terdapat data monetary dengan nilai <0 <br/>
secara logika tidak mungkin jumlah pembelian <0 maka data ini di anggap data invalid yang harus dibuang <br/>
selain data NA juga harus dibuang dari data set <br/>
```{r}

x<-filter(x, monetary >= 0) %>% na.omit(x)
print(summary(x))

```

### Scale Data
Pada metode cluster sangat dipengaruhi jarak antar point pada setiap variable <br/>
dikarenakan setiap variable memiliki satuan dan skala yang berbeda, maka perlu dilakukan sclaling dari setiap nilai<br/>
pada variable agar memiliki skala yang sama <br/>
scale dilakukan dengan menghitung z score dari masing2 nilai variable <br/>
```{r}
x.scale <- as.data.frame(scale(x[,2:4], scale = TRUE))

head(x.scale)
summary(x.scale)
x$recency_z <- x.scale$recency
x$frequency_z <- x.scale$frequency
x$monetary_z <- x.scale$monetary
```


#### 3D Plot hasil data preparation
```{r}

p <- plot_ly(x, x = ~recency_z, y = ~frequency_z, z = ~monetary_z) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p
```


## Built Model
### K-Mean Method
#### Mencari jumlah cluster optimal
Elbow method
```{r}
fviz_nbclust(x[,5:7], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

```

Silhouette method
```{r}
fviz_nbclust(x[,5:7], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

```


Kedua metode menghasilkan jumlah K optimal k=4

#### Membuat model k-mean
```{r}
km.out <- kmeans(x[,5:7], center=4, nstart=10)
x$cluster <- km.out$cluster

```
#### Plot hasil clustering
```{r}
plot(x[,5:7], col = x$cluster,
     main = "K-MEANS of RFM")
```

#### 3D plot
```{r}
p <- plot_ly(x, x = ~recency_z, y = ~frequency_z, z = ~monetary_z, color = ~cluster) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p
```

### K-MEDOIDS (PAM) Method
#### Mencari Jumlah Cluster Optimal
```{r}
fviz_nbclust(x[,5:7], pam, method = "silhouette")+
  theme_classic()
```

jumlah K Optimal adalah k=2
```{r}
pam.out<-pam(x[,5:7], 2, metric = "euclidean", stand = FALSE)
x$clusterpam <-pam.out$clustering
head(x)
```

#### Plot hasil clustering
```{r}
plot(x[,5:7], col = x$clusterpam,
     main = "K-MEDOIDS of RFM")
```

#### 3D plot
```{r}
p <- plot_ly(x, x = ~recency_z, y = ~frequency_z, z = ~monetary_z, color = ~clusterpam) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p
```

### Hirarchical Method
Compute Dissimilarity Matrix
```{r}

res.dist <- dist(x[,5:7], method = "euclidean")
res.hc <- hclust(d = res.dist, method = "complete")


plot(res.hc)
```



### Mencari Algoritma Optimal
```{r}
clmethods <- c("hierarchical","kmeans","pam")
intr <- clValid(x[,5:7], nClust = 2:6, clMethods = clmethods,validation = "internal", maxitems = 2350 ,metric = "euclidean",method = "complete")
summary(intr)
optimalScores(intr)
```
Diketahui algoritma optimal adalah Hirarki Cluster dengan jumlah cluster = 2
```{r}
hc.out <- cutree(res.hc, k=2)
x$clusterhc <- hc.out
head(x)
```

#### Plot hasil clustering
```{r}
plot(x[,5:7], col = x$clusterhc,
     main = "HC of RFM")
```

#### 3D plot
```{r}
p <- plot_ly(x, x = ~recency_z, y = ~frequency_z, z = ~monetary_z, color = ~clusterhc) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p

```


## Evaluation
Dari plot terlihat pembagian 2 kluster secara bisnis berarti apa-apa.
perlu di evaluasi Ulang model dikarenakan tidak sesuai dengan keperluan bisnis
tahapan kerja kembali ke Business Understanding dan data preparation


## Data Preparation Iterasi ke-2

#### Cek Outlier 
```{r}
boxplot(x[,5:7])
```

Data frequency dan monetary terlalu banyak outlier sehingga model kluster yang dihasilkan tidak memiliki arti

#### Membersihkan Outlier
```{r}
boxplot(x$frequency_z)
freq.outlier <- boxplot(x$frequency_z)$out

x.clean1<-x[-which(x$frequency_z %in% freq.outlier),]
boxplot(x.clean1[,5:7])
boxplot(x$monetary_z)
money.outlier <- boxplot(x.clean1$monetary_z)$out
x.clean2<-x.clean1[-which(x.clean1$monetary_z %in% money.outlier),]
boxplot(x.clean2[,5:7])
```


## Built Model Tahap 2

### Mencari Algoritma Optimal
```{r}
clmethods <- c("hierarchical","kmeans","pam")
intr <- clValid(x.clean2[,5:7], nClust = 2:4, clMethods = clmethods,validation = "internal", maxitems = 2350 ,metric = "euclidean",method = "complete")
summary(intr)
optimalScores(intr)
```


Diketahui algoritma optimal dengan dunn index terbaik adalah Hirarki Cluster dengan jumlah cluster = 2

```{r}
res.dist <- dist(x.clean2[,5:7], method = "euclidean")
res.hc <- hclust(d = res.dist, method = "complete")
plot(res.hc)
hc.out <- cutree(res.hc, k=2)
x.clean2$clusterhc <- hc.out
head(x.clean2)
```


#### Plot hasil clustering
```{r}
plot(x.clean2[,5:7], col = x.clean2$clusterhc,
     main = "HC of RFM with z scale")

plot(x.clean2[,2:4], col = x.clean2$clusterhc,
     main = "HC of RFM")
```


#### 3D plot
```{r}
p <- plot_ly(x.clean2, x = ~recency, y = ~frequency, z = ~monetary, color = ~clusterhc) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency (days)'),
                      yaxis = list(title = 'Frequency(times)'),
                      zaxis = list(title = 'Monetary(dollar)')))

p
```



## Kesimpulan
Dari data transaksi pelanggan diketahui terdapat 2 kelompok pelanggan <br/>
2 kelompok pelanggan lebih di bagi berdasarkan kekinian dari mereka melakukan transaksi <br/>
nilai Monetary terdistribsi rapat sehingga dianggap satu kelompok. sedangkan nilai frequency distribusinya terlalu kecil hanya terdiri 3 nilai sehingga dianggap satu kelompok. Pembagian kelompok lebih kepada mempertimbangkan nilai recency
Kelompok 1 adalah pembeli yang membeli kurang dari 250 hari <br/>
KeLompok 2 adalah pembeli yang membeli lebih dari 250 hari <br/>
