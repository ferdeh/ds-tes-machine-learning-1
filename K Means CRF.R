
x<-read.csv('https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv')

summary(x)

km.out <- kmeans(x[,2:4], center=3, nstart=10)

summary(km.out)
print(km.out$cluster)
print(km.out)
x$cluster <- km.out$cluster
plot(x[,2:3], col=x$cluster)
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x[,2:4], centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

plot(x[,1:4], col = x$cluster,
     main = "Cluster of Customer")
