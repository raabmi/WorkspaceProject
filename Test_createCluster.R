source('createCluster.R')

v <- c(2, 4, 5,6,5,2,2, 1, 1, 2,  2, 1,6,7,8,7,6, 5, 2,1)

data <- data.frame(name = 1:length(v), v)
data  <- as.matrix(data)

createCluster(as.matrix(data), 2, method = 'binbased')

barplot(v, names.arg = 1:length(v)+6)


#Test Creat Cluster with real data
ZD <- read.csv("C:/Users/Michaela/Dropbox/Project SS2019/Implementierung/Daten/ZD.csv", sep=";")

zd <- ZD[3, 4:48]
zd.data <- data.frame(6:50, t(zd) )

barplot(as.matrix(zd)) 

crea