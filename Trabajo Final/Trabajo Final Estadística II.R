install.packages("xtable")
library(devtools)
devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")
library(xtable)
library(ggbiplot)
library(nnet)
install.packages('caret')
library(caret)
library(MASS)
library(class)
data1 <- dioxido1
data1[,1] <- NULL

#Estandarización
dataest <- scale(data1)
rownames(dataest)<-c("Phoenix","little Rock","San Francisco","Denver","Harford","Wilmington","Washington", 
     
     "Jacksonville","Miami","Atlanta","Chicago","Indianapolis","Des Moines","Wichita", 
     
     "Louisville","New Orleans","Baltimore","Detroit","Minneapolis","Kansas City","St Louis", 
     
     "Omaha","Albuquerque","Albany","Buffalo","Cincinnati","Cleveland","Columbus", 
     
     "Philadelphia","Pittsburgh","Providence","Memphis","Nashville","Dallas","Hosuton","Salt Lake City", 
     
    "Norfolk","Richmond","Seatle","Charleston","Milwaukee") 

#Biplots
princomp <- princomp(dataest)
ggbiplot(princomp, groups =  rownames(dataest), circle = TRUE, scale = 1, obs.scale = 1, var.scale = 1) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
biplot(princomp)

#K-means
k.means.fit <-kmeans(dataest, 4, nstart = 50)
clusterss <- data.frame(k.means.fit$cluster)
dataest1 <- cbind(dataest, "Cluster" = clusterss[,1])
dataest1 <- data.frame(dataest1)

#Test-Train
sample <- sample.int(n = nrow(dataest1), size = floor(.80*nrow(dataest1)), replace = F)
train <- dataest1[sample, ]
test  <- dataest1[-sample, ]

#levels(test$Cluster) <- c(1,2,3) #En caso de error en la matriz de confusión

#REGRESIÓN LOGÍSTICA MULTINOMIAL - clasificación
mylogit <- multinom(Cluster ~ diox + temp + emp + dias + precip + viento + pob, data = train, model=TRUE)
p <- predict(mylogit, newdata = test)
test$Cluster
p
test$Cluster <- factor(test$Cluster)
confusionMatrix(p,test$Cluster)
summary(mylogit)

#OUTLIERS
m.dist <- mahalanobis(dataest, colMeans(dataest), cov(dataest))
cortar  = qchisq(p = 0.95 , df = ncol(dataest))
outliers <- dataest[m.dist>cortar,]
outliers

#KNN
pr <- knn(train,test,cl=train$Cluster,k=4)
pr
test$Cluster
test$Cluster <- factor(test$Cluster)
levels(test$Cluster) <- levels(pr) #En caso de error en la matriz de confusión
confusionMatrix(pr,test$Cluster)

## Análisis discriminante
mod = lda(train$Cluster ~.,data=train)
mod



