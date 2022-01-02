#Reading Data
library("readxl")
df <- read_excel("2k21-SdA-HW1.xls")

#Droping empty columns
empty_column <- sapply(df, function(x) all(is.na(x) | x == ""))
df= df[, !empty_column]

#Replacing NA and zero values with interpolation
df[df == 0] <- NA
install.packages("imputeTS")
library(imputeTS)
df = na_interpolation(df)

# 5 number summary for each class 
library("dplyr")
summary(subset(df %>% filter(df$class==1), select = -c(class)))
summary(subset(df %>% filter(df$class==2), select = -c(class)))

#boxplot for each class
boxplot(subset(df %>% filter(df$class==1), select = -c(class)))
boxplot(subset(df %>% filter(df$class==2), select = -c(class)))

boxplot.stats(subset(df %>% filter(df$class==1), select = c(pkthrt,rrt)))


#histogram for each class
#plotting tpthrt
c1 <- subset(df %>% filter(df$class==1), select = c(tpthrt))
hist(c1$tpthrt)
c2 <- subset(df %>% filter(df$class==2), select = c(tpthrt))
hist(c2$tpthrt)
#plotting pkthrt
c1 <- subset(df %>% filter(df$class==1), select = c(pkthrt))
hist(c1$pkthrt)
c2 <- subset(df %>% filter(df$class==2), select = c(pkthrt))
hist(c2$pkthrt)


#plotting dfdrrt
c1 <- subset(df %>% filter(df$class==1), select = c(dfdrrt))
hist(c1$dfdrrt)
c2 <- subset(df %>% filter(df$class==2), select = c(dfdrrt))
hist(c2$dfdrrt)



#plotting rrt
c1 <- subset(df %>% filter(df$class==1), select = c(rrt))
hist(c1$rrt)
c2 <- subset(df %>% filter(df$class==2), select = c(rrt))
hist(c2$rrt)



#plotting frt
c1 <- subset(df %>% filter(df$class==1), select = c(frt))
hist(c1$frt)
c2 <- subset(df %>% filter(df$class==2), select = c(frt))
hist(c2$frt)


#Normalized Data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

c1 <- subset(df %>% filter(df$class==1), select = -c(class))
for(i in 1:ncol(c1)) {       # for-loop over columns
  c1[ , i] <- normalize(c1[ , i])
}

c2 <- subset(df %>% filter(df$class==2), select = -c(class))
for(i in 1:ncol(c2)) {       # for-loop over columns
  c2[ , i] <- normalize(c2[ , i])
}



plot(c1$tpthrt, type="l", col="green",lwd=2, ylim=c(0,1.3))

plot( df$tpthrt,df$class, type="p", col=c("red","green")[df$class],lwd=2, ylim=c(0.9,2.1))
plot( df$dfdrrt,df$class, type="p", col=c("red","green")[df$class],lwd=2, ylim=c(0.9,2.1))
plot( df$pkthrt,df$class, type="p", col=c("red","green")[df$class],lwd=2, ylim=c(0.9,2.1))

plot( df$rrt,df$class, type="p", col=c("red","green")[df$class],lwd=2, ylim=c(0.9,2.1))

plot( df$frt,df$class, type="p", col=c("red","green")[df$class],lwd=2, ylim=c(0.9,2.1))





lines(c2$tpthrt, col="red", lwd=2)
title("Plotting 'tpthrt' values for both classes")
legend("topleft", legend=c("class 1", "class 2"),col=c("green", "red"), lwd=c(2,2), cex=0.7)



plot(c1$pkthrt, type="l", col="green",lwd=2, ylim=c(0,1.3))
lines(c2$pkthrt, col="red", lwd=2)
title("Plotting 'pkthrt' values for both classes")
legend("topleft", legend=c("class 1", "class 2"),col=c("green", "red"), lwd=c(2,2), cex=0.7)



plot(c1$dfdrrt, type="l", col="green",lwd=2, ylim=c(0,1.3))
lines(c2$dfdrrt, col="red", lwd=2)
title("Plotting 'dfdrrt' values for both classes")
legend("topleft", legend=c("class 1", "class 2"),col=c("green", "red"), lwd=c(2,2), cex=0.7)


plot(c1$rrt, type="l", col="green",lwd=2, ylim=c(0,1.3))
lines(c2$rrt, col="red", lwd=2)
title("Plotting 'rrt' values for both classes")
legend("topleft", legend=c("class 1", "class 2"),col=c("green", "red"), lwd=c(2,2), cex=0.7)

plot(c1$frt, type="l", col="green",lwd=2, ylim=c(0,1.3))
lines(c2$frt, col="red", lwd=2)
title("Plotting 'frt' values for both classes")
legend("topleft", legend=c("class 1", "class 2"),col=c("green", "red"), lwd=c(2,2), cex=0.7)

cor(subset(c1,select= c(tpthrt,pkthrt)))

cor(subset(c2,select= c(tpthrt,pkthrt)))

cor(subset(c1,select= c(tpthrt)),subset(c2,select= c(tpthrt)))

 
subset(c1,select= c(tpthrt))
subset(c2,select= c(tpthrt))


c3 <- data.frame(subset(c1,select= c(tpthrt)),subset(c2,select= c(tpthrt)))
cor(c3)

#install.packages("clusterSim")
library(clusterSim)
library(cluster)



DataSet[,1:8] <- data.Normalization(DataSet [,1:8],type="n4",normalization="column")
summary(DataSet)