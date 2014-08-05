set.seed(12345); 
old.par <- par(mar=rep(2,4))
dataMatrix <- matrix(rnorm(400),nrow=40)
dim(dataMatrix)
# [1] 40 10
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
## Координаты - по размерности матрицы, трастпонирование и отражение - видимо такова специфика image

# Есть ли кластеры?
par(mar=rep(2,4))
heatmap(dataMatrix)
## Видно, что нет

# Add a pattern
set.seed(678910)
for(i in 1:40){
	# flip a coin
	coinFlip <- rbinom(1,size=1,prob=0.5)
	# if coin is heads add a common pattern to that row
	if(coinFlip){
		dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
	}
}
## Добавляем случайно выбранной (с вероятностью 1/2) строке строку  0 0 0 0 0 3 3 3 3 3
# Становится вот так:
par(mar=rep(2,4))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])

# Нарисуем рядом
par(mfrow = c(1,2))
set.seed(12345); par(mar=rep(2,4))
dataMatrix <- matrix(rnorm(400),nrow=40)
dataMatrixWithPattern <- dataMatrix
set.seed(678910)
for(i in 1:40){
	# flip a coin
	coinFlip <- rbinom(1,size=1,prob=0.5)
	# if coin is heads add a common pattern to that row
	if(coinFlip){
		dataMatrixWithPattern[i,] <- dataMatrixWithPattern[i,] + rep(c(0,3),each=5)
	}
}
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
image(1:10,1:40,t(dataMatrixWithPattern)[,nrow(dataMatrixWithPattern):1])
# Видно, что справа (где добавили 3) у некоторых строк стало больше желтого
# Для некоторых строк видно, что зня- слева в средем меньше, чем зн-я справа

# Смотрим кластеры
heatmap(dataMatrix)
heatmap(dataMatrixWithPattern)
# Видно 2 кластера по столбцами (>5) и по строкам (примерно половина)

# Хотим найти этот паттерн. Ниже, чтобы было как в лекциях, 
dataMatrix <- dataMatrixWithPattern
rm(dataMatrixWithPattern)
# Посчитаем среднее по столбцами и строкам
# hclust группирует столбцы так (нужно почитать)
## dist вычисляет матрицу расстояний между строками
hh <- hclust(dist(dataMatrix)); 
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow=c(1,1))
image(dataMatrixOrdered)
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1,,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(dataMatrixOrdered),xlab="Column",ylab="Column Mean",pch=19)

# SVD
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)

# Можно попробовать так:
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(c(svd1$u[1:10,1], rep(max(svd1$u[, 1]), 30)) ,40:1,,xlab="Row",ylab="First left singular vector",pch=19)
# То есть видно, что вектор u рисуется сверху вниз, как и должно быть (это столбец)
# и показывает различия между строками
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)
plot(c(svd1$v[1:5,1], rep(min(svd1$v[, 1]), 5)),xlab="Column",ylab="First right singular vector",pch=19)
# svd1$v рисуется слева направо, по столбцам, и показывает различия между столбцами

# Строки, где компонента вектора U меньше - там меньше случайность. 
# (прервые 20 строк - не вполне случайные, есть паттерн, 
# там u[1:20, 1]-меньше, чем u[1:20, 1], 
# mean(svd1$u[1:20, 1]) = -0.11849
# mean(svd1$u[21:40, 1]) = 0.11849

# Для V: v[1:5, 1] больше, чем v[6:10, 1]. Случайность столбцов 1:5 больше, чем 6:10.
# Столбец, где конпонента V меньше - менее случайны (столбцы 6:10) 
# mean(svd1$v[1:5, 1]) = 0.02246077
# mean(svd1$v[6:10, 1]) = -0.4383196

# Для v также важно, что последние 5 имеют почти одно и то же значение - 
# потому, что мы добавили  постоянное число, 3, к последнему столбцу в половине случаев.

# Сравним со средними
par(mfrow=c(2,3))
par(mar=rep(4,4))
## Сингулярные векторы
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="U",pch=19)
plot(svd1$v[,1],xlab="Column",ylab="V",pch=19)
## Средние по строкам и столбцам
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1,,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(dataMatrixOrdered),xlab="Column",ylab="Col. Mean",pch=19)

# Components of the SVD - d and variance explained
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)
str(svd1$d)

# Relationship to principal components
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered,scale=TRUE)
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Principal Component 1",ylab="Right Singular Vector 1")
abline(c(0,1))

# Components of the SVD - variance explained
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
constantMatrix[1:5, 1:10]

svd1 <- svd(constantMatrix)
svd1$u[1:5, 1:10]
svd1$v[1:5, 1:10]
svd1$d[1:5, 1:10]
str(svd1$d)

sum(constantMatrix != svd1$u %*% diag(svd1$d, 10, 10) %*% t(svd1$v))
m1 <- svd1$u %*% diag(svd1$d, 10, 10) %*% t(svd1$v)

par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)
# Чтобы восстановить матрицу достаточно 1 вектора, произведение 1-го правого на 1 левый дает исходную матрицу
m <- matrix(c(0, 0, 1, 1), 2)
svd2 <- svd(m)
as.matrix(svd2$u[,1] * svd2$d[1], 2, 1) %*% t(as.matrix(svd2$v[,1], 2, 1))

# What if we add a second pattern?
set.seed(678910)
for(i in 1:40){
	# flip a coin
	coinFlip1 <- rbinom(1,size=1,prob=0.5)
	coinFlip2 <- rbinom(1,size=1,prob=0.5)
	# if coin is heads add a common pattern to that row
	if(coinFlip1){
		dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5),each=5)
	}
	if(coinFlip2){
		dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5),5)
	}
}
hh <- hclust(dist(dataMatrix)); dataMatrixOrdered <- dataMatrix[hh$order,]
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rep(c(0,1),each=5),pch=19,xlab="Column",ylab="Pattern 1")
plot(rep(c(0,1),5),pch=19,xlab="Column",ylab="Pattern 2")

# v and patterns of variance in rows
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd2$v[,1],pch=19,xlab="Column",ylab="First right singluar vector")
plot(svd2$v[,2],pch=19,xlab="Column",ylab="Second right singluar vector")

# d and variance explained
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)

# fast.svd function {corpcor}
# Important parameters: m,tol
install.packages("corpcor")
library(corpcor)
bigMatrix <- matrix(rnorm(1e4*40),nrow=1e4)
system.time(svd(scale(bigMatrix)))
system.time(fast.svd(scale(bigMatrix),tol=0))

# Missing values
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
svd1 <- svd(scale(dataMatrix2))
# Error in svd(scale(dataMatrix2)) : infinite or missing values in 'x'

# Imputing {impute}
# source("http://bioconductor.org/biocLite.R")
# biocLite("impute")
library(impute)
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2)); plot(svd1$v[,1],pch=19); plot(svd2$v[,1],pch=19)


# Face example
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/face.rda",destfile="face.rda")
load("face.rda")
image(t(faceData)[,nrow(faceData):1])

# Face example - variance explained
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singluar vector",ylab="Variance explained")

# Face example - create approximations
svd1 <- svd(scale(faceData))
# %*% is matrix multiplication
# Here svd1$d[1] is a constant
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]
# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5])%*% t(svd1$v[,1:5]) 
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10])%*% t(svd1$v[,1:10]) 

# Face example - plot approximations
par(mfrow=c(1,4))
image(t(faceData)[,nrow(faceData):1])
image(t(approx10)[,nrow(approx10):1])
image(t(approx5)[,nrow(approx5):1])
image(t(approx1)[,nrow(approx1):1])
