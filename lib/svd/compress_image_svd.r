#install.packages("jpeg")
library(png)
r <- readPNG("lena.png")
dim(r)
str(r)
is.matrix(r)
image(r)
dim(r)
image(1:512,1:512,t(r)[,nrow(r):1]) # правильная ориентация

r.svd <- svd(r)
d <- r.svd$d
u <- r.svd$u
v <- r.svd$v
str(u)
str(d)
str(v)

r.rec <- u %*% diag(d) %*% t(v)
image(r)
image(r.rec)

par(mfrow = c(1:2))
i <- 10
r.compr <- u[,1:i] %*% diag(d[1:i]) %*% t(v[,1:i])
image(r)
image(r.compr)

for (i in c(3, 4, 5, 10, 20, 30))
{
	r.compressed <- u[,1:i] %*% diag(d[1:i]) %*% t(v[,1:i])
	png(paste('images/Green Image SVD Compressed ', i, '.png', sep = ''))
	image(r.compressed, col = heat.colors(255))
	dev.off()
}




plot(1:2, type='n')
rasterImage(r, 1, 1, 2, 2)

r.svd <- svd(r)
d <- diag(r.svd$d)
dim(d)

u <- r.svd$u
v <- r.svd$v
plot(1:length(r.svd$d), r.svd$d)

# first approximation
u1 <- as.matrix(u[-1, 1])
v1 <- as.matrix(v[-1, 1])
d1 <- as.matrix(d[1, 1])
l1 <- u1 %*% d1 %*% t(v1)
plot(1:2, type='n')
rasterImage(l1, 1, 1, 2, 2)

depth <- 1000
us <- as.matrix(u[, 1:depth])
vs <- as.matrix(v[, 1:depth])
ds <- as.matrix(d[1:depth, 1:depth])
ls <- us %*% ds %*% t(vs)
plot(1:2, type='n')
rasterImage(l1, 1, 1, 2, 2)
