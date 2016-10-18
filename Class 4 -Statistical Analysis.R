# Week 4 Workshop
# 1. Projection

u<-c(1,2,3)
v<-c(4,5,6)
u*v

(Dot.Product.1<-sum(u*v))

Dot.Product.2<-u%*%v)

lVector<-function(vec) {
    dot.product <- sqrt(vec%*%vec)
    dot.product
}


u<-c(1.5,sqrt(3)/2)
v<-c(2,0)
Dot.Product.3<-u%*%v
Dot.Product.3

projutov <- Dot.Product.3/(v%*%v)*v
projutov


lVector(projutov)


# Matrices
M<-1:12
dim(M)<-c(3,4)
M

M<-matrix(1:12,ncol=4)
M

M<-cbind(c(1,2,3),c(4,5,6),c(7,8,9),c(10,11,12))
M

X.values<-c(1,2,3,4,5)
Y.values<-c(6,7,8,9,10)
(Product<-outer(X.values,Y.values))

contour(X.values,Y.values,log(Product))   # Outer is useful for creating contour plots



Divide<-outer(X.values,Y.values,"/")
contour(X.values,Y.values,log(Divide))


(A<-rbind(c(5,2),c(3,1)))
(A.inv<-solve(A))  # solve is used to inverse the matrix 
A%*%A.inv



#Workshop 2
dataPath <- "C:/Users/AA/Desktop/Statistical Analysis/Class 4/"
da<-read.csv(file=paste(dataPath,"documents_MScA Statistical Analysis 31007_MScA 31007 Lecture 4_LognormalSample.csv",sep="/"))
head(da$x)

hist(da$x)

median <- median(da$x)
mu <- log(median)
mu


sigma <- sqrt(2*log(mean(da$x))-2*mu)
sigma

c(mu = mu, sigma = sigma)


supressWarnings(library(MASS))
fitdistr(da$x,"log-normal")

mu1 <- mean(log(da$x))

sigma1 <- sd(log(da$x))

c(mu = mu1, sigma = sigma1)















