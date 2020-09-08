a<-2
b<-3
x<-1:100

y<-(a+rnorm(100))+(b+rnorm(100))*x+rnorm(100)
model<-lm(y~x)
