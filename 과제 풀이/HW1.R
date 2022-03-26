library(ggplot2)

X<-c(0.9, 1.3, 2.1, 2.5, 2.4, 1.7, 0.7, 1.2, 1.6)
Y<-c(2.0, 2.6, 4.3, 5.8, 5.1, 3.2, 1.8, 2.3, 3.0)
df<- data.frame(X,Y)
plot(Y~X, data=df, pch=19, cex=0.5)

lm.fit<-lm(Y~X, data=df)
summary(lm.fit)
abline(lm.fit)#1
anova(lm.fit)#2

lm.fit0<-lm(Y~0 + X, data=df)
summary(lm.fit0)#5
6.37/2.05

lm.fitW<-lm(Y~X, data=df, weights=1/X^2)#6
summary(lm.fitW)
abline(lm.fitW)
anova(lm.fitW )

varb1<- 0.108/sum((X-mean(X))^2)
t0<- (2.1626-1)/sqrt(varb1) #6.3..
qt(0.95, 7)# 1.89..
#T0>T(0.05,7) 따라서 기각한다.

varyhat<-0.108*(1/9+mean(X)^2/sum((X-mean(X))^2))
yhat <- mean(Y)- sum((X-mean(X))*(Y-mean(Y)))/sum((X-mean(X))^2)*(2-mean(X))
t<-(yhat-3.3)/sqrt(varyhat)
abs(t)-qt(0.95, 7) #0보다 크므로 기각한다.

r<-sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2) * sum((Y-mean(Y))^2))
t0<-r*sqrt(7/(1-r))
abs(t0)-qt(0.95, 7) #0보다 크므로 기각한다.

Zp<-1/2*log((1+r)/(1-r))
meanZ<-1/2*log((1+0.4)/(1-0.4))
varZ<-1/6

Z<-(Zp-meanZ)/sqrt(varZ)#4.36
qnorm(0.95)#1.64 


Z

