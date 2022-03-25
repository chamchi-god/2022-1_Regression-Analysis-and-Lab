library(ggplot2)

X<-c(0.9, 1.3, 2.1, 2.5, 2.4, 1.7, 0.7, 1.2, 1.6)
Y<-c(2.0, 2.6, 4.3, 5.8, 5.1, 3.2, 1.8, 2.3, 3.0)
df<- data.frame(X,Y)
plot(Y~X, data=df, pch=19, cex=0.5,
     xlab="father’s height (inches)", ylab="son’s height (inches)")

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
t0<- (2.1626-1)/varb1 #35.09..
qt(0.975, 7)# 2.364624
#T0>T(0.05,7) 따라서 기각한다.


