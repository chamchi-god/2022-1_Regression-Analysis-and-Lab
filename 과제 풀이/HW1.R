library(ggplot2)

X<-c(0.9, 1.3, 2.1, 2.5, 2.4, 1.7, 0.7, 1.2, 1.6)
Y<-c(2.0, 2.6, 4.3, 5.8, 5.1, 3.2, 1.8, 2.3, 3.0)
df<- data.frame(X,Y)
plot(Y~X, data=df, pch=19, cex=1)
par(mfrow = c(1, 1))
lm.fit<-lm(Y~X, data=df)
summary(lm.fit)
abline(lm.fit)#1
anova(lm.fit)#2

lm.fit0<-lm(Y~0 + X, data=df)
summary(lm.fit0)#5
anova(lm.fit0)
6.37/2.05
2.099^2*sum(Y^2)  

R<-sum((X*lm.fit0$coefficients-mean(Y))^2)/sum((Y-mean(Y))^2)

lm.fitW<-lm(Y~X, data=df, weights=1/X^2)#6
summary(lm.fitW)
abline(lm.fitW)
anova(lm.fitW )

sum(1/X^2*(Y-(sum(Y/X^2)/sum(1/X^2)))^2)

sum(1/X^2*(X-1.134808)*(Y-2.448709))/sum(1/X^2*((X-1.134808)^2))
abs(sum(1/X^2*(Y-1.8417*X-0.3587)^2))/sum((1/X-(1.134808)/X^2))

#3.10
Amazon = data.frame(Year = 1962:1978,
                    High = c(25.82, 25.35, 24.29, 24.05, 24.89, 25.35,
                             25.23, 25.06, 27.13, 27.36, 26.65, 27.13,
                             27.49, 27.08, 27.51, 27.54, 26.21),
                    Low = c(18.24, 16.50, 20.26, 20.97,19.43, 19.31, 
                            20.85, 19.54, 20.49, 21.91, 22.51, 18.81,
                            19.42, 19.10, 18.80, 18.80, 17.57))
par(mfrow = c(3, 1))
plot(High~Year, data = Amazon, pch=19, cex=1) #1
plot(Low~Year, data = Amazon, pch=19, cex=1) 
plot(High~Low, data = Amazon, pch=19, cex=1) 

lmHY<-lm(High~Year, data = Amazon)
summary(lmHY) #beta1은 0.000371의 값이 나왔는데, 이는 해가 지날수록 High가 0.000371씩 상승하고 있음을 의미한다. F통계량이 20.85이며, p-value가 0.05보다 작으므로 유의수준 0.05에서 위 회귀모형은 데이터를 잘 설명한다.
lmLY<-lm(Low~Year, data = Amazon)
summary(lmLY) #beta1은 -0.007892의 값이 나왔는데, 이는 해가 지날수록 LOW가 0.007892씩 감소하고 있음을 의미한다. F통계량이 0.0105이며, p-value가 0.05보다 크므로 유의수준 0.05에서 위 회귀모형은 데이터를 잘 설명하지 못한다.
lmHL<-lm(High~Low, data = Amazon)
summary(lmHL) #beta1은 -0.01406의 값이 나왔는데, 이는 Low가 1 커질수록 High가 0.01406씩 감소하고 있음을 의미한다. F통계량이 0.004695이며, p-value가 0.05보다 크므로 유의수준 0.05에서 위 회귀모형은 데이터를 잘 설명하지 못한다.

#3 year과 수위 데이터 뿐만 아니라 시간-삼림파괴정도, 삼림파괴정도-수위 관련 데이터가 있어야지만, 삼림파괴와 수위의 인과 관계를 알 수 있다. 삼림 파괴가 아니라 시간이 지나면서 변화된 또다른 요인에 의해 수위가 높아졌을지도 모르기 때문이다. 지금은 시간이 지날수록 최대수위가 높아지는 것 밖에 알지못한다. 

#4.4
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


