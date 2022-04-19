#1
y<-c(2.8, 3.9, 3.9, 4.4, 3.1, 3.1, 3.5, 3.6, 3.0, 3.3)
x0<-c(1,1,1,1,1,1,1,1,1,1)
x1<-c(10, 24, 25, 28, 15, 18, 22, 22, 12, 15)
x2<-c(27, 26, 28, 26, 30, 24, 27, 25, 27, 25)
x3<-c(64, 72, 80, 88, 81, 45, 46, 69, 54, 39)

x12<-lm(y~x1+x2)
summary(x12)
x13<-lm(y~x1+x3)
summary(x13)
x23<-lm(y~x2+x3)
summary(x23)

#2
x123<-lm(y~x1+x2+x3)
sm<-summary(x123)
anova(x123)
m1<-cbind(x0,x1,x2, x3)
m2<-rbind(x0,x1,x2, x3)
solve(m2%*%m1)
0.005864-qt(0.975,6)*sqrt(0.0008624387*0.02959)
0.005864+qt(0.975,6)*sqrt(0.0008624387*0.02959)
0.069788-qt(0.995,6)*sqrt(0.005399764*0.02959)
0.069788+qt(0.995,6)*sqrt(0.005399764*0.02959)
2.409213+0.069788 * 20+(-0.024767) *27 +0.005864*60 +qt(0.975,6)*sqrt(c(1,20,27,60)%*%solve(m2%*%m1)%*%c(1,20,27,60)*0.02959)#3.622998
2.409213+0.069788 * 20+(-0.024767) *27 +0.005864*60 -qt(0.975,6)*sqrt(c(1,20,27,60)%*%solve(m2%*%m1)%*%c(1,20,27,60)*0.02959)#3.35321
0.069788-qt(0.975,6)*sqrt(0.005399764*0.02959)
0.069788+qt(0.975,6)*sqrt(0.005399764*0.02959)
library(car)
I<-matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), nrow = 4)
c1<-c(0,1,-1,0)
c2<-c(0,0,1,-1)
c3<-c(0,1,-1,0)
cc<-cbind(c1,c2)
ct<-rbind(c1,c2)
yt<-rbind(y)
(I-solve(m2%*%m1)%*%cc%*%solve(ct%*%solve(m2%*%m1)%*%cc)%*%ct)%*%solve(m2%*%m1)%*%m2%*%y
cc<-cbind(c3)
ct<-rbind(c3)
cccc<-rbind(c(3))
(I-solve(m2%*%m1)%*%cc%*%solve(ct%*%solve(m2%*%m1)%*%cc)%*%ct)%*%solve(m2%*%m1)%*%m2%*%y+(solve(m2%*%m1)%*%cc%*%solve(ct%*%solve(m2%*%m1)%*%cc))%*%cccc
linearHypothesis(x123, c(0,1,0,0), 0) 
linearHypothesis(x123, rbind(c(0,1,-1,0), c(0,0,1,-1)), c(0,0))
linearHypothesis(x123, c(0,1,-1,0), 3)
help(linearHypothesis)

#3
yy<-c(81.4, 122.2, 101.7, 175.6, 150.3, 64.8, 92.1, 113.8)
xx1<-c(195, 179, 205, 204, 201, 184, 210, 209)
xx2<-c(57, 61, 60, 62, 61, 64, 68, 61)

library(ellipse) 
alpha = 0.1
lm.fit = lm(yy ~ 0+xx1+xx2)
joint_conf = ellipse(lm.fit, c(1, 2), level = 1 - alpha) # joint confidence region

plot(joint_conf, type = "l", col = "blue", lwd = 2,
     cex.main = 1.5)
points(lm.fit$coefficients[1], lm.fit$coefficients[2], lwd = 3, pch = 4, col = "blue")






