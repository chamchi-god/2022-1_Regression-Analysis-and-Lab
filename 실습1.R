install.packages("UsingR")
library(UsingR)

data("father.son")
head(father.son)

plot(sheight ~ fheight, data = father.son,
     pch = 19, cex = 0.5,
     xlab = "Father's height (inches)", ylab = "Son's height (inches)") #Scatter plot 그리기

lm.fit = lm(sheight ~ fheight, data = father.son) #Simple linear regression
summary(lm.fit) 

plot(sheight ~ fheight, data = father.son,
     pch = 19, cex = 0.5,
     xlab = "Father's height (inches)", ylab = "Son's height (inches)")
abline(lm.fit, col = "red", lwd = 2) # Scatter plot 위에 적합된 회귀직선 겹쳐 그리기

Amazon = data.frame(Year = 1962:1978,
                    High = c(25.82, 25.35, 24.29, 24.05, 24.89, 25.35,
                             25.23, 25.06, 27.13, 27.36, 26.65, 27.13,
                             27.49, 27.08, 27.51, 27.54, 26.21))
head(Amazon)


lm.fit = lm(High~Year, data = Amazon)
summary(lm.fit)

par(mfrow = c(1, 2))
scatter.smooth(x = 1:dim(Amazon)[1], y = residuals(lm.fit),
               xlab = "Year", ylab = "Residuals")
scatter.smooth(x = predict(lm.fit), y = residuals(lm.fit),
               xlab = expression(hat(y)), ylab = "Residuals")

install.packages("lmtest")
library(lmtest)

dwtest(lm.fit, alternative = "two.side") #Durbin-Watson test

dwtest(lm.fit, alternative = "greater")

dwtest(lm.fit, alternative = "less")

#Simulation data example (log transformation)
set.seed(1234)
n = 100
beta0 = 1
beta1 = 0.7
e = rnorm(n, 0, sqrt(0.1))
x = runif(n, 0, 5)
y = exp(beta0 + beta1 * x + e)
mydata = data.frame(x = x, y = y)
plot(y ~ x, data = mydata)

# Object function
SSE_lambda = function(x, y, lambda){
  if(lambda == 0){
    z_lambda = log(y) * prod(y)^(1/n)
  }else{
    z_lambda = (y^lambda -1) / lambda * prod(y)^((-1/n)*(lambda -1))
  }
  n = length(y)
  X = matrix(c(rep(1, n), x), n, 2)
  H = X %*% solve(t(X) %*% X) %*% t(X)
  I = diag(1, n)
  SSE_lambda = t(z_lambda) %*% (I - H) %*% z_lambda
  return(SSE_lambda)
}
#Vectorization
vec_SSE_lambda = Vectorize(SSE_lambda, "lambda")

# Find the optimal lambda
lambda = seq(-2, 2, 0.2)
all_SSE = vec_SSE_lambda(mydata$x, mydata$y, lambda)
(best_lambda = lambda[which.min(all_SSE)])
(min_SSE = all_SSE[which.min(all_SSE)])

# plot lambda vs SSE_lambda(log-scale)
plot(lambda, log(all_SSE),
     type = "b", lwd = 2,
     xlab = "Lambda", ylab = "Sum of Squared Error(log-scale)")
points(x = best_lambda, y = log(min_SSE),
       col = "red", cex = 2, pch = 8)

# 두 기울기의 검정
line_1 = data.frame(x = c(100, 125, 220, 205, 300, 255, 225, 175,
                          270, 170, 155, 190, 140, 290, 265),
                    y = c(218, 248, 360, 351, 470, 394, 332, 321,
                          410, 260, 241, 331, 275, 425, 367))
line_2 = data.frame(x = c(105, 215, 270, 255, 175, 135, 200, 275, 155, 320, 190, 295),
                    y = c(140, 277, 384, 341, 215, 180, 260, 361, 252, 422, 273, 410))

slope_test = function(data1, data2, alpha){
  n_1 = dim(line_1)[1]
  n_2 = dim(line_2)[1]
  xbar_1 = mean(line_1$x)
  ybar_1 = mean(line_1$y)
  S_xx_1 = sum((line_1$x - xbar_1)^2)
  S_xy_1 = t(line_1$x - xbar_1) %*% (line_1$y - ybar_1)
  beta1_1 = S_xy_1 / S_xx_1
  beta0_1 = ybar_1 - beta1_1 * xbar_1
  yhat_1 = beta0_1 + beta1_1 * line_1$x
  SSE_1 = sum((line_1$y - yhat_1)^2)
  xbar_2 = mean(line_2$x)
  ybar_2 = mean(line_2$y)
  S_xx_2 = sum((line_2$x - xbar_2)^2)
  S_xy_2 = sum(t(line_2$x - xbar_2) %*% (line_2$y - ybar_2))
  beta1_2 = S_xy_2 / S_xx_2
  beta0_2 = ybar_2 - beta1_2 * xbar_2
  yhat_2 = beta0_2 + beta1_2 * line_2$x
  SSE_2 = sum((line_2$y - yhat_2)^2)
  SSEF = SSE_1 + SSE_2
  df_SSEF = (n_1 - 2) + (n_2 - 2)
  MSEF = SSEF / df_SSEF
  t_0 = (beta1_1 - beta1_2) / sqrt(MSEF * (1/S_xx_1 + 1/S_xx_2))
  critical_value = qt(alpha/2, df = df_SSEF, lower.tail = FALSE)
  if(abs(t_0) > critical_value){
    cat("Since |t_0| > t_", alpha/2, "(", df_SSEF, ")",
        " reject the null hypothesis.", "\n",
        "Here, |t_0| = ", round(t_0, 3), " and t_", alpha/2, "(", df_SSEF, ") = ",
        round(critical_value, 3),"\n", sep = "")
  }else{
    cat("Since |t_0| <= t_", alpha/2, "(", df_SSEF, ")",
        " there is not enough evidence to reject the null hypothesis." ,"\n",
        "Here, |t_0| = ", round(t_0, 3), " and t_", alpha/2, "(", df_SSEF, ") = ",
        round(critical_value, 3),"\n", sep = "")
  
  }
}

slope_test(line_1, line_2, alpha = 0.05)


