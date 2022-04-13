library(MASS)
data(Boston)
select = grepl(pattern = "rm|tax|lstat|medv", x = names(Boston))
data = Boston[, select]
head(data)

lm.fit = lm(medv ~ rm + tax + lstat, data = data)


lm.fit_1 = lm(medv ~ rm + tax, data = data)
resid_1 = lm.fit_1$residuals

lm.fit_2 = lm(lstat ~ 0 +rm + tax, data = data)
resid_2 = lm.fit_2$residuals # X2를 X1에

data_resid = data.frame(res1 = resid_1, res2 = resid_2)
lm.fit_3 = lm(resid_1 ~ 0 + resid_2, data = data_resid)

coeff_lstat_lm.fit = as.vector(lm.fit$coefficients[4])
coeff_lstat_lm.fit_3 = as.vector(lm.fit_3$coefficients)
all.equal(coeff_lstat_lm.fit, coeff_lstat_lm.fit_3)


library(mvtnorm) # To sample from a bivariate normal distribution
set.seed(2022)
n = 1000
mu = c(1, 2)
sigma = matrix(c(1, 0.95, 0.95, 1), ncol = 2)
epsilon = rnorm(n, 0, 1)
X = rmvnorm(n = n, mean = mu, sigma = sigma)
y = 1 + 0.1 * X[,1] + 0.15 * X[,2] + epsilon
data1 = data.frame(X1 = X[,1], Y = y)
data2 = data.frame(X2 = X[,2], Y = y)
data3 = data.frame(X, Y = y)

plot(x = X[,1], y = X[,2], xlab = "X1", ylab = "X2")

lm.fit_1 = lm(y ~ X1, data = data1)
lm.fit_2 = lm(y ~ X2, data = data2)
lm.fit_3 = lm(y ~ X1 + X2, data = data3)

summary(lm.fit_1)
summary(lm.fit_2)
summary(lm.fit_3)

library(ISLR)
library(dplyr) #데이터 전처리 위한 것 
library(ellipse) # for ellipse function 신뢰구간 구할 때 필요
data("Hitters")
Hitters = Hitters %>%
  na.omit() %>%
  select(HmRun, Salary)
head(Hitters)


alpha = 0.05
lm.fit = lm(Salary ~ HmRun, data = Hitters)
joint_conf = ellipse(lm.fit, c(1, 2), level = 1 - alpha) # joint confidence region
Bonferroni_conf = confint(lm.fit, level = 1 - alpha/2) # Bonferroni simultaneous confidence intervals
int_lower = Bonferroni_conf[1, 1]
int_upper = Bonferroni_conf[1, 2]
HmRun_lower = Bonferroni_conf[2, 1]
HmRun_upper = Bonferroni_conf[2, 2]

plot(joint_conf, type = "l", col = "blue", lwd = 2,
     xlim = c(200, 450), ylim = c(5, 30), cex.main = 1.5,
     xlab = "Intercept", ylab = "HmRun", main = "Joint and Simultaneous Confidence Regions")
points(lm.fit$coefficients[1], lm.fit$coefficients[2], lwd = 3, pch = 4, col = "blue")
# Bonferroni simultaneous confidence interval for beta_0
abline(v = c(int_lower, int_upper), col = "red", lwd = 2)
# Bonferroni simultaneous confidence interval for beta_1
abline(h = c(HmRun_lower, HmRun_upper), col = "red", lwd = 2)


library(ISLR)
library(dplyr)
data("Hitters")
Hitters = Hitters %>%
  na.omit() %>%
  select(-c(League, Division, NewLeague)) # remove the factor type variable
head(Hitters)

threshold = 0.05
num_eliminate = 0
total_elimination = NULL
total_adj_R_squared = NULL
repeat{
  if(num_eliminate == 0){
    lm.fit_saturated = lm(Salary ~ ., data = Hitters)
    p_values = summary(lm.fit_saturated)$coefficients[-1, 4]
    adj_R_squared = summary(lm.fit_saturated)$adj.r.squared
  }else{
    lm.fit_eli = lm(Salary ~ ., data = Hitters_eli)
    summary(lm.fit_eli)
    p_values = summary(lm.fit_eli)$coefficients[-1, 4]
    adj_R_squared = summary(lm.fit_eli)$adj.r.squared
  }
  if(max(p_values) < threshold){
    lm.fit_final = lm.fit_eli
    eliminated.variables = eliminate
    colnames(eliminated.variables) = c(seq_along(eliminate))
    adj_R_squared = cbind(total_adj_R_squared, adj_R_squared)
    colnames(adj_R_squared) = c(seq_along(adj_R_squared) - 1)
    break
  }else{
    eliminate = names(p_values)[which.max(p_values)]
  }
  total_elimination = cbind(total_elimination, eliminate)
  eliminate = total_elimination
  total_adj_R_squared = cbind(total_adj_R_squared, adj_R_squared)
  Hitters_eli = Hitters[, !(names(Hitters) %in% eliminate)]
  num_eliminate = num_eliminate + 1
  if(num_eliminate == dim(Hitters)[2]){
    break
  }else{
    next
  }
}

eliminated.variables
adj_R_squared 
summary(lm.fit_final) 
par(mar = c(5,7,5,5))
plot(seq_along(adj_R_squared) - 1, adj_R_squared,
     type = "b", col = "red", lwd = 2,
     xlab = "number of rejections", ylab = expression(R[adj]^2))

