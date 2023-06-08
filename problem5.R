# 1) 
r <- read.table("carstopping.txt", header = TRUE)
plot(r$StopDist, r$Speed)

correlation <- cor(r$StopDist, r$Speed)
c(correlation)
cor_test <- cor.test(r$StopDist, r$Speed)
c(cor_test)

# 2) 
r_linear <- lm(r$StopDist ~ r$Speed)
c(r_linear)

# summarizes the linear regression and 
# checks if it at at 5 % significance level

summary(r_linear)
#visualize
plot(r$Speed, r$StopDist, col = 'red', main='StopDist 
     ~ Speed plot + Linear Regression', xlab = 'x', ylab = 'y' )
abline(r_linear)
# 3)
r_res <- resid(r_linear)
# plotting
library(ggplot2)
ggplot2:: qplot(r$Speed, r_res, ylab = "Residuals", xlab = "Speed", 
      main = "Residuals plot")
plot(r_res)

# 4
library(ggplot2)
ggplot() +  geom_point(aes(r$Speed, r$StopDist)) +
  labs(x='Speed', y='StopDist', title = 'Stopdist ~ speed')

sqrt.dist <- sqrt(r$StopDist)
# new scatterplot
ggplot() + geom_point(aes(r$Speed, r$StopDist)) +
  labs(x='Speed', y='StopDist', title = 'Stopdist ~ Speed')

# 5
sqrt.model <- lm(sqrt.dist ~ r$Speed)

summary(sqrt.model)

#6
sqrt.res <- resid(sqrt.model)
ggplot() + geom_point(aes(r$Speed, sqrt.res)) +
  labs(x='Speed', y='Residuals', title = 'Residuals plot')

