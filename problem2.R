# 1) population mean and the variance
r <- read.csv("ames.csv",  header = TRUE)
house_size <- mean(r$Gr.Liv.Area)
c(house_size)
v <- var(r$Gr.Liv.Area)
c(v)

#2) selects random sample of 60 from the population
# and finds the point estimate for the population mean...
data <- c(r$Gr.Liv.Area)
random <- sample(data, size = 60)
estimate_mean <- mean(random)
c(estimate_mean)

# 3) finds 95% confidence interval for the population mean 
# I used the sample selected in (2) and 
# the value of variance obtained in (1) as the standard deviation
# standard deviation = sqrt(variance)
s <- sqrt(v) 
size_data <- length(random)
error <- qnorm(0.975) * s/sqrt(size_data)
lower_limit <- estimate_mean - error
c(lower_limit)
upper_limit <- estimate_mean + error
c(upper_limit)
# 4) Vectors to store lower and upper bounds of conf. intervals
upper<-c()
lower<-c()
pop.mean<-mean

for (i in 1:50){
  x<-sample(r$Gr.Liv.Area,60)
  upper.i<-mean(x)+qnorm(0.975)*(s/sqrt(60))
  lower.i<-mean(x)-qnorm(0.975)*(s/sqrt(60))
  upper[i]<-upper.i
  lower[i]<-lower.i
}
#vectors with bounds of conf. intervals
upper
lower
#Execute code from lecture 
plot_ci <- function(lo, hi, m) {
  par(mar=c(2, 1, 1, 1), mgp=c(2.7, 0.7, 0)) #mar: to specify the number of margin lines to be assigned to each side of the plot (bottom, left, top, right)
  #mgp: to specify margin line for (axis title, axis name, axis line)
  k <- length(lo)
  ci.max <- max(rowSums(matrix(c(-1 * lo, hi), ncol=2)))  #Finding the maximum length of the confidence intervals
  xR <- m + ci.max * c(-1, 1) # x-axis 
  yR <- c(0, 41 * k / 40)       #ty-axis
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE) # axes=FALSE: to remove the x-axis and y-axis values
  abline(v=m, lty=2, col='black')  # draws a straight line of y=ax+b, h=, or v=.
  # specifies the side of the plot,
  axis(1, at=m, paste("mu = ", round(m, 4)), cex.axis=1.15) 

 
  
  for(i in 1:k) {
    x <- mean(c(hi[i], lo[i]))
    ci <- c(lo[i], hi[i])
    if (lo[i]>m | m>hi[i]) {
      col <- "red"
      points(x, i, cex=1.4, col=col)
      lines(ci, rep(i, 2), col=col, lwd=5)
    }
    
    points(x, i, pch=20, cex=1.2, col="black")
    lines(ci, rep(i, 2), col="black")
  }
}
plot_ci(lower, upper, pop.mean)

