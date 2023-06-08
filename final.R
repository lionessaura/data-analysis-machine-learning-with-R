library(tidyverse)
#for quick plots


#TASK 1

years <- c(1,2,2,3,4,5,5,6,7,7,8,9,9,10,13,15,21,25,29,30)
salary<-c(2100,2400,2320,2450,2800,2660,2830,2850,3000,3280,3350,3700,3540,5100,5080,6370,6900,7550,8400,8700)
#Boxplot for salary
median(salary)
sd(salary)
mean(salary)
quantile(salary, prob=c(.25,.5,.75))
ggplot()+
  geom_boxplot(aes(years, salary))
#Median of the salary is at 3315. 
#Boxplot is rather tall, so overall spread of salaries is huge.
#Below the median salary the spread of the salaries is small. 
#Above the median salary the spread of the salaries is huge.
#This shows that many workers have similar salaries at lower part of the scale, 
#but in the upper scale salaries are much more variable. 
#Salaries are skewed to the lower part of the graph, which suggests
#that salaries are not normally distributed.
#Upper whisker of the boxplot is rather long, which suggests
#that there is a huge spread of salaries in the top 25%
#of data.
#From boxplot we can see that while overall spread of salaries is huge,
#it is because of very variable salaries in the upper portion of the data,
#whilst most people earn similar salaries around median.

#Years & Salary plot 
ggplot()+
  geom_point(aes(years, salary))+
  labs(y='Salary', x = 'Years', title = 'Years&Salary')
  
ggplot()+
  geom_line(aes(years, salary))+
  labs(y='Salary', x = 'Years', title = 'Years&Salary')

ggplot()+
  geom_line(aes(years, salary))+
  geom_point(aes(years, salary))+
  labs(y='Salary', x = 'Years', title = 'Years&Salary')

#Salary-Years correlation coefficient
cor(salary,years)

#Correlation coefficient is equal to 0.9832246. This is very close to 1,
#which signals that there is a strong relationship between salary and years.
#And, in general, higher salaries can be explained by 
#higher number of years worked. (linear)

#-----------------------------------------------------------------------------------
#TASK 2
ames <- read.csv(file = 'ames.csv')

#1
#compute mean
mean<-mean(ames$Gr.Liv.Area,na.rm=TRUE)
mean
#compute variance
var<-var(ames$Gr.Liv.Area)
var
#2
#taking sample of 60
sample<-sample(ames$Gr.Liv.Area,60)
# The sample mean 'samplemean' is a point estimate of the population mean 
samplemean<-mean(sample,na.rm=TRUE)
samplemean
#samplemean = 1431.45 in our case

#3 
#Conf.interval = sample mean ± margin of error
#error = qnorm(alpha/2) * population st.dev / sqrt(sample size)
#calculate population st dev
s=sqrt(var)
s

alpha<-0.05
low <- samplemean-qnorm(alpha/2)*(s/sqrt(60))
low
up <- samplemean+qnorm(alpha/2)*(s/sqrt(60))
up

#calculate error
error <- qnorm(0.975)*s/sqrt(60)
#Lower bound
a=samplemean-error
a
#a = 1303.541 in our case
#Upper bound
b=samplemean+error
b
#b = 1559.359 in our case

#4
#Vectors to store lower and upper bounds of conf. intervals
upper<-c()
lower<-c()
pop.mean<-mean
#Obtaining 50 samples, 50 confidence intervals:
for (i in 1:50){
  x<-sample(ames$Gr.Liv.Area,60)
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
  xR <- m + ci.max * c(-1, 1) #the range of x-axis for the plot
  yR <- c(0, 41 * k / 40)       #the range of y-axis for the plot
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE) # axes=FALSE: to remove the x-axis and y-axis values
  abline(v=m, lty=2, col='black')  # abline() draws a straight line of the form Y=aX+b, h=, or v=.
  # lty=2: to plot with dash (-) for the type of line
  axis(1, at=m, paste("mu = ", round(m, 4)), cex.axis=1.15) # to specify the side of the plot to be drawn on the axis (1=bottom),
  #at: the point where the abline is drawn, cex.axis: font size
  
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
#Print graph
plot_ci(lower, upper, pop.mean)
#What is th actual level of confidence?
#Of the 50 generated intervals, according to the graph,
#3 do not include mean. So the confidence level from the graph is
#94% (=100%-3/50)



#-----------------------------------------------------

#TASK 3
employment <- read.csv(file = 'employment.csv')
#Hypothesis
#Gender of the applicant does not affect employment success rate 
#H0:prob(employed if female)=prob(employed if male)
#Perform the two-proportions z-test
#Form vector of counts of successes and vector of count trials
#using information from table
table(employment$employed,employment$gender)
#Perform test
prop.test(x=c(550,620), n = c(1000,1000),alternative = "two.sided",conf.level = 0.95,correct = FALSE)
#p-value = 0.001489
#We reject null hypothesis at 95% confidence level
#We can conclude that the proportion of employment successes 
#is significantly greater in the group with males.

#TASK 4
movie <- read.csv('movie.csv')

#H0: No association between nationality and fav movie genre

#Perform Pearson's Chi-Square test for association for 3x3
#Create contingency table matrix
matrix<-table(movie$country,movie$movie)
#Perform testing
model4<-chisq.test(matrix)
#Show summary
model4
#p-value = 4.921e-07 significantly lower that 5%(0.05)
#So we reject null hypothesis of no association, and 
#the results suggest to us that there exists an association between 
#nationality and fav movie genre.

#TASK 5
#1)
carstopping <- read.delim('carstopping.txt')
stopdist<-carstopping$StopDist
speed<-carstopping$Speed
#Test correlation between stopdist and speed
cor(stopdist,speed)
cor.test(stopdist,speed)
#correlation coeffitient = 0.9355037 which suggests strong correlation 
#between stopdist and speed

#2) Linear model
#Fitting
model <- lm(stopdist ~ speed)
#Summary contains all info on the linear regression,
#including coefficients and tests of significance
summary(model)
#Visualization 
#scatterplot of raw data + fitted regression line
plot(speed, stopdist, col='red', main='Stopdist ~ Speed plot + Linear regression', xlab='x', ylab='y')
abline(model)

# To access significance of the model, we need to examine F-statistic
#F-statistic: 427.7 on 1 and 61 DF,  p-value: < 2.2e-16
#Since this p-value is less than .05, the model as a whole is statistically significant.
#P.S.We can also see that model coefficients are statistically significant
# p-val(intercept) = 4.25e-08 < .05 ; p-val(speed coef)< 2e-16 < .05
#However, сoefficient of determination is smaller than expected for
#fitting linear regression on data with linear relationship
#R-squared = 0.8731 

#3)
#Residual plot
residuals<-resid(model)
#plotting
qplot(carstopping$Speed, resid(model),   
     ylab="Residuals", xlab="Speed", 
     main="Residuals plot") 
#If a model is adequate, the residuals should have 
#no obvious patterns or systematic structure.
#It looks like there is a pattern in the distribution of the residuals.
#As speed increases, increases the spread of residuals.

#4)
#Scatter plot stopdist~speed...
ggplot()+
  geom_point(aes(speed, stopdist))+
  labs(x='Speed', y = "StopDist", title='stopdist~speed')
#...looks like a curve.
#Create sqrt.dist
sqrt.dist<-sqrt(stopdist)
#New scatter plot sqrt(stopdist)~speed
ggplot()+
  geom_point(aes(speed, sqrt.dist))+
  labs(x='Speed', y = "StopDist", title='stopdist~speed')

#In the new scatter plot there seems to be a linear
#relationship between sqrt.dist and speed.

#5) New lm model
sqrt.model <- lm(sqrt.dist ~ speed)
summary(sqrt.model)

#Model as a whole is significant:
#F-statistic: 746.2 on 1 and 61 DF,  p-value: < 2.2e-16 < .05
#Coefficients are significant.
#p-val(intercept) = 1.82e-05 *** < .05
#p-val(coef speed) < 2e-16 *** < .05
#Coefficient of determination:
#R-squared = 0.9232 
#Improved compared to previous LR, which means that
#new linear regression model(with sqrt.dist) fits data better.

#6)New residuals
sqrt.residuals<-resid(sqrt.model)
#plotting
ggplot()+
  geom_point(aes(carstopping$Speed, resid(sqrt.model)))+
  labs(x="Speed", y= "Residuals", title="Residuals plot")
#New redisual plot seems chaotic,without heavy outliers and without any relationship,
#which suggests that new model is rather good at fitting data.

#TASK 6 
#1)
hospital <- read.delim('hospital.txt')
#Definition of dependent and explanatory variables 
InfctRsk<-hospital$InfctRsk
Stay<-hospital$Stay
Age<-hospital$Age
Xray<-hospital$Xray
#Compute correlation of dependent variable with each explanatory
cor(InfctRsk,Stay)
#0.5334438
cor(InfctRsk,Age)
#0.001093166
cor(InfctRsk,Xray)
#0.4533916
#Seems like there is only no correlation between InfctRsk and Age.

#Scatterplot analysis of correlation
#
ggplot()+
  geom_point(aes(Stay,InfctRsk))
#We can see correlation in the graph, we can approximately fit a line 
#to these points.
ggplot()+
  geom_point(aes(Age,InfctRsk))
#From the graph, there's hardly any correlation. 
ggplot()+
  geom_point(aes(Xray,InfctRsk))
#We can see correlation in the graph, we can approximately fit a line 
#to these points.

#2)
model6<-lm(InfctRsk~Stay+Age+Xray)
summary(model6)
# Overall model is significant:
#F-statistic:  20.7 on 3 and 109 DF,  p-value: 1.087e-10 < .05
#Coefficeints for stay & Xray are significant.
#p-val(coef Stay) = 9.88e-07 *** < .05
#p-val(coef Xray) = 0.000899 *** < .05
#Coefficeints for intercept & age are not significant.
#p-val(intercept) = 0.448003   > .05
#p-val(coef Age) = 0.330098  > .05


#residual plot
residual6<-resid(model6)
ggplot()+
  geom_point(aes(InfctRsk, resid(model6)))+
  labs(y="Residuals", x="Speed", title="Residuals plot")

#There is a clear linear patter in the distribution of residuals,
#which suggests that multiple linear regression is not a valid model
#for our case.
#Moreover, Adjusted R-squared:  0.3455  is low. Which supports
#the same conclusion.




#TASK 7
math <- read.csv('math.csv')
#Definition
gender <- math$gender
courses<- math$courses
score <- math$score
#Draw interaction plot
interaction.plot(courses,gender,score)
