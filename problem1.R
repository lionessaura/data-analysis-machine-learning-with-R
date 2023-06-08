# frame the data
year <- c(1, 2, 2, 3, 4, 5, 5, 6, 7, 7, 
          8, 9, 9, 10, 13, 15, 21, 25, 29, 30) 
salary <- c(2100, 2400, 2320, 2450, 2800, 2660, 2830, 2850, 3000, 3280,
            3350, 3700, 3540, 5100, 5080, 6370, 6900, 7550, 8400, 8700)
# draw the boxplot for the salary
boxplot(salary)
#describe the characteristics of the data of salary
salary_average <- mean(salary)
c(salary_average)
salary_median <-median(salary)
c(salary_median)

# draw the plot x- axis year, y -axis salary
plot(year, salary)

# find the correlation coefficient 
correl_coef <- cor(year, salary)
c(correl_coef)
# check the relationship between them
test_corr <- cor.test(year, salary)
c(test_corr)
