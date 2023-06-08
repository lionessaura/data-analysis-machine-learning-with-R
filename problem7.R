install.packages("car")
library('car')

r <- read.csv("math.csv", header = TRUE)
#Definition, make factors
r$gender <- factor(r$gender)
r$courses<- factor(r$courses)

summary(r)
#Draw interaction plot
interaction.plot(r$gender,r$courses,r$score)

model7 <- lm(score ~ gender + courses, data = r)
plot(model7,2)

residuals<-residuals(object = model7)
shapiro.test(residuals)

anova<-anova(model7, type = 'II')

plot(model7,1)


