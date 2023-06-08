#1)
r <- read.table('hospital.txt', header = TRUE)
# define the variables from the table
InfctRsk<-r$InfctRsk
Stay<-r$Stay
Age<-r$Age
Xray<-r$Xray
# outputs correlation of dependent variable with each explanatory
cor(InfctRsk,Stay)
cor(InfctRsk,Xray)
cor(InfctRsk,Age)

# scatterplot 
library(ggplot2)
ggplot()+
  geom_point(aes(Stay,InfctRsk))

ggplot()+
  geom_point(aes(Age,InfctRsk))

ggplot()+
  geom_point(aes(Xray,InfctRsk))


#2)
r.model<-lm(InfctRsk~Stay+Age+Xray)
summary(r.model)


#3
r.res<-resid(r.model)
ggplot()+
  geom_point(aes(InfctRsk, r.res))+
  labs(y="Residuals", x="Speed", title="Residuals plot")

