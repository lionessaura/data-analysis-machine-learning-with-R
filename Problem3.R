# read the data
r <- read.csv("employment.csv", header = TRUE)
# perform chi square  
r_table <- table(r$employed, r$gender)
chisq.test(r_table)

