# read the data 
r <- read.csv("movie.csv", header = TRUE)
# perform chi-square
r_table <- table(r$movie, r$country)
chisq.test(r_table)

