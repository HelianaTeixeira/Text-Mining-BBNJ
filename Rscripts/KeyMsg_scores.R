#'### load packages
library(here)

#'### load data
here::here()
Fname.dat <- (here("Data", "KMindex_scores.csv")) #edit csv file name
data <- read.csv(file = Fname.dat, header = TRUE, sep = ",", dec =".") 
