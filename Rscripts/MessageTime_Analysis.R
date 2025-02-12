#' #  BBNJ data analysis - exploration
#' ###author: Heliana Teixeira
#' ###date: 24-08-2021

#' clear R workspace
rm(list = ls())

#' ### load and clean data
library(here)
here::here()
auth.dat <- (here("Data", "AuthorsPubl.csv"))
geog.dat <- (here("Data", "CountriesYearPubl.csv"))
message.dat <- (here("Data", "SWOTcatsMessageTime.csv"))

data.auth <- read.csv(file = auth.dat, header = TRUE, sep = ";")
data.geog <- read.csv(file = geog.dat, header = TRUE, sep = ";")
data.mesg <- read.csv(file = message.dat, header = TRUE, sep = ";")

#' ### 1. Analysis by SWOT 4 levels (S, W, O, T) & Categories 6 levels (I to VI) & Time

#example for table
#kable(mdata2[1:5,], caption = "Table 1. BBNJ db: year of publication & geographic information (first 5 rows)")
 