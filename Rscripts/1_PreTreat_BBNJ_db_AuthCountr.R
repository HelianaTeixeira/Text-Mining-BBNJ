#' # PreProcessing BBNJ data for Text Mining analysis
#' author: Heliana Teixeira
#' date: 02-06-2021

#' clear R workspace
rm(list = ls())

#' ### load and clean data
library(here)
here::here()
Fname.dat <- (here("Data", "db.csv"))
data <- read.csv(file = Fname.dat, header = TRUE, sep = ";") 

library(tidyverse)
library(magrittr)
library(knitr)
#' ### 1. Extract selected fields into a new dataset
#' #### a) author information
mdata1 <- data %>% 
  filter(Select == "Keep")%>%
  select("uniqueID", "Author.Full.Names")
kable(mdata1[1:5,], caption = "Table 1a. BBNJ db author information (first 5 rows)")

#' #### b) year publication & geographic information
mdata2 <- data %>% 
  filter(Select == "Keep")%>%
  select("uniqueID", "Publication.Year","Country_study")
kable(mdata2[1:5,], caption = "Table 1b. BBNJ db: year of publication & geographic information (first 5 rows)")

#' #### c) Trim white spaces in text
mdata1<- mdata1 %>% 
  mutate(across(where(is.character), str_squish)) %>%
  mutate(across(where(is.character), str_trim))

mdata2<- mdata2 %>% 
  mutate(across(where(is.character), str_squish)) %>%
  mutate(across(where(is.character), str_trim))

#' #### d) split fields content into several columns by an identifier (pattern)
#' We don't know the number of the result columns before the split, so run a function that uses stringr to split a column, given the pattern and a prefix for the new n columns
split_into_multiple <- function(column, pattern, into_prefix){
  cols <- str_split_fixed(column, pattern, n = Inf)
  # Sub out the ""'s returned by filling the matrix to the right, with NAs which are useful
  cols[which(cols == "")] <- NA
  cols <- as.tibble(cols)
  # name the 'cols' tibble as 'into_prefix_1', 'into_prefix_2', ..., 'into_prefix_m' 
  # where m = # columns of 'cols'
  m <- dim(cols)[2]
  
  names(cols) <- paste(into_prefix, 1:m, sep = ".")
  return(cols)
}

#' Note: metacharacters in regex need to be escaped with double backslash in pattern e.g.: "\\|"
mdata1.split <- mdata1 %>%
  bind_cols(split_into_multiple(mdata1$Author.Full.Names, ";", "Author")) %>%
  select("uniqueID", starts_with("Author."))
kable(mdata1.split[1:5,], caption = "Table 2a. Split co-authors per article (first 5 rows)")

mdata2.split <- mdata2 %>%
  bind_cols(split_into_multiple(mdata2$Country_study, ",", "Country")) %>%
  select("uniqueID","Publication.Year", starts_with("Country."))
kable(mdata2.split[1:5,], caption = "Table 2b. Split countries per article (first 5 rows)")

#' #### complete countries missing info for uniqueID's 145, 294 & 313
mdata2.split <- mdata2.split %>%
  mutate(Country.1 = case_when(uniqueID==145 ~ "Canada", TRUE ~ Country.1))%>%
  mutate(Country.2 = case_when(uniqueID==145 ~ "Canada", TRUE ~ Country.2))%>%
  mutate(Country.3 = case_when(uniqueID==145 ~ "UK", TRUE ~ Country.3))%>%
  mutate(Country.4 = case_when(uniqueID==145 ~ "Canada", TRUE ~ Country.4))%>%
  mutate(Country.1 = case_when(uniqueID==294 ~ "Netherlands", TRUE ~ Country.1))%>%
  mutate(Country.1 = case_when(uniqueID==313 ~ "USA", TRUE ~ Country.1))

#' #### complete year missing info for uniqueID's 21, 26 & 86
#browseURL(data$DOI_link[which(data$uniqueID==21)])#2020; ERROR in DOI at source, url: https://onlinelibrary.wiley.com/doi/full/10.1111/reel.12372
browseURL(data$DOI_link[which(data$uniqueID==26)])#2020
browseURL(data$DOI_link[which(data$uniqueID==86)])#2019

mdata2.split <- mdata2.split %>%
  mutate(Publication.Year = case_when(uniqueID==21 ~ "2020", TRUE ~ as.character(Publication.Year)))%>%
  mutate(Publication.Year = case_when(uniqueID==26 ~ "2020", TRUE ~ as.character(Publication.Year)))%>%
  mutate(Publication.Year = case_when(uniqueID==86 ~ "2019", TRUE ~ as.character(Publication.Year)))

#' #### e) melt matrix by transposing split columns into a single column by "uniqueID" (removing NA's)
mdata1.melted <- mdata1.split %>%
 select(-Author.Full.Names)%>%
  pivot_longer(!uniqueID, names_to = "Author.order",values_to ="Author.name", values_drop_na = TRUE)
kable(mdata1.melted[1:5,], caption = "Table 3a. Transposed Authors' name per article uniqueID & with co-authorship order (Author.order) (first 5 rows)")

mdata2.melted <- mdata2.split %>%
  pivot_longer(!c(uniqueID,Publication.Year), names_to = "Country.order",values_to ="Country", values_drop_na = TRUE)%>%
  select(-Country.order)
kable(mdata2.melted[1:5,], caption = "Table 3b. Transposed Country per article uniqueID (first 5 rows)")

#' #### f) correct country name for uniqueID's 9 & 89
mdata2.melted <- mdata2.melted %>%
  mutate(Country = case_when(uniqueID==9 & Country== "NZ" ~ "New Zealand", TRUE ~ Country))%>%
  mutate(Country = case_when(uniqueID==89 & Country== "Virginia" ~ "USA", TRUE ~ Country))

#' #### g) Trim any left white spaces in text
mdata1.melted$Author.name <-str_trim(mdata1.melted$Author.name)
mdata1.melted$Author.name <-str_squish(mdata1.melted$Author.name)

mdata2.melted$Country <-str_trim(mdata2.melted$Country)
mdata2.melted$Country <-str_squish(mdata2.melted$Country)

#' #### h) save new dataset file
write.csv(mdata1.melted, here("./Data/AuthorsPubl.csv"), row.names = FALSE) 
write.csv(mdata2.melted, here("./Data/CountriesYearPubl.csv"), row.names = FALSE) 

#'Turn the R script into markdown output: Cmd + Shift + K

