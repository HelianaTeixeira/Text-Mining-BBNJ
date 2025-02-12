#' # PreProcessing BBNJ data for Text Mining analysis
#' ###author: Heliana Teixeira
#' ###code adapted from (credits to): https://www.tidytextmining.com/tidytext.html
#' ###date: 02-06-2021

#' clear R workspace
rm(list = ls())

#' ### load and clean data
library(here)
here::here()
Fname.dat <- (here("Data", "db.csv"))
data <- read.csv(file = Fname.dat, header = TRUE, sep = ";") 
#data$SWOT.detail <- iconv(data$SWOT.detail, "UTF-8", "ASCII", sub = "") #from special characters and quote marks
data$SWOT.detail<- gsub("\\n", "", data$SWOT.detail) #from line breaks \n
#'visualising first rows of extracted dataset:
head(data$SWOT.detail)

#' ### 1. Extract dataset for Text Mining and Sensitivity analysis of SWOT and Categories main messages.
#' #### a) from original database table extract "uniqueID" and "SWOT_categories" fields into a new data table
library(tidyverse)
library(magrittr)
library(knitr)
mdata1 <- data %>% 
  filter(Select == "Keep")%>%
  select("uniqueID", "SWOT.detail")
kable(mdata1[1:5,], caption = "Table 1. BBNJ db selected variables (first 5 rows)")

#' #### b) Trim white spaces in text
mdata1<- mdata1 %>% 
  mutate(across(where(is.character), str_squish)) %>%
  mutate(across(where(is.character), str_trim))

#' #### c) split content of "SWOT.categories" into several columns by an identifier (pattern)
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

#' Note that "|" is a metacharacter in regex, we need to escape it by using double backslash "\\|" in pattern
#' Selecting only new columns starting with 'SWOT.cats.' will remove the original 'SWOT.detail' column
mdata1.split <- mdata1 %>%
  bind_cols(split_into_multiple(mdata1$SWOT.detail, "\\|", "SWOT.cats")) %>%
  select("uniqueID", starts_with("SWOT.cats."))
kable(mdata1.split[1:5,], caption = "Table 2. Split main messages per article (first 5 rows)")

#' #### d) melt matrix by transposing all "SWOT.detail" columns into a single column SWOT.detail by "uniqueID" (removing NA's)
mdata1.melted <- mdata1.split %>% gather("new.var", "SWOT.Cats", -uniqueID, na.rm=T)
kable(mdata1.melted[1:5,], caption = "Table 3. Transpose previously split columns into a single column as new SWOT.Cats variable per article (first 5 rows)")

#' #### e) transform any entry with only 1 white space to NA
mdata1.melted  <- mdata1.melted  %>% 
  mutate_all(funs(sub("^\\s*$", NA, .)))

#' #### f) Trim any left white spaces in text
mdata1.melted$SWOT.Cats <-str_trim(mdata1.melted$SWOT.Cats)
mdata1.melted$SWOT.Cats <-str_squish(mdata1.melted$SWOT.Cats)

#' #### g) Create a new variable "Message" by splitting "SWOT.Cats" column using pattern identifier "-"
mdata1.melted.split<- mdata1.melted %>% 
  select(-new.var) %>%
  drop_na(SWOT.Cats) %>%
  separate(SWOT.Cats, into = c("SWOT.Cats", "Message"), sep = "\\s+-\\s+")
kable(mdata1.melted.split[1:5,], caption = "Table 4. Messages per SWOT - Categories combination (first 5 rows)")

#' #### check: n= 26 not split entries:
count(mdata1.melted.split %>%
        summarise(uniqueID[
          which(is.na(Message))]))

#' #### h) Correct info for remaining n= 26 not split entries:
#'  #### check type of irregularities:  
irreg<-mdata1.melted.split %>%
  summarise(SWOT.Cats[
    which(is.na(Message))])

#'  #### patterns left unsplit: "\\s+-\\s?", "\\s?-\\s+", "\\s+\\–\\s+", no "-"
mdata1.melted.split<- mdata1.melted.split %>%
  separate(SWOT.Cats, into = c("SWOT.Cats.b", "Message.b"),sep = "\\s+-\\s?", remove=FALSE)

mdata1.melted.split<- mdata1.melted.split %>%
  mutate(Message = case_when(is.na(Message) ~ Message.b, 
                             TRUE ~ Message))%>%
  select(-SWOT.Cats, -Message.b) %>%
  rename(SWOT.Cats = SWOT.Cats.b)

count(mdata1.melted.split %>%
        summarise(uniqueID[
          which(is.na(Message))]))

#'  #### patterns left unsplit: "\\s?-\\s+", "\\s+\\–\\s+", no "-"
mdata1.melted.split<- mdata1.melted.split %>%
  separate(SWOT.Cats, into = c("SWOT.Cats.b", "Message.b"),sep = "\\s?-\\s+", remove=FALSE)

mdata1.melted.split<- mdata1.melted.split %>%
  mutate(Message = case_when(is.na(Message) ~ Message.b, 
                             TRUE ~ Message))%>%
  select(-SWOT.Cats, -Message.b) %>%
  rename(SWOT.Cats = SWOT.Cats.b)

count(mdata1.melted.split %>%
        summarise(uniqueID[
          which(is.na(Message))]))

#'  #### patterns left unsplit: "\\s+\\–\\s+", no "-"
mdata1.melted.split<- mdata1.melted.split %>%
  separate(SWOT.Cats, into = c("SWOT.Cats.b", "Message.b"),sep = "\\s+–\\s+", remove=FALSE)

mdata1.melted.split<- mdata1.melted.split %>%
  mutate(Message = case_when(is.na(Message) ~ Message.b, 
                             TRUE ~ Message))%>%
  select(-SWOT.Cats, -Message.b) %>%
  rename(SWOT.Cats = SWOT.Cats.b)

count(mdata1.melted.split %>%
        summarise(uniqueID[
          which(is.na(Message))]))  

#'  #### patterns left unsplit:  no "-"  at uniqueID ==226
mdata1.melted.split %>%
  filter(nchar(SWOT.Cats)>6 & is.na(Message))

mdata1.melted.split<- mdata1.melted.split %>%
  mutate(Message = case_when(
    nchar(SWOT.Cats)>6 & is.na(Message) ~ "to bring the decision on the establishment of MPAs to the relevant organizations and request their cooperation.",
                                  TRUE ~ as.character(Message)))

count(mdata1.melted.split %>%
        summarise(uniqueID[
          which(is.na(Message))]))  

#'  #### correct error in SWOT.Cats at uniqueID 271
mdata1.melted.split<- mdata1.melted.split %>%
  mutate(SWOT.Cats = case_when(uniqueID==271 & SWOT.Cats =="O IIII"~ "O III", TRUE ~ SWOT.Cats))

#'  #### Final check
mdata1.melted.split %>%
  filter(nchar(SWOT.Cats)>6)

mdata1.melted.split<- mdata1.melted.split %>%
  mutate(SWOT.Cats = case_when(
    nchar(SWOT.Cats)>6 ~ str_sub(SWOT.Cats, end = 3),
    TRUE ~ as.character(SWOT.Cats)))

#' #### i) two new columns
mdata1.melted.split<- mdata1.melted.split %>%
  separate(SWOT.Cats, into = c("SWOT", "Category"),sep = "\\s", remove=FALSE)

#' #### j) add year to SWOTcatsMessage dataframe
data.time <- data %>% 
   filter(Select == "Keep")%>%
  select("uniqueID", "Publication.Year")

data.time$Publication.Year <- as.numeric(data.time$Publication.Year)

data.time <- data.time %>%
  mutate(Publication.Year = case_when(uniqueID==21 ~ 2020, TRUE ~ Publication.Year))%>%
  mutate(Publication.Year = case_when(uniqueID==26 ~ 2020, TRUE ~ Publication.Year))%>%
  mutate(Publication.Year = case_when(uniqueID==86 ~ 2019, TRUE ~ Publication.Year))

#' #### k) add new factor on Events to final dataset
data.time <- data.time %>%
  mutate(Negotiation.phase = if_else(Publication.Year<= 2015, "BBNJ WG", 
                                     if_else(Publication.Year>= 2016 & Publication.Year<= 2018, "PrepCom",
                                             if_else(Publication.Year>= 2019, "IGC", "NA"))))

#' #### l) join tables
mdata1.melted.split$uniqueID <- as.integer(mdata1.melted.split$uniqueID)
BBNJmessage.time<- left_join(mdata1.melted.split, data.time, by = "uniqueID")
BBNJmessage.time<- BBNJmessage.time[, c(1, 6, 7, 2, 3, 4, 5)]

#' save new dataset file with temporal dimension
write.csv(BBNJmessage.time, here("./Data/SWOTcatsMessageTime.csv"), row.names = FALSE) #edit datasetname

#'Turn the R script into markdown output: Cmd + Shift + K

