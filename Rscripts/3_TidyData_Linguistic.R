#' ## Tidy data for text linguistic analysis
#'  author: Heliana Teixeira
#'  date: 28-07-2021
#'  code adapted from book "Text Mining with R: A Tidy Approach" at 
#'  https://www.tidytextmining.com/tidytext.html
#'  https://www.rdocumentation.org/packages/stopwords/versions/2.2
#'  
#'  Using BBNJ dataset, 
#'  Considering key Messages within SWOT levels (#4), through time (Negotiation.phase #3)



#' Clear R workspace
rm(list = ls()) 

#'### load packages
library(here)
library(tidytext)
library(tidyverse)

#'### load data
here::here()
Fname.dat <- (here("Data", "SWOTcatsMessageTime.csv")) #edit csv file name
data <- read.csv(file = Fname.dat, header = TRUE, sep = ",", dec =".") 
data<- data %>%
  mutate(msg.number = row_number()) #to keep track of original message number

#'### 1. Token based analysis - data preparation
#' convert text (Message) to one-token-per-row
mesg.word <- data %>%
  unnest_tokens(Message, Message) #token = "words" (default)

#' explore more common words
mesg.word %>%
  count(Message, sort = TRUE)%>%
  head()

#' remove stop words
data(stop_words) #from 3 lexicons: onix, SMART, snowball
summary(as.factor(stop_words$lexicon)) #explore all 3 - maybe keep problem, problems (in onix)?
mesg.word <- mesg.word %>%
  anti_join(stop_words, by=c("Message"="word"))

#' check
mesg.word %>%
  count(Message, sort = TRUE)%>%
  head()

#' remove stop words personalised: agreement & bbnj (n=113; n=96)
our_words <- c("agreement", "bbnj") # create new personalised vector of terms to ignore
id <- c(1,2)
stop_words_personl <- data.frame(id, our_words) # create new df

mesg.word <- mesg.word %>%
  anti_join(stop_words_personl, by=c("Message"="our_words"))

#' check
mesg.word %>%
  count(Message, sort = TRUE)%>%
  head()

#'### 2. export data for Linguistic analysis - part B & Sentiment analysis
mesg.word.shrt1<- mesg.word %>% 
  select(SWOT.Cats,Negotiation.phase,msg.number, Message) #interaction SWOT*Categories

mesg.word.shrt2<- mesg.word %>% 
  select(SWOT,Negotiation.phase,msg.number, Message) #SWOT levels

mesg.word.shrt3<- mesg.word %>% 
  select(Category,Negotiation.phase,msg.number, Message) #Category levels

#'save new dataset files
write.csv(mesg.word.shrt1, here("./Data/MessageTidy_SWOTCats.csv"), row.names = FALSE)  # edit correct dataset name

write.csv(mesg.word.shrt2, here("./Data/MessageTidy_SWOT.csv"), row.names = FALSE)  # edit correct dataset name

write.csv(mesg.word.shrt3, here("./Data/MessageTidy_Cats.csv"), row.names = FALSE)  # edit correct dataset name

#'Turn the R script into markdown output: Cmd + Shift + K