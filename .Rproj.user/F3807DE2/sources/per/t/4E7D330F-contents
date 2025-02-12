#' ## Linguistic analysis - part C
#'  author: Heliana Teixeira
#'  date: 28-07-2021
#'  code adapted from book "Text Mining with R: A Tidy Approach" at 
#'  https://www.tidytextmining.com/tidytext.html
#'  
#'  Using BBNJ dataset, 
#'  Considering key Messages by Categories (#6) within SWOT levels (#4), through time (Negotiation.phase #3)
#'  This script applies keyword extraction techniques to analyse plain text and perform sentiment analysis.

#' Clear R workspace
rm(list = ls()) 

#### load packages ----
library(here)
library(tidyr)
library (tidytext)
library(tidyverse)
library(scales) # to visualise

#### load data ----
here::here()
Fname.dat_1 <- (here("Data", "MessageTidy_SWOT.csv")) #edit file name
data_1 <- read.csv(file = Fname.dat_1, header = TRUE, sep = ",", dec =".") 

Fname.dat_2 <- (here("Data", "MessageTidy_Cats.csv")) #edit file name
data_2 <- read.csv(file = Fname.dat_2, header = TRUE, sep = ",", dec =".") 

Fname.dat_3 <- (here("Data", "MessageTidy_SWOTCats.csv")) #edit file name
data_3 <- read.csv(file = Fname.dat_3, header = TRUE, sep = ",", dec =".") 

#'### 2. Sentiment analysis
#' There are variety of methods and dictionaries for evaluating the opinion or emotion in text. 
#' binary positive and negative categories
get_sentiments("bing")
#' assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment
#install.packages("textdata")
library(textdata)
get_sentiments("afinn")
#'categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. 
get_sentiments("nrc")
#' https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

library(stringr)
library(dplyr)

#' Pre-treatment:
#' reorder levels within factor var SWOT
data_1$SWOT <- factor(data_1$SWOT, levels = c("S", "W", "O", "T"))
#' reorder levels within factor period
data_1$Negotiation.phase <- factor(data_1$Negotiation.phase, levels = c("BBNJ WG", "PrepCom", "IGC"))

#' reorder levels within factor var SWOT
data_2$Category <- factor(data_2$Category, levels = c("I", "II", "III", "IV", "V","VI"))
#' reorder levels within factor period
data_2$Negotiation.phase <- factor(data_2$Negotiation.phase, levels = c("BBNJ WG", "PrepCom", "IGC"))

#' ### Sentiment through the SWOT levels
#'bing lexicon
SWOT.sentim <- data_1 %>%
  inner_join(get_sentiments("bing"), by = c("Message" = "word")) %>%
  count(SWOT, index= msg.number, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(SWOT.sentim, aes(index, sentiment, fill = SWOT)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~SWOT, ncol = 2) + #scales = "free_x"
  theme_classic ()+
  theme (strip.background = element_blank(), strip.placement = "outside")+
  geom_hline(yintercept=0, color = "gray")+
  labs(x = "Key messages", y = "Sentiment score (bing)")

write.csv(SWOT.sentim, here("Output_Res","SWOT.sentim_bing.csv"))

#'AFFIN lexicon
SWOT.sentim2 <- data_1 %>%
  inner_join(get_sentiments("afinn"), by = c("Message" = "word")) %>%
  count(SWOT, index = msg.number, wt=value) %>% 
  rename(sentiment = n)

ggplot(SWOT.sentim2, aes(index, sentiment, fill = SWOT)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~SWOT, ncol = 2)+ #scales = "free_x"
  theme_classic ()+
  theme (strip.background = element_blank(), strip.placement = "outside")+
  geom_hline(yintercept=0, color = "gray")+
  labs(x = "Key messages", y = "Sentiment score (AFFIN)")

write.csv(SWOT.sentim2, here("Output_Res","SWOT.sentim_AFFIN.csv"))

#'positive vs negative
library(reshape2)
library(wordcloud)
data_1 %>%
  group_by(SWOT)%>%
  inner_join(get_sentiments("bing"),by = c("Message" = "word")) %>%
  count(Message, sentiment, sort = TRUE) %>%
  acast(Message ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 40)

#' ### Sentiment through the Categories
#'bing lexicon
Cats.sentim <- data_2 %>%
  inner_join(get_sentiments("bing"), by = c("Message" = "word")) %>%
  count(Category, index= msg.number,sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(Cats.sentim, aes(index, sentiment, fill = Category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Category, ncol = 2) #scales = "free_x"

#' afinn lexicon
Cats.sentim2 <- data_2 %>%
  inner_join(get_sentiments("afinn"), by = c("Message" = "word")) %>%
  count(Category, index= msg.number, wt=value) %>%
  rename(sentiment = n)

ggplot(Cats.sentim2, aes(index, sentiment, fill = Category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Category, ncol = 2) #scales = "free_x"

#'positive vs negative
library(reshape2)
library(wordcloud)
data_2 %>%
  group_by(Category)%>%
  inner_join(get_sentiments("bing"),by = c("Message" = "word")) %>%
  count(Message, sentiment, sort = TRUE) %>%
  acast(Message ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 40)

#' ### Sentiment through the Negotiation phases
#' reorder levels within factor period
data_3$Negotiation.phase <- factor(data_3$Negotiation.phase, levels = c("BBNJ WG", "PrepCom", "IGC"))


#'bing lexicon
time.sentim <- data_3 %>%
  inner_join(get_sentiments("bing"), by = c("Message" = "word")) %>%
  count(Negotiation.phase, index= msg.number,sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

time.sentimYear <- time.sentim %>%
  mutate(last.Year= if_else(Negotiation.phase=="BBNJ WG", "2015",
                            if_else (Negotiation.phase=="PrepCom","2018", "2021")))

#reorder messages in XX by period (last.Year)
library(forcats)
time.sentimYear$index <- factor(time.sentimYear$index)
time.sentimYear$last.Year <- as.numeric(time.sentimYear$last.Year)
time.sentimYear$index <- forcats::fct_reorder(time.sentimYear$index, time.sentimYear$last.Year, min)

#plot
ggplot(time.sentimYear, aes(index, sentiment, fill = Negotiation.phase)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Negotiation.phase, ncol = 1)+ #scales = "free_x"
  theme_classic ()+
  theme (strip.background = element_blank(), strip.placement = "outside")+
  geom_hline(yintercept=0, color = "gray")+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank())+ #remove x axis ticks
  labs(x = "Key messages ordered chronologically", y = "Sentiment score (bing)")

write.csv(time.sentimYear, here("Output_Res","time.sentimYear_bing.csv"))

#' AFFIN lexicon
time.sentim2 <- data_3 %>%
  inner_join(get_sentiments("afinn"), by = c("Message" = "word")) %>%
  count(Negotiation.phase, index= msg.number, wt=value) %>%
  rename(sentiment = n)

time.sentimYear2 <- time.sentim2 %>%
  mutate(last.Year= if_else(Negotiation.phase=="BBNJ WG", "2015",
                            if_else (Negotiation.phase=="PrepCom","2018", "2021")))

#reorder messages in XX by period (last.Year)
time.sentimYear2$index <- factor(time.sentimYear2$index)
time.sentimYear2$last.Year <- as.numeric(time.sentimYear2$last.Year)
time.sentimYear2$index <- forcats::fct_reorder(time.sentimYear2$index, time.sentimYear2$last.Year, min)

#plot
ggplot(time.sentimYear2, aes(index, sentiment, fill = Negotiation.phase)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Negotiation.phase, ncol = 1)+ #scales = "free_x"
  theme_classic ()+
  theme (strip.background = element_blank(), strip.placement = "outside")+
  geom_hline(yintercept=0, color = "gray")+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank())+ #remove x axis ticks
  labs(x = "Key messages ordered chronologically", y = "Sentiment score (AFFIN)")

write.csv(time.sentimYear2, here("Output_Res","time.sentimYear2_AFFIN.csv"))

#'positive vs negative
library(reshape2)
library(wordcloud)
data_3 %>%
  group_by(Negotiation.phase)%>%
  inner_join(get_sentiments("bing"),by = c("Message" = "word")) %>%
  count(Message, sentiment, sort = TRUE) %>%
  acast(Message ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 40)

#'positive vs negative in all messages together
data_1%>%
  inner_join(get_sentiments("bing"),by = c("Message" = "word")) %>%
  count(Message, sentiment, sort = TRUE) %>%
  acast(Message ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 40)

#'Turn the R script into markdown output: Cmd + Shift + K
#'