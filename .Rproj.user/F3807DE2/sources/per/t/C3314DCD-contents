#3b. TidyData - tokens options

#'####other tokenization options include:
#'####"characters", "character_shingles", "ngrams", "skip_ngrams", "sentences", "lines", "paragraphs", "regex", "tweets" (tokenization by word that preserves usernames, hashtags, and URLS ), and "ptb" (Penn Treebank)
#'####ngrams
mesg.ngram <- data %>%
  unnest_tokens(Message, Message, token = "ngrams", n = 2)%>% #set n
  select(Negotiation.phase,msg.number, Message)

#'####explore more common words
mesg.ngram %>%
  count(Message, sort = TRUE)%>%
  head()
#'####remove stop words with fuzzy_join or regex_join
library(fuzzyjoin)
as_tibble(mesg.ngram)

ngram.test1 <- mesg.ngram %>%
  regex_anti_join(stop_words, by=c("Message"="word")) # loose info

ngram.test2 <- mesg.ngram %>%
  fuzzy_anti_join(stop_words, by=c("Message"="word"), match_fun = str_detect) # datpt function but still loose info

ngram.test <- mesg.ngram %>%
  fuzzy_anti_join(stop_words, by=c("Message"="word"), match_fun = str_detect) # define matching pattern boundary("word") - not working!!!


#' ####alternative to remove stop words
#as_tibble(data)
#' function
removeWords <- function(data, stop_words) {
  x <- unlist(strsplit(data$Message, " "))
  paste(x[!x %in% stop_words$word], collapse = " ")
}

#' remove stop words
Message.tidy <- data %>%
  mutate(message.tidy = removeWords(data, stop_words))

#'####ngrams
mesg.ngram.tidy <- Message.tidy %>%
  unnest_tokens(Message, message.tidy, token = "ngrams", n = 2)%>% #set n
  select(Negotiation.phase,msg.number, Message)

#'####sentences
mesg.sentence <- Message.tidy %>%
  mutate(msg.number = row_number())%>% # to keep track of original message number
  unnest_tokens(sentc.msg, Message, token = "sentences") 
#'####explore more common words
mesg.sentence %>%
  count(sentc.msg, sort = TRUE)%>%
  head() #No sense here but check
#' ####remove stop words

