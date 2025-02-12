#' ## Linguistic analysis - part A
#'  author: Heliana Teixeira
#'  date: 28-07-2021; modified 26-04-2002
#'  code credits to UDPipe C++ library and models provided by Milan Straka, 
#'  https://github.com/ufal/udpipe
#'  
#'  Using BBNJ dataset from Mariana Caldeira MSc Thesis. 
#'  Considering key Messages within SWOT levels (#4), through time (Negotiation.phase #3).
#'  This script applies keyword extraction techniques to analyse plain text.


#'###  load packages 
library(here)
#' linguistic analysis:
library(udpipe)
library(textrank)
#' visualization:
library(lattice) 
library(wordcloud)
library(igraph)
library(ggraph)
library(ggplot2)
library(tidyverse)

#'### load data
here::here()
Fname.dat <- (here("Data", "SWOTcatsMessageTime.csv")) #edit file name
data <- read.csv(file = Fname.dat, header = TRUE, sep = ",", dec =".") 

#' ### pre-treatment
#' convert to factor some fields of interest for analysis
data$SWOT.Cats<- as.factor(data$SWOT.Cats)
data$SWOT <- as.factor(data$SWOT)
data$Category <- as.factor(data$Category)
data$Negotiation.phase <- as.factor(data$Negotiation.phase)

#' generate datasets for factors to be analysed
#' Category
summary(data$Category) #check no. messages in each subset

dataCatsI <- data %>% filter(Category=="I")
dataCatsII <- data %>% filter(Category=="II")
dataCatsIII <- data %>% filter(Category=="III")
dataCatsIV <- data %>% filter(Category=="IV")
dataCatsV <- data %>% filter(Category=="V")
dataCatsVI <- data %>% filter(Category=="VI")

#' Negotiation Phase
summary(data$Negotiation.phase) #check no. messages in each subset

dataBBNJWG <- data %>% filter(Negotiation.phase=="BBNJ WG")
dataPrepCom <- data %>% filter(Negotiation.phase=="PrepCom")
dataIGC <- data %>% filter(Negotiation.phase=="IGC")

#' ### 1. Linguistic analysis
#' ignore - for usage out of function:
# ud_model <- udpipe_download_model(language = "english")
# ud_model <- udpipe_load_model(ud_model$file_model)
# x <- udpipe_annotate(ud_model, x = data$Message) #' Select field for analysis
# x <- as.data.frame(x)

#' download udpipe EN version file 
UDP_file <- udpipe_download_model(language = "english") #file

#' define new udpipe function
udpipe_for_df <- function(x) {
  file <- UDP_file
  model <- udpipe_load_model(file)
  x <- udpipe_annotate(model, x = x$Message) # x=as.character(x$Message) or x = x["Message"] in loops / lapply to be tested
  as.data.frame(x, detailed = TRUE)
} 

#' apply function to Cats subsets
resI<- udpipe_for_df(dataCatsI)
resII<- udpipe_for_df(dataCatsII)
resIII<- udpipe_for_df(dataCatsIII)
resIV<- udpipe_for_df(dataCatsIV)
resV<- udpipe_for_df(dataCatsV)
resVI<- udpipe_for_df(dataCatsVI)

#' apply function to timeline subsets
resBBNJWG<- udpipe_for_df(dataBBNJWG)
resPrepCom<- udpipe_for_df(dataPrepCom)
resIGC<- udpipe_for_df(dataIGC)

#' save files from resulting tables from udpipe function treatment (only Periods needed)
write.csv(resBBNJWG, here("./Data/resBBNJWG.csv"), row.names = FALSE)  # edit correct dataset name
write.csv(resPrepCom, here("./Data/resPrepCom.csv"), row.names = FALSE) 
write.csv(resIGC, here("./Data/resIGC.csv"), row.names = FALSE) 

#' remove stop words (NOUN): agreement and BBNJ (categories analysis)
resI<- resI %>% filter(token!="agreement" & token!="BBNJ")
resII<- resII %>% filter(token!="agreement" & token!="BBNJ")
resIII<- resIII %>% filter(token!="agreement" & token!="BBNJ")
resIV<- resIV %>% filter(token!="agreement" & token!="BBNJ")
resV<- resV %>% filter(token!="agreement" & token!="BBNJ")
resVI<- resVI %>% filter(token!="agreement" & token!="BBNJ")

df_Period <- dplyr::bind_rows(list(resBBNJWG, resPrepCom, resIGC), .id = 'Period')
summary(as.factor(df_Period$Period)) #verify

write.csv(df_Period, here("./Data/df_Period.csv"), row.names = FALSE) 

#'### load edited data for Period analysis
Fname.dat2 <- (here("Data", "df_Period_e.csv")) #edit file name
df_Period_e <- read.csv(file = Fname.dat2, header = TRUE, sep = ";", dec =".") 
#'note: this doc was edited outside R - see table of edited stepd NOUN to PROPN file

#' Remove stop words using edited "lemma" variable:
df_Period_e_clean<- df_Period_e %>% filter(lemma!= "agreement" & 
                                             lemma!= "ABNJ" & 
                                             lemma!= "BBNJ" & 
                                             lemma!= "ABMT" & 
                                             lemma!= "MPA" &
                                             lemma!= "marine" & 
                                             lemma!= "ocean" & 
                                             lemma!= "scientific")

#'### Option 1a: Extracting only nouns
#' Define new stats function
statsUDP <- function(x) {
  stats <- subset(x, upos %in% "NOUN")
  stats <- txt_freq(x = stats$lemma) # x=as.character(x$Message) or x = x["Message"] in loops / lapply
} 
#' ignore - for usage out of function:
# stats <- subset(x, upos %in% "NOUN")
# stats <- txt_freq(x = stats$lemma)

#' Top 30 NOUNS in each Category
statsUDP.I <- statsUDP(resI)
statsUDP.I$key <- factor(statsUDP.I$key, levels = rev(statsUDP.I$key))
barchart(key ~ freq, data = head(statsUDP.I, 30), col = "cadetblue", main = "Most occurring nouns CAT I", xlab = "Freq")

statsUDP.II <- statsUDP(resII)
statsUDP.II$key <- factor(statsUDP.II$key, levels = rev(statsUDP.II$key))
barchart(key ~ freq, data = head(statsUDP.II, 30), col = "cadetblue", main = "Most occurring nouns CAT II", xlab = "Freq")

statsUDP.III <- statsUDP(resIII)
statsUDP.III$key <- factor(statsUDP.III$key, levels = rev(statsUDP.III$key))
barchart(key ~ freq, data = head(statsUDP.III, 30), col = "cadetblue", main = "Most occurring nouns CAT III", xlab = "Freq")

statsUDP.IV <- statsUDP(resIV)
statsUDP.IV$key <- factor(statsUDP.IV$key, levels = rev(statsUDP.IV$key))
barchart(key ~ freq, data = head(statsUDP.IV, 30), col = "cadetblue", main = "Most occurring nouns CAT IV", xlab = "Freq")

statsUDP.V <- statsUDP(resV)
statsUDP.V$key <- factor(statsUDP.V$key, levels = rev(statsUDP.V$key))
barchart(key ~ freq, data = head(statsUDP.V, 30), col = "cadetblue", main = "Most occurring nouns CAT V", xlab = "Freq")

statsUDP.VI <- statsUDP(resVI)
statsUDP.VI$key <- factor(statsUDP.VI$key, levels = rev(statsUDP.VI$key))
barchart(key ~ freq, data = head(statsUDP.VI, 30), col = "cadetblue", main = "Most occurring nouns CAT VI", xlab = "Freq")

#'### Option 1b: Extracting only nouns & proper nouns
#' Define new stats function
statsUDP_npn <- function(x) {
  stats <- subset(x, upos %in% c("NOUN","PROPN"))
  stats <- txt_freq(x = stats$lemma) # x=as.character(x$Message) or x = x["Message"] in loops / lapply
}

#' Split cleaned /edited data per period
resBBNJWG_e <- df_Period_e_clean %>% filter(Period==1)
resPrepCom_e <- df_Period_e_clean %>% filter(Period==2)
resIGC_e <- df_Period_e_clean %>% filter(Period==3)

#' Top 20 NOUNS or PROPN in each Negotiation phase 
statsUDP.BBNJWG <- statsUDP_npn(resBBNJWG_e)
statsUDP.BBNJWG$key <- factor(statsUDP.BBNJWG$key, levels = rev(statsUDP.BBNJWG$key))
barchart(key ~ freq, data = head(statsUDP.BBNJWG, 20), xlim= c(0, 60), col = "cadetblue", main = "Most occurring nouns BBNJ WG phase", xlab = "Frequency")

statsUDP.PrepCom <- statsUDP_npn(resPrepCom_e)
statsUDP.PrepCom$key <- factor(statsUDP.PrepCom$key, levels = rev(statsUDP.PrepCom$key))
barchart(key ~ freq, data = head(statsUDP.PrepCom, 20), xlim= c(0, 60), col = "cadetblue", main = "Most occurring nouns Prep. Com. phase", xlab = "Frequency")

statsUDP.IGC <- statsUDP_npn(resIGC_e)
statsUDP.IGC$key <- factor(statsUDP.IGC$key, levels = rev(statsUDP.IGC$key))
barchart(key ~ freq, data = head(statsUDP.IGC, 20), xlim= c(0, 60), col = "cadetblue", main = "Most occurring nouns IGC phase", xlab = "Frequency")

#' ignore - for usage out of function:
#library(lattice) 
# stats$key <- factor(stats$key, levels = rev(stats$key))
# barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring nouns", xlab = "Freq")

#' ##Below this point NOT DONE YET:
# not done----
#' #'### Option 2: Collocation & co-occurrences
#' #'   Collocation (words following one another)
#' colloc <- keywords_collocation(x = x,
#'                                term = "token", group = c("doc_id","paragraph_id","sentence_id"),
#'                                ngram_max = 4, n_min = 15)
#' head(colloc)
#' 
#' #'  Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
#' Coocc1 <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")),
#'                       term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
#' head(Coocc1)
#' #'  Co-occurrences: How frequent do words follow one another
#' Coocc2 <- cooccurrence(x = x$lemma,
#'                       relevant = x$upos %in% c("NOUN", "ADJ"))
#' head(Coocc2)
#' #'  Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
#' Coocc3 <- cooccurrence(x = x$lemma,
#'                       relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
#' head(Coocc3)
#' 
#' #' Visualisation of co-occurrences using a network plot for the top 30 most frequent co-occurring nouns and adjectives.
#' # library(igraph)
#' # library(ggraph)
#' # library(ggplot2)
#' wordnetwork <- head(Coocc3, 30)
#' wordnetwork <- graph_from_data_frame(wordnetwork)
#' ggraph(wordnetwork, layout = "fr") +
#'   geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "lightblue") +
#'   geom_node_text(aes(label = name), col = "dodgerblue4", size = 4) +
#'   theme_graph(base_family = "Arial Narrow") +
#'   theme(legend.position = "none") +
#'   labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjectives")
#' 
#' #'### Option 3: Textrank (word network ordered by Google Pagerank)
#' textrank1 <- textrank_keywords(x$lemma, 
#'                            relevant = x$upos %in% c("NOUN", "ADJ"), 
#'                            ngram_max = 8, sep = " ")
#' textrank2 <- subset(textrank1$keywords, ngram > 1 & freq >= 5) #' 5
#' textrank2
#' 
#' #' Visualisation
#' # library(wordcloud)
#' wordcloud(words = textrank2$keyword, freq = textrank2$freq)
#' 
#' #'### Option 4: Rapid Automatic Keyword Extraction: RAKE
#' rake <- keywords_rake(x = x, 
#'                        term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
#'                        relevant = x$upos %in% c("NOUN", "ADJ"),
#'                        ngram_max = 4)
#' head(subset(rake, freq > 3))
#' 
#' #'### Option 5: Phrases
#' #'  Simple noun phrases (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
#' x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
#' phrase1 <- keywords_phrases(x = x$phrase_tag, term = x$token, 
#'                           pattern = "(A|N)+N(P+D*(A|N)*N)*", 
#'                           is_regex = TRUE, ngram_max = 4, detailed = FALSE)
#' head(subset(phrase1, ngram > 2))
#' 
#' #'### Option 6: Use dependency parsing output to get the nominal subject and the adjective of it
#' Depend <- merge(x, x, 
#'                by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
#'                by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
#'                all.x = TRUE, all.y = FALSE, 
#'                suffixes = c("", "_parent"), sort = FALSE)
#' Depend <- subset(Depend, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
#' Depend$term <- paste(Depend$lemma_parent, Depend$lemma, sep = " ")
#' Depend <- txt_freq(Depend$term)
#' 
#' # library(wordcloud)
#' wordcloud(words = Depend$key, freq = Depend$freq, min.freq = 4, max.words = 100,
#'           random.order = FALSE, colors = brewer.pal(6, "Dark2"))
#' 
#' #' save new dataset file
#' write.csv(x, here("./Data/LingPartA.csv"), row.names = FALSE)  # edit correct dataset name
#' 
#'Turn the R script into markdown output: Cmd + Shift + K
