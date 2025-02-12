#' ## Linguistic analysis - part B
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

#'### 1. Linguistic analysis - part B
#'#### 1.1 frequency comparison across groups SWOT
summary(as.factor(data_1$SWOT))

frequency_1 <- data_1 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT, Message) %>%
  group_by(SWOT) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT, values_from = proportion) %>%
  relocate(any_of(c("Message","S", "W", "O", "T"))) %>%
  pivot_longer(`W`:`T`,
               names_to = "SWOT", values_to = "proportion")

head(frequency_1)

frequency_1b <- data_1 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT, Message) %>%
  group_by(SWOT) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT, values_from = proportion) %>%
  relocate(any_of(c("Message", "W", "O", "T","S"))) %>%
  pivot_longer(`O`:`S`,
               names_to = "SWOT", values_to = "proportion")

frequency_1c <- data_1 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT, Message) %>%
  group_by(SWOT) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT, values_from = proportion) %>%
  relocate(any_of(c("Message","O","T","S","W"))) %>%
  pivot_longer(`T`:`W`,
               names_to = "SWOT", values_to = "proportion")

#'visualise it
ggplot(frequency_1, aes(x = proportion, y = `S`, 
                        color = abs(`S` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~SWOT, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "S", x = NULL)

ggplot(frequency_1b, aes(x = proportion, y = `W`, 
                        color = abs(`W` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~SWOT, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "W", x = NULL)

ggplot(frequency_1c, aes(x = proportion, y = `O`, 
                         color = abs(`O` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~SWOT, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "O", x = NULL)

#' correlation test
rSW<- cor.test(data = frequency_1[frequency_1$SWOT == "W",],
         ~ proportion + `S`)
rSW<-tidy(rSW)

rSO<-cor.test(data = frequency_1[frequency_1$SWOT == "O",],
         ~ proportion + `S`)
rSO<-tidy(rSO)

rST<-cor.test(data = frequency_1[frequency_1$SWOT == "T",],
         ~ proportion + `S`)
rST<-tidy(rST)

rWO<-cor.test(data = frequency_1b[frequency_1b$SWOT == "O",],
         ~ proportion + `W`)
rWO<-tidy(rWO)

rWT<-cor.test(data = frequency_1b[frequency_1b$SWOT == "T",],
              ~ proportion + `W`)
rWT<-tidy(rWT)

rOT<-cor.test(data = frequency_1c[frequency_1b$SWOT == "T",],
              ~ proportion + `O`)
rOT<-tidy(rOT)

corr.SWOT<-rbind(rSW,rSO,rST,rWO,rWT,rOT) # bind lists
corr.SWOT <- corr.SWOT %>%
  add_column(SWOT.levels=c("S*W","S*O","S*T","W*O","W*T","O*T"), .before ="estimate")

corr.SWOT <- corr.SWOT %>% mutate(significant=case_when(p.value <= 0.05 ~ "signif",
                             p.value > 0.05 ~ "non-signif"),
       .before ="parameter")

corr.SWOT

#'#### 1.2 frequency comparison across groups Categories
summary(as.factor(data_2$Category))

frequency_2 <- data_2 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(Category, Message) %>%
  group_by(Category) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Category, values_from = proportion) %>%
  pivot_longer(`II`:`VI`,
               names_to = "Category", values_to = "proportion")

head(frequency_2)

frequency_2b <- data_2 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(Category, Message) %>%
  group_by(Category) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Category, values_from = proportion) %>%
  relocate(any_of(c("Message","II","III","IV","V","VI","I"))) %>%
  pivot_longer(`III`:`I`,
               names_to = "Category", values_to = "proportion")

frequency_2c <- data_2 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(Category, Message) %>%
  group_by(Category) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Category, values_from = proportion) %>%
  relocate(any_of(c("Message","III","IV","V","VI","I","II"))) %>%
  pivot_longer(`IV`:`II`,
               names_to = "Category", values_to = "proportion")

frequency_2d <- data_2 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(Category, Message) %>%
  group_by(Category) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Category, values_from = proportion) %>%
  relocate(any_of(c("Message","IV","V","VI","I","II","III"))) %>%
  pivot_longer(`V`:`III`,
               names_to = "Category", values_to = "proportion")

frequency_2e <- data_2 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(Category, Message) %>%
  group_by(Category) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Category, values_from = proportion) %>%
  relocate(any_of(c("Message","V","VI","I","II","III","IV"))) %>%
  pivot_longer(`VI`:`IV`,
               names_to = "Category", values_to = "proportion")

#'visualise it
ggplot(frequency_2, aes(x = proportion, y = `I`, 
                        color = abs(`I` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format(accuracy = 0.01)) +
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Category, ncol = 5) +
  theme(legend.position="none") +
  labs(y = "I", x = NULL)

ggplot(frequency_2b, aes(x = proportion, y = `II`, 
                        color = abs(`II` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format(accuracy = 0.01)) +
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Category, ncol = 5) +
  theme(legend.position="none") +
  labs(y = "II", x = NULL)

ggplot(frequency_2c, aes(x = proportion, y = `III`, 
                         color = abs(`III` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format(accuracy = 0.01)) +
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Category, ncol = 5) +
  theme(legend.position="none") +
  labs(y = "III", x = NULL)

ggplot(frequency_2d, aes(x = proportion, y = `IV`, 
                         color = abs(`IV` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Category, ncol = 5) +
  theme(legend.position="none") +
  labs(y = "IV", x = NULL)

ggplot(frequency_2e, aes(x = proportion, y = `V`, 
                         color = abs(`V` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Category, ncol = 5) +
  theme(legend.position="none") +
  labs(y = "V", x = NULL)

#' correlation test
rI.II<- tidy(cor.test(data = frequency_2[frequency_2$Category == "II",],
                      ~ proportion + `I`))

rI.III<-tidy(cor.test(data = frequency_2[frequency_2$Category == "III",],
                 ~ proportion + `I`))

rI.IV<-tidy(cor.test(data = frequency_2[frequency_2$Category == "IV",],
                ~ proportion + `I`))

rI.V<-tidy(cor.test(data = frequency_2[frequency_2$Category == "V",],
                ~ proportion + `I`))

rI.VI<-tidy(cor.test(data = frequency_2[frequency_2$Category == "VI",],
               ~ proportion + `I`))

rII.III<-tidy(cor.test(data = frequency_2b[frequency_2b$Category == "III",],
                      ~ proportion + `II`))

rII.IV<-tidy(cor.test(data = frequency_2b[frequency_2b$Category == "IV",],
                     ~ proportion + `II`))

rII.V<-tidy(cor.test(data = frequency_2b[frequency_2b$Category == "V",],
                    ~ proportion + `II`))

rII.VI<-tidy(cor.test(data = frequency_2b[frequency_2b$Category == "VI",],
                     ~ proportion + `II`))

rIII.IV<-tidy(cor.test(data = frequency_2c[frequency_2c$Category == "IV",],
                      ~ proportion + `III`))

rIII.V<-tidy(cor.test(data = frequency_2c[frequency_2c$Category == "V",],
                     ~ proportion + `III`))

rIII.VI<-tidy(cor.test(data = frequency_2c[frequency_2c$Category == "VI",],
                      ~ proportion + `III`))

rIV.V<-tidy(cor.test(data = frequency_2d[frequency_2d$Category == "V",],
                      ~ proportion + `IV`))

rIV.VI<-tidy(cor.test(data = frequency_2d[frequency_2d$Category == "VI",],
                       ~ proportion + `IV`))

rV.VI<-tidy(cor.test(data = frequency_2e[frequency_2e$Category == "VI",],
                      ~ proportion + `V`))

corr.Cats<-rbind(rI.II,rI.III,rI.IV,rI.V,rI.VI,
                 rII.III,rII.IV,rII.V,rII.VI,
                 rIII.IV,rIII.V,rIII.VI,
                 rIV.V,rIV.VI,
                 rV.VI)# bind lists

corr.Cats <- corr.Cats %>%
  add_column(Cats.levels=c("I*II","I*III","I*IV","I*V","I*VI",
                           "II*III","II*IV","II*V","II*VI",
                           "III*IV","III*V","III*VI",
                           "IV*V","IV*VI",
                           "V*VI"), .before ="estimate")

corr.Cats <- corr.Cats %>% mutate(significant=case_when(p.value <= 0.05 ~ "signif",
                                                            p.value > 0.05 ~ "non-signif"),
                                             .before ="parameter")

corr.Cats 

#LEFT HERE
#'#### 1.3 frequency comparison across groups interaction SWOT*Categories
frequency_3 <- data_3 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT.Cats, Message) %>%
  group_by(SWOT.Cats) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT.Cats, values_from = proportion) %>%
  pivot_longer(`O II`:`W VI`,
               names_to = "SWOT.Cats", values_to = "proportion")

head(frequency_3)

#'visualise it
ggplot(frequency_3, aes(x = proportion, y = `O I`, 
                      color = abs(`O I` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.1)) +
  scale_y_log10(labels = percent_format(accuracy = 0.1)) +
  scale_color_gradient(limits = c(0, 0.003), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~SWOT.Cats, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "O I", x = NULL)

#' correlation test
Correl_SWOT.Cats <- data_3 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT.Cats, Message) %>%
  group_by(SWOT.Cats) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT.Cats, values_from = proportion)

Correl_SWOT.Cats <- Correl_SWOT.Cats %>% 
  select(-Message) %>%
  unlist(use.names = TRUE) #unlist for list to matrix

Correl_SWOT.Cats_mtx <- matrix(Correl_SWOT.Cats, ncol = 22, nrow = 1555,
                           dimnames = list( c(1:1555),
                                            c("O I","O II","O III","O IV","O V","O VI",
                                             "S I","S II","S III","S VI",
                                             "T I","T II","T III","T IV","T V","T VI",
                                             "W I","W II","W III","W IV","W V","W VI"))) # to matrix

library(Hmisc)
Correl.res<- rcorr(Correl_SWOT.Cats_mtx, type="pearson")
# signif(Correl.res$r, 2)
# signif(Correl.res$P, 2)
# signif(Correl.res$n, 2)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Table of Pearson correlations and significance values between interaction SWOT*Cats levels
Correl.resTAB<-flattenCorrMatrix(Correl.res$r, Correl.res$P) 

Correl.resTAB <- Correl.resTAB %>%
  mutate(significant=case_when(p <= 0.05 ~ "signif",
                               p > 0.05 ~ "non-signif"))

Correl.resTAB

#' prepare table for printing
Correl.table <- Correl.resTAB %>%
  select(-significant)%>% #delete variable
  arrange(desc(cor))%>% #order from higher to lower r
  filter(p<=0.05) # keep only significant interactions

library(kableExtra)
kbl(Correl.table, 
    col.names=c("levels"," ","r","p"), align = "l", digits =c(2,3),
    caption = "Pearson correlations for interaction between factors SWOT and Categories.") %>% 
  kable_paper("hover", full_width = F)%>%
  add_header_above(c("Pairwise comparison" = 2, "Pearson rho" = 1, "P-value" = 1), align = "l")%>%
  footnote(general = "Only significant interactions  (p-value <=0.05) are shown.")

#HERE below

#'visualise specific interactions
#' SII * TII
frequency_3b <- data_3 %>% 
mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT.Cats, Message) %>%
  group_by(SWOT.Cats) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT.Cats, values_from = proportion) %>%
  relocate(any_of(c("Message","S II","O I","O II","O III","O IV","O V","O VI",
                    "S I","S III","S VI",
                    "T I","T II","T III","T IV","T V","T VI",
                    "W I","W II","W III","W IV","W V","W VI")))%>%
  pivot_longer(`O I`:`W VI`,
               names_to = "SWOT.Cats", values_to = "proportion")

#' visualise scatterplot 
ggplot(subset(frequency_3b, SWOT.Cats=="T II"), aes(x = `proportion`, y = `S II`),
       color = abs(`S II` - proportion)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  # scale_color_gradient(limits = c(0, 0.01), 
  #                      low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "S II", x = "T II")

#' correlation test SII * TII
cor.test(data = frequency_3b[frequency_3b$SWOT.Cats == "T II",],
         ~ proportion + `S II`)


#' WV * OV
frequency_3c <- data_3 %>% 
mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT.Cats, Message) %>%
  group_by(SWOT.Cats) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT.Cats, values_from = proportion) %>%
  relocate(any_of(c("Message","W V","O I","O II","O III","O IV","O V","O VI",
                    "S I","S II","S III","S VI",
                    "T I","T II","T III","T IV","T V","T VI",
                    "W I","W II","W III","W IV","W VI")))%>%
  pivot_longer(`O I`:`W VI`,
               names_to = "SWOT.Cats", values_to = "proportion")

#' visualise scatterplot 
ggplot(subset(frequency_3c, SWOT.Cats=="O V"), aes(x = `proportion`, y = `W V`),
       color = abs(`W V` - proportion)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  # scale_color_gradient(limits = c(0, 0.003), 
  #                      low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "W V", x = "O V")

#' correlation test WV * OV
cor.test(data = frequency_3c[frequency_3c$SWOT.Cats == "O V",],
         ~ proportion + `W V`)
check.OV<- subset(frequency_3c, SWOT.Cats=="O V") #no common words

#' WI * TI
frequency_3d <- data_3 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT.Cats, Message) %>%
  group_by(SWOT.Cats) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT.Cats, values_from = proportion) %>%
  relocate(any_of(c("Message","W I","O I","O II","O III","O IV","O V","O VI",
                    "S I","S II","S III","S VI",
                    "T I","T II","T III","T IV","T V","T VI",
                    "W II","W III","W IV","W V","W VI")))%>%
  pivot_longer(`O I`:`W VI`,
               names_to = "SWOT.Cats", values_to = "proportion")

#' visualise scatterplot 
ggplot(subset(frequency_3d, SWOT.Cats=="T I"), aes(x = `proportion`, y = `W I`),
       color = abs(`W I` - proportion)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  # scale_color_gradient(limits = c(0, 0.001), 
  #                      low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "W I", x = "T I")

#' correlation test W I * T I
cor.test(data = frequency_3d[frequency_3d$SWOT.Cats == "T I",],
         ~ proportion + `W I`)

check.TI<- subset(frequency_3d, SWOT.Cats=="T I")

#' WIII * OIII
frequency_3e <- data_3 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(SWOT.Cats, Message) %>%
  group_by(SWOT.Cats) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = SWOT.Cats, values_from = proportion) %>%
  relocate(any_of(c("Message","W III","O I","O II","O III","O IV","O V","O VI",
                    "S I","S II","S III","S VI",
                    "T I","T II","T III","T IV","T V","T VI",
                    "W I","W II","W IV","W V","W VI")))%>%
  pivot_longer(`O I`:`W VI`,
               names_to = "SWOT.Cats", values_to = "proportion")

#' visualise scatterplot 
ggplot(subset(frequency_3e, SWOT.Cats=="O III"), aes(x = `proportion`, y = `W III`),
       color = abs(`W III` - proportion)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_log10(labels = percent_format()) +
  #scale_color_gradient(limits = c(0, 0.001), 
                       #low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "W III", x = "O III")

#' correlation test W III * O III
cor.test(data = frequency_3e[frequency_3e$SWOT.Cats == "O III",],
         ~ proportion + `W III`)

check.OIII<- subset(frequency_3e, SWOT.Cats=="O III")

#'#### 1.4 frequency comparison across groups in time (Negotiation.phase)
frequency_4 <- data_3 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(Negotiation.phase, Message) %>%
  group_by(Negotiation.phase) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Negotiation.phase, values_from = proportion) %>%
  pivot_longer(`PrepCom`:`IGC`,
               names_to = "Negotiation.phase", values_to = "proportion")

head(frequency_4)

#'visualise it
ggplot(frequency_4, aes(x = proportion, y = `BBNJ WG`, 
                      color = abs(`BBNJ WG` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Negotiation.phase, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "BBNJ WG", x = NULL)

#' correlation test
cor.test(data = frequency_4[frequency_4$Negotiation.phase == "IGC",],
~ proportion + `BBNJ WG`)

cor.test(data = frequency_4[frequency_4$Negotiation.phase == "PrepCom",], 
         ~ proportion + `BBNJ WG`)

#'#### 1.5 frequency comparison across groups of SWOT in time (Negotiation.phase)

data_4 <- data_1 %>% mutate(periodSWOT = paste0(Negotiation.phase,SWOT),
                                  .before ="msg.number")

data_4$periodSWOT<- factor(data_4$periodSWOT, levels=c("BBNJ WGS","BBNJ WGW","BBNJ WGO","BBNJ WGT",
                                               "PrepComS","PrepComW","PrepComO","PrepComT",
                                               "IGCS","IGCW","IGCO","IGCT"))

frequency_5 <- data_4 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(periodSWOT, Message) %>%
  group_by(periodSWOT) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = periodSWOT, values_from = proportion) %>%
  pivot_longer(`BBNJ WGW`:`IGCT`,
               names_to = "periodSWOT", values_to = "proportion")

head(frequency_5)

frequency_5b <- data_4 %>% 
  mutate(Message = str_extract(Message, "[a-z']+")) %>%
  count(periodSWOT, Message) %>%
  group_by(periodSWOT) %>%
  mutate(proportion = n / sum(n)) %>% #proportion within each of the levels grouped by in previous step
  select(-n) %>% 
  pivot_wider(names_from = periodSWOT, values_from = proportion) %>% # as matrix
  relocate(any_of(c("Message","BBNJ WGT","IGCT","BBNJ WGS","BBNJ WGW","BBNJ WGO",
                    "PrepComS","PrepComW","PrepComO","PrepComT",
                    "IGCS","IGCW","IGCO")))%>%
  pivot_longer(`IGCT`:`IGCO`,
               names_to = "periodSWOT", values_to = "proportion") # table prepared for corr and plotting

#'visualise it
ggplot(frequency_5, aes(x = proportion, y = `BBNJ WGS`, 
                        color = abs(`BBNJ WGS` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.03), 
                     low = "darkslategray4", high = "gray75") +
  facet_wrap(~periodSWOT, ncol = 4) +
  theme(legend.position="none") +
  labs(y = "BBNJ WGS", x = NULL)

#'visualise threats * 2 periods SWOT
ggplot(subset(frequency_5b, periodSWOT=="IGCT"), aes(x = `proportion`, y = `BBNJ WGT`),
       color = abs(`BBNJ WGT` - proportion)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Message), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(),limits = c(0.0001, 0.055)) +
  scale_y_log10(labels = percent_format(),limits = c(0.0001, 0.055)) +
  scale_color_gradient(limits = c(0, 0.02),low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "BBNJ WGT", x = "IGCT")

#' correlation test threats * 2 periods SWOT
cor.test(data = frequency_5b[frequency_5b$periodSWOT == "IGCT",],
         ~ proportion + `BBNJ WGT`)

frequency_5b_IGCT <- subset(frequency_5b, periodSWOT=="IGCT")

#'Turn the R script into markdown output: Cmd + Shift + K