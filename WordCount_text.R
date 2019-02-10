library(ggplot2)
library(tm)
library(tau)
library(plyr)
library(dplyr)
library(readr)
library(RCurl)
library(tidyverse)

a <- 90
b <- 150

stop_words<-T

data <- tm::PlainTextDocument(readr::read_lines(file = "C:/Users/Admin/Desktop/gotss1e3.txt",
                                                progress = interactive()),
                              heading = "KJB", id = basename(tempfile()),
                              language = "en", description = "Report File")


data <- tau::textcnt(
  if(stop_words==T) {tm::removeWords(tm::scan_tokenizer(data), tm::stopwords("SMART"))}
  else {
    tm::scan_tokenizer(data)
  }
  , method = "string", n = 1L, lower = 1L)

data <- plyr::ldply(data, data.frame)
Results<-dplyr::filter(data, data[,2]>a & data[,2]<b)

colnames(Results)<-c("word", "frequency")

names(data)[names(data) == ".id"] <- "words"
names(data)[names(data) == "X..i.."] <- "freq"

#select words for analysis
w <- data %>% filter( str_detect(words, "jon" )|
                        str_detect(words, "eddard")|
                        str_detect(words, "ned")|
                        str_detect(words, "robert")|
                        str_detect(words, "jaime")|
                        str_detect(words, "catelyn")|
                        str_detect(words, "cersei")|
                        str_detect(words, "daenerys")|
                        str_detect(words, "jorah")|
                        str_detect(words, "viserys")|
                        str_detect(words, "sansa")|
                        str_detect(words, "arya")|
                        str_detect(words, "robb")|
                        str_detect(words, "theon")|
                        str_detect(words, "bran")|
                        str_detect(words, "joffrey")|
                        str_detect(words, "sandor")|
                        str_detect(words, "tyrion")|
                        str_detect(words, "maester")|
                        str_detect(words, "jory")|
                        str_detect(words, "rodrik")|
                        str_detect(words, "benjen")|
                        str_detect(words, "illyrio")|
                        str_detect(words, "qotho")|
                        str_detect(words, "ros")|
                        str_detect(words, "septa")|
                        str_detect(words, "will")|
                        str_detect(words, "arryn")|
                        str_detect(words, "waymar")|
                        str_detect(words, "gared")|
                        str_detect(words, "rickon")|
                        str_detect(words, "tommen")|
                        str_detect(words, "myrcella")|
                        str_detect(words, "hodor")|
                        str_detect(words, "pentoshi")|
                        str_detect(words, "walker")|
                        str_detect(words, "wilding")|
                        str_detect(words, "khal")|
                        str_detect(words, "tommy")|
                        str_detect(words, "mikken")
                        )

#Hilite max value
w$hilte <- with(w, ifelse(freq == max(freq), T, F))


ggplot(w, aes(reorder(words, freq), y = freq, fill = hilte))+
  geom_bar(stat = "identity")+
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = c("gray", "cornflowerblue"))+
  coord_flip()+
  labs(x = "NAME", y = "FREQUENCY")
