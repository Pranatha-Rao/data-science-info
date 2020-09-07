##Calling the libraries tibble, dplyr and ggplot2
library(tibble)
library(dplyr)
library(ggplot2)

#install package udpipe
install.packages('udpipe')
#get the working directory
getwd()
#Reading the text from hindi.txt and storing it in h
h <- readLines("hindi.txt", n=1, encoding="UTF-8")

library(udpipe)
#download udpipe for hindi language
model <- udpipe_download_model(language = "hindi")
udmodel_hindi <- udpipe_load_model(file = 'hindi-hdtb-ud-2.4-190531.udpipe')

#annotate h with udmodel_hindi
s <- udpipe_annotate(udmodel_hindi, h)
x <- data.frame(s)

## To plot Part-of-speech tags from the given text,lattice package is used
library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

##Displays most common nouns in the hindi.txt
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

##Displays most occurring adjectives
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

##Displays verbs
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")

## RAKE algorithm
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

## Plotting the word cloud
library(wordcloud)
wordcloud(words = stats$key, freq = stats$freq, min.freq = 1, max.words = 100,
          random.order = FALSE, scale = c(4, .2), colors = brewer.pal(6, "Dark2"))


