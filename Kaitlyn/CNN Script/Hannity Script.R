# Loading necessary packages
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)

bar_plot_top_words <- function(dff, top_number=10)
{
  library(quanteda)
  myCorpus <- corpus(dff)
  DFM <- dfm(myCorpus,tolower=TRUE,
             remove=c(stopwords(),",",".","-","\"","'","(",")",";",":","!","$","?",""))
  
  textFreq <- textstat_frequency(DFM)
  top_word <- textFreq[1:top_number,]
  barplot(height=top_word$frequency,
          names.arg=top_word$feature,
          main="Top 10 Words")
}

word_cloud2 <- function(dff, min_frequency=100)
{
  library(quanteda)
  myCorpus <- corpus(dff)
  DFM <- dfm(myCorpus,tolower=TRUE,
             remove=c(stopwords(),",",".","-","\"","'","(",")",";",":","!","$","?",""))
  DFM1 <- DFM %>%
    dfm_trim(min_termfreq = min_frequency)
  
  # basic wordcloud
  textplot_wordcloud(DFM1)
}


df2 <- read_csv(file = 'seanhannity_tweets.csv')$Text
df3 <- read_csv(file = 'Reuters_tweets.csv')$Text


top_words(df3)


