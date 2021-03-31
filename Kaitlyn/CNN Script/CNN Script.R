# Loading necessary packages
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
         
CNN_df <- read_csv(file = 'CNN_tweets.csv')$Text

CNN_df1 <- read.csv(file = "CNN_tweets.csv")[,c(1,3)]

# Load the data as a corpus
CNN_text <- Corpus(VectorSource(CNN_df))

################## Cleaning ##############################################
# Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
CNN_text <- tm_map(CNN_text, toSpace, "/")
CNN_text <- tm_map(CNN_text, toSpace, "@")
CNN_text <- tm_map(CNN_text, toSpace, "\\|")
CNN_text <- tm_map(CNN_text, toSpace, "-")

# Convert the text to lower case
CNN_text <- tm_map(CNN_text, content_transformer(tolower))

# Remove numbers
CNN_text <- tm_map(CNN_text, removeNumbers)

# Remove english common stopwords
CNN_text <- tm_map(CNN_text, removeWords, stopwords("english"))

# Remove punctuations
CNN_text <- tm_map(CNN_text, removePunctuation)

# Eliminate extra white spaces
CNN_text <- tm_map(CNN_text, stripWhitespace)

# Text stemming - which reduces words to their root form
CNN_text <- tm_map(CNN_text, stemDocument)

# specify your custom stopwords as a character vector
CNN_text <- tm_map(CNN_text, removeWords, c("https", "cnn", "tco"))

######################### Analysis ########################################
# Build a term-document matrix
CNN_text_dtm <- TermDocumentMatrix(CNN_text)
dtm_m_CNN <- as.matrix(CNN_text_dtm)

# Sort by descreasing value of frequency
dtm_v_CNN <- sort(rowSums(dtm_m_CNN),decreasing=TRUE)
dtm_d_CNN <- data.frame(word = names(dtm_v_CNN),freq=dtm_v_CNN)

# Display the top 10 most frequent words
head(dtm_d_CNN, 10)

################### Visualizations #####################################
# Plot the most frequent words
barplot(dtm_d_CNN[1:10,]$freq, las = 2, names.arg = dtm_d_CNN[1:10,]$word,
        col ="lightblue", main ="CNN Top 10 Most Frequent Words",
        ylab = "Word Frequencies") 

# Generate word cloud
set.seed(1234)
png("CNN_wordcloud.png", width=1600,height=900)
wordcloud(words = dtm_d_CNN$word, freq = dtm_d_CNN$freq, min.freq = 100,
          scale = c(4,0.5), max.words=150, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

################## Correlations ########################################
# Find associations 
findAssocs(CNN_text_dtm, terms = c("presid","trump","covid",
                                   "new","elect","year",
                                   "biden","state"), corlimit = 0.25)

################## Sentiment Scores ###############################
# regular sentiment score using get_sentiment() function
CNN_syuzhet_vector <- get_sentiment(CNN_df1$Text, method="syuzhet")
# see the first row of the vector
head(CNN_syuzhet_vector)
# see summary statistics of the vector
summary(CNN_syuzhet_vector)

# Cleaning up sentiment data set for plots
CNN_sentiment <- cbind(CNN_df1, CNN_syuzhet_vector)

library(stringr)
dt_split <- str_split_fixed(CNN_sentiment$Datetime, " ", 2)
CNN_sentiment <- cbind(CNN_sentiment, dt_split)

CNN_sentiment <- CNN_sentiment[,-c(1)]

CNN_sentiment <- CNN_sentiment %>% 
  rename(
    Date = "1",
    Time = "2",
    Sentiment = CNN_syuzhet_vector
  )

library(lubridate)
CNN_sentiment$Date <- ymd(CNN_sentiment$Date)
CNN_sentiment$Month <- month(CNN_sentiment$Date, label = TRUE)

# Graphing overall sentiment over time
CNN_sentiment %>% 
  group_by(Date) %>% 
  summarise(avg_sentiment = mean(Sentiment)) %>% 
  ggplot() + geom_line(mapping = aes(x = Date, y = avg_sentiment)) +
  ylim(-0.8,0.8) + xlab("Month") + ylab("Average Sentiment") +
  geom_hline(yintercept = 0) +
  ggtitle("CNN Average Twitter Sentiment over the Height of the 2020 Election Season")

# Graphing the Peak Month of November
CNN_sentiment %>% 
  filter(Month == "Nov") %>% 
  group_by(Date) %>% 
  summarise(avg_sentiment = mean(Sentiment)) %>%
  ggplot() + geom_line(mapping = aes(x = Date, y = avg_sentiment)) +
  geom_hline(yintercept = 0) + ylim(-0.8,0.8) + 
  ylab("Average Sentiment") + ggtitle("CNN Average Twitter Sentiment During November 2020")
# Peak on the Nov 7-8th, Biden was announced President-Elect on Nov 7th

# Graphing the Low Month of January
CNN_sentiment %>% 
  filter(Month == "Jan") %>% 
  group_by(Date) %>% 
  summarise(avg_sentiment = mean(Sentiment)) %>%
  ggplot() + geom_line(mapping = aes(x = Date, y = avg_sentiment)) +
  geom_hline(yintercept = 0) + ylim(-0.8,0.8) + 
  ylab("Average Sentiment") + ggtitle("CNN Average Twitter Sentiment During January 2021")
# Lowest sentiment on Jan 6th, the day of the Capitol attacks

################# Emotion Classification ###############################
# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
CNN_emotion <- get_nrc_sentiment(CNN_df)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (CNN_emotion,10)

anger <- sum(CNN_emotion$anger)
anticipation <- sum(CNN_emotion$anticipation)
disgust <- sum(CNN_emotion$disgust)
fear <- sum(CNN_emotion$fear)
joy <- sum(CNN_emotion$joy)
sadness <- sum(CNN_emotion$sadness)
surprise <- sum(CNN_emotion$surprise)
trust <- sum(CNN_emotion$trust)
negative <- sum(CNN_emotion$negative)
positive <- sum(CNN_emotion$positive)

CNN_sum <- c(anger,anticipation,disgust,fear,joy,sadness,surprise,trust)
Emotions <- c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust")
Emotions <- factor(Emotions)
CNN_emotion_tibble <- tibble(CNN_sum)

row.names(CNN_emotion_tibble) <- t(Emotions)

CNN_emotion_tibble %>% 
  ggplot() + geom_col(mapping = aes(x= reorder(Emotions,CNN_sum),
                                    y=prop.table(CNN_sum),fill=Emotions)) +
  ylab("Percentage of Meaningful Words") + xlab("Emotions") + 
  ggtitle("CNN Twitter Account: Percentage of Meaningful Words by Emotion") + 
  coord_flip()

