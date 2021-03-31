# Loading necessary packages
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)

df <- read_csv(file = 'Reuters_tweets.csv')$Text

df1 <- read.csv(file = "Reuters_tweets.csv")[,c(1,3)]

# Load the data as a corpus
text <- Corpus(VectorSource(df))

################## Cleaning ##############################################
# Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
text <- tm_map(text, toSpace, "/")
text <- tm_map(text, toSpace, "@")
text <- tm_map(text, toSpace, "\\|")
text <- tm_map(text, toSpace, "-")

# Convert the text to lower case
text <- tm_map(text, content_transformer(tolower))

# Remove numbers
text <- tm_map(text, removeNumbers)

# Remove english common stopwords
text <- tm_map(text, removeWords, stopwords("english"))

# Remove punctuations
text <- tm_map(text, removePunctuation)

# Eliminate extra white spaces
text <- tm_map(text, stripWhitespace)

# Text stemming - which reduces words to their root form
text <- tm_map(text, stemDocument)

# specify your custom stopwords as a character vector
text <- tm_map(text, removeWords, c("https", "tco"))

######################### Analysis ########################################
# Build a term-document matrix
text_dtm <- TermDocumentMatrix(text)
dtm_m <- as.matrix(text_dtm)

# Issue here size too large

# Sort by descreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

# Display the top 10 most frequent words
head(dtm_d, 10)

################### Visualizations #####################################
# Plot the most frequent words
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col ="lightblue", main ="Reuters Top 10 Most Frequent Words",
        ylab = "Word Frequencies") 

# Generate word cloud
set.seed(1234)
png("Reuters_wordcloud.png", width=1600,height=900)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 100,
          scale = c(4,0.5), max.words=150, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

################## Correlations ########################################
# Find associations 
findAssocs(text_dtm, terms = c("trump","biden","elect",
                               "senat","vote","state"), 
           corlimit = 0.25)

################## Sentiment Scores ###############################
# regular sentiment score using get_sentiment() function
syuzhet_vector <- get_sentiment(df1$Text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# Cleaning up sentiment data set for plots
sentiment <- cbind(df1, syuzhet_vector)

library(stringr)
dt_split <- str_split_fixed(sentiment$Datetime, " ", 2)
sentiment <- cbind(sentiment, dt_split)

sentiment <- sentiment[,-c(1)]

sentiment <- sentiment %>% 
  rename(
    Date = "1",
    Time = "2",
    Sentiment = syuzhet_vector
  )

library(lubridate)
sentiment$Date <- ymd(sentiment$Date)
sentiment$Month <- month(sentiment$Date, label = TRUE)

# Graphing overall sentiment over time
sentiment %>% 
  group_by(Date) %>% 
  summarise(avg_sentiment = mean(Sentiment)) %>% 
  ggplot() + geom_line(mapping = aes(x = Date, y = avg_sentiment)) +
  ylim(-0.8,0.8) + xlab("Month") + ylab("Average Sentiment") +
  geom_hline(yintercept = 0) +
  ggtitle("Reuters Average Twitter Sentiment over the Height of the 2020 Election Season")

# Graphing the election month of November
sentiment %>% 
  filter(Month == "Nov") %>% 
  group_by(Date) %>% 
  summarise(avg_sentiment = mean(Sentiment)) %>%
  ggplot() + geom_line(mapping = aes(x = Date, y = avg_sentiment)) +
  geom_hline(yintercept = 0) + ylim(-0.8,0.8) + 
  ylab("Average Sentiment") + ggtitle("Reuters Average Twitter Sentiment During November 2020")
# No major peaks like we saw with CNN

# Graphing the Low Month of January
sentiment %>% 
  filter(Month == "Jan") %>% 
  group_by(Date) %>% 
  summarise(avg_sentiment = mean(Sentiment)) %>%
  ggplot() + geom_line(mapping = aes(x = Date, y = avg_sentiment)) +
  geom_hline(yintercept = 0) + ylim(-0.8,0.8) + 
  ylab("Average Sentiment") + ggtitle("Reuters Average Twitter Sentiment During January 2021")
# generally low, but not as drastically low as CNN on Jan 6th

################# Emotion Classification ###############################
# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
emotiondf <- get_nrc_sentiment(df)

head (emotiondf,10)

anger <- sum(emotiondf$anger)
anticipation <- sum(emotiondf$anticipation)
disgust <- sum(emotiondf$disgust)
fear <- sum(emotiondf$fear)
joy <- sum(emotiondf$joy)
sadness <- sum(emotiondf$sadness)
surprise <- sum(emotiondf$surprise)
trust <- sum(emotiondf$trust)
negative <- sum(emotiondf$negative)
positive <- sum(emotiondf$positive)

sum <- c(anger,anticipation,disgust,fear,joy,sadness,surprise,trust)
Emotions <- c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust")
Emotions <- factor(Emotions)
emotion_tibble <- tibble(sum)

row.names(emotion_tibble) <- t(Emotions)

emotion_tibble %>% 
  ggplot() + geom_col(mapping = aes(x= reorder(Emotions,sum),
                                    y=prop.table(sum),fill=Emotions)) +
  ylab("Percentage of Meaningful Words") + xlab("Emotions") + 
  ggtitle("Reuters Twitter Account: Percentage of Meaningful Words by Emotion") + 
  coord_flip()

