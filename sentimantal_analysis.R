# Sentimental analysis and wordcloud from twitter data
#load data
apple <- read.csv(file.choose(),header = T)
str(apple)

#build corpus
install.packages("tm")
library(tm) #for text mining
corpus <- iconv(apple$text)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#clean text
#change text to lower case
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

#remove the most common words in english
cleanset <- tm_map(corpus,removeWords,stopwords("english"))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*' , "", x)
cleanset <- tm_map(cleanset,content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,removeWords,c("aapl","apple"))
cleanset <- tm_map(cleanset,gsub,pattern="stocks",
                   replacement="stock")

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#converting data into rows and columns
#term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

#bar plot 
w <- rowSums(tdm)
w <- subset(w,w>=25)
barplot(w,las=2,col = rainbow(50))

#word cloud
install.packages("wordcloud")
library(wordcloud)
w <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8,'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.7)
#Sentiment analysis 
install.packages("syuzhet")
library(syuzhet)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("reshape2")
library(reshape2)
install.packages("dplyr")
library(dplyr)

tweets <- iconv(apple$text)
#obtain sentimental scores 
s <- get_nrc_sentiment(tweets)
head(s)

#bar plot 
barplot(colSums(s),
        las=2,
        col = rainbow(10),
        ylab = "count",
        main = "sentiment scores for apple tweets")
