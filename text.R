## TEXT ANALYTICS ##
getwd()
setwd("C:/Users/tirufamily/Desktop/Text_analytics")

library(dplyr)
library(plyr)
library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tidyverse)


tweets.df <- read.csv("C:/Users/tirufamily/Desktop/Text_analytics/demonetization-tweets.csv")
str(tweets.df)
tweets <- as.character(tweets.df$text)
head(tweets)

tweets <- gsub('[[:cntrl:]]', '', tweets)

# remove retweets
tweets <- gsub('(RT|via)((?:\\b\\W*@\\W+)+)', '', tweets)
head(tweets)
# remove at people
tweets <- gsub('@\\w+', '', tweets)

# remove punctuations
tweets<- gsub('[[:punct:]]', '', tweets)

# remove numbers
tweets <- gsub('[[:digit:]]', '', tweets)

# remove html links
tweets <- gsub('http[s]?\\w+', '', tweets)

# remove extra spaces
tweets <- gsub('[ \t]{2,}', '', tweets)
tweets<- gsub('^\\s+|\\s+$', '', tweets)

# removing NA's
tweets <- tweets[!is.na(tweets)]

# convert to lower case:
tweets <- tolower(tweets)

#Store the text in the corpus
t_corpus = VCorpus(VectorSource(tweets))
#remove stopwords
t_corpus = tm_map(t_corpus,removeWords,c(stopwords("en"),"amp","demonetiztion","obqrhlnsl","uodwxdpmmg"))
#remove whitespace
t_corpus = tm_map(t_corpus,stripWhitespace)
#create a Term Document Matrix
t_tdm = TermDocumentMatrix(t_corpus)
#convert into matrix
t_matrix = as.matrix(t_tdm)


#calculate word frequency
word_freq = rowSums(t_matrix)
word_freq = sort(word_freq,decreasing = TRUE)
#plotting the frequency of words
barplot(word_freq[1:10],col = "black",las = 2,main = "Frequency plot of words")
word_freq2 = data.frame(term = names(word_freq),num = word_freq)


#creating word cloud
wordcloud(word_freq2$term,word_freq2$num,min.freq = 150,max.words = 100,random.order = 'F',rot.per = 0.1,colors = brewer.pal(8,"Dark2"),scale = c(3,0.5),random.color = T)


#Calculating sentiment score
sentiment.score <- function(sentences, positive.words, negative.words,.progress="none")
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores <- laply(sentences, function(sentence, positive.words, negative.words)
  {
    word.list <- str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    negative.matches <- match(words, negative.words)
    positive.matches <- match(words, positive.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    
    positive.matches <- !is.na(positive.matches)
    negative.matches <- !is.na(negative.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
    score <- sum(positive.matches) - sum(negative.matches)
    
    return(score)
  }, positive.words, negative.words, .progress=.progress )
  
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
#bag of words
positive <- scan("positive.words.txt", what= "character", comment.char= ";")
negative <- scan("negative.words.txt", what= "character", comment.char= ";")

tweets.analysis <- sentiment.score(tweets, positive, negative, .progress="none")

str(tweets.analysis)

#Analysis
tweets.analysis$sentiment[tweets.analysis$score == 0] <- "Neutral" 
tweets.analysis$sentiment[tweets.analysis$score < 0] <- "Negative"
tweets.analysis$sentiment[tweets.analysis$score > 0] <- "Positive"

tweets.analysis$sentiment <- factor(tweets.analysis$sentiment)
table(tweets.analysis$score)


#statistical inferences
mean(tweets.analysis$score) # slighlty positive
median(tweets.analysis$score)
summary(tweets.analysis$sentiment) # more positive tweets than negative

#Plotting of the tweets 
ggplot(data = tweets.analysis, aes(x = score, fill = sentiment)) + 
  geom_bar() + 
  labs(title = "Sentiment Score Bar Plot", x = "Sentiment Score", y = "Tweet Count") +
  scale_x_continuous(breaks = seq(-6,6,1)) + 
  scale_y_continuous(breaks = seq(0,4000,500)) + 
  scale_fill_manual(guide = guide_legend("Sentiment"), values = c("#DD0426","#246EB9","#04B430"))

#hypothesis testing
sd<-sd(tweets.analysis$score)
sd

len<-length(tweets.analysis$score)
len

z = (mean)/(sd/sqrt(len))
z
alpha = 0.05
z.alpha = qnorm(1-alpha)
z.alpha
