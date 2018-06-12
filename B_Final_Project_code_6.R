# BIA 658 - Social Network Analysis
# Group Project Assignment - Final Code
# Topic - Sentiment analysis of tweets before and after Trump became President of the USA
# Team Members - Abhitej Kodali, Ayush Sharma, Smriti Vimal, Abhay Sharma

library(ggthemes) # Load
library(tm)
library(stringr)
library(plyr)
library(dplyr)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(RColorBrewer)
library(tidytext)
library(syuzhet)
library(reshape2)
library(scales)
library(lubridate)
library(httr)
library(slam)
library(mime)
library(R6)
library(NLP)
library(sentiment)
library(Rcpp)
library(widyr)
library(igraph)
library(ggraph)

# Reading CSV file into the program
tweets = read.csv("E:/Social Network Analytics BIA 658B/Project/trumptweets_131117_v2.1.csv", header=TRUE)
tweets$creation_date_time<-ydm_hms(paste(paste(tweets$created_at..year.,
                                                 tweets$created_at..day., tweets$created_at..month.,sep="-"),
                                           tweets$created_at..time.))
tweets$creation_month<-ymd(paste(tweets$created_at..year.,tweets$created_at..month.,1,sep="-"))

# Breaking the dataset into two groups namely before presidency(bp) and after presidency(ap)
tweets_bp<-subset(tweets,tweets$creation_date_time <= ydm_hms("2016-08-11 11:59:59"))
tweets_ap<-subset(tweets,tweets$creation_date_time > ydm_hms("2016-08-11 11:59:59"))


# Text mining process
clean_tweets <- function(txt){
  # remove retweet entities
  txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)
  # remove at people
  txt = gsub("@\\w+", "", txt)
  # remove punctuation
  txt = gsub("[[:punct:]]", "", txt)
  # remove numbers
  txt = gsub("[[:digit:]]", "", txt)
  # remove html links
  txt = gsub("http\\w+", "", txt)
  # remove unnecessary spaces
  txt = gsub("[ \t]{2,}", "", txt)
  txt = gsub("^\\s+|\\s+$", "", txt)
  
  # define "tolower error handling" function 
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply 
  txt = sapply(txt, try.error)
  
  # remove NAs in some_txt
  txt = txt[!is.na(txt)]
  names(txt) = NULL
  
  return (txt)
} 

tweets$text <- clean_tweets(tweets$text)
tweets_bp$text <- clean_tweets(tweets_bp$text)
tweets_ap$text <- clean_tweets(tweets_ap$text)

# Analysis of RT count, favorite count, tweets count
monthly_1 <- ddply(tweets, ~ creation_month, summarize, num_tweets = length(source), ave_rt=mean(retweet_count),ave_fav=mean(favorite_count))

# plot the monthly summary
ggplot(monthly_1, aes(x=creation_month)) + 
  geom_bar(aes(y=num_tweets, colour="Total Monthly Tweets"),stat = "identity") +
  ggtitle("Donald Trump's Average Monthly Tweets") + xlab("Time") + ylab("Number of Tweets") + 
  scale_x_date(date_breaks = "3 months",date_labels = '%b-%y') 

# plot the average favorite summary
ggplot(monthly_1, aes(x=creation_month)) +
  geom_line(aes(y=ave_fav, colour="Average Monthly Favorite")) +
  ggtitle("Trumps tweet's average likes") +
  xlab("Time") + ylab("Average Favorites") + scale_x_date(date_breaks = "3 months",date_labels = '%b-%y')

#plot the average RTs summary
ggplot(monthly_1, aes(x=creation_month)) + 
  geom_line(aes(y=ave_rt, colour="Average Monthly RTs")) +
  ggtitle("Donald Trump's tweets RTs count") + xlab("Time") + ylab("Average RTs") + 
  scale_x_date(date_breaks = "3 months",date_labels = '%b-%y')

#analysis of Sentiment score by Source
scoreSentiment = function(tab)
{
  tab$syuzhet = get_sentiment(tab$text, method="syuzhet")
  tab$bing = get_sentiment(tab$text, method="bing")
  tab$afinn = get_sentiment(tab$text, method="afinn")
  tab$nrc = get_sentiment(tab$text, method="nrc")
  emotions = get_nrc_sentiment(tab$text)
  n = names(emotions)
  for (nn in n) tab[, nn] = emotions[nn]
  return(tab)
}

# get the daily sentiment scores for the tweets
tweets_score <- scoreSentiment(tweets)
tweets_score_bp <- scoreSentiment(tweets_bp)
tweets_score_ap <- scoreSentiment(tweets_ap)

# get monthly summaries of the results
monthly_2 <- ddply(tweets_score, ~ source + creation_month, summarize, num_tweets = length(positive), ave_sentiment = mean(bing),
               ave_negative = mean(negative), ave_positive = mean(positive))

# plot the monthly positive sentiment
ggplot(monthly_2, aes(x=creation_month, y=ave_positive, colour=source)) + geom_line() +
  ggtitle("Donald Trump Sentiment") + xlab("Time") + ylab("Positive Sentiment") + 
  scale_x_date(date_breaks = "3 months",date_labels = '%b-%y')

# plot the monthly negative sentiment
ggplot(monthly_2, aes(x=creation_month, y=ave_negative, colour=source)) + geom_line() +
  ggtitle("Donald Trump Sentiment") + xlab("Time") + ylab("Negative Sentiment") + 
  scale_x_date(date_breaks = "3 months",date_labels = '%b-%y')

# Plot total sentiment score
sentimentTotals <- data.frame(colSums(tweets_score[,c(17:24)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = (sentimentTotals$count/sum(sentimentTotals$count)*100))) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Percent of Tweets") + ggtitle("Total Sentiment Score for all Tweets")

# Plot total sentiment score - before presidency
sentimentTotals_bp <- data.frame(colSums(tweets_score_bp[,c(17:24)]))
names(sentimentTotals_bp) <- "count"
sentimentTotals_bp <- cbind("sentiment" = rownames(sentimentTotals_bp), sentimentTotals_bp)
rownames(sentimentTotals_bp) <- NULL
ggplot(data = sentimentTotals_bp, aes(x = sentiment, y = (sentimentTotals_bp$count/sum(sentimentTotals_bp$count)*100))) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Percent of Tweets") + ggtitle("Total Sentiment Score for Tweets - Before Presidency")

# Plot total sentiment score -after presidency
sentimentTotals_ap <- data.frame(colSums(tweets_score_ap[,c(17:24)]))
names(sentimentTotals_ap) <- "count"
sentimentTotals_ap <- cbind("sentiment" = rownames(sentimentTotals_ap), sentimentTotals_ap)
rownames(sentimentTotals_ap) <- NULL
ggplot(data = sentimentTotals_ap, aes(x = sentiment, y = (sentimentTotals_ap$count/sum(sentimentTotals_ap$count)*100))) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Percent of Tweets") + ggtitle("Total Sentiment Score for Tweets - After Presidency")

# Trend of positive and negative sentiment
posnegtime <- ddply(tweets_score, ~creation_month, summarize, 
                    negative = mean(negative), 
                    positive = mean(positive))
posnegtime <- melt(group_by(posnegtime,creation_month),id.vars="creation_month")
names(posnegtime) <- c("month", "sentiment", "meanvalue")
posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])

# Plot negative and positive sentiment trend
ggplot(data = posnegtime, aes(x = month, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) + 
  scale_colour_manual(values = c("springgreen4", "firebrick3")) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  scale_x_date(breaks = date_breaks("3 months"), 
               labels = date_format("%b-%y")) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment Over Time")

# Monthly trend of sentiment score during the timeline
monthly_sentiment <- ddply(tweets_score, ~creation_month, summarize, 
                   anger = mean(anger), 
                   anticipation = mean(anticipation), 
                   disgust = mean(disgust), 
                   fear = mean(fear), 
                   joy = mean(joy), 
                   sadness = mean(sadness), 
                   surprise = mean(surprise), 
                   trust = mean(trust))
monthly_sentiment <- melt(group_by(monthly_sentiment,creation_month),id.vars="creation_month")
names(monthly_sentiment) <- c("month", "sentiment", "meanvalue")

ggplot(data = monthly_sentiment, aes(x = month, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Average sentiment score") + 
  ggtitle("Emotional Sentiment Score Trend") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y")

# emtion word cloud for before presidency
# emotion analysis: anger, anticipation, disgust, fear, joy, sadness, surprise, trust
# put everything in a single vector
all_bp = c(
  paste(tweets_score_bp$text[tweets_score_bp$anger > 0], collapse=" "),
  paste(tweets_score_bp$text[tweets_score_bp$anticipation > 0], collapse=" "),
  paste(tweets_score_bp$text[tweets_score_bp$disgust > 0], collapse=" "),
  paste(tweets_score_bp$text[tweets_score_bp$fear > 0], collapse=" "),
  paste(tweets_score_bp$text[tweets_score_bp$joy > 0], collapse=" "),
  paste(tweets_score_bp$text[tweets_score_bp$sadness > 0], collapse=" "),
  paste(tweets_score_bp$text[tweets_score_bp$surprise > 0], collapse=" "),
  paste(tweets_score_bp$text[tweets_score_bp$trust > 0], collapse=" ")
)
# clean the text
all_bp = clean_tweets(all_bp)
# remove stop-words
# adding extra domain specific stop words
all_bp = removeWords(all_bp, c(stopwords("english"), 'trump','donald','rt','realdonaldtrump',"duh", "whatever","amp"))
#
# create corpus
corpus_bp = Corpus(VectorSource(all_bp))
#
# create term-document matrix
tdm_bp = TermDocumentMatrix(corpus_bp)
#
# convert as matrix
tdm_bp = as.matrix(tdm_bp)
#
# add column names
colnames(tdm_bp) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
#
# Plot comparison wordcloud
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, 'Emotion Comparison Word Cloud - Before Presidency')
comparison.cloud(tdm_bp, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1.5, max.words=250)

2# emtion word cloud for after presidency
# emotion analysis: anger, anticipation, disgust, fear, joy, sadness, surprise, trust
# put everything in a single vector
all_ap = c(
  paste(tweets_score_ap$text[tweets_score_ap$anger > 0], collapse=" "),
  paste(tweets_score_ap$text[tweets_score_ap$anticipation > 0], collapse=" "),
  paste(tweets_score_ap$text[tweets_score_ap$disgust > 0], collapse=" "),
  paste(tweets_score_ap$text[tweets_score_ap$fear > 0], collapse=" "),
  paste(tweets_score_ap$text[tweets_score_ap$joy > 0], collapse=" "),
  paste(tweets_score_ap$text[tweets_score_ap$sadness > 0], collapse=" "),
  paste(tweets_score_ap$text[tweets_score_ap$surprise > 0], collapse=" "),
  paste(tweets_score_ap$text[tweets_score_ap$trust > 0], collapse=" ")
)
# clean the text
all_ap = clean_tweets(all_ap)
# remove stop-words
# adding extra domain specific stop words
all_ap = removeWords(all_ap, c(stopwords("english"), 'trump','donald','rt','realdonaldtrump',"duh", "whatever","amp"))
#
# create corpus
corpus_ap = Corpus(VectorSource(all_ap))
#
# create term-document matrix
tdm_ap = TermDocumentMatrix(corpus_ap)
#
# convert as matrix
tdm_ap = as.matrix(tdm_ap)
#
# add column names
colnames(tdm_ap) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
#
# Plot comparison wordcloud
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, 'Emotion Comparison Word Cloud - After Presidency')
comparison.cloud(tdm_ap, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1.5, max.words=250)

# WordCloud and Emotional bar chart
createwordcloud <-function(tweettexts,title){
  tweettexts<-clean_tweets(tweettexts)
  tweetcorpus <- Corpus(VectorSource(tweettexts))
  tweetcorpus <- tm_map(tweetcorpus, removeWords, c("duh", "whatever","amp","trump", stopwords("english")))
  tweetsdtm <- DocumentTermMatrix(tweetcorpus)
  tweetsdtm <- removeSparseTerms(tweetsdtm, 0.95)
  # Simple word cloud
  findFreqTerms(tweetsdtm, 1000)
  freq <- data.frame(sort(colSums(as.matrix(tweetsdtm)), decreasing=TRUE))
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  col=brewer.pal(6,"Dark2")
  wordcloud(tweetcorpus, min.freq=50, scale=c(3,2),rot.per = 0.15,
            random.color=F, max.words = 100, random.order=T,colors=col)
  
}

createwordcloud(tweets_bp$text,"Word Cloud for before Presidency Tweets")
createwordcloud(tweets_ap$text,"Word Cloud for after Presidency Tweets")

# Pair wise words for tweets  before Presidency
tweets_desc_bp <- data_frame(id = tweets_score_bp$id_str, desc = tweets_score_bp$text)
tweets_desc_bp <- tweets_desc_bp %>% unnest_tokens(word,desc) %>% anti_join(stop_words)
tweets_desc_bp %>% count(word, sort = TRUE)
tweets_desc_bp

my_stopwords <- data.frame(word = c(as.character(1:10),"rt","donald","trump","realdonaldtrump","amp","whatever","duh"))
tweets_desc_bp <- tweets_desc_bp %>% anti_join(my_stopwords)

desc_word_pairs_bp <- tweets_desc_bp %>% pairwise_count(word, id, sort=TRUE, upper=FALSE)
desc_word_pairs_bp

#plot graph
set.seed(1234)
desc_word_pairs_bp %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "star") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "blue") +
  geom_node_point(size = 2.5) +
  geom_node_label(aes(label = name), repel = TRUE,point.padding = unit(0.2, "lines"),label.size = 0.05) +
  theme_void()

# Pair wise words for tweets after Presidency
tweets_desc_ap <- data_frame(id = tweets_score_ap$id_str, desc = tweets_score_ap$text)
tweets_desc_ap <- tweets_desc_ap %>% unnest_tokens(word,desc) %>% anti_join(stop_words)
tweets_desc_ap %>% count(word, sort = TRUE)
tweets_desc_ap

my_stopwords <- data.frame(word = c(as.character(1:10),"rt","donald","trump","realdonaldtrump","amp","whatever","duh"))
tweets_desc_ap <- tweets_desc_ap %>% anti_join(my_stopwords)

desc_word_pairs_ap <- tweets_desc_ap %>% pairwise_count(word, id, sort=TRUE, upper=FALSE)
desc_word_pairs_ap

#plot graph
set.seed(1234)
desc_word_pairs_ap %>%
  filter(n >= 25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "star") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "blue") +
  geom_node_point(size = 2.5) +
  geom_node_label(aes(label = name), repel = TRUE,point.padding = unit(0.25, "lines"),label.size=0.05) +
  theme_void()
