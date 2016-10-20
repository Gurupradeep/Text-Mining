#setting working directory
setwd("C:/Users/Guru Pradeep/Desktop/Text-Mining")

#loading the required libraries
library(twitteR)
library(plyr)
library(tm)
library(qdap)
library(ggplot2)
#authorisation details
consumer_key <- "G4Q2yPJXAskppjiXV6bAQcsZE"
consumer_secret <- "fgy9G2n3u2rE03Gbk8QyTEX100PaA6xwAYCvb1n4J6IkSDr8vM"
access_token <- "787972320417591297-8KF84REvrOLxPt1tapffJKao1bKibs2"
access_secret <- "uMMWt0bqp8kjlxjB2NysiR7CPmxdA841EVHhViieQZh0a"

#setting up authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#extracting  tweets
tweets = searchTwitter("#DonaldTrump", n = 3000)
length(tweets)
n.tweet <- length(tweets)

# convert tweets to a data frame
tweets.df <- twListToDF(tweets)
head(tweets.df$text)

#Text cleaning


# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))

#remove urls
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#remove abbreviations
myCorpus <- tm_map(myCorpus, content_transformer(replace_abbreviation))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myStopWords<-c(stopwords("en"),"donaldtrump","donald","trump","melaniatrump","hillaryclinton","realdonaldtrump","america","foxandfriends","aninews","bfraser","rt","amp","nancypelosi","will","complicityevasion","debatenight","gop","foxnews","que","mike","donaldtrumpand","clinton","huckabee")
myCorpus <- tm_map(myCorpus, removeWords,myStopWords)

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}
myCorpus <- replaceWord(myCorpus, "abortiondonaldtrump", "abortion")
myCorpus <- replaceWord(myCorpus, "alaw", "a law")
myCorpus <- replaceWord(myCorpus, "debatenight", "debate night")
myCorpus <- replaceWord(myCorpus, "debat", "debate")
myCorpus <- replaceWord(myCorpus, "partialbirth", "partial birth")
myCorpus <- replaceWord(myCorpus, "donaldtrumpimpression", "impression")

# keep a copy for stem completion later
myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument) # stem words

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

myCorpus[[3]]$content
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))

#creating a term document matrix
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(3, Inf)))

freq.terms <- findFreqTerms(tdm, lowfreq = 500)
freq.terms

#getting terms
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 70)

#for plotting frequent terms
df <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)

# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)

#For plotting network of Terms
install.packages("Rgraphviz")
library(graph)
library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

#for sentiment analysis - method 1
require(devtools)
install_github("sentiment140", "okugami79")

# sentiment analysis
library(sentiment)
dataframe<-data.frame(text=unlist(sapply(myCorpus, '[',"content")), stringsAsFactors=F)

#Calling sentiment function
sentiments <-sentiment(dataframe$text)

#analysing results
table(sentiments$polarity)
head(sentiments$text)

library(data.table)
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1

prediction = calculateRatio(sentiments)
prediction
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")


#loading positive and negative words

pos = scan('positive-words.txt',what = 'character',comment.char = ';')
head(pos)
neg = scan('negative-words.txt',what = 'character',comment.char = ';')
head(dataframe$text)


source('sentiment_new.R')

#calling sentiment function
analysis = score.sentiment(dataframe$text,pos,neg)
table(analysis$score)
pos_anal = analysis[analysis$score >0, ]

#predicting results
source('prediction.R')
prediction = calculateRatio(analysis)
prediction
table(pos_anal$score)
hist(analysis$score)
mean(analysis$score)
my_table = table(analysis$score)
analysis$text = dataframe$text

#writing tweets with their score into csv file
write.csv(analysis,file="output.csv",row.names=FALSE)

myCorpus[[1]]$content


tdm1 <- removeSparseTerms(tdm, sparse=0.97)
nrow(tdm1)
ncol(tdm1)

#distance matrix for hclust
matrix = as.matrix(tdm1)
distmatrix = dist(scale(matrix))

#hclustering
fit = hclust(distmatrix, method = 'ward.D')
p = plot(fit)

#choosing k value based on dendogram
p = rect.hclust(fit, k = 5)
print(p)
groups = cutree(fit, k = 5)
print(groups)


#k-means algorithm
result<-kmeans(tdm1,5,nstart=20)
result$cluster
table(result$cluster)
  
  