#Import the required libraries
library(tm)
library(twitteR)
library(plyr)
library(qdap)
require(devtools)
library(ggplot2)
library(wordcloud)
library(sentiment)
library(data.table)
install_github("sentiment140", "okugami79")

#Load the necessary files
source('prediction.R')
source('sentiment.R')

#Set the path for the working directory
setwd("E:/Computer Engg/Machine learning/Text Mining/Twitter");

#Settings for obtaining access to the Twitter API
consumer_key <- "G4Q2yPJXAskppjiXV6bAQcsZE"
consumer_secret <- "fgy9G2n3u2rE03Gbk8QyTEX100PaA6xwAYCvb1n4J6IkSDr8vM"
access_token <- "787972320417591297-8KF84REvrOLxPt1tapffJKao1bKibs2"
access_secret <- "uMMWt0bqp8kjlxjB2NysiR7CPmxdA841EVHhViieQZh0a"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Search twitter for tweets related to HillaryClinton
tweets<-searchTwitter("#HillaryClinton",n=1500)
tweets.df <- twListToDF(tweets)
tweets.df[190, c("id", "created", "screenName", "replyToSN","favoriteCount", "retweetCount", "longitude", "latitude", "text")]
writeLines(strwrap(tweets.df$text[190], 100))

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
#remove urls
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
#replace abbreviations
myCorpus <- tm_map(myCorpus, content_transformer(replace_abbreviation))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
#convert to lowercase
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#add more words to English stopwords for improving performance
myStopWords<-c(stopwords("en"),"emfin","clinton","america","foxnews","donaldtrump","aninews","delhi","shennafoxmusic","hillaryclinton","bfraser","trump","podesta","rt","amp","leahr","haiti","lindasuhler","haitians","bengarrison","imwithher","podestaemails","get","pentagon","foundat","debat","obama",
               "nastywom","mike","maga","via","cnvey","realdonaldtrump","cnn","just","abc","florida","huckabee")

replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

myCorpus <- replaceWord(myCorpus, "abortiondonaldtrump", "abortion")
myCorpus <- tm_map(myCorpus, removeWords,myStopWords)

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument) # stem words
writeLines(strwrap(myCorpusCopy[[190]]$content, 60))

#Function for stemCompletion
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)

myCorpus <- Corpus(VectorSource(myCorpus))
writeLines(strwrap(myCorpus[[190]]$content, 60))

#convert to TDM
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(3, Inf)))
freq.terms <- findFreqTerms(tdm, lowfreq = 50)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 200)
df <- data.frame(term = names(term.freq), freq = term.freq)

#Plot to check the Terms and corresponding frequencies
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))
m <- as.matrix(tdm)


# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

#Obtain wordcloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 20,
          random.order = F, colors = pal)



dataframe<-data.frame(text=unlist(sapply(myCorpus, '[',"content")), stringsAsFactors=F)

#METHOD 1
#Perform sentiment analysis
sentiments <- sentiment(dataframe$text)
table(sentiments$polarity)
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
head(sentiments)

#print results
result1=calculateRatio(sentiments)
print(result1)
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")


#METHOD 2
#Import positive and negative words
pos<-scan('wordbanks/positive-words.txt',what='character',comment.char = ';')
neg<-scan('wordbanks/negative-words.txt',what='character',comment.char = ';')

#Convert to dataframe
dataframe<-data.frame(text=unlist(sapply(myCorpus, '[',"content")), stringsAsFactors=F)
head(dataframe$text)

#Perform sentiment analysis 
analysis=score.sentiment(dataframe$text,pos,neg)
#analysis=analysis[analysis$score!=0,]
table(analysis$score)

results2=calculateRatio(analysis)
print(results2)
write.csv(analysis, "output.csv", row.names = T)

tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(3, Inf)))
freq.terms <- findFreqTerms(tdm, lowfreq = 20)
tdm2 <- removeSparseTerms(tdm, sparse=0.97)
# convert the sparse term-document matrix to a standard data frame
tdm2df<- as.data.frame(inspect(tdm2))
head(freq.terms,50)
# inspect dimensions of the data frame
nrow(tdm2df) 
ncol(tdm2df) 

# cluster analysis
mydata.df.scale <- scale(tdm2df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit)

groups <- cutree(fit, k=4) # cut tree into k clusters
# draw dendogram with red borders around the k clusters
rect.hclust(fit, k=4, border="red")

result<-kmeans(tdm2df,4,nstart=20)
result$cluster

