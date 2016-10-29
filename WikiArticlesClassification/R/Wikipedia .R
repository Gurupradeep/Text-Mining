library(tm)
library(stringi)
library(qdap)
library(proxy)

#Set working directory
setwd("E:/Computer Engg/Machine learning/Text Mining/WikiMining");
wiki <- "http://en.wikipedia.org/wiki/"

#Load the necessary articles
titles <- c("Mahendra Singh Dhoni","Sachin Tendulkar","Saina Nehwal","Sardara Singh","Michael Phelps","The Prestige (film)","Avatar (2009 film)","Interstellar (film)",
            "Inception","Iron Man (2008 film)","Paneer","Pizza","Biryani","Pasta","Roti","Bharatanatyam","Kuchipudi","Ballet","Yakshagana","Kathak","Harry Potter","Nancy Drew","Panchatantra","Ramayana","As You Like It","France","India","United States","Germany","Japan")
articles <- character(length(titles))

for (i in 1:length(titles)) {
  articles[i] <- stri_flatten(readLines(stri_paste(wiki, titles[i])), col = " ")
}
articles[4]

#Pre-processing 
docs <- Corpus(VectorSource(articles))
#To replace all "" with a space 
docs2 <- tm_map(docs, function(x) stri_replace_all_regex(x, "<.+?>", " "))
docs3 <- tm_map(docs2, function(x) stri_replace_all_fixed(x, "\t", " "))
docs4 <- tm_map(docs3, PlainTextDocument)
docs <- docs4
docs4[[1]]$content

docs_copy <- docs

#function for cleaning the corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(replace_symbol))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#remove stopwords
mystopwords <- c(stopwords("en"),"also","august","decemb","franc","india","indian","isbn","januari","juli","march","may","octob","april","tendulkar","april","germani","japan","harri","septemb","novemb","avatar","american","film","book","french","febraury","yakshagana","dhoni")
docs <- tm_map(docs, removeWords,mystopwords)
docs <- clean_corpus(docs)

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
docs <- lapply(docs, stemCompletion2, dictionary=docs_copy)
docs <- Corpus(VectorSource(docs))

# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

docs<- replaceWord(docs, "articl", "article")
docs<- replaceWord(docs, "articlee", "article")
docs<- replaceWord(docs, "retriev", "retrieve")
docs<- replaceWord(docs, "retrievee", "retrieve")
docs<- replaceWord(docs, "univers", "universe")
docs<- replaceWord(docs, "universee", "universe")
docs<- replaceWord(docs, "centuri", "century")
docs<- replaceWord(docs, "countri", "country")
docs<- replaceWord(docs, "histori", "history")
docs<- replaceWord(docs, "februari", "february")

docsTDM <- DocumentTermMatrix(docs,control = list(wordLengths = c(3, Inf)))
docsTDM2 <- as.matrix(docsTDM)
docsdissim <- dist(docsTDM2, method = "cosine")

docsdissim2 <- as.matrix(docsdissim)
rownames(docsdissim2) <- titles
colnames(docsdissim2) <- titles
docsdissim2

#Method 1: Hierarchical clustering and K Means 
h <- hclust(docsdissim, method = "ward.D")
plot(h, labels = titles, sub = "")
rect.hclust(h, k=6, border="red")
result<-kmeans(docsTDM,6,nstart=20)
dtm2df<- as.data.frame(inspect(docsTDM))
head(dtm2df,1)
groups <- cutree(h, k=6)
write.csv(groups, "groups.csv", row.names = T)
groups=cbind(groups,titles)
groups
result$cluster
write.csv(result$cluster, "result.csv", row.names = T)
table(result$cluster,groups)

#Method 2: LDA (Latent Dirichlet Allocations)

library(topicmodels)
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 6

#Run LDA using Gibbs sampling
ldaOut <-LDA(docsTDM,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv")) 
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(docsTDM),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(docsTDM),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
