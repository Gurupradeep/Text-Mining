setwd("E:/Computer Engg/Machine learning/Text Mining/BagOfPopcorn")
library(devtools)
library(tm)
library(qdap)
library(stringr)
library(wordVectors)
library(randomForest)
require(NLP)
require(openNLP)
install_github("bmschmidt/wordVectors")



label_train <- read.table("labeledTrainData.tsv",sep = "\t", header = TRUE,quote="",stringsAsFactors = FALSE)
test <- read.table("testData.tsv",sep = "\t", header = TRUE,quote="",stringsAsFactors = FALSE)
unlabel_train <- read.table("unlabeledTrainData.tsv",sep = "\t", header = TRUE,quote="",stringsAsFactors = FALSE)
colnames(label_train)

corpus <- Corpus(VectorSource(c(label_train$review, unlabel_train$review)))

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, function(x) stri_replace_all_regex(x, "<.+?>", " "))
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  corpus <- tm_map(corpus, content_transformer(removeURL))
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", corpus)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

df<-clean_corpus(corpus)
df<-tm_map(corpus, PlainTextDocument)
df<-data.frame(review=unlist(sapply(df, `[`, "content")), 
                      stringsAsFactors=F)
write.table(df,"train.txt",sep="\t",row.names=FALSE)

#Build a word2vec model on labelled and unlabelled training data
model = train_word2vec("train.txt",output="train_vectors.bin",threads = 5,vectors = 300,window=10)
#nearest_to(model,model[["awesome"]])
#typeof(model)
#model[["awesome"]]

#Get a wordlist

review_to_wordlist <- function(review) {
  review <-  gsub("http[^[:space:]]*", "", review)
  review <-  gsub("[^[:alpha:][:space:]]*", "", review)
  review = gsub("[[:punct:]]", " ", review)
  review <- removeWords(review,stopwords("en"))
  review <- str_trim(review)
  review <- tolower(review)
  words <- unlist(strsplit(review," "))
  return (words)
  
}

#convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
 # sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
 # text <- as.String(text)
  
  # Sentence boundaries in text
  #sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  #sentences <- text[sentence.boundaries]
  #newsentences= c()
  #for(sentence in sentences)
  #{
  #temp= review_to_wordlist(sentence)
  #temp= removeWords(temp,"")  
  #newsentences=c(newsentences,temp)
  #}
  # return sentences
  #return (newsentences)
#}
#x=convert_text_to_sentences(head(label_train,1)$review)
#x=removeWords(x,"")
#x
#typeof(x)
#head(rownames(model),10)
#z=c(model[["king"]])
#z
#words_in_model
#words=c("good","great","bad")
#length(words)

#Calculates sum and average of word vectors of each review
makeFeatureVec=function(words)
{
  words_in_model=rownames(model)
  nwords=0
  featurevec=rep(0,300)
  for(word in words)
  {
  if(word %in% words_in_model)
    {
    model_to_vec=c(model[[word]])
    featurevec=featurevec+model_to_vec
    nwords=nwords+1
  }
  }
  answer=featurevec/nwords
  return(answer)
}


#Build a feature vector for each of the set of reviews
getAvgFeatureVecs=function(reviews)
{
counter=1
reviewfeaturesvec=list()
for (review in reviews)
{
  if(counter%%100==0)
    print(counter)
reviewfeaturesvec[[counter]]=c(makeFeatureVec(review))  
counter=counter+1
}
return (reviewfeaturesvec)
}

clean_train_reviews = list()
count=1
for( review in label_train$review)
{
  clean_train_reviews[[count]] = c(review_to_wordlist(review))
  count=count+1
}

clean_test_reviews = list()
count=1
for( review in test$review)
{
  clean_test_reviews[[count]] = c(review_to_wordlist(review))
  count=count+1
}

trainDataVecs = getAvgFeatureVecs(clean_train_reviews)
testDataVecs = getAvgFeatureVecs(clean_test_reviews)

#Build and run the randomForest classifier on the train and test data respectively

forest<-randomForest(as.factor(label_train$sentiment),data=trainDataVecs,importance=TRUE,ntree=500)
my_prediction<-predict(forest,testDataVecs,type="class")
answer<-data.frame(Id=test$id,Sentiment=my_prediction)
write.csv(answer,file="review_sentiment.csv",row.names=FALSE)