#loading the required libraries
library(tm)
library(qdap)
library(RCurl)
library(ROCR)
library(wordcloud) 
#Import RCurl for loading data via URL

test_data_url <- "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/testdata.txt"
train_data_url <- "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/training.txt"

#Load data into training and testing sets
test_data_file <- getURL(test_data_url)
train_data_file <- getURL(train_data_url)
train_data_df <- read.csv(text = train_data_file, sep='\t', header=FALSE, quote = "",stringsAsFactor=F,
  col.names=c("Sentiment", "Text"))
test_data_df <- read.csv(text = test_data_file, sep='\t', header=FALSE, quote = "",stringsAsFactor=F,
  col.names=c("Text"))

# we need to convert Sentiment to factor
train_data_df$Sentiment <- as.factor(train_data_df$Sentiment)
head(train_data_df)

#check no. of positive and negative texts
table(train_data_df$Sentiment)

#returns the average no. of words in a sentence
mean(sapply(sapply(train_data_df$Text, strsplit, " "), length))

#Creates a VCorpus object 
corpus <- Corpus(VectorSource(c(train_data_df$Text, test_data_df$Text)))


#function for cleaning the corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,stopwords("en"))
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(replace_symbol))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

clean_corpus(corpus)

#Create a DTM
#Normalisation is not really neccessary here 
dtm <- DocumentTermMatrix(corpus,control = list(weighting = weightTf, normalize = TRUE))
print(dtm)

#Remove terms appearing in less than 1% of the documents
sparse <- removeSparseTerms(dtm, 0.99)

typeof(sparse)
important_words_df <- as.data.frame(as.matrix(sparse))
colnames(important_words_df) <- make.names(colnames(important_words_df))
colnames(important_words_df)

# split into train and test
important_words_train_df <- head(important_words_df, nrow(train_data_df))
important_words_test_df <- tail(important_words_df, nrow(test_data_df))

# Add to original dataframes
train_data_words_df <- cbind(train_data_df, important_words_train_df)
test_data_words_df <- cbind(test_data_df, important_words_test_df)

# Get rid of the original Text field
train_data_words_df$Text <- NULL
test_data_words_df$Text <- NULL

set.seed(1234)

#Cross-Validation to check performance of our model
# first we create an index with 85% True values based on Sentiment
spl <- sample.split(train_data_words_df$Sentiment, .85)

# now we use it to split our data into train and test
eval_train_data_df <- train_data_words_df[spl==T,]
eval_test_data_df <- train_data_words_df[spl==F,]

#Use logistic regression for training model
log_model <- glm(Sentiment~., data=eval_train_data_df, family=binomial)
summary(log_model)

#Predict on validation dataset
log_pred <- predict(log_model, newdata=eval_test_data_df, type="response")

#Set threshold as 0.5
table(eval_test_data_df$Sentiment, log_pred>.5)


#Use ROC to get threshold 

head(log_pred)

#$Predictions has continous values between O and 1 which are predicted by our model and $labels are the actual output values which should be binary.
pred <- prediction(log_pred, eval_test_data_df$Sentiment)
perf <- performance(pred,"tpr","fpr")
plot(perf)
str(perf)
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
head(cutoffs)
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.1))


#Predict on original test data set
log_pred_test <- predict(log_model, newdata=test_data_words_df, type="response")
test_data_df$Sentiment <- log_pred_test>.5

#print a part of predicted data to see output 
set.seed(1234)
spl_test <- sample.split(test_data_df$Sentiment, .0005)
test_data_sample_df <- test_data_df[spl_test==T,]
test_data_sample_df[test_data_sample_df$Sentiment==T, c('Text')]
test_data_sample_df[test_data_sample_df$Sentiment==F, c('Text')]

#Wordcloud
set.seed(142)
freq <- sort(colSums(as.matrix(sparse)), decreasing=TRUE)
wordcloud(names(freq), freq, min.freq=25)  
