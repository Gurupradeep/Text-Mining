This file describes in detail about the Twitter Mining carried out for the prediction of American presidential election results.

# Method of prediction

Sentiment analysis using text-mining methods has been incorporated for predicting the sentiment of the tweets on the 
two opposition parties. Using the results, a suitable conclusion has been drawn regarding the election results at the time of 
performing the sentiment analysis.

# Tweets collection
The twitter hashtags "#DonaldTrump" and "#HillaryClinton" have been used.

R/DonaldTrump.R    - contains analysis of tweets belonging to "#DonaldTrump"  
R/hillaryClinton.R - contains analysis of tweets belonging to "#HillaryClinton"  

#Description 

Around 3000 tweets of each of the two hashtags were collected and cleaned using suitable text-mining methods. The stopwords were updated
iteratively by a repeated analysis of the most frequently used terms in the TermDocumentMatrix. Certain words were also replaced with
more meaningful terms in the interest of sentiment analysis. A wordcloud was obtained for the visualisation of the nature of the tweets.

Two exclusive methods have been incorporated for analysing the sentiment of the tweets.

Method 1:

This uses the "sentiment" function belonging to a package named "sentiment" obtained with a prior installation of the same using
the following command : 

install_github("sentiment140", "okugami79")

The results are then fed to a calculateRatio function which is described later.

Method 2:

Two wordbanks containing positive-words and negative-words have been used. The dataframe constructed from the cleaned corpus , along with
the wordbanks is sent to a function named "score.sentiment" defined in the R/sentiment.R file.
The function compares the terms in the dataframe with the terms in the wordbanks and returns a table with scores (weights assigned to
the terms; a term with score is more positive than a term with lesser score) .

The results are then fed to a calculateRatio function. 

The calculateRatio function defined in R/prediction.R takes the analysis scores as input.
It calculates two types of ratios - one based on magnitude of the scores, and 
                                    one based on the sign of the scores ignoring the magnitude 
                                    ( for example, a score of 2 or a score of 1 is counted as 1)

ratio.mag<-abs(positive.sum/negative.sum)
ratio<-abs(positive.count/negative.count)

These two values along with the count of neutral words are returned back to the calling functions.
These results are displayed and documented.

K Means clustering and hierarchical clustering has also been performed on the cleaned text.

#Output description

Outputs/DonaldTrump - contains the results of sentiment analysis of tweets belonging to "#DonaldTrump"
Outputs/HillaryClinton - contains the results of sentiment analysis of tweets belonging to "#HillaryClinton"

The results of the analysis are briefly summarised below :

##ANALYSIS SCORES
####HILLLARY CLINTON

output.csv in each of these directories contains the scores of each of the 3000 tweets.  
results.txt in each of these directories contains the outputs returned by the calculateRatio function, for both the methods described. 

images/DonaldTrump and images/HillaryClinton contain the images of the cluster dendrogram, wordclouds and histograms for the
corresponding tweets analysed. This provides a visualisation of the analysis performed. 




