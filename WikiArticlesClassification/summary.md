This file describes the implementation details of the classification of Wikipedia articles.

# Input
The following 25 wikipedia articles were used for classification :
"Mahendra Singh Dhoni",
"Sachin Tendulkar",
"Saina Nehwal",
"Sardara Singh",
"Michael Phelps",
"The Prestige (film)",
"Avatar (2009 film)",
"Interstellar (film)",
"Inception",
"Iron Man (2008 film)",
"Paneer",
"Pizza",
"Biryani",
"Pasta",
"Roti",
"Bharatanatyam",
"Kuchipudi",
"Ballet",
"Yakshagana",
"Kathak",
"Harry Potter",
"Nancy Drew",
"Panchatantra",
"Ramayana",
"As You Like It",
"France",
"India",
"United States",
"Germany",
"Japan"

#Description

The above articles were loaded, preprocessed by creating a corpus, cleaning the corpus and performing stemDocumentation and 
stemCompletion according to the standard text preprocessing techniques.
Necessary stopwords were removed, and certain words were replaced.

Two methods were used for Classification: 

1. K-Means and Hierarchical Clustering 
2. Topic Modelling using LDA 

1. K-Means and hierarchical clustering have been carried out and results have been documented in : 
   Images/dendrogram.png and Outputs/results.csv
   
2. Topic Modelling technique named LDA (Latent Dirichlet Allocation) has been used for classification
   Gibbs Sampling technique has been used. 
   The results of this analysis have been made available in Outputs/*.csv
   
Both the techniques have given optimal results for the provided input.
