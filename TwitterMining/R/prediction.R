calculateRatio <- function(analysis) {
  
  #loading score from dataframe
  df<-as.data.frame(table(analysis$score))
  
  #Creating a new column product for ratio.mag
  df$product = (as.numeric(as.character(df$Var1)) * as.numeric(as.character(df$Freq)))
 
  product.vector=as.numeric(unlist(df$product))
  freq.vector=as.numeric(as.character(df$Freq))
  magnitude.vector=as.numeric(as.character(df$Var1))
  
  #Calculating ratio using magnitude
  positive.sum<-sum(product.vector[product.vector>0])
  negative.sum<-sum(product.vector[product.vector<0])
  ratio.mag<-abs(positive.sum/negative.sum)
  
  #calculating ratio using sign
  positive.count<-sum(freq.vector[magnitude.vector>0])
  negative.count<-sum(freq.vector[magnitude.vector<0])
  ratio<-abs(positive.count/negative.count)
  
  #countng neutral tweets
  neutral.values=freq.vector[magnitude.vector==0]
  
  #returning final result
  result<-c(ratio.mag,ratio,neutral.values)
  return (result)
}