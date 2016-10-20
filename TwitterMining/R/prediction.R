calculateRatio <- function(analysis) {
  
  df<-as.data.frame(table(analysis$score))
  df$product = (as.numeric(as.character(df$Var1)) * as.numeric(as.character(df$Freq)))
  product.vector=as.numeric(unlist(df$product))
  freq.vector=as.numeric(as.character(df$Freq))
  magnitude.vector=as.numeric(as.character(df$Var1))
  positive.sum<-sum(product.vector[product.vector>0])
  negative.sum<-sum(product.vector[product.vector<0])
  ratio.mag<-abs(positive.sum/negative.sum)
  positive.count<-sum(freq.vector[magnitude.vector>0])
  negative.count<-sum(freq.vector[magnitude.vector<0])
  ratio<-abs(positive.count/negative.count)
  neutral.values=freq.vector[magnitude.vector==0]
  result<-c(ratio.mag,ratio,neutral.values)
  return (result)
}