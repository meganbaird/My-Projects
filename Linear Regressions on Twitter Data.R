setwd("C:/Users/bairdm2/Downloads") 
getwd()
list.files()
tweets <- read.csv("xg2.csv")

#################################FAVORITE COUNT#############################
tweets_fav<-lm(favorite_count~source+verified+xxa+xxe+xxf+xxh+xxi+xxl+xxfe+
                xxma+xxn+xxp+xxq+xxu+xxw+xdtw,
              data=tweets)
summary(tweets_fav)

#remove xxma
tweets_fav<-lm(favorite_count~source+verified+xxa+xxe+xxf+xxh+xxi+xxl+xxfe+
                xxn+xxp+xxq+xxu+xxw+xdtw,
              data=tweets)
summary(tweets_fav)

#remove xxe
tweets_fav<-lm(favorite_count~source+verified+xxa+xxf+xxh+xxi+xxl+xxfe+
                xxn+xxp+xxq+xxu+xxw+xdtw,
              data=tweets)
summary(tweets_fav)

#remove xxi
tweets_fav<-lm(favorite_count~source+verified+xxa+xxf+xxh+xxl+xxfe+xxn+xxp
              +xxq+xxu+xxw+xdtw,
              data=tweets)
summary(tweets_fav)

#remove xxf
tweets_fav<-lm(favorite_count~source+verified+xxa+xxh+xxl+xxfe+xxn+xxp
               +xxq+xxu+xxw+xdtw,
               data=tweets)
summary(tweets_fav)

#remove xxa
tweets_fav<-lm(favorite_count~source+verified+xxh+xxl+xxfe+xxn+xxp
               +xxq+xxu+xxw+xdtw,
               data=tweets)
summary(tweets_fav)

#remove xxq
tweets_fav<-lm(favorite_count~source+verified+xxh+xxl+xxfe+xxn+xxp+xxu+xxw+
                 xdtw,
               data=tweets)
summary(tweets_fav)
##besides sourceOther, this is our final model for finding a high fav count
#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)   12800.880    905.180  14.142  < 2e-16 ***
#  sourceMobile   5089.640    664.985   7.654 2.05e-14 ***
#  sourceOther    -865.967    894.606  -0.968 0.333064    
#  verifiedTRUE   2565.991    518.322   4.951 7.47e-07 ***
#  xxh           -1924.019    505.221  -3.808 0.000140 *** http
#  xxl             -37.260      7.044  -5.290 1.24e-07 *** length
#  xxfe           2150.400    476.886   4.509 6.55e-06 *** female
#  xxn            -350.183    153.894  -2.275 0.022889 *   count of numbers
#  xxp           -1871.986    557.570  -3.357 0.000788 *** politics
#  xxu            -105.665     19.473  -5.426 5.83e-08 *** upper case
#  xxw             298.599     38.423   7.771 8.18e-15 *** white space
#  xdtwB         10455.416    642.281  16.279  < 2e-16 *** retweet count B
#  xdtwC         23735.201    775.313  30.614  < 2e-16 *** retweet count C
#  xdtwD         44979.785    818.577  54.949  < 2e-16 *** retweet count D
#  xdtwE         84165.166    892.459  94.307  < 2e-16 *** retweet count E
#  xdtwF        154859.152   1093.385 141.633  < 2e-16 *** retweet count F
#  xdtwG        254356.602   1475.831 172.348  < 2e-16 *** retweet count G

######################################RETWEET COUNT#########################
tweets_ret<-lm(retweet_count~source+verified+xxa+xxe+xxf+xxh+xxi+xxl+xxfe+
                xxma+xxn+xxp+xxq+xxu+xxw,
              data=tweets)
summary(tweets_ret)

#remove xxfe
tweets_ret<-lm(retweet_count~source+verified+xxa+xxe+xxf+xxh+xxi+xxl+xxma
               +xxn+xxp+xxq+xxu+xxw,
               data=tweets)
summary(tweets_ret)

#remove xxq
tweets_ret<-lm(retweet_count~source+verified+xxa+xxe+xxf+xxh+xxi+xxl+xxma
               +xxn+xxp+xxu+xxw,
               data=tweets)
summary(tweets_ret)

#remove xxf
tweets_ret<-lm(retweet_count~source+verified+xxa+xxe+xxh+xxi+xxl+xxma
               +xxn+xxp+xxu+xxw,
               data=tweets)
summary(tweets_ret)

#remove xxn
tweets_ret<-lm(retweet_count~source+verified+xxa+xxe+xxh+xxi+xxl+xxma
               +xxp+xxu+xxw,
               data=tweets)
summary(tweets_ret)

#remove xxma
tweets_ret<-lm(retweet_count~source+verified+xxa+xxe+xxh+xxi+xxl
               +xxp+xxu+xxw,
               data=tweets)
summary(tweets_ret)

#remove xxi
tweets_ret<-lm(retweet_count~source+verified+xxa+xxe+xxh+xxl+xxp+xxu+xxw,
               data=tweets)
summary(tweets_ret)
##besides sourceOther, this is the function to use to get a high retweet
#count
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    18895.02     513.75  36.779  < 2e-16 ***
#  sourceMobile  3731.48     399.33   9.344  < 2e-16 ***
#  sourceOther   -901.90     539.46  -1.672 0.094571 .  
#  verifiedTRUE -5073.61     311.64 -16.280  < 2e-16 ***
#  xxa             45.38      14.86   3.055 0.002255 **  non word char
#  xxe           -407.52     169.22  -2.408 0.016036 *   exclamations
#  xxh           1400.06     281.17   4.979 6.43e-07 *** counts of http
#  xxl            -42.95       4.73  -9.080  < 2e-16 *** length
#  xxp          -1511.09     336.92  -4.485 7.34e-06 *** count politics
#  xxu            -32.66      12.15  -2.687 0.007227 **  count of upper case
#  xxw             83.28      24.36   3.418 0.000631 *** white space

############################################################################
############################################################################
############################################################################

##Unsupervised Learning
###########Rule Association Learning############
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
library(stringr)

tweets$text<-as.character(tweets$text)

#Get some information on the number of words used in each tweet
#sapply means you give it a vector and we applied a vector to
#each variable. split up words by spaces. length gives us length
#of words
word_count<-as.numeric(
  sapply(
    as.character(tweets$text), function(x){
      length(strsplit(x," ")[[1]])
    }
  )
)

#Lets take a look
hist(word_count)
summary(word_count)

#Prepare for market basket analysis
words<-as.character(
  sapply(
    tweets$text, function(x){
      str_replace_all(x, " ", ",")
    }
  )
)
write.csv(words,"xg2.csv",quote=FALSE,row.names=TRUE)

#Now, lets run an algorithm to do the analysis and extract the rules
tr<-read.transactions("xg2.csv",format="basket",sep=",")
#minimum amount of support and confidence that we want for our rules
#rules within 1% support and 50% confidence
rules<-apriori(tr,parameter=list(supp=0.01,conf=.8))
topRules<-rules[1:100]
inspect(topRules)

#Get some plots of the association rules
####look into why error message is popping up. says arulesViz package
####needs to be installed but it is installed and still won't run
plot(topRules)
plot(topRules,method="graph")
plot(topRules,method="grouped")

##################Clustering###################
rtclusters<-kmeans(tweets$retweet_count,3)
hist(tweets$retweet_count,breaks = 10,xlim=c(0,2000))
summary(tweets$retweet_count)
tweets$retweet_count[15]
category<-rtclusters$cluster
tweets<-data.frame(tweets,rtCategory=as.factor(category))
##as.factor is necessary since it is a categorical variable, not numeric


#Supervised Learning
#Regression Problems
library(lexicon)
library(sentimentr)
library(parallel)
library(caret)
#lexicon is a series of words with numerical values stored to them
lexicons <- list(hash_sentiment_huliu,
                 hash_sentiment_jockers,
                 hash_sentiment_jockers_rinker,
                 hash_sentiment_loughran_mcdonald,
                 hash_sentiment_nrc,
                 hash_sentiment_senticnet,
                 hash_sentiment_sentiword,
                 hash_sentiment_slangsd,
                 hash_sentiment_socal_google)

theText<-as.character(tweets$text)
theLexicon<-lexicons[[2]]

#Single-Core Processing
#textSentiments<-sapply(theText,function(i){sum(sentiment)})

#Parallel Processing
clust<-makeCluster(detectCores())
clusterEvalQ(clust,library(sentimentr))
clusterExport(clust,"theText")
clusterExport(clust,"theLexicon")
textSentiments<-parSapply(clust,1:length(theText),
                          function(x){
                            sum(sentiment(theText[x],polarity_dt=theLexicon)$sentiment)
                          })
stopCluster(clust)

tweets<-data.frame(tweets,textSentiments)
#Basic Regression:
theFormula<-retweet_count~textSentiments
olsModel<-lm(theFormula,data=tweets)
summary(olsModel)
residuals<-olsModel$residuals
#poisson has mean = variance
poisModel<-glm(theFormula,family="poisson",data=tweets)
summary(poisModel)
BIC(olsModel)
BIC(poisModel)
####Our OLS model is the better of the 2 models

#Classification Problem with Naive Bayes
library(e1071)
classFormula<-rtCategory~textSentiments+favorite_count
nbc<-naiveBayes(classFormula,data=tweets)
testTweet<-tweets[1,]
predict(nbc,testTweet)




###########################Clustering#######################
##############Flat Clustering Approaches##########
#K-Mediod Clustering
install.packages("kmed")
library(kmed)
#With Numerical Variables
tweet_nums <- tweets[,c("retweet_count","favorite_count")]
tweet_nums <- scale(tweet_nums)
#distNumeric will compute the distance matrix
dist_m<-distNumeric(tweet_nums,tweet_nums)
tweet_cluster<-fastkmed(dist_m,5)
the_cluster<-tweet_cluster$cluster
plot(tweet_nums[,1],tweet_nums[,2],col=the_cluster,xlim=c(0,1),
     ylim=c(0,1))

#With Categorical Variables
tweet_cat<-as.matrix(tweets[,c("source")])
##originally had "is_quote" in vector, needed to remove since it is not in xg2
tweet_dist<-matching(tweet_cat,tweet_cat)
cooccur(tweet_cat)

########Hierarchical Clustering Approaches#######
#Bottom-Up Approach
#First let's prepare the numerical data we would like to use
tweet_nums<-tweets[sample(1:nrow(tweets),50),c("retweet_count",
                                               "favorite_count")]
tweet_dist<-dist(tweet_nums)
fit<-hclust(tweet_dist)
plot(fit)
clust<-cutree(fit,k=5)
plot(tweet_nums$retweet_count,tweet_nums$favorite_count,col=clust,
     xlim=c(0,100),ylim=c(0,500))