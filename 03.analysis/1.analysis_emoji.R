require(data.table)
require(tidyverse)
library(gbm)
library(xgboost)
library(rpart)
library(caret)
library(doParallel)
library(tm)


emoji<-as.data.table(as.matrix(readRDS("02.data.cleaning/emoji_matrix.rds")))
all_posts<-as.data.table(as.matrix(readRDS("02.data.cleaning/all_posts_clean.rds")))
ats_matrix<-as.data.table(as.matrix(readRDS("02.data.cleaning/ats_matrix.rds")))
pic_matrix<-as.data.table(as.matrix(readRDS("02.data.cleaning/alt_matrix.rds")))
word_dtm<-as.data.table(as.matrix(readRDS("02.data.cleaning/word_dtm.rds")))
word_matrix<-as.data.table(as.matrix(readRDS("02.data.cleaning/word_matrix.rds")))
hashtag<-as.data.table(as.matrix(readRDS("02.data.cleaning/hashtag_matrix.rds")))





all_posts$followers<-as.numeric(all_posts$followers)
all_posts$hour<-as.numeric(all_posts$hour)
all_posts$madrugada<-ifelse(all_posts$hour==1|all_posts$hour==2|all_posts$hour==3|all_posts$hour==4|all_posts$hour==5|all_posts$hour==6,1,0)
all_posts$morning<-ifelse(all_posts$hour==7|all_posts$hour==8|all_posts$hour==9|all_posts$hour==10|all_posts$hour==11|all_posts$hour==12,1,0)
all_posts$aftevening<-ifelse(all_posts$hour==13|all_posts$hour==14|all_posts$hour==15|all_posts$hour==16|all_posts$hour==17|all_posts$hour==18,1,0)
all_posts$nighttime<-ifelse(all_posts$hour==19|all_posts$hour==20|all_posts$hour==21|all_posts$hour==22|all_posts$hour==23|all_posts$hour==0,1,0)
#all_posts$emojis<-all_posts$emoji
all_posts$emoji<-NULL
all_posts$postdate<-all_posts$date
all_posts$date<-NULL
all_posts$hour<-NULL
all_posts$likes<-as.numeric(trimws(all_posts$likes))


all_posts <- tibble::rowid_to_column(all_posts, "ID")



simple<-lm(likes~weekday+morning+aftevening+nighttime+isMulti+followers, data=all_posts)
simple

pic_matrix_2<-pic_matrix

colnames(pic_matrix) <- paste0('pic_', colnames(pic_matrix)) 


posts<-cbind(all_posts,emoji,hashtag,word_matrix,pic_matrix,ats_matrix)

posts$alt=NULL
posts$desc=NULL
posts$desc_clean=NULL
posts$hashtags=NULL
posts$mentions=NULL
posts$url=NULL
posts$emoji_text=NULL
posts$user=NULL
posts$scrapedate=NULL
posts$ID=NULL
posts$hash_ct<-as.numeric(posts$hash_ct)
posts$post_ct<-as.numeric(posts$post_ct)
posts$mention_ct<-as.numeric(posts$mention_ct)
posts$emoji_ct<-as.numeric(posts$emoji_ct)
posts$likes<-scale(posts$likes)
posts$isMulti<-as.factor(posts$isMulti)


set.seed(2)
N =dim(posts)[1]
train_index =sample(N, 0.75 * N)
train = posts[train_index,]
test = posts[-train_index,]

require(Matrix)

train2<-as.data.frame(sapply(train,as.numeric))
test2<-as.data.frame(sapply(test,as.numeric))
train2$weekday<-as.factor(train$weekday)
test2$weekday<-as.factor(test$weekday)

min(test$postdate)

diff=as.Date(strptime(train$postdate, "%Y-%m-%d"))-as.Date(strptime("2018-01-01", "%Y-%m-%d"))
diffNum=as.numeric(diff)

train2$postdate<-diffNum
diff2=as.Date(strptime(test$postdate, "%Y-%m-%d"))-as.Date(strptime("2018-01-01", "%Y-%m-%d"))
diffNum2=as.numeric(diff2)
test2$postdate<-diffNum2


#posts_sm<-Matrix(posts, sparse = TRUE)

big.tree =rpart(likes~.,method="anova",data=train,
                control=rpart.control(minsplit=5,cp=.0001))
nbig =length(unique(big.tree$where))
cpvec = big.tree$cptable[,"CP"]
ntree =length(cpvec)
in_bag_error =rep(0,ntree)
out_bag_error =rep(0,ntree)
tree_size =rep(0,ntree)



for(i in 1:ntree) {
  temptree =prune(big.tree,cp=cpvec[i])
  tree_size[i] =length(unique(temptree$where))
  in_bag_error[i] =sum((train$likes-round(predict(temptree)))^2)
  ofit =round(predict(temptree,test))
  out_bag_error[i] =sum((test$likes-ofit)^2)}

out_bag_error=sqrt(out_bag_error/nrow(test))
in_bag_error =sqrt(in_bag_error/nrow(train))


rgl =range(c(in_bag_error,out_bag_error))
plot(tree_size, in_bag_error, type = "l", col = "red", lwd = 3, ylab = "RMSE")
lines(tree_size, out_bag_error, col = "blue", lwd = 3)
legend("topright",legend=c("in-sample","out-of-sample"),lwd=3,col=c("red","blue"))





tree_depth =c(5, 10, 15, 20)
tree_num =c(1000, 2000, 5000)
lambda=c(.001, 0.01, 0.1)
Boosting_params =expand.grid(tree_depth,tree_num,lambda)
num_params =nrow(Boosting_params)
out_bag_error =rep(0,num_params)
in_bag_error =rep(0,num_params)
results_boost =vector("list",num_params)
for(i in 1:num_params) 
  {boost_fit =gbm(likes~.,
                  data=train2,
                  distribution="gaussian",
                  interaction.depth=Boosting_params[i,1],
                  n.trees=Boosting_params[i,2],
                  shrinkage=Boosting_params[i,3])
  pred_in_bag =round(predict(boost_fit,
                             n.trees=Boosting_params[i,2]))
  pred_out_bag=round(predict(boost_fit,newdata=test2,
                             n.trees=Boosting_params[i,2]))
  out_bag_error[i] =sum((test2$likes-pred_out_bag)^2)
  in_bag_error[i] =sum((train2$likes-pred_in_bag)^2)
  results_boost[[i]]=boost_fit}
in_bag_error =round(sqrt(in_bag_error/nrow(train2)),3)
out_bag_error =round(sqrt(out_bag_error/nrow(test2)),3)
















tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$MissClass, probs = probs), min(x$results$MissClass))) +
    theme_bw()
}

mC = function(data, lev=NULL, model=NULL){
  mC_val = mean(data$pred != data$obs)
  c(MissClass = mC_val)
}

fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 5,
  summaryFunction = mC,
  returnResamp = "final")
fitControl2 <- trainControl(## 5-fold CV
  method = "cv",
  number = 5,
  returnResamp = "final")

cl <- makePSOCKcluster(5)
registerDoParallel(cl)



set.seed(1)
fit1 <- train(likes ~ .,
              data=train,
              method = 'glm',
              family=binomial(),
              metric='MissClass',
              maximize=FALSE,
              trControl = fitControl,
              na.action  = na.pass)

lassoGrid=expand.grid(alpha = 1,
                      lambda = 0.009278)



