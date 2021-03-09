library(tidyverse)
library(caret)
library(doParallel)
library(rpart.plot)


all_posts = readRDS('./02.data.cleaning/all_posts_clean.rds')
posts_m = readRDS("./02.data.cleaning/word_matrix.rds")
emoji_m = readRDS('./02.data.cleaning/emoji_matrix.rds')
hash_m = readRDS('./02.data.cleaning/hashtag_matrix.rds')
ats_m = readRDS('./02.data.cleaning/ats_matrix.rds')
alt_m = readRDS('./02.data.cleaning/alt_matrix.rds')

all_posts$likes_scaled = as.numeric(all_posts$likes_scaled)
all_posts$period = 'Early'
all_posts$period[as.numeric(all_posts$hour) > 6] = 'Morning'
all_posts$period[as.numeric(all_posts$hour) > 12] = 'Midday'
all_posts$period[as.numeric(all_posts$hour) > 18] = 'Evening'
all_posts$period= as.factor(all_posts$period)


##drop unecessary vars
all_posts = all_posts %>%
  select(-c(alt, desc, post_ct, scrapedate, date, mentions, hashtags, emoji,
            emoji_text, days))

##create flags
all_posts = all_posts %>%
  mutate(hash_flag = hash_ct>0,
         mention_flag = mention_ct>0,
         emoji_flag = emoji_ct>0)


train = all_posts[all_posts$train==1,] %>% select(-train)
test = all_posts[!all_posts$train==1,] %>% select(-train)

###tuning plot function
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}


fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 4,
  returnResamp = "final")

#cl <- makePSOCKcluster(10)
#registerDoParallel(cl)

set.seed(1)
fit1 <- train(likes_scaled ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct,
              data=train,
              method = 'lm',
              trControl = fitControl)

set.seed(1)
fit2 <- train(likes_scaled ~ isMulti + weekday + period + hash_flag + 
                mention_flag + emoji_flag,
              data=train,
              method = 'lm',
              trControl = fitControl)



##random forest

train_all = cbind(model.matrix(likes_scaled ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct -1 ,
                          data=all_posts),
                  as.matrix(hash_m),
                  as.matrix(ats_m),
                  as.matrix(alt_m),
                  as.matrix(posts_m))


samp2000 = sample(nrow(train_all), 2000)

subtrain = train_all[samp2000,]
subtrain2 = train_all2[samp2000,]

follow_tree = train(y=all_posts$likes,
                    x=train_all,
                    method='rpart',
                    #cp=.008,
                    tuneLength=10,
                    trControl = fitControl)

rpart.plot(follow_tree$finalModel)

gbmgrid = expand.grid(interaction.depth = c(5,10,15),
                      n.trees = c(50,100,150,200,250,500,750,1000,1250,1500,1750,2000,2500,3000),
                      shrinkage = 1e-3,
                      n.minobsinnode = c(3))
set.seed(1)
fit4 <- train(y=train$likes_scaled[samp1000],
              x=subtrain,
              method = 'gbm',
              tuneGrid = gbmgrid,
              trControl = fitControl)

gbmgrid = expand.grid(interaction.depth = c(15),
                      n.trees = c(1750),
                      shrinkage = 1e-3,
                      n.minobsinnode = c(3))
set.seed(1)
fit4b <- train(y=train$likes_scaled,
              x=train_all,
              method = 'gbm',
              tuneGrid = gbmgrid,
              trControl = fitControl)

rnggrid = expand.grid(
  mtry=c(5,50,100,150,250,500),
  splitrule = c('gini','variance','extratrees'),
  min.node.size = c(3,5,10))
set.seed(1)
fit3 <- train(y=train$likes_scaled[samp2000],
              x=subtrain,
              method = 'ranger',
              importance='impurity',
              tuneGrid=rnggrid,
              trControl = fitControl)

registerDoSEQ()
stopCluster(cl)


xgbgrid = expand.grid(nrounds=c(seq(50,1000,by=50)),
                      eta = c(0.01),
                      max_depth = c(5,10,15),
                      colsample_bytree = .8,
                      min_child_weight = c(1,3,6),
                      subsample=.8,
                      gamma=0)
set.seed(1)
fit5 <- train(y=train$likes_scaled[samp2000],
              x=subtrain,
              method = 'xgbTree',
              verbose=TRUE,
              tuneGrid=xgbgrid,
              trControl = fitControl)
