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

tpc10 = readRDS('./02.data.cleaning/topics10.rds')
tpc30 = readRDS('./02.data.cleaning/topics30.rds')


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

cl <- makePSOCKcluster(4)
registerDoParallel(cl)
fit3 = train(y=train$likes/train$followers,
                    x=train_all,
                    method='rpart',
                    tuneLength=15,
                    trControl = fitControl)
registerDoSEQ()
stopCluster(cl)


###Linear fit with flags
# fit2 <- train(likes_scaled ~ isMulti + weekday + period + hash_flag + 
#                 mention_flag + emoji_flag,
#               data=train,
#               method = 'lm',
#               trControl = fitControl)
# saveRDS(fit2, file='./03.analysis/models/scaled_ols_flags.rds')
# 
# fit2 = readRDS('./03.analysis/models/scaled_ols_flags.rds')


###Full dataset with all word matrices
train_all = cbind(model.matrix(likes_scaled ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct -1 ,
                          data=train),
                  as.matrix(hash_m[all_posts$train==1,]),
                  as.matrix(ats_m[all_posts$train==1,]),
                  as.matrix(alt_m[all_posts$train==1,]),
                  as.matrix(posts_m[all_posts$train==1,]))

###Dataset with topic models;
train_tpc10 = train %>% 
  left_join(tpc10, by='url') %>%
  replace(is.na(.),0) %>%
  model.matrix(likes_scaled ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct +
                 topic1 + topic2 + topic3 + topic4 + topic5 + topic6 + topic7 + topic8 + topic9 + topic10 -1, 
               data=.)
train_tpc30 = train %>% 
  left_join(tpc30, by='url') %>%
  replace(is.na(.),0) %>%
  model.matrix(likes_scaled ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct +
                 topic1 + topic2 + topic3 + topic4 + topic5 + topic6 + topic7 + topic8 + topic9 + topic10 + 
                 topic11 + topic12 + topic13 + topic14 + topic15 + topic16 + topic17 + topic18 + topic19 + 
                 topic20 + topic21 + topic22 + topic23 + topic24 + topic25 + topic26 + topic27 + topic28 + 
                 topic29 + topic30-1, 
               data=.)


###2000 obs sample for tuning
samp2000 = sample(nrow(train_all), 2000)
subtrain = train_all[samp2000,]

###Simple Decision Tree
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl)
# fit3 = train(y=train$likes_scaled,
#                     x=train_all,
#                     method='rpart',
#                     tuneLength=50,
#                     trControl = fitControl)
# registerDoSEQ()
# stopCluster(cl)
# saveRDS(fit3, file='./03.analysis/models/scaled_tree.rds')
# 
# fit3 = readRDS('./03.analysis/models/scaled_ols_flags.rds')
# rpart.plot(fit3$finalModel)



###Random Forest
# rnggrid = expand.grid(
#   mtry=c(5,50,100,150,250,500),
#   splitrule = c('maxstat','variance','extratrees'),
#   min.node.size = c(3,5,10))
# set.seed(1)
# fit4_test <- train(y=train$likes_scaled[samp2000],
#               x=subtrain,
#               method = 'ranger',
#               importance='impurity',
#               tuneGrid=rnggrid,
#               trControl = fitControl)
# ##The final values used for the model were mtry = 500, splitrule = maxstat and min.node.size = 10.
# set.seed(1)
# rnggrid = expand.grid(
#   mtry=c(500),
#   splitrule = c('maxstat'),
#   min.node.size = c(10))
# fit4 <- train(y=train$likes_scaled,
#               x=train_all,
#               method = 'ranger',
#               importance='impurity',
#               tuneGrid=rnggrid,
#               trControl = fitControl)
# saveRDS(fit3, file='./03.analysis/models/scaled_ranger_all.rds')
# 
# fit4 = readRDS('./03.analysis/models/scaled_ranger_all.rds')


# xgbgrid = expand.grid(nrounds=c(seq(50,1000,by=50)),
#                       eta = c(0.01),
#                       max_depth = c(5,10,15),
#                       colsample_bytree = .8,
#                       min_child_weight = c(1,3,6),
#                       subsample=.8,
#                       gamma=0)
# set.seed(1)
# fit5tune <- train(y=train$likes_scaled[samp2000],
#               x=subtrain,
#               method = 'xgbTree',
#               verbose=TRUE,
#               tuneGrid=xgbgrid,
#               trControl = fitControl)
##The final values used for the model were nrounds = 250, max_depth = 10, eta = 0.01, gamma = 0, colsample_bytree = 0.8, min_child_weight = 6 and subsample = 0.8.
# xgbgrid = expand.grid(nrounds=c(seq(50,1000,by=50)),
#                       eta = c(0.01),
#                       max_depth = c(10),
#                       colsample_bytree = .8,
#                       min_child_weight = c(6),
#                       subsample=.8,
#                       gamma=0)
# set.seed(1)
# fit5 <- train(y=train$likes_scaled,
#               x=train_all,
#               method = 'xgbTree',
#               verbose=TRUE,
#               tuneGrid=xgbgrid,
#               trControl = fitControl)
# saveRDS(fit5, file='./03.analysis/models/scaled_xgboost_all.rds')

plot(varImp(fit5), 10, main='Top 10 Variables')


# xgbgrid = expand.grid(nrounds=c(seq(50,2000,by=50)),
#                       eta = c(0.01),
#                       max_depth = c(1,2,3),
#                       colsample_bytree = .8,
#                       min_child_weight = c(1,3,6),
#                       subsample=.8,
#                       gamma=0)
# set.seed(1)
# fit6tune <- train(y=train$likes_scaled[samp2000],
#               x=train_tpc10[samp2000,],
#               method = 'xgbTree',
#               verbose=TRUE,
#               tuneGrid=xgbgrid,
#               trControl = fitControl)
#The final values used for the model were nrounds = 800, max_depth = 1, eta = 0.01, gamma =
#0, colsample_bytree = 0.8, min_child_weight = 6 and subsample = 0.8.
# xgbgrid = expand.grid(nrounds=c(seq(50,2000,by=50)),
#                       eta = c(0.01),
#                       max_depth = c(1),
#                       colsample_bytree = .8,
#                       min_child_weight = c(6),
#                       subsample=.8,
#                       gamma=0)
# set.seed(1)
# fit6 <- train(y=train$likes_scaled,
#                   x=train_tpc10,
#                   method = 'xgbTree',
#                   verbose=TRUE,
#                   tuneGrid=xgbgrid,
#                   trControl = fitControl)
# saveRDS(fit6, file='./03.analysis/models/scaled_xgboost_tpc10.rds')

# xgbgrid = expand.grid(nrounds=c(seq(50,2000,by=50)),
#                       eta = c(0.01),
#                       max_depth = c(1),
#                       colsample_bytree = .8,
#                       min_child_weight = c(3),
#                       subsample=.8,
#                       gamma=0)
# set.seed(1)
# fit7 <- train(y=train$likes_scaled,
#               x=train_tpc10,
#               method = 'xgbTree',
#               verbose=TRUE,
#               tuneGrid=xgbgrid,
#               trControl = fitControl)
# saveRDS(fit7, file='./03.analysis/models/scaled_xgboost_tpc30.rds')
