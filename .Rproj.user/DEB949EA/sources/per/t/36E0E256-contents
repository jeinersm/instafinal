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

all_posts = all_posts %>%
  group_by(user) %>%
  mutate(mean_likes = mean(likes),
            std_likes = sd(likes)) %>%
  ungroup()

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


###Full dataset with all word matrices
test_all = cbind(model.matrix(likes_scaled ~  isMulti + weekday + period + hash_ct + mention_ct + emoji_ct + followers -1 ,
                               data=test),
                  as.matrix(hash_m[all_posts$train==0,]),
                  as.matrix(ats_m[all_posts$train==0,]),
                  as.matrix(alt_m[all_posts$train==0,]),
                  as.matrix(posts_m[all_posts$train==0,]))

###Dataset with topic models;
test_tpc10 = test %>% 
  left_join(tpc10, by='url') %>%
  replace(is.na(.),0) %>%
  model.matrix(likes_scaled ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct +
                 topic1 + topic2 + topic3 + topic4 + topic5 + topic6 + topic7 + topic8 + topic9 + topic10 -1, 
               data=.)
test_tpc30 = test %>% 
  left_join(tpc30, by='url') %>%
  replace(is.na(.),0) %>%
  model.matrix(likes_scaled ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct +
                 topic1 + topic2 + topic3 + topic4 + topic5 + topic6 + topic7 + topic8 + topic9 + topic10 + 
                 topic11 + topic12 + topic13 + topic14 + topic15 + topic16 + topic17 + topic18 + topic19 + 
                 topic20 + topic21 + topic22 + topic23 + topic24 + topic25 + topic26 + topic27 + topic28 + 
                 topic29 + topic30-1, 
               data=.)



###Scaled


test_rmse = function(mdl,x,y,desc){
  pred = predict(mdl,x)
  oos_rmse = postResample(pred=pred,
                          obs=y)[[1]]
  print(paste(desc,oos_rmse, collapse = ": "))
  print(mdl$bestTune)
  return(oos_rmse)
}


fit1 = readRDS('./03.analysis/models/scaled_xgboost_all.rds')
test_rmse(fit1,test_all,test$likes_scaled,'Scaled XGBoost All Vars')

fit1 = readRDS('./03.analysis/models/scaled_xgboost_tpc10.rds')
test_rmse(fit1,test_tpc10,test$likes_scaled,'Scaled XGBoost 10 topics')

fit1 = readRDS('./03.analysis/models/scaled_xgboost_tpc30.rds')
test_rmse(fit1,test_tpc30,test$likes_scaled,'Scaled XGBoost 30 topics')

fit1 = readRDS('./03.analysis/models/scaled_ranger_all.rds')
test_rmse(fit1,test_all,test$likes_scaled,'Scaled ranger all vars')

fit1 = readRDS('./03.analysis/models/scaled_ols_flags.rds')
test_rmse(fit1,test,test$likes_scaled,'OLS flagged variables')

fit1 = readRDS('./03.analysis/models/simple_scaled_ols_flags.rds')
test_rmse(fit1,test,test$likes,'OLS flagged variables')

fit1 = readRDS('./03.analysis/models/ranger_all.rds')
test_rmse(fit1,test_all,test$likes,'ranger followers')

fit1 = readRDS('./03.analysis/models/xgboost_all.rds')
test_rmse(fit1,test_all,test$likes,'xgboost followers')
