library(tidyverse)
library(caret)
library(doParallel)
library(rpart.plot)
library(Matrix)
library(glmnet)


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
         emoji_flag = emoji_ct>0,
         likes_bin2high = as.factor(likes_bin3=='high'),
         likesfoll = likes/followers)


train = all_posts[all_posts$train==1,] %>% select(-train)
test = all_posts[!all_posts$train==1,] %>% select(-train)


train_all = cbind(model.matrix(likesfoll ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct -1 ,
                               data=train),
                  as.matrix(hash_m[all_posts$train==1,]),
                  as.matrix(ats_m[all_posts$train==1,]),
                  as.matrix(alt_m[all_posts$train==1,]),
                  as.matrix(posts_m[all_posts$train==1,]))

train_all = as(model.matrix(train$likesfoll ~.^2 -1,
                                data=as.data.frame(train_all)),'sparseMatrix')

set.seed(13)
samp2000 = sample(nrow(train_all), 2000)

subtrain = train_all[samp2000,]

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
fit2 <- train(likes/followers ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct,
              data=train,
              method="glm",
              trControl = fitControl)



fit9 <- train(
  y=train$likesfoll[samp2000],
  x=subtrain,
  method = "glmnet",
  trControl = fitControl,
  tuneLength = 10
)
