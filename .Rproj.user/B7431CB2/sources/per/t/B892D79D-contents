library(tidyverse)
library(keras)


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

test_all = cbind(model.matrix(likesfoll ~ isMulti + weekday + period + hash_ct + mention_ct + emoji_ct -1 ,
                               data=test),
                  as.matrix(hash_m[all_posts$train==0,]),
                  as.matrix(ats_m[all_posts$train==0,]),
                  as.matrix(alt_m[all_posts$train==0,]),
                  as.matrix(posts_m[all_posts$train==0,]))

#train_all = as(model.matrix(train$likesfoll ~.^2 -1,
#                                data=as.data.frame(train_all)),'sparseMatrix')


vocab_size <- dim(train_all)[2]

model <- keras_model_sequential()
model %>% 
  layer_embedding(input_dim = vocab_size, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% summary()

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)

history <- model %>% fit(
  train_all,
  train$likes_bin2=='high',
  epochs = 40,
  batch_size = 128,
  validation_data = list(test_all, test$likes_bin2=='high'),
  verbose=1
)


set.seed(13)
samp2000 = sample(nrow(train_all), 2000)

subtrain = train_all[samp2000,]
