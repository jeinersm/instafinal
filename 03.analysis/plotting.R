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


fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 4,
  returnResamp = "final")

train_all2 = cbind(model.matrix(likes_scaled ~ followers + isMulti + weekday + period + hash_ct + mention_ct + emoji_ct -1 ,
                                data=train),
                   as.matrix(hash_m[all_posts$train==1,]),
                   as.matrix(ats_m[all_posts$train==1,]),
                   as.matrix(alt_m[all_posts$train==1,]),
                   as.matrix(posts_m[all_posts$train==1,]))

samp2000 = sample(nrow(train_all), 2000)

subtrain = train_all[samp2000,]
subtrain2 = train_all2[samp2000,]

follow_tree = train(y=train$likes[samp2000],
                    x=subtrain2,
                    method='rpart',
                    #cp=.008,
                    tuneLength=10,
                    trControl = fitControl)

rpart.plot(follow_tree$finalModel)


require(data.table)
B<-as.data.table(train_all)


train_all$too_many<-train_all$emoji_ct>=10

(likes_scaled~too_many,data=train_all, main="Do Emoji Predict Number of Likes?",
        xlab="Number of Emoji", ylab="Scaled Likes") 

ggplot(data=train_all,aes(x=too_many,y=mean(likes_scaled)))+geom_bar()

train_all$emoji_groups<-ifelse(train_all$emoji_ct==0,0,ifelse(train_all$emoji_ct<5,"1-5",ifelse(train_all$emoji_ct<10,"6-9","10+")))

train_all$hashes<-ifelse(train_all$hash_ct==0,0,ifelse(train_all$hash_ct==1,1,ifelse(train_all$hash_ct==2,2,ifelse(train_all$hash_ct==3,3,"4+"))))


plot1<-train_all %>%
  group_by(emoji_groups)%>%
  summarise(likes=mean(likes_scaled))

ggplot(data=plot1,aes(x=emoji_groups,y=likes))+geom_bar(stat="identity")+xlab("Number of Emoji in Post")


plot2<-train_all %>%
  group_by(hashes)%>%
  summarise(likes=mean(likes_scaled))

ggplot(data=plot2,aes(x=hashes,y=likes))+geom_bar(stat="identity")+xlab("Number of Hashtags in Post")



emoji_ct           100.00
mention_ct          76.36
contains_people     67.31
hash_ct             62.72
@victoriassecret    61.55
contains_person     51.85
periodMidday        39.84
contains_outdoor    36.93
periodEvening       35.55
weekdaySaturday     35.01
weekdayWednesday    34.60
periodMorning       33.69
contains_standing   33.19
weekdayMonday



