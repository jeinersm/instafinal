library(maptpx)
library(wordcloud)

posts_m = readRDS("./02.data.cleaning/word_matrix.rds")
emoji_m = readRDS('./02.data.cleaning/emoji_matrix.rds')
hash_m = readRDS('./02.data.cleaning/hashtag_matrix.rds')
ats_m = readRDS('./02.data.cleaning/ats_matrix.rds')
alt_m = readRDS('./02.data.cleaning/alt_matrix.rds')

#everything
#tpc = topics(cbind(posts_m,emoji_m,hash_m,ats_m),K=c(5, seq(10,100,5),seq(120,200,20),seq(250,450,50)))
#tpc = topics(cbind(posts_m,emoji_m,hash_m,ats_m),K=c(seq(5,15,1)))

#words only
#tpc = topics(posts_m,K=c(5, seq(10,100,5),seq(120,200,20),seq(250,450,50)))
#tpc = topics(posts_m,K=c(seq(35,100,5),seq(120,200,20),seq(250,450,50)))
#tpc = topics(posts_m,K=c(seq(5,15,1)))

#K=8 is winner


set.seed(1)
tpc = topics(cbind(posts_m,emoji_m,hash_m,ats_m),K=10, bf=TRUE)
par(mfrow=c(1,2))
set.seed(4)
wordcloud(row.names(tpc$theta), freq=tpc$theta[,2], min.freq=0.004)
set.seed(123)
wordcloud(row.names(tpc$theta), freq=tpc$theta[,3], min.freq=0.004)
#7 is obvious advertisement/promotional material


set.seed(1)
tpc30 = topics(cbind(posts_m,emoji_m,hash_m,ats_m),K=30, bf=TRUE)

tpc_df = as.data.frame(tpc$omega) %>% setNames(paste0('topic', names(.))) %>% rownames_to_column('url')
tpc30_df = as.data.frame(tpc30$omega) %>% setNames(paste0('topic', names(.))) %>% rownames_to_column('url')

saveRDS(tpc_df, file='./02.data.cleaning/topics10.rds')
saveRDS(tpc30_df, file='./02.data.cleaning/topics30.rds')

