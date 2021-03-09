library(tm)
library(Matrix)
library(tidyverse)

all_posts = readRDS('./02.data.cleaning/all_posts_clean.rds')

posts = as.data.frame(all_posts[c('url','desc_clean')])
names(posts) = c('doc_id','text')

twCorpus = Corpus(DataframeSource(posts))
twCorpus = tm_map(twCorpus, content_transformer(tolower))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
twCorpus = tm_map(twCorpus, content_transformer(removeNumPunct))
#Remove Stop Words
stops = c(stopwords('english'),'amp', 'see', 'can','now')
twCorpus = tm_map(twCorpus, removeWords, stops) 
twCorpus = tm_map(twCorpus, stripWhitespace)

#stem completion copy
twCorpusCopy = twCorpus

#stem words
twCorpus = tm_map(twCorpus, stemDocument)

# Create the dtm from the corpus: 
# Minimum lenghth of 3, minimum frequency of 10x
posts_dtm <- DocumentTermMatrix(twCorpus,
                                 control=list(wordLengths = c(3, Inf),
                                              bounds = list(global = c(10,Inf))))
# Print out tweets_dtm data
posts_dtm

removeCommonTerms <- function (x, pct) 
{
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
            is.numeric(pct), pct > 0, pct < 1)
  m <- if (inherits(x, "DocumentTermMatrix")) 
    t(x)
  else x
  t <- table(m$i) < m$ncol * (pct)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix")) 
    x[, termIndex]
  else x[termIndex, ]
}

posts_dtm = removeCommonTerms(posts_dtm, .70)
posts_dtm
posts_dtm = removeSparseTerms(posts_dtm, 0.995)
posts_dtm

# Save the dtm for topic models
saveRDS(posts_dtm, file='./02.data.cleaning/word_dtm.rds')

# Convert tweets_dtm to a matrix: tweets_m
posts_m <- Matrix(as.matrix(posts_dtm),sparse = TRUE)
# Print the dimensions of tweets_m
dim(posts_m)
#dim(tweets_m_big)

count_posts = as.data.frame(colSums(as.data.frame(as.matrix(posts_dtm))))




###emoji
emoji = as.data.frame(all_posts[c('url','emoji_text')])
names(emoji) = c('doc_id','text')

twCorpus = Corpus(DataframeSource(emoji))

# Create the dtm from the corpus: 
# Minimum length of 1, minimum frequency of 10x
emoji_dtm <- DocumentTermMatrix(twCorpus,
                                control=list(wordLengths = c(1, Inf),
                                             bounds = list(global = c(10,Inf))))
inspect(emoji_dtm)

emoji_dtm = removeCommonTerms(emoji_dtm, .70)
emoji_dtm
emoji_dtm = removeSparseTerms(emoji_dtm, 0.995)
emoji_dtm

# Convert tweets_dtm to a matrix: tweets_m
emoji_m <- Matrix(as.matrix(emoji_dtm),sparse = TRUE)
dim(emoji_m)

##What are the most common emoji?
count_emoji = as.data.frame(colSums(as.data.frame(as.matrix(emoji_dtm))))



###alt_text
alt_text = as.data.frame(all_posts[c('url','alt')])
names(alt_text) = c('doc_id','text')
alt_text$text <- gsub("[[:digit:]]+\\s","",alt_text$text)
alt_text$text <- gsub("one or more ","",alt_text$text)
alt_text$text <- gsub("\\s","_",alt_text$text)
alt_text$text <- gsub(",_"," ",alt_text$text)
alt_text$text <-trimws(alt_text$text)
prefix <- function(x){
  vec = strsplit(x," ")
  newvec = c()
  for(i in vec){
    if (length(i)>=1){
      newvec = c(newvec,paste0('contains_',i))
    }
  }
  return(paste(newvec,collapse=' '))
}
alt_text$text <- lapply(alt_text$text,prefix)

aCorpus = Corpus(DataframeSource(alt_text))

# Create the dtm from the corpus: 
# Minimum length of 1, minimum frequency of 10x
alt_dtm <- DocumentTermMatrix(aCorpus,
                                control=list(wordLengths = c(1, Inf),
                                             bounds = list(global = c(10,Inf))))
inspect(alt_dtm)

alt_dtm = removeCommonTerms(alt_dtm, .70)
alt_dtm
alt_dtm = removeSparseTerms(alt_dtm, 0.995)
alt_dtm

# Convert tweets_dtm to a matrix: tweets_m
alt_m <- Matrix(as.matrix(alt_dtm),sparse = TRUE)
dim(alt_m)

##What are the most common alt_text?
count_alttext = as.data.frame(colSums(as.data.frame(as.matrix(alt_dtm))))



###hash
hash = as.data.frame(all_posts[c('url','hashtags')])
names(hash) = c('doc_id','text')

twCorpus = Corpus(DataframeSource(hash))

# Create the dtm from the corpus: 
# Minimum length of 1, minimum frequency of 10x
hash_dtm <- DocumentTermMatrix(twCorpus,
                                control=list(wordLengths = c(1, Inf),
                                             bounds = list(global = c(10,Inf))))
inspect(hash_dtm)

hash_dtm = removeCommonTerms(hash_dtm, .70)
hash_dtm
hash_dtm = removeSparseTerms(hash_dtm, 0.999)
hash_dtm

# Convert tweets_dtm to a matrix: tweets_m
hash_m <- Matrix(as.matrix(hash_dtm),sparse = TRUE)
dim(hash_m)

##What are the most common hash?
count_hash = as.data.frame(colSums(as.data.frame(as.matrix(hash_dtm))))


###ats
ats = as.data.frame(all_posts[c('url','mentions')])
names(ats) = c('doc_id','text')

twCorpus = Corpus(DataframeSource(ats))

# Create the dtm from the corpus: 
# Minimum length of 1, minimum frequency of 10x
ats_dtm <- DocumentTermMatrix(twCorpus,
                               control=list(wordLengths = c(1, Inf),
                                            bounds = list(global = c(10,Inf))))
inspect(ats_dtm)

ats_dtm = removeCommonTerms(ats_dtm, .70)
ats_dtm
ats_dtm = removeSparseTerms(ats_dtm, 0.999)
ats_dtm

# Convert tweets_dtm to a matrix: tweets_m
ats_m <- Matrix(as.matrix(ats_dtm),sparse = TRUE)
dim(ats_m)

##What are the most common ats?
count_ats = as.data.frame(colSums(as.data.frame(as.matrix(ats_dtm))))

saveRDS(posts_m, file='./02.data.cleaning/word_matrix.rds')
saveRDS(emoji_m, file='./02.data.cleaning/emoji_matrix.rds')
saveRDS(hash_m, file='./02.data.cleaning/hashtag_matrix.rds')
saveRDS(ats_m, file='./02.data.cleaning/ats_matrix.rds')
saveRDS(alt_m, file='./02.data.cleaning/alt_matrix.rds')

