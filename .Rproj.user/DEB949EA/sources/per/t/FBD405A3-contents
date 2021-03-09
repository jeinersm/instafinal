PackageList=c('caret','tidyverse','scales','kableExtra','doParallel','mclust',
              'gridExtra','gamlr','gbm','glmnet','randomForest','rpart.plot',
              'xgboost','ranger','data.table','rpart','feather','cld2',
              'tm','Matrix', 'topicmodels', 'maptpx', 'wordcloud','e1071')

NewPackages=PackageList[!(PackageList %in% 
                            installed.packages()[,"Package"])]
if(length(NewPackages)) install.packages(NewPackages)