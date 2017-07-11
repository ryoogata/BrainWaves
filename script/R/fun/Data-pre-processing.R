require(caret)
require(dplyr)

# 指数表示の回避
options(scipen=10)

#
# データの読み込み
#
#all <- data.table::fread("./data/train20170623.tsv"
#all <- data.table::fread("./data/train.tsv"
#all <- data.table::fread("./data/train.maxPooling10.20170627.tsv"
#all <- data.table::fread("./data/train.maxPooling5.20170701.tsv"
#all <- data.table::fread("./data/train.maxPooling15.20170701.tsv"
#all <- data.table::fread("./data/train.maxPooling8.20170702.tsv"
#all <- data.table::fread("./data/train.maxPooling6.20170702.tsv"
all <- data.table::fread("./data/train.max5min2Pooling.20170711.tsv"
                         ,stringsAsFactors = FALSE
                         ,sep = "\t"
                         ,data.table = FALSE
                         ,encoding = "UTF-8"
                         )

#test <- data.table::fread("./data/test.tsv"
#test <- data.table::fread("./data/test.maxPooling10.20170627.tsv"
#test <- data.table::fread("./data/test.maxPooling5.20170701.tsv"
#test <- data.table::fread("./data/test.maxPooling15.20170701.tsv"
#test <- data.table::fread("./data/test.maxPooling8.20170702.tsv"
#test <- data.table::fread("./data/test.maxPooling6.20170702.tsv"
test <- data.table::fread("./data/test.max5min2Pooling.20170711.tsv"
                         ,stringsAsFactors = FALSE
                         ,sep = "\t"
                         ,data.table = FALSE
                         ,encoding = "UTF-8"
)


# 目的変数名を response に変更
names(all)[names(all) == "Y"] <- "response"

#
# Dummy 変数なし
#

# all から train/test のデータを抽出
all.train <- all
# all.train$response <- as.factor(all.train$response)
# all.test <- all[which(all$data == "test"),]

# 再現性のため乱数シードを固定
#set.seed(10)
set.seed(100)

# 訓練データと検証データに分割する
# Train 用の列番号を作成
# all.train$cv_group <- sample(1:10, nrow(all.train), replace=TRUE, prob=rep(1/10, 10))
# train.train <- all.train[which(all.train$cv_group != 1 & all.train$cv_group != 2), ]
# train.test <- all.train[which(all.train$cv_group == 1 | all.train$cv_group == 2), ]

trainIndex <- createDataPartition(all.train$response, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train.train <- all.train[trainIndex, ]
train.test <- all.train[-trainIndex,]

# 不要な列: id, cv_group を削除
# train.train <- subset(train.train, select = -c(id, cv_group))
# train.test <- subset(train.test, select = -c(id, cv_group))
# all.test <- subset(all.test, select = -c(data))

dim(train.train)
# [1]   122 30002


dim(train.test)
# [1]    22 30002


#
# Dummy 変数なし ( nearZeroVar() 適用 )
#
 
# all.response <- subset(all, select = c(response))
# all.Descr <- subset(all, select = -c(id, response))
# 
# # 情報量が少ない ( 分散がほぼ 0 ) の変数を除く
# nzv <- caret::nearZeroVar(all.Descr)
# 
# names(all)[nzv]
# # [1] "away_11"     "stadium_lng"
# 
# filterdDescr <- all.Descr[,-nzv]
# all.nzv <- cbind(filterdDescr,all.response)
# 
# # all.nzv から train/test のデータを抽出
# all.nzv.train <- all.nzv[which(all.nzv$data == "train"),]
# all.nzv.train$response <- as.factor(all.nzv.train$response)
# all.nzv.test <- all.nzv[which(all.nzv$data == "test"),]
# 
# # 再現性のため乱数シードを固定
# set.seed(10)
# 
# # 訓練データと検証データに分割する
# # Train 用の列番号を作成
# all.nzv.train$cv_group <- sample(1:10, nrow(all.nzv.train), replace=TRUE, prob=rep(1/10, 10))
# train.nzv.train <- all.nzv.train[which(all.nzv.train$cv_group != 1 & all.nzv.train$cv_group != 2), ]
# train.nzv.test <- all.nzv.train[which(all.nzv.train$cv_group == 1 | all.nzv.train$cv_group == 2), ]
# 
# # 不要な列: data, cv_group を削除
# train.nzv.train <- subset(train.nzv.train, select = -c(data, cv_group))
# train.nzv.test <- subset(train.nzv.test, select = -c(data, cv_group))

# dim(train.nzv.train)
# [1] 1580   95

# dim(train.nzv.test)
# [1] 373  95