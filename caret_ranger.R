require(caret)
require(caretEnsemble)
require(doParallel)
require(dplyr)

require(ranger)

source("script/R/fun/tools.R")
source("script/R/fun/rsquaredSummary.R")

# 保存した結果の読み込み
RESULT_DF <- "result.ranger.df"

if ( file.exists(paste0("result/", RESULT_DF, ".data")) ) {
  assign(RESULT_DF, readRDS(paste0("result/", RESULT_DF, ".data")))
}


#
# 前処理
#
source("script/R/fun/Data-pre-processing.R")

#my_preProcess <- c("center", "scale")
my_preProcess <- NULL

data_preProcess <- "none"

if ( data_preProcess == "none") {
  TRAIN <- all.train
  TRAIN.TRAIN <- train.train
  #TRAIN.TRAIN <- TRAIN
  TRAIN.TEST <- train.test
  TEST <- test
} else if ( data_preProcess == "nzv") {
  TRAIN <- all.nzv.train
  TRAIN.TRAIN <- train.nzv.train
  TRAIN.TEST <- train.nzv.test
  TEST <- test
} else if ( data_preProcess == "dummy") {
  TRAIN <- train.dummy
  TRAIN.TRAIN <- train.dummy.train
  TRAIN.TEST <- train.dummy.test
  TEST <- test.dummy
} else if ( data_preProcess == "dummy.nzv.highlyCorDescr") {
  TRAIN <- train.dummy.nzv.highlyCorDescr
  TRAIN.TRAIN <- train.dummy.nzv.highlyCorDescr.train
  TRAIN.TEST <- train.dummy.nzv.highlyCorDescr.test
  TEST <- test.dummy.nzv.highlyCorDescr
}


#
# ranger
#

# 変数指定 ( 共通設定)
nresampling <- 10
n_repeats <- 10
METHOD <- "cv" # "repeatedcv", "boot"

# 変数指定 ( モデル個別 )
MTRY <- c(50:60)

# seeds の決定
set.seed(123)
SEEDS <- vector(mode = "list", length = n_repeats * nresampling)
for(i in 1:(n_repeats * nresampling)) SEEDS[[i]] <- sample.int(1000, length(MTRY))
SEEDS[[n_repeats * nresampling + 1]] <- sample.int(1000, 1)

# 再抽出方法毎に INDEX を作成
set.seed(123)
if ( METHOD == "cv" ) {
  INDEX <- createFolds(TRAIN.TRAIN$response, k = nresampling, returnTrain = TRUE)
} else if ( METHOD == "repeatedcv" ){
  INDEX <- createMultiFolds(TRAIN.TRAIN$response, k = nresampling, times = n_repeats)
} else if ( METHOD == "boot" ){
  INDEX <- createResample(TRAIN.TRAIN$response, n_repeats)
}

set.seed(123)
doParallel <- trainControl(
  method = METHOD
  ,number = nresampling
  ,repeats = n_repeats
  ,classProbs = FALSE
  ,allowParallel = TRUE
  ,verboseIter = TRUE
  ,summaryFunction = rsquaredSummary
  ,savePredictions = "final"
  ,index = INDEX
  ,seeds = SEEDS
)


# 説明変数一覧の作成
explanation_variable <- names(subset(TRAIN, select = -c(response, id)))

cl <- makeCluster(detectCores(), type = 'PSOCK', outfile = " ")
registerDoParallel(cl)

model_list <- caretList(
  x = TRAIN.TRAIN[,explanation_variable]
  ,y = TRAIN.TRAIN$response
  ,trControl = doParallel
  ,preProcess = my_preProcess
  ,tuneList = list(
    fit = caretModelSpec(
      method = "ranger"
      ,metric = "Rsquared" 
      ,tuneGrid = expand.grid(
        mtry = MTRY
      )
      ,importance = 'impurity'
    )
  )
)

model_list[[1]]$times
model_list[[1]]$finalModel

#
# モデル比較
#
allPrediction <- caret::extractPrediction(
                              list(model_list[[1]])
                              ,testX = subset(TRAIN.TEST, select = explanation_variable)
                              ,testY = unlist(subset(TRAIN.TEST, select = c(response)))
                             )

# dataType 列に Test と入っているもののみを抜き出す
testPrediction <- subset(allPrediction, dataType == "Test")
tp <- subset(testPrediction, object == "Object1")

# 精度確認 ( 決定係数: coefficient of determination )
rsquaredSummary(tp)

# 結果の保存
if (exists(RESULT_DF)){
  assign(RESULT_DF, dplyr::bind_rows(list(eval(parse(text = RESULT_DF)), summaryResult.Rsquared(model_list[[1]]))))
} else {
  assign(RESULT_DF, summaryResult.Rsquared(model_list[[1]]))
}
saveRDS(eval(parse(text = RESULT_DF)), paste0("result/", RESULT_DF, ".data"))


# predict() を利用した検算 
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test.verification <- predict(model_list[[1]], TRAIN.TEST, type="raw")
} else {
  # preProcess を指定している場合
  pred_test.verification <- preProcess(
    subset(TRAIN.TEST, select = -c(response))
    ,method = my_preProcess
  ) %>%
  predict(model_list[[1]], ., type="raw")
}

# 精度確認 ( 決定係数: coefficient of determination )
rsquaredSummary(data.frame(pred = pred_test.verification, obs = tp$obs))


# 学習用データ全てを利用してデルを作成
finalModel <- caretList(
  x = TRAIN[,explanation_variable]
  ,y = TRAIN$response
  ,trControl = doParallel
  #,preProcess = my_preProcess
  ,tuneList = list(
    fit = caretModelSpec(
      method = "ranger"
      ,metric = "Rsquared" 
      ,tuneGrid = expand.grid(
        mtry = model_list[[1]]$bestTune["mtry"]
      )
      ,importance = 'impurity'
    )
  )
)

stopCluster(cl)
registerDoSEQ()

#
# 評価用データにモデルの当てはめ
#
if (is.null(finalModel[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test <- predict(finalModel[[1]], TEST, type="raw")
  
  PREPROCESS <- "no_preProcess"
} else {
  # preProcess を指定している場合
  pred_test <- preProcess(
      TEST
      ,method = my_preProcess
    ) %>%
      predict(finalModel[[1]], ., type="raw")
  
  PREPROCESS <- paste(my_preProcess, collapse = "_")
}

#submitの形式で出力(CSV)
#データ加工
out <- data.frame(TEST$id, pred_test)
out$pred_test <- round(out$pred_test)

sapply(out, function(x) sum(is.na(x)))


# 予測データを保存
for(NUM in 1:10){
  DATE <- format(jrvFinance::edate(from = Sys.Date(), 0), "%Y%m%d")
  SUBMIT_FILENAME <- paste("./submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_ranger.csv", sep = "")
  
  if ( !file.exists(SUBMIT_FILENAME) ) {
    write.table(out, #出力データ
                SUBMIT_FILENAME, #出力先
                quote = FALSE, #文字列を「"」で囲む有無
                col.names = FALSE, #変数名(列名)の有無
                row.names = FALSE, #行番号の有無
                sep = "," #区切り文字の指定
    )
    break
  }
}

