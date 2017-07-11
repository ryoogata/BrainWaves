# N <- 10
# LENGTH <- length(train[1,-c(1, 30002)])
# ROW <- train[1,-c(1, 30002)]


maxPooling <- function(ROW, N = 10, LENGTH = 30000){
  # N 刻みの vector を作成
  START <- seq(1, LENGTH, by = N)

  # 空の vector の作成
  RESULT <- as.numeric(NULL)

  # Max Pooling の実行
  for( i in START ){
   RESULT <- append( RESULT , (max(ROW[i:( i + N - 1)])))
  }

  return(RESULT)
}