library(randomForest)
library(tree)
library(caret)
library(doParallel)

load("clean_autos.Rdata")
set.seed(1)

# multicore solution from http://topepo.github.io/caret

cl <- makePSOCKcluster(12)
registerDoParallel(cl)

trainctrl <- trainControl(
  method = "cv",
  verboseIter = TRUE)

trys <- 5*(5:15)
samples <- 2^(7:14)
rf.sampled <- list()
train.sample <- train[sample(1:nrow(train), nrow(train)),]

for (N in samples) {
  print(paste0("samples: ", N))
  
  start.time <- proc.time()
  rf <- train(price ~. -postalCode -district -model -seller -offerType -lastSeen -dateCrawled -dateCreated,
                      data = train.sample[1:N],
                      method = 'rf',
                      ntree = 500,
                      tuneGrid = data.frame(mtry = trys),
                      trControl = trainctrl
  )
  stop.time <- proc.time()
  run.time <- stop.time - start.time
  print(run.time)
  
  rf$test.mse <- mean((predict(rf, newdata = test) - test$price)^2)
  rf$run.time <- run.time[3]
  rf$N <- N
  rf.sampled[[length(rf.sampled) + 1]] <- rf
  
}

stopCluster(cl)

save(rf.sampled, file = "rf.sampled.Rdata")

# full model at mtry = 40
cl <- makePSOCKcluster(12)
registerDoParallel(cl)


trys <- 40

start.time <- proc.time()
rf <- train(price ~. -postalCode -district -model -seller -offerType -lastSeen -dateCrawled -dateCreated,
            data = train,
            method = 'rf',
            ntree = 500,
            tuneGrid = data.frame(mtry = trys),
            trControl = trainctrl
)
stop.time <- proc.time()
run.time <- stop.time[3] - start.time[3]
print(run.time)

test.mse <- mean((predict(rf, newdata = test) - test$price)^2)
print(test.mse)

rf.model <- rf$finalModel
rf.caret.model <- rf
save(rf.caret.model, test.mse, run.time, file = "rf.final.caret.Rdata")
save(rf.model, test.mse, file = "rf.final.Rdata")
  
stopCluster(cl)
plot(rf.model)