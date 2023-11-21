library(h2o)
h2o.init()

train_data <- h2o.importFile("1-data/full_data.csv")
test_data <- h2o.importFile("1-data/test_data.csv")

summary(train_data)
colnames(test_data)

y <- "y"
x <- setdiff(names(train_data), c(y, "id"))

train_data$y <- as.factor(train_data$y)

splits <- h2o.splitFrame(train_data, c(0.6, 0.2), seed = 123)
train <- h2o.assign(splits[[1]], "train")
valid <- h2o.assign(splits[[2]], "valid")
test <- h2o.assign(splits[[3]], "test")

auto_model_generator <- h2o.automl(x = x,
                                   y = y,
                                   training_frame = train,
                                   validation_frame = valid,
                                   max_runtime_secs = 300,
                                   sort_metric = "AUC",
                                   max_runtime_secs_per_model = 30,
                                   exclude_algos = c("StackedEnsemble"))

auto_model_generator@leaderboard

selected_model <- auto_model_generator@leader
selected_model_performance <- h2o.performance(selected_model, newdata = test)
selected_model_auc <- selected_model_performance@metrics$ausc

prediction_results <- predict(selected_model, newdata = test_data)

prediction_results %>%
  as.tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select (id, y) %>%
  write_csv("5-predictions/predictions1.csv")

h2o.saveModel(selected_model, "4-model/GBM_Model_V1")

gbm_model <- h2o.gbm(x = x,
                     y = y,
                     training_frame = train,
                     validation_frame = valid)

gbm_test_performance <- h2o.performance(gbm_model, newdata = test)
gbm_auc = gbm_test_performance@metrics$AUC
