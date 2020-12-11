#' Build RandomForest model
#'
#' Build a RandomForest model
#' @param data A data frame containing element geochemistries of known origin
#' @param classifier The name/number of the column which contains the data to train the model (e.g. eruption, volcano)
#' @return A tephrochron object containing the dataset, the randomForest model, and a range of evaluation objects
#' @examples
#' model.rf <- (data, classifier);
#'
#'
build_rf <- function(data, classifier){
##Build a random forest model using known data
  if(!all(c("SiO2",	"TiO2",	"Al2O3",	"FeOt",	"MnO",	"MgO",	"CaO",	"Na2O",	"K2O") %in% colnames(data))) {
    valid <- c(valid, "@data' must have 'SiO2',	'TiO2',	'Al2O3',	'FeOt',	'MnO',	'MgO',	'CaO',	'Na2O', and	'K2O' columns")
  }

  data.full <- dplyr::select(data, c({{ classifier }}, "SiO2",	"TiO2",	"Al2O3",	"FeOt",	"MnO",	"MgO",	"CaO",	"Na2O",	"K2O"))
  colnames(data.full) <- c("class.rf", "SiO2",	"TiO2",	"Al2O3",	"FeOt",	"MnO",	"MgO",	"CaO",	"Na2O",	"K2O")
  data.full$class.rf <- as.factor(data.full$class.rf)
  data.full <- stats::na.omit(data.full)

  ##Divide the dataset into a training and test dataset
  base::set.seed(1234)
  train <- base::sample(1:nrow(data.full), size = round(nrow(data.full)*2/3), replace = FALSE)
  data.train <- data.full[train,]
  data.test <- data.full[-train,]

##Build the model
model.rf <- randomForest::randomForest(class.rf ~ ., data = data.train)

model.performance.train <- table(stats::predict(model.rf), data.train$class.rf)
error.plot <- graphics::plot(model.rf)
variable.importance <- randomForest::importance(model.rf)
eruption.pred <- stats::predict(model.rf, newdata=data.test)
model.performance.test <- table(eruption.pred, data.test$class.rf)
confusion.matrix <- table(eruption.pred, data.test$class.rf)
accuracy <- (sum(diag(confusion.matrix)))/sum(confusion.matrix)

output <- list(model.rf, error.plot, variable.importance, model.performance.test, confusion.matrix, accuracy)
names(output) <- c("model.rf", "error.plot", "variable.importance", "model.performance.test", "confusion.matrix", "accuracy")

return(output)
}

