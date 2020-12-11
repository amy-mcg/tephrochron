#' Build a numerical model to classify tephra using major element geochemistry
#'
#' Build a numerical model to classify tephra using major element geochemistry
#' @param data A data frame containing element geochemistries of unknown origin
#' @param classifier The name/number of the column which contains the data to train the model (e.g. eruption, volcano)
#' @return Confusion matrices for a variety of models,
#' @examples
#' data.classified <- (data, model.rf);
#'
#'
#
select_model <- function(data, classifier){
#' Runs multiple numerical models, and allows you to choose the most effective
  if(!all(c("SiO2",	"TiO2",	"Al2O3",	"FeOt",	"MnO",	"MgO",	"CaO",	"Na2O",	"K2O") %in% colnames(data))) {
    valid <- c(valid, "@data' must have 'SiO2',	'TiO2',	'Al2O3',	'FeOt',	'MnO',	'MgO',	'CaO',	'Na2O', and	'K2O' columns")
  }

  data.full <- dplyr::select(data, c({{ classifier }}, "SiO2",	"TiO2",	"Al2O3",	"FeOt",	"MnO",	"MgO",	"CaO",	"Na2O",	"K2O"))
  colnames(data.full) <- c("class.tc", "SiO2",	"TiO2",	"Al2O3",	"FeOt",	"MnO",	"MgO",	"CaO",	"Na2O",	"K2O")
  data.full$class.tc <- as.factor(data.full$class.tc)
  data.full <- na.omit(data.full)

pred.vars <- c('SiO2', 'TiO2', 'Al2O3', 'FeOt', 'MnO', 'MgO', 'CaO', 'Na2O', 'K2O')

data.full$class.tc <- as.factor(data.full$class.tc)
data.full <- na.omit(data.full)

# Normalise data (unsure if this step is necessary? test)
data.full[pred.vars] <- (data.full[pred.vars] / rowSums(data.full[pred.vars])) * 100

base::set.seed(1234)
train <- base::sample(1:nrow(data.full), size = round(nrow(data.full)*2/3), replace = FALSE)
data.train <- data.full[train,]
data.test <- data.full[-train,]

# Train classifiers

# Define preprocessing requirements
pp <- c('center', 'scale')

# Define cross-validation resampling regime using 10fold cross validation (repeatedcv); class probabilities will be computed for (along with predicted values) in each resample
ctrl <- trainControl(method = "repeatedcv", #10fold cross validation
                     repeats = 10, classProbs = TRUE)

seed <- 1234

# Create a tuning grid over which to perform a gridsearch for SVM hyperparameters
svmGrid <- expand.grid(sigma = seq(0.5, 1, 0.05), C = seq(1, 15, 1.5))

# SVM with radial kernel and a custom tuning grid
set.seed(seed)
SVMsigmasource2 <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                         data = data.train, trControl = ctrl,
                         method = "svmRadialSigma", metric = "Kappa", tuneGrid = svmGrid,
                         preProc = pp)


# SVM with radial kernel, set to only provide "raw" classifications, no prob. model.
set.seed(seed)
SVMsigmaRaw <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                     data = data.train, trControl = trainControl(method = "repeatedcv", #10fold cross validation
                                                                     repeats = 10, classProbs = FALSE),
                     method = "svmRadialSigma", metric = "Kappa", tuneLength = 10,
                     preProc = pp)

#CART
set.seed(seed)
CARTsource <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                    data = data.train, trControl = ctrl,
                    method = "rpart", metric = "Kappa", tuneLength = 10)

#Random Forest
set.seed(seed)
RFsource <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                  data = data.train, trControl = ctrl,
                  method = "rf", metric = "Kappa", tuneLength = 8, preProc = pp)

#K Nearest Neighbors
set.seed(seed)
KNNsource <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                   data = data.train, trControl = ctrl,
                   method = "knn", metric = "Kappa", tuneLength = 20, preProc = pp)

# Naive Bayes
set.seed(seed)
NBsource <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                  data = data.train, trControl = ctrl,
                  method = "naive_bayes", metric = "Kappa", tuneLength = 10,
                  preProc = pp)

#Linear Discriminant Analysis
set.seed(seed)
LDAsource <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                   data = data.train, trControl = ctrl,
                   method = "lda", preProc = pp)

#C5.0
set.seed(seed)
C5source <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                  data = data.train, trControl = ctrl,
                  method = "C5.0", metric = "Kappa", tuneLength = 10, preProc = pp)

#Artificial Neural Network (ANN), feed-forward neural network w /single hidden layer & variable neurons
set.seed(seed)
ANNsource <- train(class.tc~SiO2 + TiO2 + Al2O3 + FeOt + MnO + MgO + CaO + Na2O + K2O,
                   data = data.train, trControl = ctrl, trace = FALSE,
                   method = "nnet", metric = "Kappa", tuneLength = 10, preProc = pp)


# Make predictions on the ensemble training set using component models and save predictions for
data.test.outputs <- data.frame(matrix(unlist(predict(object = list(CARTsource, KNNsource, LDAsource, NBsource, ANNsource, RFsource, SVMsigmasource2, C5source), data.test, type = "prob")), nrow = nrow(data.test)))

# Copy the "label" field to the predictions for comparison
data.test.outputs <- data.test$label

# This block of code tests base learners against test data
predicts.CART <- predict(CARTsource, data.test, type = "raw")
u.CART <- union(predicts.CART, data.test$label)
t.CART<- table (factor(predicts.CART, u.CART), factor(data.test$label, u.CART))
t.CART.CM <- confusionMatrix(t.CART)$overall

predicts.KNN <- predict(KNNsource, data.test, type = "raw")
u.KNN <- union(predicts.KNN, data.test$label)
t.KNN<- table (factor(predicts.KNN, u.KNN), factor(data.test$label, u.KNN))
t.KNN.CM <- confusionMatrix(t.KNN)$overall

predicts.LDA <- predict(LDAsource, data.test, type = "raw")
u.LDA <- union(predicts.LDA, data.test$label)
t.LDA<- table (factor(predicts.LDA, u.LDA), factor(data.test$label, u.LDA))
t.LDA.CM <- confusionMatrix(t.LDA)$overall

predicts.NB <- predict(NBsource, data.test, type = "raw")
u.NB <- union(predicts.NB, data.test$label)
t.NB<- table (factor(predicts.NB, u.NB), factor(data.test$label, u.NB))
t.NB.CM <- confusionMatrix(t.NB)$overall

predicts.ANN <- predict(ANNsource, data.test, type = "raw")
u.ANN <- union(predicts.ANN, data.test$label)
t.ANN<- table (factor(predicts.ANN, u.ANN), factor(data.test$label, u.ANN))
t.ANN.CM <- confusionMatrix(t.ANN)$overall

predicts.RF <- predict(RFsource, data.test, type = "raw")
u.RF <- union(predicts.RF, data.test$label)
t.RF<- table (factor(predicts.RF, u.RF), factor(data.test$label, u.RF))
t.RF.CM <- confusionMatrix(t.RF)$overall

predicts.SVM <- predict(SVMsigmasource2, data.test, type = "raw")
u.SVM <- union(predicts.SVM, data.test$label)
t.SVM<- table (factor(predicts.SVM, u.SVM), factor(data.test$label, u.SVM))
t.SVM.CM <- confusionMatrix(t.SVM)$overall

predicts.C5 <- predict(C5source, data.test, type = "raw")
u.C5 <- union(predicts.C5, data.test$label)
t.C5<- table (factor(predicts.C5, u.C5), factor(data.test$label, u.C5))
t.C5.CM <- confusionMatrix(t.C5)$overall

confusion.matrices <- list(t.CART.CM, t.KNN.CM, t.LDA.CM, t.NB.CM, t.ANN.CM, t.RF.CM, t.SVMsigma2.CM, t.C5.CM)

names(confusion.matrices) <- c("CART", "K Nearest Neighbors", "Linear Discriminant Analysis", "Naive Bayes", "Artificial Neural Network", "Random Forest", "SVM", "C5.0")

return(confusion.matrices)
}
