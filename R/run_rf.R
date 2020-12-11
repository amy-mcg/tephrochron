#' Use a RandomForest model to classify tephra
#'
#'
#' @param data A data frame containing element geochemistries of unknown origin
#' @return An additional column in the data frame stating the most likely origin for the tephra, based on the model
#' @examples
#' data.classified <- (model.rf, new.data);
#'
#'
#


run_rf <- function(model.rf, new.data){
  ##Run a random forest model to classify unknown data
  if(!all(c("SiO2",	"TiO2",	"Al2O3",	"FeOt",	"MnO",	"MgO",	"CaO",	"Na2O",	"K2O") %in% colnames(new.data))) {
    valid.2 <- c(valid.2, "@data' must have 'SiO2',	'TiO2',	'Al2O3',	'FeOt',	'MnO',	'MgO',	'CaO',	'Na2O', and	'K2O' columns")
  }

  data.full <- dplyr::select(new.data, c("SiO2",	"TiO2",	"Al2O3",	"FeOt",	"MnO",	"MgO",	"CaO",	"Na2O",	"K2O"))

eruption.pred <- predict(model.rf$model.rf, newdata = data.full, type = "prob")

output <- cbind(new.data, eruption.pred)


  return(output)
}

