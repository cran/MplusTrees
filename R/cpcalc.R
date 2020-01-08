#' Calculates cp values to try for cross-validation
#'
#' Gives the cp values based on intervals defined by \code{rpart$cptable} 
#' to evaluate using cross-validation
#'
#' @param model \code{rpart} model
#' @details \code{rpart$cptable} includes intervals defining the ranges 
#' for which \code{cp} values produce each split. Like \code{xpred.rpart},
#' this function uses the geometric mean of these intervals to produce
#' \code{cp} values that can be evaluated with cross-validation.
#' @return A vector of \code{cp} values to try
#' @author Sarfaraz Serang borrowing heavily from the \code{xpred.rpart}
#' function in the \code{rpart} package.
#'
#' @noRd

cpcalc = function(model){
  cps = model$cptable[,1]
  cps = sqrt(cps * c(5, cps[-length(cps)]))
  cps[1] = (1 + model$cptable[1,1])/2
  return(cps)
}
