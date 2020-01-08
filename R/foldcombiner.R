#' Combines folds for use in cross-validation
#'
#' Takes folds from \code{foldmaker()} and aggregates them to create training
#' and tests sets for each fold.
#'
#' @param folds folds from \code{foldmaker()}
#' @return A list of length k, with each element containing a \emph{train}
#' and \emph{test} dataset based on that fold.
#' @author Sarfaraz Serang
#'
#' @noRd

foldcombiner = function(folds){
  k = length(folds)
  agg = vector("list",k)
  for(i in 1:k){
    foldtemp = folds
    foldtemp[[i]] = NULL
    agg[[i]][["train"]] = do.call("rbind",foldtemp)
    agg[[i]][["test"]] = folds[[i]]
  }
  return(agg)
}
