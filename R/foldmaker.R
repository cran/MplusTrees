#' Creates folds for use in cross-validation
#'
#' Separates data into k equal folds for k-fold cross-validation.
#'
#' @param data Original dataset
#' @param k Number of folds
#' @return A list of length k, with each element corresponding to a fold
#' @author Sarfaraz Serang, borrowing heavily on the \code{createFolds()}
#' function in the \code{caret} package by Max Kuhn.
#'
#' @noRd

foldmaker = function (data, k){
  y = 1:nrow(data)
  breaks <- unique(quantile(y, probs = seq(0, 1, length = 5)))
  y <- factor(as.character(cut(y, breaks, include.lowest = TRUE)))
  numInClass <- table(y)
  foldVector <- vector(mode = "integer", length(y))
  for (i in 1:length(numInClass)) {
    min_reps <- numInClass[i]%/%k
    if (min_reps > 0) {
      spares <- numInClass[i]%%k
      seqVector <- rep(1:k, min_reps)
      if (spares > 0)
        seqVector <- c(seqVector, sample(1:k, spares))
      foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
    }
    else {
      foldVector[which(y == names(numInClass)[i])] <- sample(1:k,size = numInClass[i])
    }
  }
  out <- split(seq(along = y), foldVector)
  folds = vector("list",length(out))
  for(i in 1:length(out)){
    folds[[i]] = data[out[[i]],]
  }
  return(folds)
}
