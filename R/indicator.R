#' Creates indicator variables for categorical variables
#'
#' Breaks up single columns of nominal variables into multiple columns of indicator
#' variables. This allows rpart to treat them as nominal, not ordinal variables.
#'
#' @param data Original dataset
#' @param catvars Vector of names of categorical variables
#' @param rpartFormula Formula of the form ~ variable names
#' @return A list containing new dataset (with indicator variables instead
#' of original column of nominal variable) and new rpartFormula (using indicator
#' variables instead of original nominal variable)
#' @author Sarfaraz Serang
#'
#' @noRd


indicator = function(data,catvars=catvars,rPartFormula=rPartFormula){
  if(any((catvars %in% names(data))==F)){
    stop("catvars variable not in dataset")
  }
  catvar.nums = which(names(data)%in%catvars)
  newdata = data
  for(i in catvar.nums){
    newdata[,i] = as.factor(data[,i])
    if(length(levels(newdata[,i]))<3){
      stop("catvars variable has fewer than 3 levels")
    }
    newdata = cbind(newdata, model.matrix(with(newdata,
      as.formula(paste("~",names(data)[i],"-1",sep="")))))
  }
  newdata=newdata[,-catvar.nums]
  covs1 = strsplit(as.character(rPartFormula)[2]," \\+ ")[[1]]
  if(any((catvars %in% covs1)==F)){
    stop("catvars variable not in rPartFormula")
  }
  covs2 = names(newdata)[(ncol(data)-length(catvars)+1):ncol(newdata)]
  covs3 = c(covs1,covs2)
  covs4 = covs3[-which(covs3%in%catvars)]
  rpf = as.formula(paste("~",paste(covs4,collapse="+"),sep=""))
  
  catdata = vector("list")
  catdata$data = newdata
  catdata$rpf = rpf
  return(catdata)
}