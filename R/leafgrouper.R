#' Separate people into groups based on their terminal node position
#'
#' Takes a tree and a (test) dataset and splits the data by leaf position
#'
#' @param model rpart model
#' @param data a (test) dataset
#' @return A list of length equal to the number of leaves, with each element
#' containing a subset of the original data of only those individuals in that
#' leaf.
#' @author Sarfaraz Serang
#'
#' @noRd

leafgrouper = function(model, data){
  fit = path.rpart(model,print.it=F,
          nodes=row.names(model$frame[model$frame[,"var"] == "<leaf>",]))
  leaves = vector("list",length(fit))
  names(leaves) = names(fit)

  #to translate split names into function readable format
  splittranslate = function(split){
    dice = strsplit(split,"")[[1]]
    if(any(dice=="<")|any(dice==">")){
      return(split)
    }
    else if(grepl("\\w=",split)){
      s1 = strsplit(split,"=")[[1]]
      s2 = paste(s1[1],"=='",s1[2],"'",sep="")
      return(s2)
    }
  }

  if(length(fit)==1){
    leaves[[1]] = data
    return(leaves)
  }
  else if(length(fit)>1){
    for(i in 1:length(leaves)){
      splits = fit[[i]][-1]
      sdata = data
      for(j in 1:length(splits)){
        s = splittranslate(splits[[j]])
        members = eval(parse(text=s),sdata)
        sdata = sdata[members,]
      }
      leaves[[i]] = sdata
    }
    return(leaves)
  }
}
