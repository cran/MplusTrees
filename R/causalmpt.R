#' Causal Mplus Trees
#'
#' Uses M\emph{plus} Trees to match on structural equation model parameters in matching subsample.
#' Then estimates Conditional Average Treatment Effects (CATEs) in holdout estimation subsample.
#'
#' @param script An \code{MplusAutomation} script file
#' @param data Dataset that is specified in the script
#' @param rPartFormula Formula of the form ~ variable names
#' @param group id variable. If not specified an id variable is created for each row
#' @param treat Treatment variable
#' @param outcome Univariate outcome of interest (dependent variable in mean comparison tests)
#' @param est.samp Proportion of sample to be used as holdout sample (estimation subsample)
#' @param ... Other arguments to \code{MplusTrees} for building Mplus Tree
#' @details See documentation for \code{MplusTrees()} for further information on tree building process.
#' Takes terminal nodes from Mplus Tree and considers them "matched". Splits estimation subsample into
#' groups defined by covariate pattern in terminal nodes from Mplus Tree. Performs t tests in each group
#' with \code{treat} as independent variable and \code{outcome} as dependent variable to estimate CATEs.
#' Also performs ANOVA to determine if treatment effect differs by group (interaction).
#' @references Serang, S., & Sears, J. (2021). Tree-based matching on structural equation model
#' parameters. Behavioral Data Science, 1, 31-53.
#' @return An object of class '\code{causalmpt}'. Tree structure drawn from \code{MplusTrees()}. CATEs
#' estimated in estimation (holdout) subsample. Provides results of t tests to estimate CATEs in each
#' group and ANOVA to examine group differences in treatment effect.
#' @author Sarfaraz Serang
#' @import MplusAutomation
#' @import rpart
#' @import nlme
#' @importFrom stats terms as.formula model.matrix pchisq quantile predict aov t.test
#' @export
#' @examples
#' \dontrun{
#' library(lavaan)
#'
#' script = mplusObject(
#'    TITLE = "Causal Mplus Trees Example",
#'    MODEL = "f1 BY x1-x3;",
#'    usevariables = c('x1','x2','x3'),
#'    rdata = HolzingerSwineford1939)
#'
#' fit.cmpt = causalmpt(script, HolzingerSwineford1939, group=~id,
#'    rPartFormula=~school+grade,
#'    control=rpart.control(minsplit=100, minbucket=100, cp=.01),
#'    treat="sex", outcome="x4")
#' fit.cmpt
#' }

causalmpt <- function(script,
                      data,
                      rPartFormula,
                      group= ~ id,
                      treat,
                      outcome,
                      est.samp = .2,
                      ...){

  #use temporary directory
  wd=getwd()
  on.exit(setwd(wd))
  setwd(tempdir())

  #randomly split data into matching subsample and estimation subsample
  matchSampInd = sample.int(nrow(data),size=round((1-est.samp)*nrow(data)))
  matchData = data[matchSampInd,]
  estData = data[-matchSampInd,]

  #build Mplus Tree
  tree = MplusTrees(script, matchData, rPartFormula, ...)

  #fits model separately in treated and untreated groups to see how well matched they are
  groupingName = attr(terms(nlme::splitFormula(form=group,'~')[[1]]),"term.labels")
  for(i in 1:length(tree$terminal)){
    #splitting up dataset to run models on treated/untreated separately
    matchDataNode = matchData[tree$where==tree$terminal[i],]
    matchDataByTreat = split(matchDataNode,matchDataNode[,treat])

    #fit models to each treated/untreated
    script1 = script2 = script
    script1$rdata = matchDataByTreat[[1]]
    fit1 = mplusModeler(script1, run=1L, modelout="Model.1.inp")
    tree$matchEst[[tree$terminal[i]]][[1]] = fit1$results$parameters$unstandardized[,1:4]

    script2$rdata = matchDataByTreat[[2]]
    fit2 = mplusModeler(script2, run=1L, modelout="Model.1.inp")
    tree$matchEst[[tree$terminal[i]]][[2]] = fit2$results$parameters$unstandardized[,1:4]

    names(tree$matchEst[[tree$terminal[i]]]) = names(matchDataByTreat)
  }

  #extract groupings
  path = path.rpart(tree$rpart_out,nodes=tree$terminal,print.it=F)
  leaves = vector("list",length(path))
  names(leaves) = names(path)

  #to translate split names into function readable format
  splitTranslate = function(split){
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

  #put people from estimation sample into groups found from matched sample
  for(i in 1:length(leaves)){
    splits = path[[i]][-1]
    sdata = estData
    for(j in 1:length(splits)){
      spt = splitTranslate(splits[[j]])
      members = eval(parse(text=spt),sdata)
      sdata = sdata[members,]
    }
    leaves[[i]] = sdata
  }

  #estimating CATEs in each group
  estGroups = leaves
  CATEs = vector("list",length(estGroups))
  names(CATEs) = names(estGroups)
  for(i in 1:length(CATEs)){
    y = as.numeric(as.matrix(estGroups[[i]][outcome]))
    treatment = as.factor(as.matrix(estGroups[[i]][treat]))
    CATEs[[i]]$CATE = unname(t.test(y~treatment)$estimate[2] - t.test(y~treatment)$estimate[1])
    CATEs[[i]]$sample.sizes = table(as.matrix(estGroups[[i]][treat]))
    CATEs[[i]]$t.test = t.test(y~treatment)
  }

  #estimating whether CATEs differ by group
  grp = y = trt = NULL
  for(i in 1:length(estGroups)){
    trt = c(trt, as.factor(as.matrix(estGroups[[i]][treat])))
    y = c(y,as.numeric(as.matrix(estGroups[[i]][outcome])))
    grp = c(grp,rep(names(estGroups)[i], nrow(estGroups[[i]])))
  }
  grp = factor(grp)

  estData1 = data.frame(treat = trt,outcome = y,group = grp)
  fit = aov(y ~ treat*group, data=estData1)

  tree$CATE = CATEs
  tree$CATExGrp = summary(fit)

  class(tree) <- "causalmpt"

  return(tree)
}
