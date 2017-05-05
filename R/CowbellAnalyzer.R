

#' Computes a response surface as segmented linear regression that resembles a cowbell.
#'
#'
#' The application case for which this package has been constructed for is the response analysis
#' of one dependend variable that depends on two independend variable. Additionally a prior information is
#' given on the minimal and maximal values of those three variables. This is the case if those variables originate
#' from questions using Likert scales.
#' This information is indicated upfront with the function \code{\link{generateCowbellConcept}}.
#' The response surface resembles a quarter segment of a cowbell. It starts with a certain value at the
#' dependend variable if any of the independent variables are at their minimal value. If both independent variables
#' have reached their breakpoint value a new value with a plateau will be returned. Going to the plateau a rising linear
#' ridge is used. Where to both sides of the ridge there is only a linear dependency on one of the independent variables.
#' In general this model may be worth trying if one assumes that
#' there is a point of saturation for both independent variables and that up this point essentially the smaller one of both values
#' is determining the result on the dependend variable.
#' This is computed in a second step with the help of the
#' function \code{\link{generateCowbell}}.
#'
#'
#'
#' @docType package
#' @name cowbell
NULL

#' allFun: Data set for Fluency, Absorption, Fit and Fun.
#'
#' Data set of two games containing measurements of Fluency, Absorption, Fit (= Perceived difficulty) and Fun.
#' Fluency and Absorption are measured on a scale from 1..7, Fun and Fit are measured  1..9.
#'
#' @name allFun
#' @docType data
NULL


#' testA: Artificial data set for testing.
#'
#' This is an artificial data with a perfect cowbell structure that is used for testing.
#'
#' @name testA
#' @docType data
NULL


# ==========================  All related to cowbell concept



#' Expresses the fitting formula and the value range of the variables.
#'
#' Generates a concept which is basically the formula used for the regression analysis and minimal and maximal
#' values of the dependent and two independent variables. As this package creates a very specific regression model
#' the formula is always of the form Dest ~ SrcA * SrcB. If one of the minimal and maximal values are omitted,
#' the minimum or maximum value of the data set will be used later on.
#'
#' @param formula The formula essentially specifying the names of the dependend variable (here Dest)
#'            and the two independent variables (here SrcA, SrcB). Example: Dest ~ SrcA * SrcB.
#' @param minDest The a prior known minimal value the dependend variable (Dest) can have.
#' @param maxDest The a prior known maximal value the dependend variable (Dest) can have.
#' @param minSrcA The a prior known minimal value the first dependend variable (SrcA) can have.
#' @param maxSrcA The a prior known maximal value the first dependend variable (SrcA) can have.
#' @param minSrcB The a prior known minimal value the second dependend variable (SrcB) can have.
#' @param maxSrcB The a prior known maximal value the second dependend variable (SrcB) can have.
#'
#' @return List of the aggregate information. This is used in the regression analysis.
#' @export
#'
#' @examples
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
generateCowbellConcept<-function(formula, minDest = NA, maxDest = NA, minSrcA = NA, maxSrcA = NA, minSrcB = NA, maxSrcB = NA)
{
   destinationDescr<-list(variable = all.vars(formula)[1], minVal = minDest, maxVal = maxDest)
   srcADescr<-list(variable = all.vars(formula)[2], minVal = minSrcA, maxVal = maxSrcA)
   srcBDescr<-list(variable = all.vars(formula)[3], minVal = minSrcB, maxVal = maxSrcB)
   result<-list(srcA = srcADescr, srcB = srcBDescr, dest = destinationDescr, formula = formula)
   class(result)<-"cowbellConcept"
   return ( result )
}



#' Summarizes the cowbell concept with the formula and value ranges.
#'
#' Implementation of the \code{\link{print}} generic.
#'
#' @param x  Object generated with function \code{\link{generateCowbellConcept}}
#' @param ... Unused for compatibility only.
#'
#' @export
#' @seealso \code{\link{generateCowbellConcept}}
#'
#' @examples
#' # Generate a concept and display it.
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' concept
print.cowbellConcept<-function(x,...)
{
    cat("Formula: ", deparse(x$formula), "\n")
    cat(x$srcA$variable," : ", x$srcA$minVal, " ... ", x$srcA$maxVal, "\n")
    cat(x$srcB$variable," : ", x$srcB$minVal, " ... ", x$srcB$maxVal, "\n")
    cat(x$dest$variable," : ", x$dest$minVal, " ... ", x$dest$maxVal, "\n")
}



#=======================================================================================================
# Here is all related to to cowbell analysis



#' Performs the segmented linear regression analysis generating the cowbell function.
#'
#' This function takes the cowbell definition that was created with \code{\link{generateCowbellConcept}} and performs
#' a regression analysis. Additionally it also moves the breaking point to the maximal values of the independent
#' variables to later on test for the significance of the breaking point. This function needs relatively long to
#' compute as it uses a gradient based optimizer for optimizing a non - linear model.
#'
#' @param concept  The previously in function \code{\link{generateCowbellConcept}} specified concept.
#' @param table  The table that at least contains the data for the dependent and the two independent variables specified in concept.
#' @param iterations The number of iteration that should be done with the gradient optimizer.
#' @param learningRate The step size that should be applied in the gradient optimizer.
#'
#' @return A list with the data, the model with and without breakpoint and the F-Statistics.
#' @export
#'
#' @seealso  \code{\link{generateCowbellConcept}}
#'
#' @examples
#' # Run a simplified anaylsis with 10 iterations only (to save time.)
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' data(allFun)
#' test<-generateCowbell(concept, allFun, 10)
generateCowbell<-function(concept, table, iterations = 1000, learningRate = 0.01)
{

  if (!checkConsistency(concept, table))
    return ( FALSE)

  destVec<-table[[concept$dest$variable]]
  srcAVec<-table[[concept$srcA$variable]]
  srcBVec<-table[[concept$srcB$variable]]

  # Check for NAs in boundaries.
  if(is.na(concept$dest$minVal))
    concept$dest$minVal<-min(destVec)
  if(is.na(concept$dest$maxVal))
    concept$dest$maxVal<-max(destVec)

  if(is.na(concept$srcA$minVal))
    concept$srcA$minVal<-min(srcAVec)
  if(is.na(concept$srcA$maxVal))
    concept$srcA$maxVal<-max(srcAVec)

  if(is.na(concept$srcB$minVal))
    concept$srcB$minVal<-min(srcBVec)
  if(is.na(concept$srcB$maxVal))
    concept$srcB$maxVal<-max(srcBVec)


  cat("Working on full model.\n")
  # Do the computation for the full model.
  params<-constructStartParameters(concept)
  for(i in 1 : iterations)
  {
    params<-doGradientStep(params, TRUE, destVec, srcAVec, srcBVec, learningRate)
    params<-enforceConstrains(concept, params)
  }
  optimParams<-constructComputeFriendlySet(params)

  cat("Working on model without breakpoint.\n")
  # Same computation for the reduced model to get the signigicanze of the break point.
  paramsNoBreak<-constructStartWithoutBreakpoint(concept)
  for(i in 1 : iterations)
  {
    paramsNoBreak<-doGradientStep(paramsNoBreak, FALSE, destVec, srcAVec, srcBVec, learningRate)
    paramsNoBreak<-enforceConstrains(concept, paramsNoBreak)
  }
  optimParamsNoBreak<-constructComputeFriendlySet(paramsNoBreak)

  fStat<-fStatistics(optimParams, destVec, srcAVec, srcBVec)
  fStatReduced<-fStatisticReduced(optimParams,optimParamsNoBreak, destVec, srcAVec, srcBVec)

  fullModel<-list(parameters = params,
                  computeFriendlyParams = optimParams,
                  rSquared = rSquaredForModel(optimParams, destVec, srcAVec, srcBVec ))
  reducedModel<-list(parameters = paramsNoBreak,
                     computeFriendlyParams = optimParamsNoBreak,
                     rSquared = rSquaredForModel(optimParamsNoBreak, destVec, srcAVec, srcBVec ))

  result<-list(fullModel = fullModel, reducedModel = reducedModel,
               concept = concept, data = table,
               fStat = fStat, fStatBreakpoint = fStatReduced)
  class(result)<-"cowbell"
  return ( result )
}


#' Summarizes the cowbell regression analysis
#'
#' Prints the used formula, the R squared and the F statistics in comparison with a constant function
#' (average of values).
#'
#' Implementation of the \code{\link{print}} generic.
#'
#' @param x     Object generated with function \code{\link{generateCowbell}}
#' @param ...   Only for compatibility purposes.
#'
#' @export
#'
#' @examples
#' # Run a simplified anaylsis with 10 iterations only (to save time.)
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' data(allFun)
#' test<-generateCowbell(concept, allFun, 10)
#' test
print.cowbell<-function(x, ...)
{
  cat("Formula: ", deparse(x$concept$formula), "\n")
  cat("R Squared:", x$fullModel$rSquared, "\n")
  cat("F-Statistic:", x$fStat$Fvalue, " on ", x$fStat$dfA, " and ", x$fStat$dfB, " DF p-value:", x$fStat$pValue )
}

#' Generates the core information of the cowbell analysis.
#'
#' Generates a list that contains relevant information of the cowbell analysis. \code{concept} contains the formula and the minimal
#' and maximal values. \code{baseDefinition} contains the information for the cowbell. This is the minimal and the maximal value the
#' cowbell can reach and the position of the breaking point. \code{baseDefinitionReduced} only contains the minimal and the maximal value,
#' the breakpoint is not included in that model. \code{functionString} is a string version of the R function that generates the cowbell.
#' \code{functionStringReduced} is the string version without the breaking point. \code{fstatistic} contains the fstatistic information of the
#' cowbell model in contrast to the constant function. \code{fstatisticBreakpoint} describes the F-statistics of the full cowbell model against the version
#' without break point.
#'
#' Implementation of the \code{\link{summary}} generic.
#'
#' @param object       The resulting object of \code{\link{generateCowbell}}.
#' @param ... Just for compatibility purposes.
#'
#' @return List with the mentioned values.
#' @export
#'
#' @examples
#' # Run a simplified anaylsis with 10 iterations only (to save time.)
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' data(allFun)
#' test<-generateCowbell(concept, allFun, 10)
#' summary(test)$functionString
summary.cowbell<-function(object, ...)
{

  fM<-object$fullModel
  rM<-object$reducedModel
  ans <- list(concept = object$concept,
              baseDefinition = list(minDepend = fM$parameters$bh, maxDepend = fM$parameters$th,
                                    breakPointA = fM$parameters$xth, breakPointB = fM$parameters$yth),
              baseDefinitionReduced = list(minDepend = rM$parameters$bh, maxDepend = rM$parameters$th),

              functionString = generateFunctionString(object$concept, fM$computeFriendlyParams),
              functionStringReduced = generateFunctionStringNoBreakpoint(object$concept, rM$computeFriendlyParams),

              rSquared = fM$rSquared,
              rSquaredReduced = rM$rSquared,
              fstatistic = list(value = object$fStat$Fvalue,  numdf = object$fStat$dfA, dendf = object$fStat$dfB,
                                  pval = object$fStat$pValue),
              fstatisticBreakpoint = list(value = object$fStatBreakpoint$Fvalue,  numdf = object$fStatBreakpoint$dfA,
                                          dendf = object$fStatBreakpoint$dfB,
                                          pval = object$fStatBreakpoint$pValue))
  class(ans)<-"summary.cowbell"
  ans
}


#' Prints the summary obtained by \code{\link{summary.cowbell}}.
#'
#' The output states the concept consisting of the formula and the value range, that was given as a prior information.
#' Then follows the characteristics of the cowbell function. This is the minimal and maximal value of the dependent variable.
#' The maximum is reached at the plateau part and the minimal at the outer ring of the cowbell. It gets reached if
#' any of the two independent variables have reached their minimum. The R function with which the cowbell gets computed is then
#' appended. It follows the R squared and F-statistics in comparison with a constant function.
#'
#' The following analysis is done to check the significance of the breakpoint. The breakpoint gets eliminated by removing
#' the plateau. The linear rising ridge of the cowbell is rised up to the specified maximum of the independent variables.
#' Therefore only the minimal and maximal value of the dependent variable is left of the definition. What follows is the
#' string that characterizes the R function and the R Squared of the used model. The following F-Statistics compares the
#' full model with the breakpoint against this reduced model without to estimate the significance of the breakpoint.
#'
#' Implementation of the \code{\link{print}} generic.
#'
#'
#'
#' @param x      The object to print generated by \code{\link{summary.cowbell}}
#' @param ...    Just for compatibility purposes.
#'
#' @export
#'
#' @examples
#' # Run a simplified anaylsis with 10 iterations only (to save time.)
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' data(allFun)
#' test<-generateCowbell(concept, allFun, 10)
#' summary(test)
print.summary.cowbell<-function(x, ...)
{
  cat("Base concept:\n")
  cat("===================================================================\n")
  print(x$concept)
  cat("\nBase definition segmented two dimensional linear regression:\n")
  cat("===================================================================\n")
  cat("Minimal Value dependend variable ",x$concept$dest$variable,": ", x$baseDefinition$minDepend, "\n")
  cat("Maximal Value dependend variable ",x$concept$dest$variable, ": ", x$baseDefinition$maxDepend, "\n")
  cat("Breaking point of independend variable ", x$concept$srcA$variable, ": ", x$baseDefinition$breakPointA, "\n")
  cat("Breaking point of independend variable ", x$concept$srcB$variable, ": ", x$baseDefinition$breakPointB, "\n\n")
  cat("Computation of ", x$concept$dest$variable, ": \n")
  cat("===================================================================\n")
  cat(x$functionString)
  cat("\n\nStatistics:\n")
  cat("===================================================================\n")
  cat("R Squared:", x$rSquared, "\n")
  cat("F-Statistic (comparison constant function):", x$fstatistic$value, " on ", x$fstatistic$numdf, " and ", x$fstatistic$dendf, " DF p-value:", x$fstatistic$pval )
  cat("\n\n===================================================================\n")
  cat("No breaking point - reduced model\n")
  cat("===================================================================\n")
  cat("Minimal Value dependend variable ",x$concept$dest$variable,": ", x$baseDefinitionReduced$minDepend, "\n")
  cat("Maximal Value dependend variable ",x$concept$dest$variable, ": ", x$baseDefinitionReduced$maxDepend, "\n")
  cat("Computation of ", x$concept$dest$variable, ": \n")
  cat("===================================================================\n")
  cat(x$functionStringReduced)
  cat("\n\nStatistics:\n")
  cat("===================================================================\n")
  cat("R Squared:", x$rSquaredReduced, "\n")
  cat("F-Statistic (comparison full model against reduced):", x$fstatisticBreakpoint$value, " on ", x$fstatisticBreakpoint$numdf, " and ", x$fstatisticBreakpoint$dendf, " DF p-value:", x$fstatisticBreakpoint$pval )
}



#' Plots the obtained cowbell function.
#'
#' Generates a three dimension plot of cowbell function. Additionally the data points of the original data set are added
#' in the visualization. The function with and without breaking point can be visualized.
#' Implementation of the \code{\link{plot}} generic.
#'
#' @param x     The data obtained by function \code{\link{generateCowbell}}.
#' @param breakPointUsed  Defaults to TRUE and indicates if we want to use the version with breakpoint (or not).
#' @param ... Just for compatibility purposes.
#'
#' @export
#'
#' @examples
#' # Run a simplified anaylsis with 10 iterations only (to save time.)
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' data(allFun)
#' test<-generateCowbell(concept, allFun, 10)
#' plot(test)
plot.cowbell<-function(x, breakPointUsed = TRUE, ...)
{
  concept<-x$concept
  xVal<-seq(x$concept$srcA$minVal,concept$srcA$maxVal, length.out =  100)
  yVal<-seq(x$concept$srcB$minVal,concept$srcB$maxVal, length.out =  100)
  if (breakPointUsed)
    zVal<-outer(xVal, yVal, functionMapper, x$fullModel$computeFriendlyParams)
  else
    zVal<-outer(xVal, yVal, functionMapper, x$reducedModel$computeFriendlyParams)
  colorLut<-grDevices::heat.colors(100)

  zscaled<- 100 * (zVal - concept$dest$minVal) / (concept$dest$maxVal - concept$dest$minVal) + 1
  rgl::plot3d(x$data[[concept$srcA$variable]], x$data[[concept$srcB$variable]],
         x$data[[concept$dest$variable]], col=c("green"), size = 4,
         xlab = concept$srcA$variable, ylab = concept$srcB$variable, zlab = concept$dest$variable)
  rgl::surface3d(xVal, yVal, zVal, color=colorLut[zscaled], alpha=0.95)
}


#'  Performs a prediction on the cowbell model that has been generated.
#'
#'  Implementation of the \code{\link{predict}} generic. The provided data has to have the exact column names
#'  that were used when the cowbell analysis was done. If no data is provided the original data is used.
#'
#' @param object The data obtained by function \code{\link{generateCowbell}}.
#' @param newdata The data set to perform the prediction on. If omitted the original data is used.
#' @param ...    Just for compatibility purposes.
#'
#' @return The vector with the predicted data.
#' @export
#'
#' @examples
#' # Run a simplified anaylsis with 10 iterations only (to save time.)
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' data(allFun)
#' test<-generateCowbell(concept, allFun, 10)
#' predict(test)
predict.cowbell<-function(object, newdata, ...)
{
  if (missing(newdata) || is.null(newdata))
     newdata = object$data

   if (!checkConsistency(object$concept, newdata))
     return ( NULL )

   functionMapper(newdata[[object$concept$srcA$variable]], newdata[[object$concept$srcB$variable]],
                  object$fullModel$computeFriendlyParams)

}



#' Implementation of the \code{\link{fitted}} generic.
#'
#' @param object The data obtained by function \code{\link{generateCowbell}}.
#' @param ... Just for compatibility purposes.
#'
#' @return List with predicted values
#' @export
#'
#' @examples
#' # Run a simplified anaylsis with 10 iterations only (to save time.)
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' data(allFun)
#' test<-generateCowbell(concept, allFun, 10)
#' fitted(test)
fitted.cowbell<-function(object, ...)
{
  functionMapper(object$data[[object$concept$srcA$variable]],
                 object$data[[object$concept$srcB$variable]],
                 object$fullModel$computeFriendlyParams)
}



#' Implementation of the \code{\link{residuals}} generic.
#'
#' @param object The data obtained by function \code{\link{generateCowbell}}.
#' @param ... Just for compatibility purposes.
#'
#' @return Vector with the residuals to the data.
#' @export
#'
#' @examples
#' # Run a simplified anaylsis with 10 iterations only (to save time.)
#' concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
#' data(allFun)
#' test<-generateCowbell(concept, allFun, 10)
#' residuals(test)
residuals.cowbell<-function(object, ...)
{
    object$data[[object$concept$dest$variable]] - fitted.cowbell(object)
}



#========================= Helper functions for cowbell.

checkConsistency<-function(concept, table)
{
  colNames<-names(table)
  if (! (concept$dest$variable %in% colNames) )
  {
    cat("Independent variable ", concept$dest$variable, " not in table!")
    return ( FALSE )
  }
  if (! (concept$srcA$variable %in% colNames) )
  {
    cat("Dependend variable ", concept$srcA$variable, " not in table!")
    return ( FALSE )
  }
  if (! (concept$srcB$variable %in% colNames) )
  {
    cat("Dependend variable ", concept$srcB$variable, " not in table!")
    return ( FALSE )
  }

  return ( TRUE )
}


# Needed for the outer product in the visualization.
functionMapper<-function(x,y, computeFriendlyParams)
{
  z<-rep(0, length(x))
  for(i in 1 : length(z))
  {
    z[i] <- evaluateFunction(computeFriendlyParams, x[i], y[i])
  }

  return ( z )
}


# Creates a guessing starting parameter set from the cowbell concept.
# xmin, ymin: Parameters where we start caluclating
# xth, yth: Parameters where maximum value is reached.
# bh: Minimal value we assume
# th: Maximal value we assume
constructStartParameters<-function(concept)
{
    list(xmin = concept$srcA$minVal, ymin = concept$srcB$minVal,
         xth =  0.25 * concept$srcA$minVal + 0.75 * concept$srcA$maxVal,
         yth = 0.25 * concept$srcB$minVal + 0.75 * concept$srcB$maxVal,
         bh = 0.8 * concept$dest$minVal + 0.2 * concept$dest$maxVal,
         th = 0.2 * concept$dest$minVal + 0.8 * concept$dest$maxVal)
}

# Same function as above only that the break point is set to max and therefore to non-existant.
constructStartWithoutBreakpoint<-function(concept)
{
  list(xmin = concept$srcA$minVal, ymin = concept$srcB$minVal,
       xth = concept$srcA$maxVal,
       yth = concept$srcB$maxVal,
       bh = 0.8 * concept$dest$minVal + 0.2 * concept$dest$maxVal,
       th = 0.2 * concept$dest$minVal + 0.8 * concept$dest$maxVal)
}

# Verfies if all constraints are still met.
# Params:
# concept: The concept that contains all the constraints.
# parameters: The parameters to eventually modify.
enforceConstrains<-function(concept, parameters)
{
  if (parameters$xth < concept$srcA$minVal)
    parameters$xth <- concept$srcA$minVal
  if (parameters$xth > concept$srcA$maxVal)
    parameters$xth <- concept$srcA$maxVal

  if (parameters$yth < concept$srcB$minVal)
    parameters$yth <- concept$srcB$minVal
  if (parameters$yth > concept$srcB$maxVal)
    parameters$yth <- concept$srcB$maxVal

  if (parameters$bh < concept$dest$minVal)
    parameters$bh <- concept$dest$minVal
  if (parameters$bh > concept$dest$maxVal)
    parameters$bh <- concept$dest$maxVal

  if (parameters$th < concept$dest$minVal)
    parameters$th <- concept$dest$minVal
  if (parameters$th > concept$dest$maxVal)
    parameters$th <- concept$dest$maxVal

  return (parameters)
}


# Generates a compute friendly data set of the parameter list from above.
constructComputeFriendlySet<-function(paramSet)
{
   delX <- paramSet$xth - paramSet$xmin
   delY <- paramSet$yth - paramSet$ymin
   delH <- paramSet$th - paramSet$bh

   list( allMaxX = paramSet$xth,  # x avlue where plateau is reached
         allMaxY = paramSet$yth,  # y value where plateau is reached
         allMaxRes = paramSet$th, # the value we reach in plateau         (x > allMaxx) && (y > allMaxY) return allMaxRes
         decX = - delY,           #  x factor for decision variable
         decY =   delX,           #  y factor for decision variable
         decTar = - paramSet$xmin * delY + paramSet$ymin * delX, # dec variabke (x * decX + y * decY > decTar: caseA , baseB)
         caseAOff = paramSet$bh - paramSet$xmin * delH / delX,
         caseAFac = delH / delX,   # caseA : value = caseAOff + x * caseAFac
         caseBOff = paramSet$bh - paramSet$ymin * delH / delY,
         caseBFac = delH / delY   # caseB : value = caseBOff + y * caseBFac
         )

}


# Applies the compute friendly data set to a data pairing x and y
evaluateFunction<-function(computeSet, x, y)
{
  if ((x >= computeSet$allMaxX) && (y >= computeSet$allMaxY ))
    return (computeSet$allMaxRes)

  if (x * computeSet$decX + y * computeSet$decY > computeSet$decTar )
    return (computeSet$caseAOff + x * computeSet$caseAFac)
  else
    return (computeSet$caseBOff + y * computeSet$caseBFac)
}


# Computes the F-statistics comparing the function with a constant function.
# comparing with linear functions does not make sense here, because a restriction
# of cow bell to linear is not possible.
fStatistics<-function(computeFriendlySet, destVec, srcAVec, srcBVec)
{
  target<-length(destVec)
  average<-mean(destVec)

  sumPredict<-0.0
  sumMean<-0.0

  for(i in 1 : target)
  {
    realTarget<-destVec[i]
    estimatedTarget<-evaluateFunction(computeFriendlySet,
                                      srcAVec[i], srcBVec[i])
    sumPredict<-sumPredict + (realTarget - estimatedTarget)^2
    sumMean<-sumMean + (realTarget - average)^2
  }

  dfA<- 4-1
  dfB<- target - 4
  Fvalue<-((sumMean - sumPredict)/dfA) /(sumPredict / dfB)
  pValue<-stats::pf(Fvalue, dfA, dfB, lower.tail = FALSE)
  list(Fvalue = Fvalue, dfA = dfA, dfB = dfB, pValue = pValue)
}

# Computes the F-statistics comparing the function with the comparison function without breakPoint.
# comparing with linear functions does not make sense here, because a restriction
# of cow bell to linear is not possible.
fStatisticReduced<-function(computeFriendlySet, computeFriendlySetNoBreak, destVec, srcAVec, srcBVec)
{
  target<-length(destVec)

  sumPredict<-0.0
  sumPredictNoBreak<-0.0

  for(i in 1 : target)
  {
    realTarget<-destVec[i]
    estimatedTarget<-evaluateFunction(computeFriendlySet,
                                      srcAVec[i], srcBVec[i])
    sumPredict<-sumPredict + (realTarget - estimatedTarget)^2


    estimatedReduced<-evaluateFunction(computeFriendlySetNoBreak,
                                       srcAVec[i], srcBVec[i])
    sumPredictNoBreak<-sumPredictNoBreak + (realTarget - estimatedReduced)^2
  }

  dfA<- 4- 2
  dfB<- target - 4
  Fvalue<-((sumPredictNoBreak - sumPredict)/dfA) /(sumPredict / dfB)
  pValue<-stats::pf(Fvalue, dfA, dfB, lower.tail = FALSE)
  list(Fvalue = Fvalue, dfA = dfA, dfB = dfB, pValue = pValue)
}

# Computes the Rsquarred for a model.
rSquaredForModel<-function(computeFriendlySet, destVec, srcAVec, srcBVec)
{
  target<-length(destVec)
  sum<-0.0

  for(i in 1 : target)
  {
    realTarget<-destVec[i]
    estimatedTarget<-evaluateFunction(computeFriendlySet,
                                      srcAVec[i], srcBVec[i])
    sum<-sum + (realTarget - estimatedTarget)^2
  }
  return (1.0 - ( sum / target) / stats::var(destVec) )
}


# Computes a gradient with respect to xth, yth, bh and th
# and returns an adjusted parameter set.
# Params:
# simpleParams: The param set as obtained by constructComputeFriendlySet,
# breakpoint indicates if we use the break point or not.
# destVec: Vector with desintation variables.
# srcAVec: Vector with srcA Variables.
# srcBVec: Vector with srcB Variables.
# learningRate: The learning rate we apply.
doGradientStep<-function(simpleParams, useBreakpoint, destVec, srcAVec, srcBVec, learningRate)
{

  result<-rep(0, 4)

  cfp<-constructComputeFriendlySet(simpleParams)
  basePoint<-rSquaredForModel(cfp, destVec, srcAVec, srcBVec)


  if (useBreakpoint)
  {
    adjustedParams<-simpleParams
    adjustedParams$xth <- adjustedParams$xth + learningRate
    cfp<-constructComputeFriendlySet(adjustedParams)
    result[1]<-rSquaredForModel(cfp, destVec, srcAVec, srcBVec) - basePoint

    adjustedParams<-simpleParams
    adjustedParams$yth <- adjustedParams$yth + learningRate
    cfp<-constructComputeFriendlySet(adjustedParams)
    result[2]<-rSquaredForModel(cfp, destVec, srcAVec, srcBVec) - basePoint
  }
  else
  {
    result[1] <- 0.0
    result[2] <- 0.0
  }

  adjustedParams<-simpleParams
  adjustedParams$bh <- adjustedParams$bh + learningRate
  cfp<-constructComputeFriendlySet(adjustedParams)
  result[3]<-rSquaredForModel(cfp, destVec, srcAVec, srcBVec) - basePoint

  adjustedParams<-simpleParams
  adjustedParams$th <- adjustedParams$th + learningRate
  cfp<-constructComputeFriendlySet(adjustedParams)
  result[4]<-rSquaredForModel(cfp, destVec, srcAVec, srcBVec) - basePoint


  #Normalize
  result<- learningRate * (result / sqrt(sum(result^2)))

  simpleParams$xth <- simpleParams$xth + result[1]
  simpleParams$yth <- simpleParams$yth + result[2]
  simpleParams$bh <- simpleParams$bh + result[3]
  simpleParams$th <- simpleParams$th + result[4]

  return ( simpleParams )

}


# Generates a printavkle String out of a compute set.
# Params:
# concept: The original concept used.
# computeSet: The compute friendly data set used.
generateFunctionString<-function(concept, computeSet)
{
  srcAVar<-concept$srcA$variable
  srcBVar<-concept$srcB$variable
  paste(sep="", concept$dest$variable,"<-function(", srcAVar,", ",srcBVar, ")\n","{\n",
      "  if ((",srcAVar," >= ", computeSet$allMaxX, ") && (",srcBVar," >= ", computeSet$allMaxY, "))\n",
     "     return (", computeSet$allMaxRes, ")\n",
     "  if (",srcAVar," * (", computeSet$decX, ") + ",srcBVar," * ",computeSet$decY, " > ", computeSet$decTar,")\n",
       "     return (", computeSet$caseAOff, " + ",srcAVar," * ", computeSet$caseAFac, ")\n",
     "  else\n",
       "     return (", computeSet$caseBOff, " + ",srcBVar," * ", computeSet$caseBFac, ")\n","}\n")
}

# Generates a printavkle String out of a compute set under no breakpoint assumptions.
# Params:
# concept: The original concept used.
# computeSet: The compute friendly data set used.
generateFunctionStringNoBreakpoint<-function(concept, computeSet)
{
  srcAVar<-concept$srcA$variable
  srcBVar<-concept$srcB$variable
  paste(sep="", concept$dest$variable,"<-function(", srcAVar,", ",srcBVar, ")\n","{\n",
        "  if (",srcAVar," * (", computeSet$decX, ") + ",srcBVar," * ",computeSet$decY, " > ", computeSet$decTar,")\n",
        "     return (", computeSet$caseAOff, " + ",srcAVar," * ", computeSet$caseAFac, ")\n",
        "  else\n",
        "     return (", computeSet$caseBOff, " + ",srcBVar," * ", computeSet$caseBFac, ")\n","}\n")
}


#============================================================================




