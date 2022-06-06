library(rpart)

# Functions for user splits: init, eval and split

# init function
itemp <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
}

# eval function
etemp <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label = wmean, deviance = rss)
}

# split function
stemp <- function(y, wt, x, parms, continuous)
{
  prob = runif(1,0,1)
  # Center y
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    if (prob<parms$m)
      return(list(goodness = goodness, direction = sign(lmean)))
    else
      return(list(goodness = double(length(goodness)), direction = sign(lmean)))
  } else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    # For anova splits, we can order the categories by their means
    # then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    if (prob<parms$m)
      return(list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2), direction = ux[ord]))
    else
      return(list(goodness = double(length(left.wt)), direction = ux[ord]))

  }
}

# Functions for random forest: fit and predict
#' Binary Classification with Random Forest
#'
#' Build random forest for binary classification where all trees are created with the rpart package.
#' @param formula A data frame or a matrix of predictors, or a formula describing the model to be fitted.
#' @param data Data frame containing the variables in the model.
#' @param ntree Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
#' @param m_frac Probability for each variable to be randomly sampled as candidate at each split.
#' @param minsplit The minimum number of observations that must exist in a node of a tree in order for a split to be attempted.
#' @return A list of ntree 'rpart' objects.
#' @examples 
#' fit = randForest(target ~ ., data=df_train, ntree=50, m_frac=0.1)
#' fit = randForest(satisfied ~ class + delay, data=planes, ntree=500, m_frac=0.5)
#' @export
randForest <- function(formula, data, ntree=50, m_frac=0.2, minsplit=20){
  set.seed(0)
  fit_list = list()

  method_list = list(eval = etemp, split = stemp, init = itemp)
  cat('\nTree out of ', ntree, ':', sep='')
  for (i in 1:ntree) {
    if (i %% (as.integer(ntree/10))==0 || ntree<10)
      cat(' ', i)
    # create subset with replacement
    df = sample_frac(data, size = 1, replace = TRUE)
    # build tree with rpart and user split function
    fit = rpart(formula, data=df, method=method_list, parms=list(m_frac=m_frac), control=list(minsplit=minsplit))
    # fit = rpart(formula=formula, method='anova', data=df)
    fit_list = c(fit_list, list(fit))
  }
  class(fit_list) = 'randForest'
  return(fit_list)
}

# predict function
#' predict.randForest: predict method for randForest objects
#'
#' Prediction of test data using random forest.
#' @param object an object of class 'randForest', as that created by the function 'randForest'.
#' @param data Data frame containing new data.
#' @return A vector of predicted values (0 or 1) is returned.
#' @examples 
#' pred = predict(fit, df_test)
#' @export
predict.randForest <- function(object, data, ...){
  
  scores = numeric(nrow(data))
  
  # cast votes for each tree in forest
  for (fit in object) {
    pred = predict(fit, data)
    # normalize scores
    pred = pred - min(pred)
    pred = pred / max(pred)
    if(all(!is.na(pred)))
      scores = scores + pred
  }
  scores = scores/length(object)

  scores[scores<0.5] = 0
  scores[scores>=0.5] = 1

  return(scores)
}

