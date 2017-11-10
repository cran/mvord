#' @title Marginal Predictions for Multivariate Ordinal Regression Models.
#'
#' @description
#' Obtains marginal predictions/fitted measures for objects of class \code{"mvord"}.
#' @param object of class \code{mvord}
#' @param type \code{c("prob", "class", "pred","cum.prob")}
# #' @param newdata (optional) data frame of new covariates and new responses.
# #' The names of the variables should correspond to the names of the
# #'  variables used to fit the model. By default the data on which the model
# #'  was estimated is considered.
#' @param subjectID (optional) vector specifying for which subjectIDs the predictions\cr or fitted values should be computed.
#' @param ... further arguments passed to or from other methods.
#' @details The following types can be chosen in \code{marginal.predict}:
#' \tabular{ll}{
#'   \code{type} \tab description\cr
#'   \code{"prob"} \tab (default) fitted marginal probabilities for the observed response categories.\cr
#'   \code{"class"} \tab fitted marginal classes of the observed responses\cr
#'   \code{"pred"} \tab linear predictor \cr
#'   \code{"cum.prob"} \tab fitted marginal cumulative probabilities for the observed response categories\cr
#'   \code{"all.prob"} \tab fitted marginal probabilities for all ordered classes of each response
#'   }
# #'   \code{newdata} has to be in the same data format as in the fitted object of class \code{"mvord"}.
#'
#' The current implementation supports only in-sample predictions.
#' The rownames of the output correspond to the subjectIDs.
#' @seealso \link{predict.mvord}, \code{\link{get.prob}}
#' @export
marginal.predict <- function(object, type = "prob", subjectID = NULL, ...){
  #NEWDATA is NULL
  args <- list(...)
  exist <- "newdata" %in% names(args)
  if(!exist) newdata <- NULL
  if (!is.null(newdata)) stop("newdata is not supported at the moment!")
  if(is.null(newdata)){
    x <- object$rho$x
    y <- object$rho$y
    error.struct.x <- attr(object$error.struct, "covariate")
  } else if(object$rho$function.name == "mvord") {
    data.mvord <- mvord_data(newdata, object$rho$index, object$rho$response.name, unique(c(object$rho$x.names, object$rho$weights.name)),
                                 y.levels = object$rho$response.levels, response.names = object$rho$response.names)

    y <- data.mvord$y
    x <- lapply(1:object$rho$ndim, function(j) {
      if(all(is.na(data.mvord$x[[j]]))){
        tmp <- matrix(NA, ncol = ncol(object$rho$x[[j]]), nrow = nrow(y))
        tmp
      } else {
      tmp <- model.matrix(as.formula(paste0("~",deparse(object$rho$formula[[3]]))), data.mvord$x[[j]])
      attribute <- attr(tmp, "assign")
      tmp <- tmp[match(rownames(data.mvord$x[[j]]),rownames(tmp)),]
      rownames(tmp) <- rownames(data.mvord$x[[j]])
      attr(tmp, "assign") <- attribute
      tmp
      }
    })
  } else if(object$rho$function.name == "mvord2"){
    y <- newdata[,object$rho$y.names]
    x <- lapply(1:object$rho$ndim, function(j) model.matrix(object$rho$formula,
              model.frame(object$rho$formula, newdata, na.action = function(x)x)))
 }

  if(is.null(subjectID)) ind <- seq_len(NROW(y)) else ind <- match(subjectID, rownames(y))

  ## getbetas

  beta <- lapply(seq_len(object$rho$ndim), function(j){
    sapply(seq_along(object$rho$coef.names), function(p) {
      ifelse(is.na(object$rho$coef.ind[[j]][,p]), 0, object$beta[object$rho$coef.ind[[j]][,p]])
    })
  })

  pred.fixed <- sapply(1:object$rho$ndim, function(j) (rowSums(object$rho$x[[j]] * beta[[j]]) + object$rho$offset[[j]])[ind] )
  sigma <- get_error_struct(object, type = "sigmas")[ind]
  stddevs <- sqrt(t(sapply(sigma, diag)))

  if(type == "pred"){
    colnames(pred.fixed) <- object$rho$y.names
    rownames(pred.fixed) <- rownames(y)[ind]
    return(pred.fixed)
  } else if(type == "class"){
    y.ord <- sapply(1:object$rho$ndim, function(j){
      cut(pred.fixed[,j],c(-Inf,object$theta[[j]],Inf), labels= levels(object$rho$y[,j]))}, simplify = "array")
    colnames(y.ord) <- object$rho$y.names
    rownames(y.ord) <- rownames(y)[ind]
    return(y.ord)
  } else if(type %in% c("prob", "cum.prob", "all.prob")){
    theta.lower <- lapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]]))
    theta.upper <- lapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value))

    probs <- lapply(1:object$rho$ndim, function(j){
      pr <- sapply(1:length(theta.lower[[j]]), function(k){
        pred.lower <- (theta.lower[[j]][k] - pred.fixed[,j])/stddevs[,j]
        pred.upper <- (theta.upper[[j]][k] - pred.fixed[,j])/stddevs[,j]
        object$rho$link$F_uni(pred.upper) - object$rho$link$F_uni(pred.lower)
      })
      colnames(pr) <- levels(object$rho$y[, j])
      rownames(pr) <- rownames(y)[ind]
      pr
    })
    if(type == "prob"){
      prob <- sapply(1:object$rho$ndim, function(j) {
        ind2 <- cbind(1:nrow(probs[[j]]), match(y[ind, j],colnames(probs[[j]])))
        probs[[j]][ind2]})
      colnames(prob) <- object$rho$y.names
      rownames(prob) <- rownames(y)[ind]
      return(prob)
    } else if(type == "cum.prob"){
      cum.prob <- sapply(1:object$rho$ndim, function(j) {ind2 <- match(object$rho$y[ind, j],colnames(probs[[j]]))
      sapply(1:nrow(probs[[j]]), function(i) if(is.na(ind2[i])) NA else  sum(probs[[j]][i,seq_len(ind2[i])]))})
      colnames(cum.prob) <- object$rho$y.names
      rownames(cum.prob) <- rownames(y)[ind]
      return(cum.prob)
    } else if(type == "all.prob"){
      names(probs) <- object$rho$y.names
      return(probs)
    }
  }
}


#' @title Predict method for Multivariate Ordinal Regression Models.
#' @description Obtains predicted or fitted values for objects of class \code{"mvord"}.
#' @param object of class \code{mvord}
#' @param type \code{c("class.max", "prob", "cum.prob")}
# #' @param newdata (optional) data frame of new covariates and new responses.
#' @param subjectID (optional) vector specifying for which subjectIDs the predictions\cr or fitted values should be computed.
#' @param ... further arguments passed to or from other methods.
#' @details
#' \tabular{ll}{
#'   \code{type} \tab description\cr
#'   \code{"class.max"} \tab combination of response categories with the highest probability in the fitted model\cr
#'   \code{"prob"} \tab (default) fitted joint probability for the observed response categories\cr
#'   \code{"cum.prob"} \tab fitted joint cumulative probability for the observed response categories
#'   }
# #' \code{newdata} has to be in the same data format as in the fitted object of class \code{"mvord"}.
#'
#' The current implementation supports only in-sample predictions.
#' The rownames of the output correspond to the subjectIDs.
#' @seealso \code{\link{marginal.predict}}, \code{\link{get.prob}}
#' @method predict mvord
#' @export
predict.mvord <- function(object, type = "prob", subjectID = NULL, ...){
  # checks
  if (is.null(object$rho$link$F_multi)) stop("Multivariate probabilities cannot be computed! Try marginal.predict()!")

  #NEWDATA is NULL
  args <- list(...)
  exist <- "newdata" %in% names(args)
  if(!exist) newdata <- NULL
  if (!is.null(newdata)) stop("newdata is not supported at the moment!")

  if(is.null(newdata)){
    x <- object$rho$x
    y <- object$rho$y
    error.struct.x <- attr(object$error.struct, "covariate")

  } else if(object$rho$function.name == "mvord") {
    data.mvord <- mvord_data(newdata, object$rho$index, object$rho$response.name, unique(c(object$rho$x.names, object$rho$weights.name)),
                                 y.levels = object$rho$response.levels, response.names = object$rho$response.names)

    y <- data.mvord$y
    x <- lapply(1:object$rho$ndim, function(j) {
      if(all(is.na(data.mvord$x[[j]]))){
        tmp <- matrix(NA, ncol = ncol(object$rho$x[[j]]), nrow = nrow(y))
        tmp
      } else {
      tmp <- model.matrix(as.formula(paste0("~",deparse(object$rho$formula[[3]]))), data.mvord$x[[j]])
      attribute <- attr(tmp, "assign")
      tmp <- tmp[match(rownames(data.mvord$x[[j]]),rownames(tmp)),]
      rownames(tmp) <- rownames(data.mvord$x[[j]])
      attr(tmp, "assign") <- attribute
      tmp
      }
    })
    if(is.null(subjectID)) ind <- seq_len(nrow(y)) else ind <- match(subjectID, rownames(y))

  } else if(object$rho$function.name == "mvord2"){
    y <- newdata[,object$rho$y.names]
    x <- lapply(1:object$rho$ndim, function(j) model.matrix(object$rho$formula,
                                                            model.frame(object$rho$formula, newdata, na.action = function(x)x)))
  }

  if(is.null(subjectID)) ind <- seq_len(nrow(y)) else ind <- match(subjectID, rownames(y))

  ## get correlation/covariance matrices
  sigma <- get_error_struct(object$error.struct, type ="sigmas")

  ## get betas
  beta <- lapply(seq_len(object$rho$ndim), function(j){
    sapply(seq_along(object$rho$coef.names), function(p) {
      ifelse(is.na(object$rho$coef.ind[[j]][,p]), 0, object$beta[object$rho$coef.ind[[j]][,p]])
    })
  })

  pred.fixed <- sapply(1:object$rho$ndim, function(j) (rowSums(object$rho$x[[j]] * beta[[j]]) + object$rho$offset[[j]])[ind] )
  theta.lower <- sapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]])[y[ind, j]])
  theta.upper <- sapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value)[y[ind, j]])
  stddevs <- sqrt(t(sapply(sigma, diag)))[ind, ]
  pred.lower <- (theta.lower - pred.fixed)/stddevs
  pred.lower[is.na(pred.lower)] <- -10000
  pred.upper <- (theta.upper - pred.fixed)/stddevs
  pred.upper[is.na(pred.upper)] <- 10000

  if(type == "prob"){
    prob <- object$rho$link$F_multi(U = pred.upper, L = pred.lower,
                              list_R = lapply(sigma, cov2cor))
    names(prob) <- rownames(y)[ind]
    return(prob)
  } else if (type == "cum.prob"){
    pred.lower <- matrix(-10000, ncol = object$rho$ndim, nrow = nrow(theta.lower))
    cum.prob <- object$rho$link$F_multi(U = pred.upper, L = pred.lower,
                              list_R = lapply(sigma, cov2cor))
    names(cum.prob) <- rownames(y)[ind]
    return(cum.prob)
  } else if(type == "class.max"){
    theta.lower.all <- lapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]]))
    theta.upper.all <- lapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value))

    #all combinations
    cats <- lapply(1:object$rho$ndim, function(j) 1:(object$rho$ntheta[j] + 1))
    if (prod(object$rho$ntheta + 1) > 1e6) {
       stop("Number of class combinations over 1000000. Try get.prob() for desired class combinations.")
    } else {
    cmbn <- expand.grid(cats)

    pred.fixed[is.na(pred.fixed)] <- 0

    probs <- sapply(seq_len(nrow(cmbn)), function(i){
      if (i %% 100 == 0)  cat('Computed probabilities for', i, 'out of', nrow(cmbn),'combinations\n')
      theta.lower <- sapply(1:object$rho$ndim, function(j) theta.lower.all[[j]][cmbn[i,j]])
      theta.upper <- sapply(1:object$rho$ndim, function(j) theta.upper.all[[j]][cmbn[i,j]])
      pred.lower <- - sweep(pred.fixed, 2, theta.lower)/stddevs
      pred.upper <- - sweep(pred.fixed, 2, theta.upper)/stddevs
      object$rho$link$F_multi(U = pred.upper, L = pred.lower,
                       list_R = lapply(sigma, cov2cor))
    })
    ind.max <- apply(probs,1,which.max)
    class.max <- cmbn[ind.max,]
    rownames(class.max) <- rownames(y)[ind]
    return(class.max)
   }
  }
}

#' @title Extracts fitted Probabilities for Multivariate Ordinal Regression Models.
#'
#' @description
#' Extracts fitted probabilities for given response categories from a fitted model of class \code{"mvord"}.
#' @param object of class \code{mvord}
#' @param response.cat vector or matrix with response categories (for each subject one row with q multiple measurements).
# #' @param newdata (optional) data frame of new covariates and new responses.
# #' The names of the variables should correspond to the names of the
# #'  variables used to fit the model. By default the data on which the model
# #'  was estimated is considered.
#' @param subjectID (optional) vector specifying for which subjectIDs the predictions\cr or fitted values should be computed.
#' @param ... further arguments passed to or from other methods.
#' @details
# #' \code{newdata} has to be in the same data format as in the fitted object of class \code{"mvord"}.
#'
#' The current implementation supports only in-sample predictions.
#' The rownames of the output correspond to the subjectIDs.
#' @seealso \code{\link{predict.mvord}}, \code{\link{marginal.predict}}
#' @export
get.prob <- function(object, response.cat, subjectID = NULL, ...) {
  #checks
  if (is.null(object$rho$link$F_multi)) stop("Multivariate probabilities cannot be computed! Try marginal.predict()!")

  args <- list(...)
  exist <- "newdata" %in% names(args)
  if(!exist) newdata <- NULL
  if (!is.null(newdata)) stop("newdata is not supported at the moment!")
  if(is.null(newdata)){
    x <- object$rho$x
    y <- object$rho$y
    error.struct.x <- attr(object$error.struct, "covariate")

  } else if(object$rho$function.name == "mvord") {
    data.mvord <- mvord_data(newdata, object$rho$index,
    	                     object$rho$response.name, unique(c(object$rho$x.names, object$rho$weights.name)),
                             y.levels = object$rho$response.levels,
                             response.names = object$rho$response.names)

    y <- data.mvord$y
    x <- lapply(1:object$rho$ndim, function(j) {
      if(all(is.na(data.mvord$x[[j]]))){
        tmp <- matrix(NA, ncol = ncol(object$rho$x[[j]]), nrow = nrow(y))
        tmp
      } else {
        tmp <- model.matrix(as.formula(paste0("~",deparse(object$rho$formula[[3]]))), data.mvord$x[[j]])
        attribute <- attr(tmp, "assign")
        tmp <- tmp[match(rownames(data.mvord$x[[j]]),rownames(tmp)),]
        rownames(tmp) <- rownames(data.mvord$x[[j]])
        attr(tmp, "assign") <- attribute
        tmp
      }
    })
    if(is.null(subjectID)) ind <- seq_len(nrow(y)) else ind <- match(subjectID, rownames(y))

  } else if(object$rho$function.name == "mvord2"){
    y <- newdata[,object$rho$y.names]
    x <- lapply(1:object$rho$ndim, function(j) model.matrix(object$rho$formula,
                                                            model.frame(object$rho$formula, newdata, na.action=function(x)x)))
  }

  if(is.null(subjectID)) ind <- seq_len(nrow(y)) else ind <- match(subjectID, rownames(y))

   ## get correlation/covariance matrices
  sigma <- get_error_struct(object$error.struct, type ="sigmas")
  stddevs <- sqrt(t(sapply(sigma, diag)))[ind, ]

  if(is.vector(response.cat)) response.cat <- matrix(response.cat, nrow = 1)
  response.cat <- lapply(1:object$rho$ndim, function(j){
  	if (!all(response.cat[,j] %in% levels(y[,j])))  stop("response.cat are different from the categories in the original data set")
    else ordered(response.cat[,j], levels = levels(y[,j]))
   })
  beta <- lapply(seq_len(object$rho$ndim), function(j){
    sapply(seq_along(object$rho$coef.names), function(p) {
      ifelse(is.na(object$rho$coef.ind[[j]][,p]), 0, object$beta[object$rho$coef.ind[[j]][,p]])
    })
  })

  pred.fixed <- sapply(1:object$rho$ndim, function(j) (rowSums(object$rho$x[[j]] * beta[[j]]) + object$rho$offset[[j]])[ind] )
  if (is.null(dim(pred.fixed)))  dim(pred.fixed) <- c(1, length(pred.fixed))

  theta.lower <- sapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]])[response.cat[[j]]])
  theta.upper <- sapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value)[response.cat[[j]]])
  pred.lower <- -sweep(pred.fixed, 2, theta.lower)/stddevs
  pred.lower[is.na(pred.lower)] <- -10000
  pred.upper <- -sweep(pred.fixed, 2, theta.upper)/stddevs
  pred.upper[is.na(pred.upper)] <- 10000
  prob <- object$rho$link$F_multi(U = pred.upper, L = pred.lower,
                    list_R = lapply(sigma, cov2cor))
  names(prob) <- rownames(y)[ind]
  return(prob)
}
