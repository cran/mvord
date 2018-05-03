#' @title Marginal Predictions for Multivariate Ordinal Regression Models.
#'
#' @description
#' Obtains marginal predictions/fitted measures for objects of class \code{'mvord'}.
#' @param object an objecz of class \code{'mvord'}.
#' @param type types \code{"prob"}, \code{"class"}, \code{"pred"}, \code{"cum.prob"} are available.
# #' @param newdata (optional) data frame of new covariates and new responses.
# #' The names of the variables should correspond to the names of the
# #'  variables used to fit the model. By default the data on which the model
# #'  was estimated is considered.
#' @param subjectID (optional) vector specifying for which subjectIDs the predictions\cr or fitted values should be computed.
#' @param ... further arguments passed to or from other methods.
#' @details The following types can be chosen in \code{marginal_predict}:
#' \tabular{ll}{
#'   \code{type} \tab description\cr
#'   \code{"prob"} \tab (default) fitted marginal probabilities for the observed response categories.\cr
#'   \code{"class"} \tab fitted marginal classes of the observed responses.\cr
# #'   \code{"pred"} \tab linear predictor \cr
#'   \code{"cum.prob"} \tab fitted marginal cumulative probabilities for the observed response categories.\cr
#'   \code{"all.prob"} \tab fitted marginal probabilities for all ordered classes of each response.
#'   }
# #'   \code{newdata} has to be in the same data format as in the fitted object of class \code{'mvord'}.
#'
#' The current implementation supports only in-sample predictions.
#' The row names of the output correspond to the subjectIDs.
#' @seealso \code{\link{predict.mvord}}, \code{\link{joint_probabilities}}
#' @export
marginal_predict <- function(object, type = "prob", subjectID = NULL, ...){
  #NEWDATA is NULL
  if(!(type %in% c("prob", "class", "cum.prob", "all.prob"))) stop("invalid type!")
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
        rhs.form <- object$formula
        rhs.form[[2]] <- NULL
      tmp <- model.matrix(rhs.form, data.mvord$x[[j]])
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

  if(is.null(subjectID)) ind <- seq_len(NROW(y)) else {
    if(!all(subjectID %in% rownames(y))) stop("Not all subjectIDs in data!")
    ind <- match(subjectID, rownames(y))
  }

  sigma <- error_structure(object, type = "sigmas")[ind]
  stddevs <- sqrt(t(sapply(sigma, diag)))

  if(type == "prob"){
  XcatL <- list()
  XcatU <- list()
  for (j in seq_len(object$rho$ndim)) {
    ncat <- object$rho$ntheta[j] + 1
    mm <- model.matrix(~ - 1 + y[ind,j] : x[[j]][ind,],
                       model.frame(~ - 1 + y[ind,j] : x[[j]][ind,],
                                   na.action = function(x) x))
    XcatL[[j]] <- mm[,-(ncat * (seq_len(object$rho$p) - 1) + 1), drop = F]
    XcatU[[j]] <- mm[,-(ncat * seq_len(object$rho$p)), drop = F]

  }

  pred.fixedU  <- sapply(1:object$rho$ndim, function(j) {
     b <- lapply(seq_along(object$rho$constraints),function(i)
       object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
     object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
     XcatU[[j]] %*% unlist(b) + object$rho$offset[[j]][ind]
  })
  pred.fixedL  <- sapply(1:object$rho$ndim, function(j) {
     b <- lapply(seq_along(object$rho$constraints),function(i)
       object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
     object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
     XcatL[[j]] %*% unlist(b) + object$rho$offset[[j]][ind]
  })

  theta.lower <- sapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]])[y[ind, j]])
  theta.upper <- sapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value)[y[ind, j]])

  prob <- sapply(1:object$rho$ndim, function(j){
      pred.lower <- (theta.lower[,j] - pred.fixedL[,j])/stddevs[,j]
      pred.upper <- (theta.upper[,j] - pred.fixedU[,j])/stddevs[,j]
      object$rho$link$F_uni(pred.upper) - object$rho$link$F_uni(pred.lower)
  })
  colnames(prob) <- object$rho$y.names
  rownames(prob) <- rownames(y)[ind]
return(prob)
  } else if(type %in% c("class", "cum.prob", "all.prob")){
    theta.lower <- lapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]]))
    theta.upper <- lapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value))

    probs <- lapply(1:object$rho$ndim, function(j){
      pr <- sapply(1:object$rho$ncat[j], function(k){

        ytmp <- y[ind,j]
        ytmp[seq_along(ytmp)] <- levels(ytmp)[k]
          ncat <- object$rho$ntheta[j] + 1
          mm <- model.matrix(~ - 1 + ytmp : x[[j]][ind,],
                             model.frame(~ - 1 + ytmp : x[[j]][ind,],
                                         na.action = function(x) x))
          XcatL <- mm[,-(ncat * (seq_len(object$rho$p) - 1) + 1), drop = F]
          XcatU <- mm[,-(ncat * seq_len(object$rho$p)), drop = F]

          b <- lapply(seq_along(object$rho$constraints),function(i)
            object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
              object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
          pred.fixedU  <- XcatU %*% unlist(b) + object$rho$offset[[j]][ind]

          b <- lapply(seq_along(object$rho$constraints),function(i)
            object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
              object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
          pred.fixedL  <- XcatL %*% unlist(b) + object$rho$offset[[j]][ind]

        theta.lower <- sapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]])[ytmp])
        theta.upper <- sapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value)[ytmp])

        pred.lower <- (theta.lower[,j] - pred.fixedL)/stddevs[,j]
        pred.upper <- (theta.upper[,j] - pred.fixedU)/stddevs[,j]
        object$rho$link$F_uni(pred.upper) - object$rho$link$F_uni(pred.lower)
      })
      colnames(pr) <- levels(object$rho$y[, j])
      rownames(pr) <- rownames(y)[ind]
      pr
    })
    names(probs) <- object$rho$y.names

    if(type == "class"){
      y.ord <- do.call("cbind.data.frame", lapply(1:object$rho$ndim, function(j) {
        ordered(apply(probs[[j]], 1, function(i) object$rho$levels[[j]][which.max(i)]), levels = object$rho$levels[[j]])
      }))
      colnames(y.ord) <- object$rho$y.names
      return(y.ord)
    } else if(type == "cum.prob"){
      cum.prob <- sapply(1:object$rho$ndim, function(j) {ind2 <- match(object$rho$y[ind, j],colnames(probs[[j]]))
      sapply(1:nrow(probs[[j]]), function(i) if(is.na(ind2[i])) NA else  sum(probs[[j]][i,seq_len(ind2[i])]))})
      colnames(cum.prob) <- object$rho$y.names
      rownames(cum.prob) <- rownames(y)[ind]
      return(cum.prob)
    } else if(type == "all.prob"){
      return(probs)
    }
  }
}


#' @title Predict method for Multivariate Ordinal Regression Models.
#' @description Obtains predicted or fitted values for objects of class \code{'mvord'}.
#' @param object an object of class \code{'mvord'}.
#' @param type types \code{"class"}, \code{"prob"} and \code{"cum.prob"} are available.
# #' @param newdata (optional) data frame of new covariates and new responses.
#' @param subjectID (optional) vector specifying for which subjectIDs the predictions\cr or fitted values should be computed.
#' @param ... further arguments passed to or from other methods.
#' @details
#' \tabular{ll}{
#'   \code{type} \tab description\cr
#'   \code{"class"} \tab combination of response categories with the highest probability in the fitted model.\cr
#'   \code{"prob"} \tab (default) fitted joint probability for the observed response categories.\cr
#'   \code{"cum.prob"} \tab fitted joint cumulative probability for the observed response categories.
#'   }
# #' \code{newdata} has to be in the same data format as in the fitted object of class \code{'mvord'}.
#'
#' The current implementation supports only in-sample predictions.
#' The row names of the output correspond to the subjectIDs.
#' @seealso \code{\link{marginal_predict}}, \code{\link{joint_probabilities}}
#' @method predict mvord
#' @export
predict.mvord <- function(object, type = "prob", subjectID = NULL, ...){
  # checks
  if (is.null(object$rho$link$F_multi)) stop("Multivariate probabilities cannot be computed! Try marginal_predict()!")
  if(!(type %in% c("prob", "class", "cum.prob"))) stop("invalid type!")
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
        rhs.form <- object$formula
        rhs.form[[2]] <- NULL
      tmp <- model.matrix(rhs.form, data.mvord$x[[j]])
      attribute <- attr(tmp, "assign")
      tmp <- tmp[match(rownames(data.mvord$x[[j]]),rownames(tmp)),]
      rownames(tmp) <- rownames(data.mvord$x[[j]])
      attr(tmp, "assign") <- attribute
      tmp
      }
    })
    if(is.null(subjectID)) ind <- seq_len(nrow(y)) else {
      if(!all(subjectID %in% rownames(y))) stop("Not all subjectIDs in data!")
      ind <- match(subjectID, rownames(y))
    }

  } else if(object$rho$function.name == "mvord2"){
    y <- newdata[,object$rho$y.names]
    x <- lapply(1:object$rho$ndim, function(j) model.matrix(object$rho$formula,
                                                            model.frame(object$rho$formula, newdata, na.action = function(x)x)))
  }

  if(is.null(subjectID)) ind <- seq_len(nrow(y)) else {
    if(!all(subjectID %in% rownames(y))) stop("Not all subjectIDs in data!")
    ind <- match(subjectID, rownames(y))
  }
  ## get correlation/covariance matrices
  sigma <- error_structure(object$error.struct, type ="sigmas")
  stddevs <- sqrt(t(sapply(sigma, diag)))[ind, ]

  XcatL <- list()
  XcatU <- list()
  for (j in seq_len(object$rho$ndim)) {
    ncat <- object$rho$ntheta[j] + 1
    mm <- model.matrix(~ - 1 + y[ind,j] : x[[j]][ind,],
                       model.frame(~ - 1 + y[ind,j] : x[[j]][ind,],
                                   na.action = function(x) x))
    XcatL[[j]] <- mm[,-(ncat * (seq_len(object$rho$p) - 1) + 1), drop = F]
    XcatU[[j]] <- mm[,-(ncat * seq_len(object$rho$p)), drop = F]

  }
  pred.fixedU  <- sapply(1:object$rho$ndim, function(j) {
    b <- lapply(seq_along(object$rho$constraints),function(i)
      object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
        object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
    XcatU[[j]] %*% unlist(b) + object$rho$offset[[j]][ind]
  })
  pred.fixedL  <- sapply(1:object$rho$ndim, function(j) {
    b <- lapply(seq_along(object$rho$constraints),function(i)
      object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
        object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
    XcatL[[j]] %*% unlist(b) + object$rho$offset[[j]][ind]
  })

  theta.lower <- sapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]])[y[ind, j]])
  theta.upper <- sapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value)[y[ind, j]])

  pred.lower <- (theta.lower - pred.fixedL)/stddevs
  pred.lower[is.na(pred.lower)] <- -10000
  pred.upper <- (theta.upper - pred.fixedU)/stddevs
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
  } else if(type == "class"){
    theta.lower.all <- lapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]]))
    theta.upper.all <- lapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value))

    #all combinations
    cats <- lapply(1:object$rho$ndim, function(j) 1:(object$rho$ntheta[j] + 1))
    if (prod(object$rho$ntheta + 1) > 1e6) {
       stop("Number of class combinations over 1000000. Try joint_probabilities() for desired class combinations.")
    } else {
    cmbn <- expand.grid(cats)
    cmbn.labels <- expand.grid(object$rho$levels)

    probs <- sapply(seq_len(nrow(cmbn)), function(i){
      if (i %% 100 == 0)  cat('Computed probabilities for', i, 'out of', nrow(cmbn),'combinations\n')

      ###############################################
      response.cat <- sapply(1:object$rho$ndim, function(j) object$rho$levels[[j]][cmbn[i,j]])
      response.cat <- matrix(response.cat, ncol = length(response.cat), nrow = length(ind), byrow = TRUE)#matrix(response.cat, nrow = 1)
      response.cat <- lapply(1:object$rho$ndim, function(j){
        if (!all(response.cat[,j] %in% c(NA, levels(y[,j]))))  stop("response.cat are different from the categories in the original data set")
        else ordered(response.cat[,j], levels = levels(y[,j]))
      })

      XcatL <- list()
      XcatU <- list()
      for (j in seq_len(object$rho$ndim)) {
        ncat <- object$rho$ntheta[j] + 1
        mm <- model.matrix(~ - 1 + response.cat[[j]] : object$rho$x[[j]][ind,],
                           model.frame(~ - 1 + response.cat[[j]] : object$rho$x[[j]][ind,],
                                       na.action = function(x) x))
        XcatL[[j]] <- mm[,-(ncat * (seq_len(object$rho$p) - 1) + 1), drop = F]
        XcatU[[j]] <- mm[,-(ncat * seq_len(object$rho$p)), drop = F]
      }

      pred.fixedU  <- sapply(1:object$rho$ndim, function(j) {
        b <- lapply(seq_along(object$rho$constraints),function(i)
          object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
            object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
        XcatU[[j]] %*% unlist(b) + object$rho$offset[[j]][ind]
      })
      pred.fixedL  <- sapply(1:object$rho$ndim, function(j) {
        b <- lapply(seq_along(object$rho$constraints),function(i)
          object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
            object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
        XcatL[[j]] %*% unlist(b) + object$rho$offset[[j]][ind]
      })
      theta.lower <- sapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]])[response.cat[[j]]])
      theta.upper <- sapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value)[response.cat[[j]]])
      pred.lower <- (theta.lower - pred.fixedL)/stddevs
      pred.upper <- (theta.upper - pred.fixedU)/stddevs
      pred.lower[is.na(pred.lower)] <- -10000
      pred.upper[is.na(pred.upper)] <- 10000
      ############################################
      object$rho$link$F_multi(U = pred.upper, L = pred.lower,
                       list_R = lapply(sigma, cov2cor))
    })
    ind.max <- apply(probs,1,which.max)
    class <- cmbn.labels[ind.max,]
    rownames(class) <- rownames(y)[ind]
    colnames(class) <- object$rho$y.names
    return(class)
   }
  }
}

#' @title Extracts fitted Probabilities for Multivariate Ordinal Regression Models.
#'
#' @description
#' Extracts fitted probabilities for given response categories from a fitted model of class \code{'mvord'}.
#' @param object an object of class \code{'mvord'}.
#' @param response.cat vector or matrix with response categories (for each subject one row of length equal to the number of multiple measurements).
# #' @param newdata (optional) data frame of new covariates and new responses.
# #' The names of the variables should correspond to the names of the
# #'  variables used to fit the model. By default the data on which the model
# #'  was estimated is considered.
#' @param type \code{"prob"} for joint probabilities and \code{"cum.prob"} for joint cumulative probabilities.
#' @param subjectID (optional) vector specifying for which subjectIDs the predictions\cr or fitted values should be computed.
#' @param ... further arguments passed to or from other methods.
#' @details
# #' \code{newdata} has to be in the same data format as in the fitted object of class \code{'mvord'}.
#'
#' The current implementation supports only in-sample predictions.
#' The row names of the output correspond to the subjectIDs.
#' @seealso \code{\link{predict.mvord}}, \code{\link{marginal_predict}}
#' @export
joint_probabilities <- function(object, response.cat, type = "prob", subjectID = NULL, ...) {
  #checks
  if (is.null(object$rho$link$F_multi)) stop("Multivariate probabilities cannot be computed! Try marginal_predict()!")

  args <- list(...)
  exist <- "newdata" %in% names(args)
  if(!type %in% c("prob", "cum.prob")) stop("Invalid type chosen. Only types prob and cum.prob are available.")
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
        rhs.form <- object$formula
        rhs.form[[2]] <- NULL
        tmp <- model.matrix(rhs.form, data.mvord$x[[j]])
        attribute <- attr(tmp, "assign")
        tmp <- tmp[match(rownames(data.mvord$x[[j]]),rownames(tmp)),]
        rownames(tmp) <- rownames(data.mvord$x[[j]])
        attr(tmp, "assign") <- attribute
        tmp
      }
    })
    if(is.null(subjectID)) ind <- seq_len(nrow(y)) else {
      if(!all(subjectID %in% rownames(y))) stop("Not all subjectIDs in data!")
      ind <- match(subjectID, rownames(y))
    }

  } else if(object$rho$function.name == "mvord2"){
    y <- newdata[,object$rho$y.names]
    x <- lapply(1:object$rho$ndim, function(j) model.matrix(object$rho$formula,
                                                            model.frame(object$rho$formula, newdata, na.action=function(x)x)))
  }

  if(is.null(subjectID)) ind <- seq_len(nrow(y)) else ind <- match(subjectID, rownames(y))

   ## get correlation/covariance matrices
  sigma <- error_structure(object$error.struct, type ="sigmas")
  stddevs <- sqrt(t(sapply(sigma, diag)))[ind, ]

  if(is.vector(response.cat)) response.cat <- matrix(response.cat, ncol = length(response.cat), nrow = length(ind), byrow = TRUE)#matrix(response.cat, nrow = 1)
  response.cat <- lapply(1:object$rho$ndim, function(j){
  	if (!all(response.cat[,j] %in% c(NA, levels(y[,j]))))  stop("response.cat are different from the categories in the original data set")
    else ordered(response.cat[,j], levels = levels(y[,j]))
   })

  XcatL <- list()
  XcatU <- list()
  for (j in seq_len(object$rho$ndim)) {
    ncat <- object$rho$ntheta[j] + 1
    mm <- model.matrix(~ - 1 + response.cat[[j]] : object$rho$x[[j]][ind,],
                       model.frame(~ - 1 + response.cat[[j]] : object$rho$x[[j]][ind,],
                                   na.action = function(x) x))
    XcatL[[j]] <- mm[,-(ncat * (seq_len(object$rho$p) - 1) + 1), drop = F]
    XcatU[[j]] <- mm[,-(ncat * seq_len(object$rho$p)), drop = F]

  }

  pred.fixedU  <- sapply(1:object$rho$ndim, function(j) {
     b <- lapply(seq_along(object$rho$constraints),function(i)
       object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
     object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
     XcatU[[j]] %*% unlist(b) + object$rho$offset[[j]][ind]
  })
  pred.fixedL  <- sapply(1:object$rho$ndim, function(j) {
     b <- lapply(seq_along(object$rho$constraints),function(i)
       object$rho$constraints[[i]][object$rho$inds.cat[[j]],,drop=F] %*%
     object$beta[object$rho$nbeta.first[i] + seq_len(object$rho$npar.beta[i]) - 1])
     XcatL[[j]] %*% unlist(b) + object$rho$offset[[j]][ind]
  })
  if (is.null(dim(pred.fixedU)))  dim(pred.fixedU) <- c(1, length(pred.fixedU))
  if (is.null(dim(pred.fixedL)))  dim(pred.fixedL) <- c(1, length(pred.fixedL))

  theta.lower <- sapply(1:object$rho$ndim, function(j) c(-object$rho$inf.value, object$theta[[j]])[response.cat[[j]]])
  theta.upper <- sapply(1:object$rho$ndim, function(j) c(object$theta[[j]], object$rho$inf.value)[response.cat[[j]]])
  pred.lower <- (theta.lower - pred.fixedL)/stddevs
  pred.upper <- (theta.upper - pred.fixedU)/stddevs
  pred.lower[is.na(pred.lower)] <- -10000
  pred.upper[is.na(pred.upper)] <- 10000

  if(type == "cum.prob") pred.lower <- matrix(-10000, ncol = object$rho$ndim, nrow = nrow(theta.lower))
  prob <- object$rho$link$F_multi(U = pred.upper, L = pred.lower,
                    list_R = lapply(sigma, cov2cor))
  names(prob) <- rownames(y)[ind]
  return(prob)
}
