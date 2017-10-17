set.threshold.type <- function(rho){
#fixall
  if (all(sapply(1:rho$ndim, function(j) all(!is.na(rho$threshold.values[[j]]))))) {#all thresholds are fixed in all dimensions
      if (rho$intercept == FALSE) cat("We suggest to include an intercept in the model (formula = y ~ 1 + ...)")
      type <- "fixall"
#fix2first
  } else if (all(sapply(1:rho$ndim, function(j){
    #all first two thresholds are not NA
      (all(length(which(!is.na(rho$threshold.values[[j]])))==length(c(1,2))) && all(which(!is.na(rho$threshold.values[[j]])) == c(1,2))
      ) || ((length(rho$threshold.values[[j]]) == 1) &&  (which(!is.na(rho$threshold.values[[j]])) == 1))
      }))){
      if (rho$error.structure$type %in% c("corGeneral", "corAR1", "corEqui")){
        cat("We suggest to fix only one threshold or the intercept in a correlation model.\n")
      }
      if ((rho$error.structure$type %in% c("covGeneral"))&& (rho$intercept.type == "fixed")){
        cat("We suggest to fix either two thresholds or one threshold and the intercept in a covGeneral model.\n")
      }
      type <- "fix2first"
#fix2firstlast
  } else if (all(sapply(1:rho$ndim, function(j){
      (all(length(which(!is.na(rho$threshold.values[[j]])))==length(c(1,rho$ntheta[j]))) &&
       all(which(!is.na(rho$threshold.values[[j]])) == c(1,rho$ntheta[j]))#all first and last two thresholds are not NA
      ) || ((length(rho$threshold.values[[j]]) == 1) &&  (which(!is.na(rho$threshold.values[[j]])) == 1))
      }))){
      if (rho$error.structure$type %in% c("corGeneral", "corAR1", "corEqui")){
        cat("We suggest to fix only one threshold or the intercept in a correlation model.\n")
      }
      if ((rho$error.structure$type %in% c("covGeneral"))&& (rho$intercept.type == "fixed")){
        cat("We suggest to fix either two thresholds or one threshold and the intercept in a covGeneral model.\n")
      }
      type <- "fix2firstlast"
#fix1first
      #all first thresholds are not NA (and no additional)
  } else if (all(sapply(1:rho$ndim, function(j) (length(which(!is.na(rho$threshold.values[[j]])) >= 1) &&
                                                 all(which(!is.na(rho$threshold.values[[j]])) == 1))))){
      if ((rho$error.structure$type == "covGeneral") && (rho$intercept.type == "flexible")) stop("Model with covGeneral is not identifiable.
                          Please either fix two thresholds or one threshold and the intercept.\n", call. = FALSE)
      if ((rho$error.structure$type %in% c("corGeneral", "corAR1", "corEqui"))&& (rho$intercept.type == "fixed")){
        cat("We suggest to fix only one threshold or the intercept in a correlation model.\n")
      }
      type <- "fix1first"
#flexible
  } else if (all(sapply(1:rho$ndim, function(j) all(is.na(rho$threshold.values[[j]]))))){#all thresholds NA
      if (rho$error.structure$type == "covGeneral") stop("Model with covGeneral is not identifiable.
                                                        Please either fix two thresholds or one threshold and the intercept.\n", call. = FALSE)
      if ((rho$error.structure$type %in% c("corGeneral", "corAR1", "corEqui")) && (rho$intercept.type == "flexible")){
          stop("Model is not identifiable. Please either fix one threshold or the intercept.", call. = FALSE)
      }
      type <- "flexible"
#ERRORS
  } else stop("Either fix all thresholds in one or more outcome dimensions,
              or consistently in all other outcome dimensions, all first thresholds or none.\n", call. = FALSE)
  if((rho$error.structure$type == "covGeneral") && (rho$binary == TRUE) && rho$intercept == TRUE){
      stop("In the presence of binary outcomes intercept and at least one threshold
                                  have to be fixed to some value.\n", call. = FALSE)
  }
  type
}


set.error.structure <- function(error.structure, data.x, ndim){
  if (error.structure$formula == ~1 ) {
    if (error.structure$type %in% c("corEqui", "corAR1")) {
     error.structure$x <- model.matrix(error.structure$formula, data.x[[1]])
   } else {
     error.structure$x <- as.factor(model.matrix(error.structure$formula, data.x[[1]]))
   }
  } else if (error.structure$type %in% c("corEqui", "corAR1")){

    tmp <- lapply(1:ndim, function(j) {
      tmp1 <- model.matrix(error.structure$formula, data.x[[j]])
      attribute <- attr(tmp1, "assign")
      tmp2 <- tmp1[match(rownames(data.x[[j]]),rownames(tmp1)),]
      rownames(tmp2) <- rownames(data.x[[j]])
      attr(tmp2, "assign") <- attribute
      tmp2
    }
    )
    error.x <- sapply(1:ncol(tmp[[1]]), function(k){
      xtcol <- do.call(cbind,lapply(tmp, `[`,,k))
      xtcol_final <- apply(xtcol,1,function(i) unique(i[!is.na(i)]))
      if(is.list(xtcol_final)) stop("Covariates used in the error structure need to be constant across multiple observations", call.=FALSE)
      xtcol_final
    })

    colnames(error.x) <- colnames(tmp[[1]])
    attr(error.x, "assign") <- attr(tmp[[1]], "assign")

    error.structure$x <- error.x
  } else if (error.structure$type %in%  c("corGeneral", "covGeneral")){
    #depends on one factor
    if (length(all.vars(error.structure$formula[[2]])) > 1) stop("only one factor is allowed in covGeneral and corGeneral", call. = FALSE)
    if (!is.factor(data.x[[1]][,all.vars(error.structure$formula)])){
      stop("variable must be of type factor in covGeneral and corGeneral", call. = FALSE)
    }
    tmp <- lapply(1:ndim,function(j) data.x[[j]][,all.vars(error.structure$formula)])

    if (!all(sapply(1:ndim, function(j) all(tmp[[1]]==tmp[[j]], na.rm=T)))){
      stop("Covariate dependent variables need to be constant across multiple observations", call. = FALSE)
    }
    error.structure$x <- as.factor(apply(do.call("cbind.data.frame", tmp), 1, function(x) unique(x[!is.na(x)])))
     } else {
    #depends on one factor
    if (length(all.vars(error.structure$formula[[2]])) > 1) stop("only one factor is allowed in covGeneral and corGeneral", call. = FALSE)
    if (!is.factor(data[,all.vars(error.structure$formula)])) stop("variable must be of type factor in covGeneral and corGeneral", call. = FALSE)
    error.structure$x <- data.x[,all.vars(error.structure$formula)]
  }
  error.structure
}


set.error.structure.mvord2 <- function(error.structure, data){
  if (error.structure$type  %in%  c("corEqui", "corAR1")){
    error.structure$x <- model.matrix(error.structure$formula, data)
 # } else if (error.structure$type == "corAR1"){
  #  stop("corAR1 is not applicable in mvord2", call. = FALSE)
  } else if (error.structure$type %in%  c("corGeneral", "covGeneral")){
    if (error.structure$formula == ~ 1){
      error.structure$x <- as.factor(model.matrix(error.structure$formula, data))
    } else{ #depends on one factor
      if (length(all.vars(error.structure$formula[[2]])) > 1) stop("only one factor is allowed in covGeneral and corGeneral", call. = FALSE)
      if (!is.factor(data[,all.vars(error.structure$formula)])) stop("variable must be of type factor in covGeneral and corGeneral", call. = FALSE)
      error.structure$x <- data[,all.vars(error.structure$formula)]
    }
  }
  error.structure
}

checkArgs <- function(rho){
  #CHECK if treshold.values is in line with threshold.constraints
  if (length(rho$threshold.constraints) != rho$ndim) stop("dimensions of threshold.values and number of thresholds do not match", call. = FALSE)
    if (any(sapply(1:rho$ndim, function(j) length(rho$threshold.values[[j]]) != rho$ntheta[j])))
      stop("dimensions of threshold.values and number of thresholds do not match", call. = FALSE)
  for (j in unique(rho$threshold.constraints)){
    ind <- which(rho$threshold.constraints == j)
    if (length(unique(rho$threshold.values[ind]))!=1){
        stop("If constraints are set on thresholds (by threshold.constraints), threshold.values need to be specified accordingly
              for these outcome dimensions. Maybe dimensions do not have the same number of threshold parameters.", call. = FALSE)
    }
  }
  if (nrow(rho$coef.constraints) != rho$ndim) stop("row dimension of coef.constraints and outcome dimension do not match (?factor)", call. = FALSE)

  for (j in 1:ncol(rho$coef.constraints)){
    indj <- unique(rho$coef.constraints[,j])
    indj <- indj[!is.na(indj)]
    lapply(seq_len(length(indj)), function(k) {
      tmpind <- which(rho$coef.constraints[,j] == indj[k])
      tmp <- rho$coef.values[tmpind,j]
      if(length(unique(tmp)) != 1) stop("If constraints are set on the coefficients (by coef.constraints),
                                        coef.values need to be specified accordingly for these outcome dimensions.", call. = FALSE)
    })
    }
}

##########################################
###### AUXILIARY FUNCTIONS ##############
###########################################################################
rectbiv.norm.prob <- function(U, L, r) {
  # computes the rectangle probabilities for biv.normal-distribution
    p1 <- pbivnorm(U[, 1], U[, 2], r)
    p2 <- pbivnorm(L[, 1], U[, 2], r)
    p3 <- pbivnorm(U[, 1], L[, 2], r)
    p4 <- pbivnorm(L[, 1], L[, 2], r)
    ## replace NaN
    p1[is.nan(p1)] <- 0
    p2[is.nan(p2)] <- 0
    p3[is.nan(p3)] <- 0
    p4[is.nan(p4)] <- 0
    pr <- p1 - p2 - p3 + p4
    return(pr)
}

biv.nt.prob2 <-function (df, lower, upper,
                         mean=c(0,0), r) {
  # computes the rectangle probabilities for biv.t-distribution
    nu <- df
    rho <- r
    infin <- as.integer(c(2, 2))
    prob <- as.double(0)
    a <- .Fortran("smvbvt", prob, nu, lower, upper, infin, rho,
                  PACKAGE = "mvord")
    return(a[[1]])
}

casesNA <- function(y) {
  # returns a column vector v, assigning a different number/label to each unique combination of NA pattern
  contrast <- as.matrix((!is.na(y)) + 0)
  contrast.char <- apply(contrast, 1, paste, collapse = "") # make each contrast row as character i.e., 111 has all three response variables
  factor(contrast.char, levels = unique(contrast.char))
}

split.NA.pattern <- function(y) {
  # y should be a data.frame
  # returns a list, whose elements are groups of the matrix y according to each unique combination of response variables
  if (!is.data.frame(y)) y <- as.data.frame(y)
  yg <- split(y, casesNA(y)) # grouped y by cases of NA pattern
  lapply(yg, function(x)  which(rownames(y) %in% rownames(x)))
}



getStart.values <- function(rho){
 gammas <- sapply(1:rho$ndim, function(j) {
   if (rho$npar.theta.opt[j] != 0){
    theta <- if (rho$ntheta[j] >= 2) polr(rho$y[, j] ~1)$zeta else 0
    if (!grepl("mvlogit", rho$link$name)) theta <- theta/1.7
    c(theta[1L], log(diff(theta)))[1:rho$npar.theta.opt[j]]
  } else NULL
})
c(unlist(gammas), rep(0, rho$npar.betas))
}


transf.par.cor <- function(par, rho) {
  sigmas <- rho$transf.sigmas(par[rho$npar.thetas + rho$npar.betas + seq_len(rho$npar.sigmas)],rho)
  theta <- rho$transf.thresholds(par[seq_len(rho$npar.thetas)], rho)
  par_beta <- par[rho$npar.thetas + seq(rho$npar.betas)]
  beta <- sapply(1:ncol(rho$coef.constraints), function(j){
    sapply(1:nrow(rho$coef.constraints), function(i,j) ifelse(is.na(rho$ind.coef[i,j]), rho$coef.values[i,j], par_beta[rho$ind.coef[i, j]]), j)
  })
  pred.fixed <- sapply(1:rho$ndim, function(j) rho$x[[j]] %*% beta[j, ])
  theta.lower <- sapply(1:rho$ndim, function(j) c(-rho$inf.value, theta[[j]])[rho$y[, j]])
  theta.upper <- sapply(1:rho$ndim, function(j) c(theta[[j]], rho$inf.value)[rho$y[, j]])
  pred.lower <- (theta.lower - pred.fixed)
  pred.upper <- (theta.upper - pred.fixed)
  list(U = pred.upper,
       L = pred.lower,
       sigmas = sigmas)
}

transf.par.cov <- function(par, rho) {
  exp.par.sd <- exp(par[rho$npar.thetas + rho$npar.betas +
    rho$npar.cor * rho$ncor.levels +
    seq_len(rho$npar.cor.sd * rho$ncor.levels)])
  sigmas <- rho$transf.sigmas(par[rho$npar.thetas + rho$npar.betas + seq_len(rho$npar.sigmas)],rho,
                                exp.par.sd)
  exp.par.sd <- lapply(1:rho$ncor.levels, function(l) exp.par.sd[ (l-1) * rho$npar.cor.sd + seq_len(rho$npar.cor.sd) ])
  theta <- rho$transf.thresholds(par[seq_len(rho$npar.thetas)], rho)
  par_beta <- par[rho$npar.thetas + seq_len(rho$npar.betas)]
  beta <- sapply(1:ncol(rho$coef.constraints), function(j){
    sapply(1:nrow(rho$coef.constraints), function(i,j)
      ifelse(is.na(rho$ind.coef[i,j]), rho$coef.values[i,j], par_beta[rho$ind.coef[i, j]]), j)
  })
  pred.fixed <- sapply(1:rho$ndim, function(j) rho$x[[j]] %*% beta[j, ])
  theta.lower <- sapply(1:rho$ndim, function(j) c(-rho$inf.value, theta[[j]])[rho$y[, j]])
  theta.upper <- sapply(1:rho$ndim, function(j) c(theta[[j]], rho$inf.value)[rho$y[, j]])
  ## make matrix of exp.par.sd
  lev <- match(rho$error.structure$x, rho$error.structure$levels)
  pred.lower <- theta.lower - pred.fixed
  pred.upper <- theta.upper - pred.fixed
  for (l in 1:rho$ncor.levels){
  pred.lower[lev==l,] <-t(t(pred.lower[lev==l,])/exp.par.sd[[l]])
  pred.upper[lev==l,] <-t(t(pred.upper[lev==l,])/exp.par.sd[[l]])
  }
  sigmas <- lapply(sigmas, cov2cor)
  list(U = pred.upper,
       L = pred.lower,
       sigmas = sigmas,
       std.dev = exp.par.sd)
}
#########################################################################
## transformation of the threshold parameters (to ensure monotonicity) ##
#########################################################################
transf.thresholds.fixall <- function(gamma, rho){
  rho$threshold.values
}

transf.thresholds.fix1.first <- function(gamma, rho){
  ## \theta_j = a + exp(gamma_1) + .. + exp(gamma_j)
  lapply(seq_len(rho$ndim), function(j) {
    cumsum(c(rho$threshold.values.fixed[[j]][1], exp(gamma[rho$ind.thresholds[[j]]])))
  })
}

transf.thresholds.fix2.first <- function(gamma, rho){
  ## \theta_j = a + b + exp(gamma_1) + .. + exp(gamma_j)
     lapply(seq_len(rho$ndim), function(j) {
       a <- rho$threshold.values.fixed[[j]][1] ## a bounds
       b <- rho$threshold.values.fixed[[j]][2] ## b bounds
       if (is.na(b)) b <- NULL ## it implies one can have binary with fix2first
       c(a, cumsum(c(b, exp(gamma[rho$ind.thresholds[[j]]]))))
     })
}

transf.thresholds.fix2.firstlast <- function(gamma, rho){
  ## (theta_j - theta_{j-1})/(1 - theta_j) = exp(gamma_j)/(1 + exp(gamma_j))
  lapply(seq_len(rho$ndim), function(j){
    gamma1  <- gamma[rho$ind.thresholds[[j]]]
    a <- rho$threshold.values.fixed[[j]][1]
    b <- rho$threshold.values.fixed[[j]][2]
    if (!is.na(b)) {
    recursive.theta <- function(i) {
      if (i == 0) 0
      else return ((exp(gamma1[i]) + recursive.theta(i - 1))/(1 + exp(gamma1[i])))
    }
    theta <- unlist(sapply(seq_len(length(gamma1)), function(i) recursive.theta(i)))
    c(0, theta, 1) * (b - a) + a
    } else a
    })
}

transf.thresholds.flexible <- function(gamma, rho){
  lapply(1:rho$ndim, function(j)
    if (anyNA(rho$threshold.values[[j]])){
      if (rho$ntheta[j] > 1) {
        cumsum(c(gamma[rho$ind.thresholds[[j]][1]],
                 exp(gamma[rho$ind.thresholds[[j]][2:rho$ntheta[j]]])))
      } else if (rho$ntheta[j] == 1) gamma[rho$ind.thresholds[[j]]] else NULL
    } else rho$threshold.values[[j]]
  )
}

##############################################################################
## transformation of the parameters of the correlation/covariance structure ##
##############################################################################

transf.sigmas.spheric <- function(par.nu, rho, exp.par.sd = NULL) {
  lapply(1:rho$ncor.levels, function(l) {
    nu <- par.nu[(l - 1) * rho$npar.cor + seq_len(rho$npar.cor)]
    angles <- pi * exp(nu)/(1 + exp(nu))
    cosmat <- diag(rho$ndim)
    cosmat[lower.tri(cosmat)] <- cos(angles)
    S1 <- matrix(0, nrow = rho$ndim, ncol = rho$ndim)
    S1[, 1L] <- 1
    S1[-1L, -1L][lower.tri(S1[-1L, -1L], diag = T)] <- sin(angles)
    tLmat <- sapply(1:rho$ndim,
                    function(j) cosmat[j, ] * cumprod(S1[j, ]))
    sigma <- crossprod(tLmat)
    if (is.null(exp.par.sd)) {
      return(sigma)
    } else {
     stdev <-  exp.par.sd[(l - 1) * rho$npar.cor.sd + seq_len(rho$npar.cor.sd)]
     return(t(stdev * sigma) * stdev)
    }
  })
}

transf.sigmas.corAR1 <- function(par.sigma, rho, exp.par.sd = NULL) {
  w <- par.sigma[seq_len(rho$npar.cor)]
  z <- rho$error.structure$x %*% w
  r <- z2r(z)
  lapply(seq_len(length(r)), function(i){
    sigma <- diag(rho$ndim)
    sigma[lower.tri(sigma)]  <- r[i]^sequence((rho$ndim-1):1)
    sigma <- sigma + t(sigma) - diag(diag(sigma))
    sigma
  })
}

transf.sigmas.corEqui <- function(par.sigma, rho, exp.par.sd = NULL) {
  w <- par.sigma[seq_len(rho$npar.cor)]
  z <- rho$error.structure$x %*% w
  z2r(z)
}

z2r <- function (z) {
  ifelse(z > 354, 1, (exp(2 * z) - 1)/(1 + exp(2 * z)))
}

getInd.coef <- function(coef.constraints, coef.values) {
  ind <- matrix(NA,ncol = ncol(coef.constraints), nrow = nrow(coef.constraints))
  k <- 1
  i <- 1
  for(j in 1:ncol(coef.constraints)){
    if (is.na(coef.values[i,j])){
      ind[i,j] <- k
      k <- k + 1
    }
  }
  for(i in 2:nrow(coef.constraints)){
    for(j in 1:ncol(coef.constraints)){
      if (is.na(coef.values[i,j]) && !(coef.constraints[i,j] %in% coef.constraints[1:(i-1),j])){
        ind[i,j] <- k
        k <- k + 1
      }
      if (is.na(coef.values[i,j]) && coef.constraints[i,j] %in% coef.constraints[1:(i-1),j]){
        ind[i,j] <- ind[min(which(coef.constraints[i,j] == coef.constraints[1:(i-1),j])),j]
      }
    }
  }
  ind
}

getInd.thresholds <- function(threshold.constraints,rho){
  rho$npar.theta.opt <- rho$npar.theta
  rho$npar.theta.opt[duplicated(threshold.constraints)] <- 0
  cs <- c(0, cumsum(rho$npar.theta.opt)[-length(rho$npar.theta.opt)])
  lapply(seq_len(rho$ndim), function(j){
    if (!duplicated(threshold.constraints)[j]) {
        seq_len(rho$npar.theta[j]) + cs[j]
    } else {
        indj <- which(threshold.constraints == threshold.constraints[j])
        if(length(unique(rho$npar.theta[indj])) != 1)
            stop("Constraints on threshold parameters are not valid
                (different number of categories)", call. = FALSE)
        seq_len(rho$npar.theta[indj[1]]) + cs[indj[1]]
    }
  })
}

get.labels.theta <- function(rho,j) {
  lev <- levels(rho$y[, j])
  sapply(1:(rho$ntheta[j]), function(i){
    paste(lev[i], lev[i + 1], sep = "|")
  })
}

#' @title Error Structures in mvord
#' @description Different \code{error.structures} are available in \pkg{mvord}:
#' \itemize{
#' \item general correlation structure (default) \code{corGeneral(~1)},
#' \item general covariance structure \code{covGeneral(~1)},
#' \item factor dependent correlation structure \code{covGeneral(~f)},
#' \item factor dependent covariance structure \code{covGeneral(~f)},
#' \item covariate dependent equicorrelation structure \code{corEqui(~S)},
#' \item AR(1) correlation structure \code{corAR1(~1)}, or
#' \item covariate dependent AR(1) correlation structure \code{corAR1(~S)}.
#' }
#' See \code{\link{error.structures}} or vignette.
#' @param formula \code{\link{formula}} object
#' @export
#' @name error.structures
corGeneral <- function(formula) UseMethod("corGeneral")
#' @rdname error.structures
#' @export
corGeneral.formula <- function(formula){
  return(list(type = "corGeneral", formula = formula))
}

# #' @details Equicorelation structure
# #' @param formula \code{\link{formula}} object
#' @export
#' @rdname error.structures
corEqui <- function(formula) UseMethod("corEqui")
#' @rdname error.structures
#' @export
corEqui.formula <- function(formula){
  return(list(type = "corEqui", formula = formula))
}

# #' @details General (symmetric) covariance structure
# #' @param formula \code{\link{formula}} object
#' @export
#' @rdname error.structures
covGeneral <- function(formula) UseMethod("covGeneral")
#' @rdname error.structures
#' @export
covGeneral.formula <- function(formula){
  return(list(type = "covGeneral", formula = formula))
}

# #' @details AR(1) correlation structure
# #' @param formula \code{\link{formula}} object
#' @export
#' @rdname error.structures
corAR1 <- function(formula) UseMethod("corAR1")
#' @rdname error.structures
#' @export
corAR1.formula <- function(formula){
  return(list(type = "corAR1", formula = formula))
}

backtransf.sigmas <- function(R){
  J <- nrow(R)
  l <- t(chol(R))
  angmat <- matrix(1,ncol=J,nrow=J)
  angmat[-1,1] <- acos(l[-1,1])
  for (j in 2:(J-1)){
    sinprod <- apply(sin(angmat[, seq_len(j-1), drop=F]), 1, prod) ## denominator in division
    angmat[-(1:j),j]<-acos((l/sinprod)[-(1:j),j])
  }
  angdivpi <- angmat[lower.tri(angmat)]/pi
  log(angdivpi/(1-angdivpi))
}

get.sigma.i <- function(object) {
  if (object$rho$error.structure$type == "corEqui") {
    lapply(1:object$rho$n, function(i) {
        tmp <- matrix(object$r[i], nrow = object$rho$ndim, ncol = object$rho$ndim)
        diag(tmp) <- 1
        tmp
      })
  } else {
  lev <- match(object$rho$error.structure$x, object$rho$error.structure$levels)
  lapply(1:object$rho$n, function(i) object$sigmas[lev[i]][[1]])
  }
}

# #' @title Data preparation for mvord
# #'
# #' @description
# #' This function is an (internally) used to transforms the \code{data}, into a "multivariate setting",
# #' where all repeated measurements are matched accordingly to their ID. A matrix of all ordinal responses with \code{J} columns
# #' as well as a list of length \code{J} of matrices with all the covariates are created.
# #' @param data a \code{data.frame}, where each row corresponds to a single measurement.
# #' @param index is an (optional) argument that specifies the index for the subjects and the response index of the multiple measurement.
# #' This is usually performed
# #' by a character vector of length two specifying the column names of the subject index and
# #' the multiple response index in data. The default value of index is NULL assuming that the
# #' first column of data contains the subject index and the second column the multiple response index.
# #' @param y.names column name of \code{data} where the ordinal observations are stored.
# #' @param x.names column names of all the covariates in {data}.
# #' @param y.levels (optional) list of length \code{J} that specifies the levels of each repeated measurement. If the categories
# #' differ across repeated measurements (either the number of categories or the category labels) it is recommended to set them.
# #' @param response.names (optional) vector of names of the repeated measurements in \code{data}
# #' which specifies the ordering of the repeated measurements.
# #' @export

mvord.data <- function(data, index, y.names, x.names,
                         y.levels = NULL, response.names = NULL) {
  df <- list()
  if (any(duplicated(data[,index]))) stop("duplicated observation(s) for one index", call. = FALSE)
  index.levels <- levels(as.factor(data[, index[2]]))
  data.split <- split(data[,c(y.names, index[1])], data[, index[2]])
  data.split.y <- lapply(seq_len(length(data.split)), function(j) {
    colnames(data.split[[j]]) <- c(index.levels[j],index[1])
    data.split[[j]]})
  df$y <- Reduce(function(...) merge(..., by = index[1], all = TRUE), data.split.y )
  df$y <- df$y[, - match(index[1], colnames(df$y)), drop = FALSE]
  response.names.NA <- c()
  if (!is.null(response.names)) {
    response.names.NA <- response.names[!response.names %in% colnames(df$y)]
    df$y <- cbind(df$y, matrix(NA, ncol = length(response.names.NA), nrow = nrow(df$y), dimnames = list(c(),response.names.NA)))
    df$y <- df$y[,as.character(response.names)]
  }
  colnames.y <- colnames(df$y)
  if (is.null(y.levels)) {
    df$y <- do.call(cbind.data.frame, lapply(1:ncol(df$y), function(j) ordered(df$y[, j])))
  } else {
    df$y <- do.call(cbind.data.frame, lapply(1:ncol(df$y), function(j) {
      if (!all(unique(df$y[!is.na(df$y[, j]), j]) %in% y.levels[[j]])) stop("levels of response do not match with y.levels", call. = FALSE)
      ordered(df$y[, j], levels = y.levels[[j]])}
      ))
  }
  colnames(df$y) <- colnames.y
  #X
  data.split.x <- split(data[, c(x.names, index[1])], data[, index[2]])
  names.x <- names(data.split.x)
  #set colnames (otherwise warning due to identical colnames in reduce)
  data.split.x <- lapply(seq_len(length(data.split.x)), function(j) {
    colnames(data.split.x[[j]]) <- c(paste(x.names,j, sep = "."), index[1])
    data.split.x[[j]]})


  xdatadf <- Reduce(function(...) merge(...,by = index[1], all = TRUE), data.split.x)#[, -1]
  subject_id_names <- xdatadf[,index[1]]
  rownames(xdatadf) <- subject_id_names
  xdatadf <- xdatadf[, -match(index[1], colnames(xdatadf)), drop = FALSE]
  xdatadf <- cbind(xdatadf, matrix(NA, ncol = length(response.names.NA) * length(x.names),
                                   nrow = nrow(xdatadf)))
  df$x <- lapply(1:(length(index.levels) + length(response.names.NA)), function(i) {
    tmp <- xdatadf[,(i - 1) * length(x.names) + seq_len(length(x.names))]
    names(tmp) <- x.names
    tmp
  })

  names(df$x) <- c(names.x, response.names.NA)
  if (!is.null(response.names)) df$x <- df$x[as.character(response.names)]

  rownames(df$y) <- subject_id_names
  indallNA <- which(rowSums(is.na(df$y)) == NCOL(df$y))
  if (length(indallNA) != 0) {
    df$y <- df$y[-indallNA, ]
    df$x <- lapply(df$x, function(a) {a <- a[-indallNA, ]; a})
  }

  df
}

check <- function(...){
  stopifnot(...)
}

theta2gamma <- function(theta){
  gamma <- c()
  gamma[1] <- theta[1]
  if(length(theta) >= 2){
    for (i in 2:length(theta)){
      gamma[i] <- log(theta[i] - theta[i-1])
    }
  }
  gamma
}

r2sigma <- function(r, object){
  if(object$rho$error.structure$type == "corAR1"){
    sigma <- lapply(seq_len(length(r)), function(i){
      tmp <- diag(object$rho$ndim)
      tmp[lower.tri(tmp)]  <- r[i]^sequence((object$rho$ndim-1):1)
      tmp <- tmp + t(tmp) - diag(diag(tmp))
      rownames(tmp) <- colnames(tmp) <- colnames(object$rho$y)
      tmp
    })
  } else if (object$rho$error.structure$type == "corEqui") {
    sigma <- lapply(seq_len(length(r)), function(i) {
      tmp <- matrix(r[i], nrow = object$rho$ndim, ncol = object$rho$ndim)
      diag(tmp) <- 1
      rownames(tmp) <- colnames(tmp) <- colnames(object$rho$y)
      tmp
    })
  }
  sigma
}


#' @title Multivariate link functions in mvord
#' @description Different \code{link} functions are available in \pkg{mvord}:
#' @details We allow for two different link functions, the multivariate probit
#' link and the multivariate logit link.
#' For the multivariate probit
#' link a multivariate normal distribution for the errors is applied. The
#' normal bivariate probabilities which enter the pairwise log-likelihood
#' are computed with the package \pkg{pbivnorm}.
#'
#' For the multivariate logit link a \eqn{t} copula based multivariate
#' distribution with logistic margins is used.
#'   The \code{mvlogit()} function has an optional integer valued argument
#' \code{df} which specifies the degrees of freedom to be used for the
#' \eqn{t} copula.  The default value of the degrees of freedom parameter is
#' 8. We restrict the degrees of freedom to be integer valued because the
#' most efficient routines for computing bivariate \eqn{t}~probabilities do
#' not support non-integer degrees of freedom. For further details see vignette.
#' @param df integer specifying the degrees of freedom of the t copula
#' @export
#' @name mvlinks
mvprobit <- function() return(list(name = "mvprobit"))
#' @export
#' @rdname mvlinks
mvlogit <- function(df = 8L){
  return(list(name = "mvlogit", df = df))
}
