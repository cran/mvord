set_threshold_type <- function(rho){
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
      if (rho$error.structure$type ==  "correlation"){
        cat("We suggest to fix only one threshold or the intercept in a correlation model.\n")
      }
      if ((rho$error.structure$type == "covariance")&& (rho$intercept.type == "fixed")){
        cat("We suggest to fix either two thresholds or one threshold and the intercept in a cov_general model.\n")
      }
      type <- "fix2first"
#fix2firstlast
  } else if (all(sapply(1:rho$ndim, function(j){
      (all(length(which(!is.na(rho$threshold.values[[j]])))==length(c(1,rho$ntheta[j]))) &&
       all(which(!is.na(rho$threshold.values[[j]])) == c(1,rho$ntheta[j]))#all first and last two thresholds are not NA
      ) || ((length(rho$threshold.values[[j]]) == 1) &&  (which(!is.na(rho$threshold.values[[j]])) == 1))
      }))){
      if (rho$error.structure$type ==  "correlation"){
        cat("We suggest to fix only one threshold or the intercept in a correlation model.\n")
      }
      if ((rho$error.structure$type == "covariance")&& (rho$intercept.type == "fixed")){
        cat("We suggest to fix either two thresholds or one threshold and the intercept in a cov_general model.\n")
      }
      type <- "fix2firstlast"
#fix1first
      #all first thresholds are not NA (and no additional)
  } else if (all(sapply(1:rho$ndim, function(j) (length(which(!is.na(rho$threshold.values[[j]])) >= 1) &&
                                                 all(which(!is.na(rho$threshold.values[[j]])) == 1))))){
      if ((rho$error.structure$type == "covariance") && (rho$intercept.type == "flexible")) stop("Model with cov_general is not identifiable.
                          Please either fix two thresholds or one threshold and the intercept.\n", call. = FALSE)
      if ((rho$error.structure$type == "correlation")&& (rho$intercept.type == "fixed")){
        cat("We suggest to fix only one threshold or the intercept in a correlation model.\n")
      }
      type <- "fix1first"
#flexible
  } else if (all(sapply(1:rho$ndim, function(j) all(is.na(rho$threshold.values[[j]]))))){#all thresholds NA
      if (rho$error.structure$type == "covariance") stop("Model with cov_general is not identifiable.
                                                        Please either fix two thresholds or one threshold and the intercept.\n", call. = FALSE)
      if ((rho$error.structure$type == "correlation") && (rho$intercept.type == "flexible")){
          stop("Model is not identifiable. Please either fix one threshold or the intercept.", call. = FALSE)
      }
      type <- "flexible"
#ERRORS
  } else stop("Either fix all thresholds in one or more outcome dimensions,
              or consistently in all other outcome dimensions, all first thresholds or none.\n", call. = FALSE)
  if((rho$error.structure$type == "covariance") && (rho$binary == TRUE) && rho$intercept == TRUE){
      stop("In the presence of binary outcomes intercept and at least one threshold
                                  have to be fixed to some value.\n", call. = FALSE)
  }
  type
}
check_args_optimizer <- function(rho){
  allmeth <- c("Nelder-Mead",  "BFGS",  "CG",  "L-BFGS-B", "nlm",
    "nlminb", "spg", "ucminf", "newuoa", "bobyqa", "nmkb", "hjkb", "Rcgmin", "Rvmmin")
  if (is.character(rho$solver) && !(rho$solver %in% allmeth)) stop("Solver name not among the allowed methods in optimx.")
}

check_args_error.structure <- function(error.structure, data){
  allmeth <- c("cor_general",  "cov_general",  "cor_ar1",
   "cor_equi", "cor_ident", "cor_rel_var")
  if (!(as.character(error.structure[[1]]) %in% allmeth)) stop("error.structure not among allowed methods in mvord.")

  if(!class(error.structure[[2]]) == "formula") stop("formula in error.structure has to be of class formula.", call. = FALSE)

  if(any(!(all.vars(error.structure[[2]]) %in% colnames(data)))) stop("All variables in error.structure have to exist in data.", call. = FALSE)

}

check_args_thresholds <- function(rho){
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
}

check_args_coef <- function(rho){
  if (nrow(rho$coef.constraints) != rho$ndim) stop("Row dimension of coef.constraints and outcome dimension do not match.", call. = FALSE)
  if (NCOL(rho$coef.constraints) != NCOL(rho$x[[1]])) stop("Column dimension of coef.constraints and number of columns in model matrix do not match.
                                                           Maybe (Intercept) is included or specify constraints for factors or interaction terms accordingly.", call. = FALSE)

  for (j in seq_len(NCOL(rho$coef.constraints))){
    indj <- unique(rho$coef.constraints[,j])
    indj <- indj[!is.na(indj)]
    lapply(seq_along(indj), function(k) {
      tmpind <- which(rho$coef.constraints[,j] == indj[k])
      tmp <- rho$coef.values[tmpind,j]
      if(length(unique(tmp)) != 1) stop("If constraints are set on the coefficients (by coef.constraints),
                                        coef.values need to be specified accordingly for these outcome dimensions.", call. = FALSE)
    })
  }
}
#
# check_args_coef2 <- function(rho){
#   if (nrow(rho$coef.constraints) != rho$ndim) stop("Row dimension of coef.constraints and outcome dimension do not match.", call. = FALSE)
#   tmp <- sum(apply(apply(rho$coef.constraints, 2, is.na),2,all) * apply(!apply(rho$coef.values, 2, is.na),2,all))
#   if ((NCOL(rho$coef.constraints)-tmp) != NCOL(rho$x[[1]])) stop("Column dimension of coef.constraints and number of columns in model matrix do not match.
#                                                                  Maybe (Intercept) is included or specify constraints for factors or interaction terms accordingly.", call. = FALSE)
#
#   for (j in seq_len(NCOL(rho$coef.constraints))){
#     indj <- unique(rho$coef.constraints[,j])
#     indj <- indj[!is.na(indj)]
#     lapply(seq_along(indj), function(k) {
#       tmpind <- which(rho$coef.constraints[,j] == indj[k])
#       tmp <- rho$coef.values[tmpind,j]
#       if(length(unique(tmp)) != 1) stop("If constraints are set on the coefficients (by coef.constraints),
#                                         coef.values need to be specified accordingly for these outcome dimensions.", call. = FALSE)
#     })
#     }
#   }

check_args_constraints <- function(rho){
  if (!all(rho$coef.names %in% names(rho$constraints)))
    stop("coef.constraints need to be specified for all covariates.", call. = FALSE)
  #check dimensions of rho$constraints
  if(any(sapply(rho$constraints, NROW) != rho$nthetas)) stop("coef.constraints need to have number of total categories rows
                                                           (sum of number of categories for all dimensions).", call. = FALSE)

}

check_args_input1 <- function(rho, data){
  #data
  if(any(!(all.vars(rho$formula) %in% colnames(data)))) stop("All variables in formula have to exist in data.", call. = FALSE)
  ## index
  if(!all(rho$index %in% colnames(data))) stop("index names do not match with column names of data", call.=FALSE)
  if (any(duplicated(data[, rho$index]))) stop("Duplicated indexes: Observation(s) have the same subject and measurement index", call. = FALSE)
  # PL.lag
  if(!is.null(rho$PL.lag)) {
    if (rho$PL.lag <= 0) stop("PL.lag must be greater than 0", call. = FALSE)
  }
  # weights
  if(!is.null(rho$weights.name) && !is.character(rho$weights.name)) stop("argument weights has to be of type character.", call. = FALSE)
  }

check_args_input2 <- function(rho, data){
  #dim == 1
  if (rho$ndim == 1) stop("The outcome dimension of the model is 1. Better use e.g., MASS::polr() or ordinal::clm() instead.", call. = FALSE)
  #offset
  if(!is.null(rho$offset) && length(rho$offset) != rho$ndim) stop("offset has to be of length of the dimension of the model.", call. = FALSE)
  #PL.lag
  if(!is.null(rho$PL.lag)) {
    if (rho$error.structure$name != "cor_ar1") stop("Use PL.lag only with cor_ar1 error.structure", call. = FALSE)
    #  && !is.integer(rho$PL.lag)) stop("PL.lag must be positive and of type integer.", call. = FALSE)
    if (rho$PL.lag > rho$ndim) stop("PL.lag exceeds dimension of the model.", call. = FALSE)
  }
}

set_args_other <- function(rho) {
  ## coef contraints
  ## can be null, matrix, vector, list
  ## if null set to matrix
  if(is.null(rho$coef.constraints)){
    if(NCOL(rho$x[[1]]) > 0){
      rho$coef.constraints <- matrix(1:rho$ndim, ncol = NCOL(rho$x[[1]]), nrow = rho$ndim)
    } else {
      rho$coef.constraints <- matrix(integer(), ncol = 0, nrow = rho$ndim)
    }
  }
  ## list and coef values can't be used 
  if(is.list(rho$coef.constraints) && !is.null(rho$coef.values)) stop("This coef.constraints design requires offsets instead of coef.values.", call.=FALSE)
  if(!is.list(rho$coef.constraints) & NCOL(rho$coef.constraints) > 0){
      if(is.vector(rho$coef.constraints)) {
        if(NCOL(rho$x[[1]]) > 0){
          rho$coef.constraints <- matrix(rho$coef.constraints, ncol = NCOL(rho$x[[1]]), nrow = rho$ndim)
        } else {
          rho$coef.constraints <- matrix(integer(), ncol = 0, nrow = rho$ndim)
        }
      }
    if(is.null(rho$coef.values)){
    rho$coef.values <- matrix(NA, ncol = ncol(rho$coef.constraints),
      nrow = nrow(rho$coef.constraints))
    rho$coef.values[is.na(rho$coef.constraints)] <- 0 #default 0
    }
    #check if coef.values fit to coef.constraints
    check_args_coef(rho)
    # set coef.constraints NA  coef.values are set
    rho$coef.constraints[!is.na(rho$coef.values)] <- NA 
    rho$intercept.type <- ifelse(rho$intercept == FALSE, "fixed",
       ifelse(any(is.na(rho$coef.values[,1])), "flexible", "fixed"))
  } else {
    rho$intercept.type <- ifelse(rho$intercept == FALSE, "fixed", "flexible")
  } 
  ## threshold.contraints
  if(is.null(rho$threshold.constraints)) rho$threshold.constraints <- 1:rho$ndim
  ## PL.lag
  if (is.null(rho$PL.lag)) rho$PL.lag <- rho$ndim
  if (rho$PL.lag != round(rho$PL.lag)) {
    cat("PL.lag represents number of time units and must be a positive integer.
      Rounding to closest integer.\n")
    rho$PL.lag <- round(rho$PL.lag)
  }
  rho
}
##########################################
###### AUXILIARY FUNCTIONS ##############
##########################################
check_rank <- function(j,rho){
  y <- rho$y[,j]
  ind <- !is.na(y)
  y <- y[ind]
  lev <- levels(y)
  llev <- length(lev)
  y <- unclass(y)
  q <- llev %/% 2L
  y1 <- (y > q)

  if(rho$intercept == TRUE) x <- rho$x[[j]][ind,] else x <- cbind(Intercept = rep(1, rho$n), rho$x[[j]])[ind,]

  offset <- rho$offset[[j]][ind]
  method <- rho$link$name
suppressWarnings(
  if(method == "mvlogit") fit <- glm.fit(x, y1, family = binomial(), offset = offset) else{
    fit <- glm.fit(x, y1, family = binomial("probit"), offset = offset)
  }
)

  coefs <- fit$coefficients
  if(any(is.na(coefs))) {
    warning("Design appears to be rank-deficient, dropping some coefficients may help.")
  }
}




get_start_values <- function(rho){
 gammas <- sapply(1:rho$ndim, function(j) {
   if (rho$npar.theta.opt[j] != 0){
    theta <- if (rho$ntheta[j] >= 2) polr(rho$y[, j] ~1)$zeta else 0
    if (!grepl("mvlogit", rho$link$name)) theta <- theta/1.7
    c(theta[1L], log(diff(theta)))[1:rho$npar.theta.opt[j]]
  } else NULL
})
c(unlist(gammas), rep(0, rho$npar.betas))
}

build_correction_thold_fun <- function(k, rho) {
    ..l.. <- match(rho$threshold.constraints[k],
                   rho$threshold.constraints)
    f <- function(beta, k, rho)  {
        betatildemu <-  beta * rho$mat_center_scale
        br <- drop(crossprod(rho$contr_theta, betatildemu[,1]))
        - br[rho$inds.cat[[k]]] + br[rho$inds.cat[[..l..]]]
    }
    f
}

build_correction_thold_fun0 <- function(j, rho) {
  ..j.. <- j
  f <- function(beta, k, rho)  {
    rep(0L, rho$ntheta[..j..])
  }
  f
}

transf_par <- function(par, rho) {
  par_sigma <- par[rho$npar.thetas + rho$npar.betas +
    seq_len(attr(rho$error.structure, "npar"))]
  sigmas <- build_error_struct(rho$error.structure, par_sigma)
  par_beta <- par[rho$npar.thetas + seq_len(rho$npar.betas)]
  betatilde <- rho$constraints_mat %*% par_beta
  par_theta <- rho$transf_thresholds(par[seq_len(rho$npar.thetas)], rho, betatilde)

  thetatilde <- lapply(seq_len(rho$ndim), function(j)
    par_theta[[j]] + rho$thold_correction[[j]](betatilde, k = j, rho = rho))

  pred.upper  <- sapply(seq_len(rho$ndim), function(j) {
   th_u <- c(thetatilde[[j]], rho$inf.value)[rho$y[, j]]
   xbeta_u <- c(rho$XcatU[[j]] %*% betatilde[rho$indjbeta[[j]]])
   th_u - xbeta_u - rho$offset[[j]]
  })/sigmas$sdVec
  pred.lower  <- sapply(seq_len(rho$ndim), function(j) {
    th_l <- c(-rho$inf.value, thetatilde[[j]])[rho$y[, j]]
    xbeta_l <- c(rho$XcatL[[j]] %*% betatilde[rho$indjbeta[[j]]])
    th_l - xbeta_l - rho$offset[[j]]
  })/sigmas$sdVec
  list(U = pred.upper, L = pred.lower,
       corr_par = sigmas$rVec, sd_mat = sigmas$sdVec)
}
#########################################################################
## transformation of the threshold parameters (to ensure monotonicity) ##
#########################################################################
transf_thresholds_fixall <- function(gamma, rho, betatilde){
  betatildemu <- betatilde * rho$mat_center_scale
  br <- drop(crossprod(rho$contr_theta, betatildemu[,1]))
  lapply(seq_len(rho$ndim), function(j) {
    ..l.. <- match(rho$threshold.constraints[j],
                   rho$threshold.constraints)
    rho$threshold.values[[j]] - br[rho$inds.cat[[..l..]]]
  })
}

transf_thresholds_fix1_first <- function(gamma, rho, betatilde){
  ## \theta_j = a + exp(gamma_1) + .. + exp(gamma_j)
  betatildemu <- betatilde * rho$mat_center_scale
  br <- drop(crossprod(rho$contr_theta, betatildemu[,1]))
  lapply(seq_len(rho$ndim), function(j) {
    ## TODO make nicer
    correction <- rho$thold_correction[[j]](betatilde, k = j, rho)[1]
    a <- rho$threshold.values.fixed[[j]][1] - br[rho$inds.cat[[j]][1]] - correction
    cumsum(c(a, exp(gamma[rho$ind.thresholds[[j]]])))
  })
}

transf_thresholds_fix2_first <- function(gamma, rho, betatilde){
  ## \theta_j = a + b + exp(gamma_1) + .. + exp(gamma_j)
  betatildemu <- betatilde * rho$mat_center_scale
  br <- drop(crossprod(rho$contr_theta, betatildemu[,1]))
  lapply(seq_len(rho$ndim), function(j) {
    correction <- rho$thold_correction[[j]](betatilde, k = j, rho)[1:2]
    a <- rho$threshold.values.fixed[[j]][1] - br[rho$inds.cat[[j]][1]] - correction[1]
    b <- rho$threshold.values.fixed[[j]][2] - br[rho$inds.cat[[j]][2]] - correction[2]
        if (is.na(b)) b <- NULL ## it implies one can have binary with fix2first
        c(a, cumsum(c(b, exp(gamma[rho$ind.thresholds[[j]]]))))
    })
}

transf_thresholds_fix2_firstlast <- function(gamma, rho, betatilde){
  ## (theta_j - theta_{j-1})/(1 - theta_j) = exp(gamma_j)/(1 + exp(gamma_j))
  betatildemu <- betatilde * rho$mat_center_scale
  br <- drop(crossprod(rho$contr_theta, betatildemu[,1]))
  lapply(seq_len(rho$ndim), function(j){
    correction <- rho$thold_correction[[j]](betatilde, k = j, rho)[c(1, rho$ntheta[j])]
    gamma1  <- gamma[rho$ind.thresholds[[j]]]
    a <- rho$threshold.values.fixed[[j]][1] - br[rho$inds.cat[[j]][1]] - correction[1]
    b <- rho$threshold.values.fixed[[j]][2] - br[rho$inds.cat[[j]][rho$ntheta[j]]] - correction[2]
    if (!is.na(b)) {
      recursive.theta <- function(i) {
        if (i == 0) 0
        else return ((exp(gamma1[i]) + recursive.theta(i - 1))/(1 + exp(gamma1[i])))
      }
      theta <- unlist(sapply(seq_along(gamma1), function(i) recursive.theta(i)))
      c(0, theta, 1) * (b - a) + a
    } else a
    })
}

transf_thresholds_flexible <- function(gamma, rho, betatilde = NULL){
  lapply(1L:rho$ndim, function(j)
    if (anyNA(rho$threshold.values[[j]])){
      if (rho$ntheta[j] > 1L) {
        cumsum(c(gamma[rho$ind.thresholds[[j]][1]],
                 exp(gamma[rho$ind.thresholds[[j]][2L:rho$ntheta[j]]])))
      } else if (rho$ntheta[j] == 1L) gamma[rho$ind.thresholds[[j]]] else NULL
    } else rho$threshold.values[[j]]
  )
}
##############################################################################
get_ind_thresholds <- function(threshold.constraints,rho){
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

get_labels_theta <- function(rho,j) {
  lev <- levels(rho$y[, j])
  sapply(1:(rho$ntheta[j]), function(i){
    paste(lev[i], lev[i + 1], sep = "|")
  })
}

backtransf_sigmas <- function(R){
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

mvord_data <- function(data, index, y.names, x.names,
      y.levels, response.names) {
  ## check if response is ordered factor. Set response levels accordingly
  if (is.null(y.levels) & is.ordered(data[,y.names])){
    y.levels <- rep(list(levels(data[,y.names])), length(response.names))
  }
  df <- list()
  data.split.y <- split(data[,c(y.names, index[1])],
    factor(data[, index[2]], levels = response.names))
  data.split.x <- split(data[, c(x.names, index[1])],
        factor(data[, index[2]], levels = response.names))
  #set colnames (otherwise warning due to identical colnames in reduce)
  for (j in seq_along(data.split.y)) {
      colnames(data.split.y[[j]])[1] <- response.names[j]
      colnames(data.split.x[[j]]) <- c(paste(x.names,j, sep = "."), index[1])
  }


  df$y <- Reduce(function(...) merge(..., by = index[1], all = TRUE),
    data.split.y)
  subject_id_names <- df$y[,index[1]]
  df$y <- df$y[, - match(index[1], colnames(df$y)), drop = FALSE]
  if (is.null(y.levels)) {
    df$y <- cbind.data.frame(lapply(df$y, ordered))
    df$ylevels <- lapply(seq_len(ncol(df$y)), function(j) levels(df$y[,j]))
  } else {
    df$ylevels <- y.levels
    for (j in seq_along(y.levels)) {
      ## check levels for each response
      if (!all(levels(df$y[, j]) %in% df$ylevels[[j]]))
        warning("levels of response do not all match with response levels", call. = FALSE)
      if (!all(y.levels[[j]] %in% unique(df$y[, j])))
        warning(sprintf("For response %i, not all response
          levels are observed. Model might be non-identifiable if
          the thresholds for this response are not restricted.", j),
        call.=FALSE)
      df$y[, j] <- ordered(df$y[, j], levels = y.levels[[j]])
    }
  }
  rownames(df$y) <- subject_id_names

  xdatadf <- Reduce(function(...) merge(...,by = index[1], all = TRUE), data.split.x)
  rownames(xdatadf) <- subject_id_names
  xdatadf <- xdatadf[, -match(index[1], colnames(xdatadf)), drop = FALSE]
  df$x <- lapply(1:length(response.names), function(i) {
    tmp <- xdatadf[,(i - 1) * length(x.names) + seq_along(x.names), drop = FALSE]
    names(tmp) <- x.names
    tmp
  })
  names(df$x) <- response.names
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

is.offset <- function(expr) {
  sapply(expr, function(x) ("offset"  %in% x) && (length(x) > 1))
}

get_constraints <- function(rho){
if(is.list(rho$coef.constraints)){
  ## check if nrow is sum(ncat_j - 1)
  if(!(all(sapply(rho$coef.constraints, nrow) == rho$nthetas))) stop("The constraint matrices must have nrow equal to sum(ncat_j - 1).", call. = FALSE)
  if(is.null(names(rho$coef.constraints))) names(rho$coef.constraints) <- rho$coef.names
  if (!all(rho$coef.names %in% names(rho$coef.constraints))) stop("coef.constraints need to be specified for all covariates
                                                                    and intercept if included.", call. = FALSE)
  constraints <- rho$coef.constraints[match(rho$coef.names, names(rho$coef.constraints))]
  constraints <- lapply(constraints, as.matrix)
} else{ #matrix to VGAM
  constraints <- lapply(seq_len(NCOL(rho$coef.constraints)), function(p) {
    tmp <- matrix(0,
      ncol = sum(!is.na(unique(rho$coef.constraints[, p]))),
      nrow = rho$nthetas)
    if(!is.na(rho$coef.constraints[1, p])) tmp[seq_len(rho$ntheta[1]), 1] <- 1
    for (j in 2:nrow(rho$coef.constraints)){
      if (is.na(rho$coef.constraints[j, p])){
        tmp <- tmp
      } else if (rho$coef.constraints[j, p] %in% rho$coef.constraints[1:(j-1), p]){
        tmp[rho$ncat.first.ind[j]:sum(rho$ntheta[seq_len(j)]),
            which(rho$coef.constraints[j, p] %in% rho$coef.constraints[1:(j-1), p])] <- 1
      } else{
        tmp[rho$ncat.first.ind[j]:sum(rho$ntheta[seq_len(j)]),
            sum(!is.na(unique(rho$coef.constraints[1:(j-1),p]))) + 1] <- 1
      }
    }
    tmp
  })
  constraints <- constraints[sapply(constraints,NCOL)!=0]
  }
  constraints <- lapply(seq_along(constraints), function(p) {
    colnames(constraints[[p]]) <- paste(rho$coef.names[p], seq_len(NCOL(constraints[[p]])))
    rownames(constraints[[p]]) <- unlist(lapply(1:rho$ndim, function(j)
      get_labels_theta(rho, j)))
    constraints[[p]]
  })
  if(NCOL(rho$coef.constraints) == 0) constraints <- NULL
  names(constraints) <- rho$coef.names
  constraints
}

get_ind_coef <- function(constraints, rho){
  lapply(seq_len(rho$ndim), function(j){
  ind <- as.integer(rho$y[, rho$y.names[j]])
  sapply(seq_along(rho$coef.names), function(p) {
    if(NCOL(constraints[[p]]) == 0) tmp <- rep(NA, rho$ntheta[j]) else{
      #CHeCK if no 1 in row
      tmp <- apply(constraints[[p]][rho$ncat.first.ind[j]:sum(rho$ntheta[seq_len(j)]), , drop = FALSE] == 1, 1,
                   function(x) {
                     y <- which(x)
                     if(length(y) > 0) y else NA
                   })
    }
    #CHECK order
    tmp[ind] + sum(rho$npar.beta[seq_len(p-1)])
  })
})
}

set_offset <- function(rho){
if (all(sapply(rho$offset, is.null))) {
  offset <- if (any(rho$coef.values != 0, na.rm = TRUE)){
    tmp <- rho$coef.values
    tmp[is.na(tmp)] <- 0
    #tmp
    lapply(1:rho$ndim, function(j){
      tmp2 <- c(rho$x[[j]] %*% tmp[j,])
      tmp2[is.na(tmp2)] <- 0
      tmp2
    }
    )} else  offset <- lapply(1:rho$ndim, function(j) rep(0, rho$n))
    offset
} else rho$offset
}


set_offset_up <- function(rho){
if (all(sapply(rho$offset, is.null))) {
  if (any(rho$coef.values != 0, na.rm = TRUE)){
    tmp <- rho$coef.values
    tmp[is.na(tmp)] <- 0
    rho$offset <- lapply(1:rho$ndim, function(j){
      tmp2 <- c(rho$x[[j]] %*% tmp[j,])
      tmp2[is.na(tmp2)] <- 0
      tmp2
    })
  } else {
    rho$offset <- lapply(1:rho$ndim, function(j) rep(0, rho$n))
  }
}
  if (!is.null(rho$coef.values)){
  rho$wh_fix <- which(colSums(is.na(rho$coef.values)) == 0)
  if (length(rho$wh_fix) != 0){
  for (j in 1:rho$ndim) {
     attribute <- attr(rho$x[[j]], "assign")
     rho$x[[j]] <-  rho$x[[j]][, -rho$wh_fix, drop = F]
     attr(rho$x[[j]], "assign") <- attribute[-rho$wh_fix]
  }
  }
}
  rho
}


#' @title Control functions for mvord()
#' @description Control arguments are set for \code{mvord()}.
#' @param se logical, if \code{TRUE} standard errors are computed.
#' @param start.values vector of (optional) starting values.
#' @param solver character string containing the name of the applicable solver of \code{\link{optimx}} (default is \code{"newuoa"})
#'  or wrapper function for user defined solver.
#' @param solver.optimx.control a list of control arguments to be passed to \code{\link{optimx}}. See \code{\link{optimx}}.
# #' @param scale If \code{scale = TRUE}, then for each response the corresponding covariates of \code{\link{class}} \code{"numeric"} are standardized before fitting,
# #'  i.e., by substracting the mean and dividing by the standard deviation.
#' @seealso \code{\link{mvord}}
#' @export
mvord.control <- function(se = TRUE,
                          start.values = NULL,
                          solver = "newuoa",
                          solver.optimx.control = list(maxit=200000, trace = 0, kkt = FALSE)){
  if (is.null(solver.optimx.control$maxit)) solver.optimx.control$maxit <- 200000
  if (is.null(solver.optimx.control$kkt)) solver.optimx.control$kkt <- FALSE
  if (is.null(solver.optimx.control$trace)) solver.optimx.control$trace <- 0
  list(se = se, start.values = start.values, solver = solver,
    solver.optimx.control = solver.optimx.control)
}


# residuals.mvord <- function(object){
#   probs <- marginal.predict(object, type = "all.prob")
#   cum.probs <- lapply(probs, function(x) t(apply(x,1,cumsum)))
#   y <- object$rho$y
#
#   residuals <- lapply(1:object$rho$ndim, function(j){
#     p1 <- cbind(0,cum.probs[[j]])[cbind(1:nobs(object),as.integer(y[,j]))]
#     p2 <- 1 - cum.probs[[j]][cbind(1:nobs(object),as.integer(y[,j]))]
#     out <- p1 - p2
#     names(out) <- rownames(y)
#     out
#   })
#   names(residuals) <- object$rho$y.names
#   return(residuals)
# }


#Mc Fadden's Pseudo R^2
#' @title Pseudo \eqn{R^2} for objects of class 'mvord'
#' @description This function computes Mc Fadden's Pseudo \eqn{R^2} for objects of class \code{'mvord'}.
#' @param object an object of class \code{'mvord'}.
#' @param adjusted if \code{TRUE}, then adjusted Mc Fadden's Pseudo \eqn{R^2} is computed.
#' @seealso \code{\link{mvord}}
#' @export
pseudo.R.squared <- function(object, adjusted = FALSE){
  #fit model ~ 1
  formula <- object$rho$formula
  formula[[3]] <- 1

  model0 <- mvord(formula = formula,
                  data = object$rho$y,
                  error.structure = cor_ident(~1))
  if (adjusted){
    1 - (logLik(object) - length(object$rho$optpar)) / logLik(model0)
  } else{
    1 - logLik(object) / logLik(model0)
  }
}


scale_mvord <- function(df){
  if(NCOL(df) == 0) list(x = df, mu = 0, sc = 1)else{
  mu <- apply(df, 2, mean, na.rm = TRUE)
  sc <- apply(df, 2, sd, na.rm = TRUE)
  sc[sc == 0] <- 1

  x <- sapply(seq_len(NCOL(df)), function(p){
    (df[,p] - mu[p])/sc[p]
  })
  rownames(x) <- rownames(df)
  list(x = x, mu = mu, sc = sc)
  }
}

reduce_size.mvord <- function(object){
  out <- object
  out$rho$x <- NULL
  out$rho$y <- NULL
  out$rho$error.structure <- NULL
  out$rho$weights <- NULL
  out$rho$offset <- NULL
  out$rho$ind_kl <- NULL
  tmp <- out$rho$link$name
  out$rho$link <- NULL
  out$rho$link$name <- tmp
  out$rho$constraints_scaled <- NULL
  out$rho$constraints_mat <- NULL
  out$rho$thold_correction <- NULL
  #out$rho$V <- NULL
  #out$rho$H.inv <- NULL
  out$rho$varGamma <- NULL
  out$rho$contr_theta <- NULL
  attributes(out$error.struct)$subjnames <- NULL
  out
}

reduce_size2.mvord <- function(object){
  out <- object
  #out$rho$x <- NULL
  #out$rho$y <- NULL
  out$rho$error.structure <- NULL
  out$rho$weights <- NULL
  #out$rho$offset <- NULL
  out$rho$ind_kl <- NULL
  #tmp <- out$rho$link$name
  #out$rho$link <- NULL
  #out$rho$link$name <- tmp
  out$rho$constraints_scaled <- NULL
  out$rho$constraints_mat <- NULL
  out$rho$thold_correction <- NULL
  #out$rho$V <- NULL
  #out$rho$H.inv <- NULL
  out$rho$varGamma <- NULL
  out$rho$contr_theta <- NULL
  attributes(out$error.struct)$subjnames <- NULL
  out
}
