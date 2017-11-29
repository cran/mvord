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
    if (nrow(rho$coef.constraints) != rho$ndim) stop("row dimension of coef.constraints and outcome dimension do not match", call. = FALSE)

  for (j in 1:ncol(rho$coef.constraints)){
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


#TODO
check_args_constraints <- function(rho){
  if (!all(rho$coef.names %in% names(rho$constraints)))
    stop("coef.constraints need to be specified for all covariates.", call. = FALSE)
  #check dimensions of rho$constraints
  if(any(sapply(rho$constraints, NROW) != rho$nthetas)) stop("coef.constraints need to have number of total categories rows
                                                           (sum of number of categories for all dimensions).", call. = FALSE)


}

##########################################
###### AUXILIARY FUNCTIONS ##############
##########################################
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

## par <- rho$optpar
transf_par <- function(par, rho) {
  tparsigma <- par[rho$npar.thetas + rho$npar.betas +
    seq_len(attr(rho$error.structure, "npar"))]
  sigmas <- build_error_struct(rho$error.structure, tparsigma)
  theta <- rho$transf_thresholds(par[seq_len(rho$npar.thetas)], rho)
  par_beta <- par[rho$npar.thetas + seq_len(rho$npar.betas)]
  pred.fixedU  <- sapply(1:rho$ndim, function(j) {
     b <- lapply(seq_along(rho$constraints),function(i)
     rho$constraints[[i]][rho$inds.cat[[j]],,drop=F] %*%
     par_beta[rho$nbeta.first[i] + seq_len(rho$npar.beta[i]) - 1])
      rho$XcatU[[j]] %*% unlist(b) + rho$offset[[j]]
    })
  pred.fixedL  <- sapply(1:rho$ndim, function(j) {
     b <- lapply(seq_along(rho$constraints),function(i)
     rho$constraints[[i]][rho$inds.cat[[j]],,drop=F] %*%
     par_beta[rho$nbeta.first[i] + seq_len(rho$npar.beta[i]) - 1])
      rho$XcatL[[j]] %*% unlist(b) + rho$offset[[j]]
    })
  theta.lower <- sapply(1:rho$ndim, function(j) c(-rho$inf.value, theta[[j]])[rho$y[, j]])
  theta.upper <- sapply(1:rho$ndim, function(j) c(theta[[j]], rho$inf.value)[rho$y[, j]])
  pred.lower <- (theta.lower - pred.fixedL)/sigmas$sdVec
  pred.upper <- (theta.upper - pred.fixedU)/sigmas$sdVec
  list(U = pred.upper,
       L = pred.lower,
       corr_par = sigmas$rVec,
       sd_mat = sigmas$sdVec)
}
#########################################################################
## transformation of the threshold parameters (to ensure monotonicity) ##
#########################################################################
transf_thresholds_fixall <- function(gamma, rho){
  rho$threshold.values
}

transf_thresholds_fix1_first <- function(gamma, rho){
  ## \theta_j = a + exp(gamma_1) + .. + exp(gamma_j)
  lapply(seq_len(rho$ndim), function(j) {
    cumsum(c(rho$threshold.values.fixed[[j]][1], exp(gamma[rho$ind.thresholds[[j]]])))
  })
}

transf_thresholds_fix2_first <- function(gamma, rho){
  ## \theta_j = a + b + exp(gamma_1) + .. + exp(gamma_j)
     lapply(seq_len(rho$ndim), function(j) {
       a <- rho$threshold.values.fixed[[j]][1] ## a bounds
       b <- rho$threshold.values.fixed[[j]][2] ## b bounds
       if (is.na(b)) b <- NULL ## it implies one can have binary with fix2first
       c(a, cumsum(c(b, exp(gamma[rho$ind.thresholds[[j]]]))))
     })
}

transf_thresholds_fix2_firstlast <- function(gamma, rho){
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
    theta <- unlist(sapply(seq_along(gamma1), function(i) recursive.theta(i)))
    c(0, theta, 1) * (b - a) + a
    } else a
    })
}

transf_thresholds_flexible <- function(gamma, rho){
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
get_ind_thresholds <- function(threshold.constraints,rho){
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
                         y.levels = NULL, response.names = NULL, contrasts) {
  df <- list()
  if (any(duplicated(data[,index]))) stop("duplicated observation(s) for one index", call. = FALSE)
  index.levels <- levels(as.factor(data[, index[2]]))
  data.split <- split(data[,c(y.names, index[1])], data[, index[2]])
  data.split.y <- lapply(seq_along(data.split), function(j) {
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
    df$ylevels <- lapply(seq_len(ncol(df$y)), function(j) levels(df$y[,j]))
  } else {
    df$ylevels <- y.levels
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
  data.split.x <- lapply(seq_along(data.split.x), function(j) {
    colnames(data.split.x[[j]]) <- c(paste(x.names,j, sep = "."), index[1])
    data.split.x[[j]]})


  xdatadf <- Reduce(function(...) merge(...,by = index[1], all = TRUE), data.split.x)
  subject_id_names <- xdatadf[,index[1]]
  rownames(xdatadf) <- subject_id_names
  xdatadf <- xdatadf[, -match(index[1], colnames(xdatadf)), drop = FALSE]
  xdatadf <- cbind(xdatadf, matrix(NA, ncol = length(response.names.NA) * length(x.names),
                                   nrow = nrow(xdatadf)))
  df$x <- lapply(1:(length(index.levels) + length(response.names.NA)), function(i) {
    tmp <- xdatadf[,(i - 1) * length(x.names) + seq_along(x.names), drop = FALSE]
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
  attr(df, "contrasts") <- contrasts
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
  constraints<-constraints[sapply(constraints,NCOL)!=0]
  }
  constraints <- lapply(seq_along(constraints), function(p) {
    colnames(constraints[[p]]) <- paste(rho$coef.names[p], seq_len(NCOL(constraints[[p]])))
    rownames(constraints[[p]]) <- unlist(lapply(1:rho$ndim, function(j)
      get_labels_theta(rho, j)))
    constraints[[p]]
  })
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
  wh_fix <- which(colSums(is.na(rho$coef.values)) == 0)
  if (length(wh_fix) != 0){
  for (j in 1:rho$ndim) {
     rho$x[[j]] <-  rho$x[[j]][, -wh_fix, drop = F]
  }
  }
}
  rho
}

#is.partial_prop_odds <- function(x, ncat, first.ind){
#  inds <- lapply(seq_len(length(ncat)), function(j) seq_len(ncat[j]) + first.ind[j] - 1)
#  sapply(seq_len(length(ncat)), function(j){
#    if (all(apply(x[inds[[j]], ,drop = FALSE] == 1, 2, all) | apply(x[inds[[j]], ,drop = FALSE] == 0, 2, all))) FALSE else TRUE
#  })
#}
