mvord.fit <- function(rho){
  if(!all(sapply(rho$y, is.ordered))) stop("Responses need to be ordered factors", call.=FALSE)
  if(is.null(rho$PL.lag)) rho$PL.lag <- rho$ndim

  ## number of thresholds per outcome
  rho$ntheta <- sapply(1:rho$ndim, function(j) nlevels(rho$y[, j]) - 1) # no of categories - 1

  rho$threshold.values <- if (is.null(rho$threshold.values)) {
                              if ((rho$error.structure$type == "correlation") && (rho$intercept.type == "fixed")) {
                              lapply(1:rho$ndim, function(j) rep(NA,rho$ntheta[j]))
                          } else if((rho$error.structure$type %in% c("correlation")) && (rho$intercept.type == "flexible")){
                              lapply(1:rho$ndim, function(j) if(rho$ntheta[j] == 1) 0 else c(0,rep(NA,max(rho$ntheta[j]-1,0))))
                          } else if ((rho$error.structure$type %in% c("covariance")) && (rho$intercept.type == "flexible")) {
                              lapply(1:rho$ndim, function(j) if(rho$ntheta[j] == 1) 0 else c(0,1,rep(NA,max(rho$ntheta[j]-2,0))))
                          } else if ((rho$error.structure$type %in% c("covariance")) && (rho$intercept.type == "fixed")) {
                              lapply(1:rho$ndim, function(j) if(rho$ntheta[j] == 1) 0 else c(0,rep(NA,max(rho$ntheta[j]-1,0))))
                          }
                        } else {
                          if (length(rho$threshold.values) != rho$ndim) stop("Length of threshold values does not match number of outcomes")
                          else rho$threshold.values
                        }
  #values of fixed (non-NA) threshold parameters
  rho$threshold.values.fixed <- lapply(rho$threshold.values, function(x) x[!is.na(x)])
  ## number of non-fixed thresholds per rater
  rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))

  #checks if binary outcome is present
  rho$binary <- any(sapply(1:rho$ndim, function(j) length(rho$threshold.values[[j]]) == 1))

  #roh$threshold.type
  rho$threshold <- set_threshold_type(rho)

  #number of columns in the covariate matrix
  rho$p <- ncol(rho$x[[1]])

  rho$coef.names <- colnames(rho$x[[1]])

  rho$ncat <- rho$ntheta + 1

  rho$nthetas <- sum(rho$ntheta)
  rho$ncats <- sum(rho$ncat)
  rho$ncat.first.ind <- cumsum(c(1,rho$ncat))[-(length(rho$ncat)+1)]

  rho$constraints <- get_constraints(rho)

  #vector of number of parameters for each coefficient
  rho$npar.beta <- sapply(rho$constraints, NCOL)

  #names rho$constraints
  rho$constraints <- lapply(1:length(rho$constraints), function(p){
    if (NCOL(rho$constraints[[p]]) != 0) colnames(rho$constraints[[p]]) <- paste(names(rho$constraints)[p], 1:rho$npar.beta[p])
    rownames(rho$constraints[[p]]) <- rho$rownames.constraints
    rho$constraints[[p]]
  })
  names(rho$constraints) <- rho$coef.names

  check_args_constraints(rho)

  #get indices of betas in par (only betas)
  rho$coef.ind <- get_ind_coef(rho)

  #set offsets from coef.values
  rho$offset <- set_offset(rho)

  if(is.null(rho$threshold.constraints)) rho$threshold.constraints <- 1:rho$ndim
  #number of flexible threshold parameters (in optimizer)
  rho$npar.theta.opt <- rho$npar.theta
  rho$npar.theta.opt[duplicated(rho$threshold.constraints)] <- 0
  rho$n <- nrow(rho$y)

  #number of total coefficients
  rho$npar.betas <- sum(rho$npar.beta)

  ##INCLUDE CHECKS here
  check_args_thresholds(rho)
##############################################################################################
  rho$ind.thresholds <- get_ind_thresholds(rho$threshold.constraints,rho)

  rho$npar.thetas <- sum(rho$npar.theta.opt)

  rho$inf.value <- 10000

  ###############################
  # error.structure
  ###############################
  if (is.null(rho$start.values)) {
  rho$start <- c(get_start_values(rho),
                 rep(0, attr(rho$error.structure, "npar")))
  } else {
    if(length(unlist(rho$start.values)) !=  (rho$npar.thetas + rho$npar.betas)){
      cat(paste0("length should be ", rho$npar.thetas + rho$npar.betas))
      stop("start.values (theta + beta) has false length", call. = FALSE)
    }

    #transform theta starting values
    theta.start <- rho$threshold.values
    gamma.start <- lapply(1:rho$ndim, function(j) {theta.start[[j]][is.na(theta.start[[j]])] <-  rho$start.values$theta[[j]]
    theta2gamma(theta.start[[j]])
    })
    #transform error.struct parameters
    #TODO

    rho$start <- c(unlist(lapply(1:rho$ndim, function(j) gamma.start[[j]][is.na(rho$threshold.values[[j]])])), #gammas
                   unlist(rho$start.values$beta), #betas
                   rep(0, attr(rho$error.structure, "npar")))
  }

  rho$transf_thresholds <- switch(rho$threshold,
                                  flexible      = transf_thresholds_flexible,
                                  fix1first     = transf_thresholds_fix1_first,
                                  fix2first     = transf_thresholds_fix2_first,
                                  fix2firstlast = transf_thresholds_fix2_firstlast,
                                  fixall        = transf_thresholds_fixall)
  ## help variables to save computation in the likelihood function
  rho$combis <- combn(rho$ndim, 2, simplify = F)
  rho$dummy_pl_lag <- sapply(rho$combis, function(x)
    diff(x) <= rho$PL.lag)
  rho$combis <- rho$combis[rho$dummy_pl_lag == 1]
  ## for which subjects is q_i = 1
  ind_i <- rowSums(!is.na(rho$y)) == 1
  rho$ind_univ <- which(!is.na(rho$y) & ind_i, arr.ind=T)
  ## index for subjects containing pair c(k,l)
  rho$ind_kl <- lapply(rho$combis, function(kl)
    rowSums(!is.na(rho$y[, kl])) == 2)

  ##############################################
  if(is.character(rho$solver)){
    rho$optRes <- suppressWarnings(optimx(rho$start, function(x) PLfun(x, rho),
                                method = rho$solver,
                                hessian = FALSE,
                                #itnmax = 5,
                                control = rho$control))#list(maxit=200000, trace = 1, kkt = FALSE)))
    if (rho$optRes["convcode"] != 0){
      stop("NO/FALSE CONVERGENCE - choose a different optimizer, different starting values or standardize the covariates")
    }
    if (!is.null(rho$control$maxit)) maxit <- rho$control$maxit else maxit <- rho$control$eval.max
    if (rho$optRes["fevals"] >= maxit){
      warning("reached function evalution limit")
    }
    rho$optpar <- unlist(rho$optRes[1:length(rho$start)])
    rho$objective <- unlist(rho$optRes["value"])
    } else if(is.function(rho$solver)){
     #TODO. checks
      #arguments solver
      #arguments output
      #control ? warning
      #implement feval limit if exists
     rho$optRes <- rho$solver(rho$start, function(x) PLfun(x, rho), rho$control)
     rho$optpar <- rho$optRes$optpar
     #rho$objvalue
     rho$objective <- rho$optRes$objvalue
  }


  if (rho$se) {
    rho <- PL_se(rho)
  }

  res <- list()
  res <- mvord_finalize(rho)

  names(unlist(res$theta))[is.na(unlist(rho$threshold.values))]
  if (rho$se) {
  rownames(rho$varGamma) <- colnames(rho$varGamma) <- c(names(unlist(res$theta))[is.na(unlist(rho$threshold.values))][!duplicated(unlist(rho$ind.thresholds))],
                                                        names(res$beta),
                                                        attr(res$error.struct, "parnames"))
  }
  res$rho <- rho
  res$rho$y.NA.ind <- NULL
  res$rho$bivpfun <- NULL
  res$rho$pfun <- NULL
  res$rho$transf_thresholds <- NULL
  res$rho$get_ind_thresholds <- NULL

  res$constraints <- rho$constraints[sapply(rho$constraints, NCOL) != 0]

  class(res) <- "mvord"

  return(res)
}
##########################################################
######                      PL             ###############
##########################################################
# k<-1
# h<-1
# par <- rho$start
PLfun <- function(par, rho){
  tmp <- transf_par(par, rho)
  pred.upper <- tmp$U
  pred.lower <- tmp$L
  r_mat <- tmp$corr_par[, rho$dummy_pl_lag == 1, drop = F]
  ## check for q_i = 1
  pr <- rho$link$F_uni(pred.upper[rho$ind_univ]) -
    rho$link$F_uni(pred.lower[rho$ind_univ])
  pr[pr < .Machine$double.eps] <- .Machine$double.eps
  loglik <- sum(rho$weights[rho$ind_univ[, 1]] * log(pr))
  ## iterate over bivariate pairs
  for (h in seq_along(rho$combis)){
    ind_i <- rho$ind_kl[[h]]
    r <- r_mat[ind_i, h]
    prh <- rho$link$F_biv_rect(
      U = pred.upper[ind_i, rho$combis[[h]], drop = F],
      L = pred.lower[ind_i, rho$combis[[h]], drop = F],
      r = r)
    prh[prh < .Machine$double.eps] <- .Machine$double.eps
    loglik <- loglik + sum(rho$weights[ind_i] * log(prh))
  }
  - loglik
}

.onLoad <- function(library, pkg)
{
  library.dynam("mvord", pkg, library)
  invisible()
}
