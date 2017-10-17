mvord.fit <- function(rho){
  if(!all(sapply(rho$y, is.ordered))) stop("Responses need to be ordered factors", call.=FALSE)
  # split y according to missingness pattern
  rho$y.NA.ind <- split.NA.pattern(rho$y)
  if(is.null(rho$PL.lag)) rho$PL.lag <- rho$ndim

  ## number of thresholds per outcome
  rho$ntheta <- sapply(1:rho$ndim, function(j) nlevels(rho$y[, j]) - 1) # no of categories - 1

  rho$threshold.values <- if (is.null(rho$threshold.values)) {
                              if ((rho$error.structure$type %in% c("corGeneral", "corAR1", "corEqui")) && (rho$intercept.type == "fixed")) {
                              lapply(1:rho$ndim, function(j) rep(NA,rho$ntheta[j]))
                          } else if((rho$error.structure$type %in% c("corGeneral", "corAR1", "corEqui")) && (rho$intercept.type == "flexible")){
                              lapply(1:rho$ndim, function(j) if(rho$ntheta[j] == 1) 0 else c(0,rep(NA,max(rho$ntheta[j]-1,0))))
                          } else if ((rho$error.structure$type %in% c("covGeneral")) && (rho$intercept.type == "flexible")) {
                              lapply(1:rho$ndim, function(j) if(rho$ntheta[j] == 1) 0 else c(0,1,rep(NA,max(rho$ntheta[j]-2,0))))
                          } else if ((rho$error.structure$type %in% c("covGeneral")) && (rho$intercept.type == "fixed")) {
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
  rho$threshold <- set.threshold.type(rho)

  #number of columns in the covariate matrix
  rho$p <- ncol(rho$x[[1]])


  rho$ind.coef <- getInd.coef(rho$coef.constraints, rho$coef.values)

  if(is.null(rho$threshold.constraints)) rho$threshold.constraints <- 1:rho$ndim
  #number of flexible threshold parameters (in optimizer)
  rho$npar.theta.opt <- rho$npar.theta
  rho$npar.theta.opt[duplicated(rho$threshold.constraints)] <- 0


  rho$n <- nrow(rho$y)

  #number of total coefficients
  rho$npar.betas <- max(c(rho$ind.coef,0), na.rm = TRUE)

  ##INCLUDE CHECKS here
  checkArgs(rho)
##############################################################################################

  rho$ind.thresholds <- getInd.thresholds(rho$threshold.constraints,rho)

  rho$npar.thetas <- sum(rho$npar.theta.opt)

  rho$inf.value <- 1000

  # check if degrees of freedom for the t-distribution are integer.
  if(rho$link$name == "mvlogit" && !is.integer(rho$link$df)) {
    warning("Degrees of freedom in mvlogit() need to be integer. Rounding to the closest integer.")
    rho$link$df <- as.integer(rho$link$df)
  }


  ###############################
  # error.structure
  ###############################
  #number of correlation parameters for a matrix
  rho$npar.cor <- switch(rho$error.structure$type,
                              corGeneral = (rho$ndim) * (rho$ndim - 1)/2,
                              covGeneral = (rho$ndim) * (rho$ndim - 1)/2,
                              corEqui = ncol(rho$error.structure$x),
                              corAR1 = ncol(rho$error.structure$x))
  #number of sigmas
  rho$ncor.levels <- switch(rho$error.structure$type,
                              corGeneral = if(rho$error.structure$formula == ~1)
                                              1 else nlevels(rho$error.structure$x),
                              covGeneral = if(rho$error.structure$formula == ~1)
                                              1 else nlevels(rho$error.structure$x),
                              corEqui    = 1,
                              corAR1     = 1)

  if (rho$error.structure$type == "covGeneral") {
    rho$npar.cor.sd <- rho$ndim
    rho$start.lvar <- rep(0, rho$npar.cor.sd)
  } else {
    rho$npar.cor.sd <- 0
    rho$start.lvar <- NULL
  }
  #number of parameters for all sigmas
  rho$npar.sigmas <- rho$ncor.levels * (rho$npar.cor + rho$npar.cor.sd)

  rho$error.structure$levels <- switch(rho$error.structure$type,
                                       corGeneral = levels(rho$error.structure$x),
                                       covGeneral = levels(rho$error.structure$x),
                                       corEqui = NULL,
                                       corAR1 = NULL)

  if (is.null(rho$start.values)) {
  rho$start <- c(getStart.values(rho),
                 rep(0, rho$npar.sigmas))
  }else{
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
                   rep(0,rho$npar.sigmas))
  }

  rho$transf.thresholds <- switch(rho$threshold,
                                  flexible      = transf.thresholds.flexible,
                                  fix1first     = transf.thresholds.fix1.first,
                                  fix2first     = transf.thresholds.fix2.first,
                                  fix2firstlast = transf.thresholds.fix2.firstlast,
                                  fixall        = transf.thresholds.fixall)

  rho$transf.sigmas <- switch(rho$error.structure$type,
                              corGeneral = transf.sigmas.spheric,
                              covGeneral = transf.sigmas.spheric,
                              corEqui    = transf.sigmas.corEqui,
                              corAR1     = transf.sigmas.corAR1)

  rho$transf.par <- switch(rho$error.structure$type,
                           corGeneral = transf.par.cor,
                           corEqui    = transf.par.cor,
                           covGeneral = transf.par.cov,
                           corAR1     = transf.par.cor)
  rho$pfun <- switch(rho$link$name,
                 mvprobit = pnorm,
                 mvlogit  = function(q) pt(q, df = rho$link$df))# function for univariate probabilities
  rho$bivpfun <- switch(rho$link$name,
                     mvprobit = function(U, L, r) rectbiv.norm.prob(U, L, r),
                     mvlogit  = function(U, L, r) {
                       sapply(seq_len(nrow(U)), function(i)
                       biv.nt.prob2(df = rho$link$df,
                                    lower = L[i, ],
                                    upper = U[i, ],
                                    r     = r[i]))}) # function for bivariate probabilities

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
  res <- mvord.finalize(rho)
  res$rho <- rho
  res$rho$y.NA.ind <- NULL
  res$rho$bivpfun <- NULL
  res$rho$pfun <- NULL
  res$rho$transf.par <- NULL
  res$rho$transf.sigmas <- NULL
  res$rho$transf.thresholds <- NULL
  res$rho$getInd.thresholds <- NULL
  #res$rho$x <- NULL
  #res$rho$y <- NULL
  #res$rho$weights <- NULL

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
  tmp <- rho$transf.par(par, rho)
  pred.upper <- tmp$U
  pred.lower <- tmp$L
  sigmas <- tmp$sigmas
  if (rho$link$name=="mvlogit"){
     pred.upper <- qt(plogis(pred.upper), df = rho$link$df)
     pred.lower <- qt(plogis(pred.lower), df = rho$link$df)
     pred.upper[pred.upper > rho$inf.value] <- rho$inf.value
     pred.lower[pred.lower < -rho$inf.value] <- -rho$inf.value
  }
  vecPL <- sapply(1:length(rho$y.NA.ind), function(k){
    q <- as.numeric(strsplit(names(rho$y.NA.ind[k]), "")[[1]])# turn "1_0_1"to 1 0 1)
    if (sum(q) == 0) {
      0
    } else {
    ind <- rho$y.NA.ind[[k]]
    if (rho$error.structure$type %in% c("corGeneral", "covGeneral")) {
      lev <- match(rho$error.structure$x[ind], rho$error.structure$levels)
    }

    U <-  pred.upper[ind, , drop = F]
    L <-  pred.lower[ind, , drop = F]

    if (sum(q) == 1){
      pr <- rho$pfun(U[, q == 1]) - rho$pfun(L[, q == 1])
      pr[pr < .Machine$double.eps] <- .Machine$double.eps
      sum(rho$weights[ind] * log(pr))
    } else {
      combis <- combn(which(q == 1), 2)
      combis <- combis[,which((combis[2,] - combis[1,])  <= rho$PL.lag), drop = FALSE]
      lprk <- 0
      for (h in seq_len(ncol(combis))){
        r <- switch(rho$error.structure$type,
                    corGeneral = sapply(sigmas[lev],'[', combis[1, h], combis[2 ,h]),
                    covGeneral = sapply(sigmas[lev],'[', combis[1, h], combis[2 ,h]),
                    corEqui    = sigmas[ind, 1],
                    corAR1     = sapply(sigmas[ind],'[', combis[1, h], combis[2 ,h]))
        prh <- rho$bivpfun(U = U[, combis[,h], drop = F],
                           L = L[, combis[,h], drop = F],
                           r = r)
        prh[prh < .Machine$double.eps] <- .Machine$double.eps
        lprk <- lprk + sum(rho$weights[ind] * log(prh))
      }
      lprk
    }
  }
  })
  -sum(vecPL)
}


.onLoad <- function(library, pkg)
{
  library.dynam("mvord", pkg, library)
  invisible()
}
