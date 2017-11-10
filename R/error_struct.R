#' @title Error Structures in mvord
#' @description Different \code{error.structures} are available in \pkg{mvord}:
#' \itemize{
#' \item general correlation structure (default) \code{cor_general(~1)},
#' \item general covariance structure \code{cov_general(~1)},
#' \item factor dependent correlation structure \code{cor_general(~f)},
#' \item factor dependent covariance structure \code{cov_general(~f)},
#' \item covariate dependent equicorrelation structure \code{cor_equi(~S)},
#' \item AR(1) correlation structure \code{cor_ar1(~1)}, or
#' \item covariate dependent AR(1) correlation structure \code{cor_ar1(~S)}.
#' }
#' See \code{\link{error_struct}} or vignette.
#' @param formula \code{\link{formula}} object
#' @export
#' @name error_struct
cov_general <-
  ## Constructor for the cov_general class
  function(formula = ~ 1)
{
  value <- list(name = "cov_general",
                formula = formula,
                type = "covariance")
  attr(value, "formula") <- formula
  class(value) <- c("cov_general", "error_struct")
  value
}
#' @rdname error_struct
#' @export
cor_general <-
  ## Constructor for the cor_general class
  function(formula = ~ 1)
{
  value <- list(name = "cor_general", formula = formula,
                type = "correlation")
  attr(value, "formula") <- formula
  class(value) <- c("cor_general", "error_struct")
  value
}
#' @rdname error_struct
#' @export
cor_ar1 <-
  ## Constructor for the cor_ar1 class
  function(formula = ~ 1)
{
  value <- list(name = "cor_ar1",
                formula = formula,
                type = "correlation")
  attr(value, "formula") <- formula
  class(value) <- c("cor_ar1", "error_struct")
  value
}
#' @rdname error_struct
#' @export
cor_equi <-
  ## Constructor for the cor_equi class
  function(formula = ~ 1)
{
  value <- list(name = "cor_equi",
                formula = formula,
                type = "correlation")
  attr(value, "formula") <- formula
  class(value) <- c("cor_equi", "error_struct")
  value
}

build_error_struct <-
  ## extractor for correlation matrix
  function(object, ...) UseMethod("build_error_struct")

initialize <-
  ## initializes the structures
  function(object, ...) UseMethod("initialize")

finalize <-
  ## finalizes the structures
  function(object, ...) UseMethod("finalize")

get_covariate <-
  ## initializes the structures
  function(object, ...) UseMethod("get_covariate")

init_fun <-
  ## initializes the structures
  function(object, ...) UseMethod("init_fun")

#################
##   Methods for error_struct
#################
formula.error_struct <-
  ## Accessor for the covariate formula
  function(x, ...) eval(attr(x, "formula"))

get_covariate.error_struct <-
  function(object, data.x)
{
  covar_mat <- lapply(data.x, function(x)
    model.matrix(formula(object), model.frame(formula(object), x,
      na.action = function(x) x)))
  ## check if covariate matrices are equal
  if (!all(sapply(1:(length(covar_mat) - 1), function(i)
    all(covar_mat[[i]] == covar_mat[[i+1]], na.rm = T)))) {
  	stop("Covariates in error structure must be
  		  constant across outcomes!")
  }
  # make one matrix
  covar_mat1 <- sapply(1:ncol(covar_mat[[1]]), function(k){
      xtcol <- do.call(cbind,lapply(covar_mat, `[`,,k))
      xtcol_final <- apply(xtcol,1,function(i) unique(i[!is.na(i)]))
      xtcol_final
    })
  attributes(covar_mat1) <- attributes(covar_mat[[1]])
  covar_mat1
}

initialize.error_struct <-
  ## initializes some attributes of error_struct objects
  ## takes as data the output on mvord.data
  function(object, data)
{
  attr(object, "ynames") <- colnames(data$y)
  attr(object, "subjnames") <- rownames(data$y)

  attr(object, "ndim") <- length(data$x)
  attr(object, "nobs") <- nrow(data$x[[1]])
  attr(object, "covariate") <-
    get_covariate(object, data.x = data$x)
  object
}

#########################################
### Methods for cov_general

init_fun.cov_general <-
function(object,  data)
{
	## initializes some attributes of error_struct objects
	form <- formula(object)
  if (length(all.vars(form)) > 1)
  	stop("Only one factor is supported in cov_general.")
  ## if intercept included rewrite formula without
  if (length(all.vars(form)) == 1 & attr(terms(form), "intercept") == 1)
    attr(object, "formula") <- as.formula(sprintf("~ 0 + %s", all.vars(form)))
  object <- initialize(object, data)
	ndim <- attr(object, "ndim")
	npar1 <- ndim * (ndim - 1)/2
	attr(object, "npar") <- (npar1 + ndim)* NCOL(attr(object, "covariate"))
    if(length(all.vars(form)) == 1 && !is.factor(data$x[[1]][, all.vars(form)]))
    	stop("For cov_general covariate must be factor!")
  	object
}

build_error_struct.cov_general <-
function(object, tpar)
{
  ## takes the transformed parameters and builds initializes some attributes of cor_general objects
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  nlev <- NCOL(covar)
  npar.cor <- attr(object, "npar")/nlev  - ndim
  corr_pars <- sapply(1:nlev, function(l) {
    nu <- tpar[(l - 1) * npar.cor + seq_len(npar.cor)]
    angles <- pi * exp(nu)/(1 + exp(nu))
    cosmat <- diag(ndim)
    cosmat[lower.tri(cosmat)] <- cos(angles)
    S1 <- matrix(0, nrow = ndim, ncol = ndim)
    S1[, 1L] <- 1
    S1[-1L, -1L][lower.tri(S1[-1L, -1L], diag = T)] <- sin(angles)
    tLmat <- sapply(1:ndim,
                    function(j) cosmat[j, ] * cumprod(S1[j, ]))
    sigma <- crossprod(tLmat)
    sigma[lower.tri(sigma)]
  })
  if (is.null(ncol(corr_pars)))
  	dim(corr_pars) <- c(1, nlev)
  rVec <- covar %*% t(corr_pars)
  pos <- npar.cor * nlev
  sd_lev <- sapply(1:nlev,
  	function(l) exp(tpar[pos + (l - 1) * ndim + seq_len(ndim)]))
  sdVec <- covar %*% t(sd_lev)
  return(list(rVec = rVec, sdVec = sdVec))
}

finalize.cov_general <-
function(object, tpar)
{
  ## takes the transformed parameters and builds initializes some attributes of cor_general objects
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  nlev <- NCOL(covar)
  npar.cor <- attr(object, "npar")/nlev - ndim
  corr_mat <- lapply(1:nlev, function(l) {
    nu <- tpar[(l - 1) * npar.cor + seq_len(npar.cor)]
    angles <- pi * exp(nu)/(1 + exp(nu))
    cosmat <- diag(ndim)
    cosmat[lower.tri(cosmat)] <- cos(angles)
    S1 <- matrix(0, nrow = ndim, ncol = ndim)
    S1[, 1L] <- 1
    S1[-1L, -1L][lower.tri(S1[-1L, -1L], diag = T)] <- sin(angles)
    tLmat <- sapply(1:ndim,
                    function(j) cosmat[j, ] * cumprod(S1[j, ]))
    sigma <- crossprod(tLmat)
    sigma[lower.tri(sigma)]
  })
  pos <- npar.cor * nlev
  cov_vec <- c(unlist(corr_mat),
    exp(tpar[(pos + 1) : attr(object, "npar")]))
  ## names
  ynames <- attr(object, "ynames")
  ind <- combn(ndim,2)
  fnames <- colnames(covar)
  if (object$formula == ~1) {
    ## correlation names
    names.corr <-
      sapply(seq_len(NCOL(ind)), function(j)
      sprintf("corr %s %s", attr(object, "ynames")[ind[1,j]],
        attr(object, "ynames")[ind[2,j]]))

    ## std deviation names
    names.sigma <- paste("sigma", ynames)
    } else { ## if factor dependent
      ## correlation names
      names.corr.pair <- apply(ind, 2, function(i)
        paste(ynames[i], collapse = " "))
      names.corr <- paste("corr", rep(fnames, each = NCOL(ind)),
            rep(names.corr.pair, nlev))
      ## std deviation names
      names.sigma <- paste("sigma",
        rep(fnames, each = ndim),
        rep(ynames, nlev))
    }
    names(cov_vec)  <-  c(names.corr, names.sigma)
    attr(object, "par") <- cov_vec

     attr(object, "parnames") <- c(names.corr, names.sigma)
     object
}
################################
#########################################
###*# Methods for  cor_general

init_fun.cor_general <-
function(object,  data)
{
	## initializes some attributes of error_struct objects
	form <- formula(object)
    if (length(all.vars(form)) > 1)
  	    stop("Only one factor is supported in cor_general.")
    ## if intercept included rewrite formula without
  	if (length(all.vars(form)) == 1 & attr(terms(form), "intercept") == 1)
        attr(object, "formula") <- as.formula(sprintf("~ 0 + %s", all.vars(form)))
  object <- initialize(object, data)
	npar1 <- attr(object, "ndim") * (attr(object, "ndim") - 1)/2
	attr(object, "npar") <- npar1 * NCOL(attr(object, "covariate"))
    if(length(all.vars(form)) == 1 && !is.factor(data$x[[1]][, all.vars(form)]))
    	stop("For cor_general covariate must be factor!")
  object
}

build_error_struct.cor_general <-
function(object, tpar)
{
  ## takes the transformed parameters and builds initializes some attributes of cor_general objects
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  nlev <- NCOL(covar)
  npar1 <- attr(object, "npar")/nlev
  corr_pars <- sapply(1:nlev, function(l) {
    nu <- tpar[(l - 1) * npar1 + seq_len(npar1)]
    angles <- pi * exp(nu)/(1 + exp(nu))
    cosmat <- diag(ndim)
    cosmat[lower.tri(cosmat)] <- cos(angles)
    S1 <- matrix(0, nrow = ndim, ncol = ndim)
    S1[, 1L] <- 1
    S1[lower.tri(S1, diag = T)][-(1:ndim)] <- sin(angles)
    #S1[-1L, -1L][lower.tri(S1[-1L, -1L], diag = T)] <- sin(angles)
    tLmat <- sapply(1:ndim,
                    function(j) cosmat[j, ] * cumprod(S1[j, ]))
    sigma <- crossprod(tLmat)
    sigma[lower.tri(sigma)]
  })
 # if (is.null(ncol(corr_pars)))
 # 	dim(corr_pars) <- c(1, nlev)
  rVec <- tcrossprod(covar, corr_pars)
  sd <- rep(1, ndim)
  return(list(rVec = rVec, sdVec = sd))
}

finalize.cor_general <-
function(object, tpar)
{
  ## takes the transformed parameters and builds initializes some attributes of cor_general objects
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  nlev <- NCOL(covar)
  npar1 <- attr(object, "npar")/nlev
  corr_mat <- lapply(1:nlev, function(l) {
    nu <- tpar[(l - 1) * npar1 + seq_len(npar1)]
    angles <- pi * exp(nu)/(1 + exp(nu))
    cosmat <- diag(ndim)
    cosmat[lower.tri(cosmat)] <- cos(angles)
    S1 <- matrix(0, nrow = ndim, ncol = ndim)
    S1[, 1L] <- 1
    S1[-1L, -1L][lower.tri(S1[-1L, -1L], diag = T)] <- sin(angles)
    tLmat <- sapply(1:ndim,
                    function(j) cosmat[j, ] * cumprod(S1[j, ]))
    sigma <- crossprod(tLmat)
    sigma[lower.tri(sigma)]
  })
  corr_vec <- unlist(corr_mat)
  ## names
  ynames <- attr(object, "ynames")
    ind <- combn(ndim,2)

    if (object$formula == ~1) {
      ## correlation names
      names.corr <-
      sapply(seq_len(NCOL(ind)), function(j)
      sprintf("corr %s %s", attr(object, "ynames")[ind[1,j]],
        attr(object, "ynames")[ind[2,j]]))
    } else { ## if factor dependent
      ## correlation names
      names.corr.pair <- apply(ind, 2, function(i)
        paste(ynames[i], collapse = " "))
      names.corr <- paste("corr", rep(colnames(covar), each = NCOL(ind)),
            rep(names.corr.pair, ncol(covar)))
    }
    names(corr_vec)  <- names.corr
    attr(object, "par") <- corr_vec

     attr(object, "parnames") <- names.corr
     object
  }

################################
#########################################
###*# Methods for  cor_ar1

init_fun.cor_ar1 <-
function(object,  data)
{
  object <- initialize(object, data)
	attr(object, "npar") <- NCOL(attr(object, "covariate"))
  object
}

build_error_struct.cor_ar1 <-
function(object, tpar)
{
  ## takes the transformed parameters and builds initializes some attributes of cor_general objects
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  z <- covar %*% tpar
  r <- z2r(z)
  corr_pars <- sapply(seq_along(r), function(i){
    sigma <- diag(ndim)
    sigma[lower.tri(sigma)]  <- r[i]^sequence((ndim-1):1)
    sigma <- sigma + t(sigma) - diag(ndim)
    sigma[lower.tri(sigma)]
  })
  if (is.null(ncol(corr_pars)))
    dim(corr_pars) <- c(1, length(corr_pars))
  sdVec <- rep(1, ndim)
  list(rVec = t(corr_pars), sdVec=sdVec)
}

finalize.cor_ar1 <-
function(object, tpar)
{
  covar <- attr(object, "covariate")
  names(tpar) <- colnames(covar)
  attr(object, "par") <- tpar
  attr(object, "parnames") <- colnames(covar)
  object
}
################################
#########################################
###*# Methods for  cor_equi
init_fun.cor_equi <-
function(object,  data)
{
  ## initializes some attributes of cor_equi objects
  form <- formula(object)
  object <- initialize(object, data)
  attr(object, "npar") <- NCOL(attr(object, "covariate"))
  object
}

build_error_struct.cor_equi <-
function(object, tpar)
{
  ## tpar argument: transformed parameters (from optimizer)
  ## builds the correlation and standard deviation parameters for cor_equi objects
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  z <- covar %*% tpar
  r <- z2r(z)
  npar1 <- ndim * (ndim - 1)/2
  corr_pars <- matrix(rep(r, npar1), ncol = npar1)
  if (is.null(ncol(corr_pars)))
    dim(corr_pars) <- c(length(corr_pars), 1)
  sdVec <- rep(1, ndim)
  list(rVec = corr_pars, sdVec=sdVec)
}

finalize.cor_equi <-
function(object, tpar)
{
  ## finalizes some attributes of cor_equi objects
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  names(tpar) <- colnames(covar)
  attr(object, "par") <- tpar
  attr(object, "parnames") <- colnames(covar)
  object
}
########################################################
#' @title Extracts Error Structure of Multivariate Ordinal Regression Models.
#' @description
#' \code{get_error_struct} is a generic function which extracts for each subject the estimated
#' error structure parameters from objects of class \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param type choose type \code{c("sigmas", "alpha", "corr", "z")}
#' @param ... further arguments passed to or from other methods.
#' @details \itemize{
#' \item{\code{sigmas}} {extracts the correlation/covariance matrices corresponding to each subject.
#'             Applicable in line with \code{cor_general, cov_general, cor_equi, cor_ar1}}
#' \item{\code{alpha}} {extracts the parameters of covariate dependent error structure.
#' Applicable in line with \code{cor_equi, cor_ar1}}
#' \item{\code{corr}} {extracts the subject-specific correlation parameters. Applicable in l
#' ine with \code{cor_equi, cor_ar1}}
#' \item{\code{z}} {extracts the subject-specific Fisher-z score. Applicable in line
#' with \code{cor_equi, cor_ar1}}}
#' @export
get_error_struct <- function(object, type, ...) UseMethod("get_error_struct")
#' @rdname get_error_struct
#' @export
get_error_struct.mvord <- function(object, type = NULL, ...)  {
  val <- get_error_struct(object$error.struct, type = type , ...)
  val
}
get_error_struct.cor_general <- function(object, type, ...){
  npar <- attr(object, "npar")
  par <- attr(object, "par")
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  ynames <- attr(object, "ynames")
  nlev <- NCOL(covar)
  npar.cor <- npar/nlev
  corr_lev <- lapply(1:nlev, function(l) {
    sigma <- diag(ndim)
    sigma[lower.tri(sigma)] <- par[(l - 1) * npar.cor + seq_len(npar.cor)]
    s <- sigma + t(sigma) - diag(ndim)
    colnames(s) <- rownames(s) <- ynames
    s
  })
  indlev <- apply(covar, 1, function(x) which(x == 1))
  corr_n <- corr_lev[indlev]
  names(corr_n) <- attr(object, "subjnames")
  corr_n
}

get_error_struct.cov_general <- function(object, type, ...){
  npar <- attr(object, "npar")
  par <- attr(object, "par")
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  ynames <- attr(object, "ynames")
  nlev <- NCOL(covar)
  npar.cor <- npar/nlev - ndim
  cov_lev <- lapply(1:nlev, function(l) {
    R <- diag(ndim)
    R[lower.tri(R)] <- par[(l - 1) * npar.cor + seq_len(npar.cor)]
    R <- R + t(R) - diag(ndim)
    s <- par[nlev * npar.cor + (l - 1) * ndim + seq_len(ndim)]
    sigma <- t(s * R) * s
    colnames(sigma) <- rownames(sigma) <- ynames
    sigma
  })
  indlev <- apply(covar, 1, function(x) which(x == 1))
  cov_n <- cov_lev[indlev]
  names(cov_n) <- attr(object, "subjnames")
  cov_n
}

get_error_struct.cor_equi <- function(object, type, ...){
  npar <- attr(object, "npar")
  par <- attr(object, "par")
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  ynames <- attr(object, "ynames")
  covar <- attr(object, "covariate")
  z <- covar %*% par
  colnames(z) <- "Fisher-z Score"
  r <- z2r(z)
  colnames(r) <- "Correlation"
  sigmas <- lapply(seq_along(r), function(i) {
      tmp <- matrix(r[i], nrow = ndim, ncol = ndim)
      diag(tmp) <- 1
      rownames(tmp) <- colnames(tmp) <- ynames
      tmp
  })
  names(sigmas)  <-  attr(object, "subjnames")

  if (!is.null(type)){
    par <- switch(type,
           alpha = par,
           sigmas =  sigmas,
           corr = r,
           z = z)
  }
  return(par)
}

get_error_struct.cor_ar1 <- function(object, type, ...){
  npar <- attr(object, "npar")
  par <- attr(object, "par")
  ndim <- attr(object, "ndim")
  covar <- attr(object, "covariate")
  ynames <- attr(object, "ynames")
  covar <- attr(object, "covariate")
  z <- covar %*% par
  colnames(z) <- "Fisher-z Score"
  r <- z2r(z)
  colnames(r) <- "Correlation"
  sigmas <-  lapply(seq_along(r), function(i){
      tmp <- diag(ndim)
      tmp[lower.tri(tmp)]  <- r[i]^sequence((ndim-1):1)
      tmp <- tmp + t(tmp) - diag(ndim)
      rownames(tmp) <- colnames(tmp) <- ynames
      tmp
    })
  names(sigmas)  <-  attr(object, "subjnames")

  if (!is.null(type)){
    par <- switch(type,
           alpha = par,
           sigmas =  sigmas,
           corr = r,
           z = z)
  }
  return(par)
}

##########################################
## IF numeric SE should be computed ... ##
##########################################
corr_jac <- function(object, tpar, ...) UseMethod("corr_jac")
corr_jac.cor_general <- function(object, tpar){
  nlev <- NCOL(attr(object, "covariate"))
  ndim <- attr(object, "ndim")
  npar.cor <- ndim * (ndim - 1)/2
  lapply(1:nlev, function(l)
    t(sapply(1:npar.cor, function(i) grad(function(x) corr_jac_num_fct(ndim, x, i),
      x=tpar[(l - 1) * npar.cor + seq_len(npar.cor)]))))
}
corr_jac.cov_general <- function(object, tpar){
  nlev <- NCOL(attr(object, "covariate"))
  ndim <- attr(object, "ndim")
  npar.cor <- ndim * (ndim - 1)/2
  l <- lapply(1:nlev, function(l)
    t(sapply(1:npar.cor, function(i) grad(function(x) corr_jac_num_fct(ndim, x, i),
      x=tpar[(l - 1) * npar.cor + seq_len(npar.cor)]))))
  l[length(l) + seq_len(nlev * ndim)] <-
      exp(tpar[npar.cor * nlev + seq_len(ndim * nlev)])
  l
}

corr_jac.cor_ar1 <- function(object, tpar){
  list(diag(attr(object, "npar")))
}
corr_jac.cor_equi<- function(object, tpar){
  list(diag(attr(object, "npar")))
}

corr_jac_num_fct <- function(ndim, nu, i){
  # i is the ith correlation parameter
  L <- diag(ndim)
  angles <- pi * exp(nu)/(1 + exp(nu))
  L[lower.tri(L)] <- cos(angles)
  S <-  matrix(0, nrow = ndim - 1, ncol = ndim - 1)
  S[lower.tri(S,diag=T)] <- sin(angles)
  S <- apply(cbind(1, rbind(0, S)), 1, cumprod)
  L <- L * t(S)
  sigma <- tcrossprod(L)
  sigma[lower.tri(sigma)][i]
}


z2r <- function (z) {
  ifelse(z > 354, 1, (exp(2 * z) - 1)/(1 + exp(2 * z)))
}

#' @title Print Method for class error_struc.
#' @description Prints error structure of class \code{\link{error_struct}}.
#' @param x object of class \code{\link{error_struct}}
#' @param ... further arguments passed to or from other methods.
#' @method print error_struct
#' @export
print.error_struct <- function(x, ...){
  cat("Parameters of the error structure:\n")
  print(attr(x, "par"), ...)
}