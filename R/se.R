##########################################################
###### PL Standard Errors - no parallel    ###############
##########################################################
PL_se <- function(rho){
  if (is.null(rho$link$deriv.fun)) {
    par <- rho$optpar
    ## construct Jacobian
    J <- make_jac_matrix(rho)
    J.inv <- solve(J)
    if(rho$control$trace != 0) cat("Computing variability and hessian matrix numerically ... \n")
    Vi_num <- matrix(0, ncol = length(par), nrow = rho$n)
    for (i in 1:rho$n) {
      if (i %% 100 == 0)  cat('Computed gradient for', i, 'out of', rho$n,'subjects\n')
      Vi_num[i, ] <- grad(function(par) neg_logPL_comp_i(par, rho, i), par, method = "Richardson")
    }
    cat("\n")
    rho$Vi_num <-  Vi_num %*% J.inv
    rho$V <- rho$n/(rho$n - length(par)) * crossprod(rho$Vi_num) # original variability matrix
    if(rho$control$trace != 0) cat("\nComputing Hessian numerically ... \n")
    Ht <-  hessian(function(par) PLfun(par, rho), par,
                   method = "Richardson",
                   method.args=list(eps=1e-6)) # Fisher matrix H(Gamma transf)
    rho$H.inv <-   J %*%  solve(Ht) %*% t(J)
  } else {
    if(rho$control$trace != 0) cat("Computing variability and hessian matrix analytically ... \n")
    derivs_for_se <- derivs_ana(rho)
    rho$V <- rho$n/(rho$n - NCOL(derivs_for_se$V)) * derivs_for_se$V  ## correct for degrees of freedom
    rho$H.inv <- solve(derivs_for_se$H)
  }
    rho$varGamma <- rho$H.inv %*% rho$V %*% rho$H.inv ## inverse godambe
    rho$seGamma <- sqrt(diag(rho$varGamma))
    if(rho$control$trace != 0) cat("Done computing the standard errors!\n")
    rho$claic <- 2 * rho$objective + 2 * sum(diag(rho$V %*% rho$H.inv))
    rho$clbic <- 2 * rho$objective + log(rho$n) * sum(diag(rho$V %*% rho$H.inv))
    rho
}
#############################################################################
# #' @title Derivative of rectangle probability wrt correlation parameter
# #' @description This function computes the derivative of the rectangle probability wrt correlation parameter
# #' @param Uk upper predictor for the k-th response
# #' @param Ul upper predictor for the l-th response
# #' @param Lk lower predictor for the k-th response
# #' @param Ll lower predictor for the l-th response
# #' @param r value or vector of the correlation parameters
# #' @param fun function computing the derivative of the bivariate pdf (depends on link)
d_corr_rect <- function(Uk, Ul, Lk, Ll, r, fun) {
   - fun(Uk, Ul, r) + fun(Uk, Ll, r) + fun(Lk, Ul, r) - fun(Lk, Ll, r)
}

d_theta_rect <- function(Uk, Ul, Lk, Ll,
                             r,
                             Umatk, Lmatk,
                             d_biv_fun, sdfack) {
    ## derivatives of the rectangle probabilities wrt to thresholds
    UU <-  d_biv_fun(Uk, Ul, r)
    UL <-  d_biv_fun(Uk, Ll, r)
    LU <-  d_biv_fun(Lk, Ul, r)
    LL <-  d_biv_fun(Lk, Ll, r)
    - 1/sdfack * ((UU - UL) * Umatk  - (LU - LL) * Lmatk)
}
d_beta_rect <- function(Uk, Ul, Lk, Ll, r,
                        XmatkU, XmatkL,
                        d_biv_fun, sdfack) {
    ## derivatives of the rectangle probabilities wrt to thresholds
    UU <-  d_biv_fun(Uk, Ul, r)
    UL <-  d_biv_fun(Uk, Ll, r)
    LU <-  d_biv_fun(Lk, Ul, r)
    LL <-  d_biv_fun(Lk, Ll, r)
    1/sdfack * ((UU - UL) * XmatkU - (LU - LL) * XmatkL)
}
d_stddev_rect <- function(Uk, Ul, Lk, Ll, r, d_biv_fun){
    UU <- d_biv_fun(Uk, Ul, r)
    UL <- d_biv_fun(Uk, Ll, r)
    LU <- d_biv_fun(Lk, Ul, r)
    LL <- d_biv_fun(Lk, Ll, r)
    (UU * Uk - UL * Uk - LU  * Lk + LL * Lk)
}

d_theta_rect_kl <- function(k, l, indkl, rho) {
    pick.col.theta <- switch(rho$threshold,
                          flexible      = seq_len(rho$ntheta[k]),
                          fix1first     = seq_len(rho$ntheta[k])[-1],
                          fix2first     = seq_len(rho$ntheta[k])[-c(1,2)],
                          fix2firstlast = seq_len(rho$ntheta[k] - 1)[-1])
    FOO <- d_theta_rect(Uk = rho$U[indkl, k], Ul = rho$U[indkl, l],
              Lk = rho$L[indkl, k], Ll = rho$L[indkl, l], r = rho$r,
              Umatk = rho$B1[[k]][indkl, pick.col.theta, drop = F],
              Lmatk = rho$B2[[k]][indkl, pick.col.theta, drop = F],
              d_biv_fun = rho$link$deriv.fun$dF2dx,
              sdfack =  rho$std.dev.mat[indkl, k])
    return(FOO)
}

d_beta_rect_klp <- function(k, l, p, s, indkl, rho) {
     bb <- rho$XcatL[[k]][indkl, (p -1) * rho$ntheta[k] + seq_len(rho$ntheta[k]), drop = FALSE]
     bbbL <- bb %*% rho$constraints[[p]][rho$inds.cat[[k]], s, drop = FALSE]
     bb<- rho$XcatU[[k]][indkl, (p -1)*rho$ntheta[k] + seq_len(rho$ntheta[k]), drop = FALSE]
     bbbU<- bb %*% rho$constraints[[p]][rho$inds.cat[[k]],s, drop = FALSE]
     FOO <- d_beta_rect(Uk = rho$U[indkl, k], Ul = rho$U[indkl, l],
                 Lk = rho$L[indkl, k], Ll = rho$L[indkl, l], r = rho$r,
                 XmatkU =bbbU, XmatkL =bbbL,
                 d_biv_fun = rho$link$deriv.fun$dF2dx,
                 sdfack = rho$std.dev.mat[indkl, k])
     return(FOO)
}


derivs_ana <- function(rho){
  ############################################
  ## function for analytic gradient and hessian
  #############################################
  par <- rho$optpar
  ############################################
  par_sigma <- par[rho$npar.thetas + rho$npar.betas +
    seq_len(attr(rho$error.structure, "npar"))]
  sigmas <- build_error_struct(rho$error.structure, par_sigma)

  ## Upper and lower matrices
  if (rho$p > 0) {
  rho$XcatU <- vector("list", rho$ndim)
  rho$XcatL <- vector("list", rho$ndim)

  for (j in seq_len(rho$ndim)) {
      ncat <- rho$ntheta[j] + 1
      mm <- model.matrix(~ - 1 + rho$y[,j] : rho$x[[j]],
              model.frame(~ - 1 + rho$y[,j] : rho$x[[j]],
              na.action = function(x) x))
      rho$XcatL[[j]] <- mm[,-(ncat * (seq_len(rho$p) - 1) + 1), drop = F]
      rho$XcatU[[j]] <- mm[,-(ncat * seq_len(rho$p)), drop = F]
    }
  beta_cat <- bdiag(rho$constraints) %*% rho$beta
  } else {
    beta_cat <- integer()
    rho$XcatU <- lapply(seq_len(rho$ndim), function(x) integer()) #creates warning, but OK
    rho$XcatL <- lapply(seq_len(rho$ndim), function(x) integer()) #In th_u - xbeta_u : Recycling array of length 1 in vector-array arithmetic is deprecated

  }
  rho$U  <- sapply(1:rho$ndim, function(j) {
    th_u <- c(rho$theta[[j]], rho$inf.value)[rho$y[, j]]
    xbeta_u <- c(rho$XcatU[[j]] %*% beta_cat[rho$indjbeta[[j]]])
    th_u - xbeta_u - rho$offset[[j]]
  })/sigmas$sdVec
  rho$L  <- sapply(1:rho$ndim, function(j) {
    th_l <- c(-rho$inf.value, rho$theta[[j]])[rho$y[, j]]
    xbeta_l <- c(rho$XcatL[[j]] %*% beta_cat[rho$indjbeta[[j]]])
    th_l - xbeta_l - rho$offset[[j]]
  })/sigmas$sdVec
  ########################################################  s
  rho$std.dev.mat <- sigmas$sdVec ## matrix of standard deviations
  if (is.null(dim(rho$std.dev.mat))) rho$std.dev.mat <- matrix(1, ncol = rho$ndim, nrow = rho$n)
  std.dev.mat <- rho$std.dev.mat
  if (!rho$error.structure$name %in% c("cor_equi", "cor_ar1")){
    lev <- apply(attr(rho$error.structure, "covar"),1,
      function(x) which(x==1))
  } else {
    lev <- rep(1, rho$n)
  }
  npar.err <-  attr(rho$error.structure, "npar")
  npar.cor <-  attr(rho$error.structure, "npar.cor")
  npar.sd <-  attr(rho$error.structure, "npar.sd")
  ########################################################
  ## prepare contrast matrices for threshold parameters
  #########################################################
  B2 <- lapply(1:rho$ndim, function(j)
    1 * (col(matrix(0, rho$n, rho$ntheta[j] + 1)) ==
           c(unclass(rho$y[, j]))))
  rho$B1 <- lapply(1:rho$ndim, function(i) as.matrix(B2[[i]][,-(rho$ntheta[i] + 1), drop = FALSE]))
  rho$B2 <- lapply(1:rho$ndim, function(i) as.matrix(B2[[i]][,-1, drop = FALSE]))
  ###############################################################


  ######################################################
  ## First the univariate case (q_i = 1)
  ######################################################
  h_list <- NULL
  if (dim(rho$ind_univ)[1] != 0){
    ## if univariate observations:
    h_list <- lapply(unique(rho$ind_univ[, 2]), function(j) {
      indj <- rho$ind_univ[rho$ind_univ[, 2] == j, , drop = F]
      ## for each j compute the univariate probabilities
      pr <- rep(1, rho$n)
      pr[indj[,1]] <- rho$link$F_uni(rho$U[indj]) - rho$link$F_uni(rho$L[indj])
      pr[pr < .Machine$double.eps] <- .Machine$double.eps
      #########################
      ## Gradient components:
      #########################
      dtheta <- dbeta <- dcorr <- dstddev <- NULL
      ## 1: dtheta
      ##
      pick.col.theta <- switch(rho$threshold,
        flexible      = seq_len(rho$ntheta[j]),
        fix1first     = seq_len(rho$ntheta[j])[-1],
        fix2first     = seq_len(rho$ntheta[j])[-c(1,2)],
        fix2firstlast = seq_len(rho$ntheta[j] - 1)[-1])
      if (length(pick.col.theta) != 0) {
        dtheta <- matrix(0, nrow = rho$n, ncol = rho$npar.thetas)
        colposj <- rho$ind.thresholds[[j]][1] + seq_len(rho$npar.theta[j]) - 1
        dtheta[indj[,1], colposj] <-
          (rho$link$deriv.fun$dF1dx(rho$L[indj]) * rho$B2[[j]][indj[,1], pick.col.theta] -
           rho$link$deriv.fun$dF1dx(rho$U[indj]) * rho$B1[[j]][indj[,1], pick.col.theta])/rho$std.dev.mat[indj]
      }
      ## 2: dbeta
      ##
      if (rho$npar.betas > 0) {
        dbeta <- matrix(0, nrow = rho$n, ncol = rho$npar.betas)
        XtmpUj <- as.matrix(rho$XcatU[[j]][indj[,1], ] %*% bdiag(rho$constraints)[rho$indjbeta[[j]],])
        XtmpLj <- as.matrix(rho$XcatL[[j]][indj[,1], ] %*% bdiag(rho$constraints)[rho$indjbeta[[j]],])
        dbeta[indj[,1], ] <- (rho$link$deriv.fun$dF1dx(rho$U[indj]) * XtmpUj -
          rho$link$deriv.fun$dF1dx(rho$L[indj]) * XtmpLj)/rho$std.dev.mat[indj]
      }
      ## 3: dcorr
      ## no correlation in univariate case
      if (npar.cor > 0){
        dcorr <- matrix(0, nrow = rho$n, ncol = npar.cor)
      }
      ## 4: dstddev
      ##
      if (npar.sd > 0){
        dstddev <- matrix(0, nrow = rho$n, ncol = npar.sd)
        if (rho$error.structure$name == "cov_general") {
          arr.ind.j <- cbind(indj[,1], (lev[indj[,1]] - 1) * rho$ndim + j)
          dstddev[arr.ind.k] <- (rho$link$deriv.fun$dF1dx(rho$U[indj]) * rho$U[indj] -
              rho$link$deriv.fun$dF1dx(rho$L[indj]) * rho$L[indj])/rho$std.dev.mat[indj]
        }
      }
      rho$weights * 1/pr * cbind(dtheta, dbeta, dcorr, dstddev)
    })
  }
  #####################################
   ## take each possible pair (k, l)
  ######################################
  r_mat <- sigmas$rVec[, rho$dummy_pl_lag == 1, drop = F]

  it0 <- length(h_list)
  #it <- 1
  for (it in (it0 + seq_along(rho$combis))) {
    comb <- rho$combis[[it - it0]]
    ## for each pair make an indicator for each subject where the pair applies
    indkl <- rho$ind_kl[[it - it0]]
    k <- comb[1]
    l <- comb[2]
    ## correlation
    rho$r <- r_mat[indkl, (it - it0)]
    ## pr_{kl}
    pr <- rep(1, rho$n)
    pr[indkl] <- rho$link$F_biv_rect(U = rho$U[indkl, comb, drop = F],
                                     L = rho$L[indkl, comb, drop = F],
                                     r = rho$r)
    pr[pr < .Machine$double.eps] <- .Machine$double.eps

    ## vector h_kl will contain the gradient for all d log p_{kl}/d pars
    dtheta <- dbeta <- dcorr <- NULL
    ##################
    ## dtheta
    ##################
    if (sum(rho$npar.theta.opt) > 0) {
      dtheta <- matrix(0, ncol = rho$npar.thetas, nrow = rho$n)
      # check in constraints if the threshold values are the same \theta_k = \theta_l
      th_constraints_unique <- unique(rho$threshold.constraints)
      tmp <- lapply(th_constraints_unique, function(j) {
          indjj <- (1:rho$ndim)[rho$threshold.constraints == j]
          indj <- indjj[indjj %in% comb]
          dth <- switch(length(indj) + 1,
                  matrix(0, nrow = sum(indkl), ncol = rho$npar.theta.opt[indjj[1]]),
                  d_theta_rect_kl(indj, comb[comb != indj], indkl, rho),
                  d_theta_rect_kl(k, l, indkl, rho) + d_theta_rect_kl(l, k, indkl, rho))
          dth
      })
      dtheta[indkl, ] <- do.call("cbind", tmp)
    }
    ##################
    ## dbeta
    ##################
    if (rho$npar.betas > 0) {
      dbeta <- matrix(0, ncol = rho$npar.betas, nrow = rho$n)
      ## for which covariates are there betas to be estimated
      inds.cats.kl <- rho$inds.cat[c(k,l)]
      inddouble <-lapply(rho$constraints, function(x){
        indx <- rep(0, NCOL(x))
        for (s in seq_len(NCOL(x))){
          if (all(x[inds.cats.kl[[1]], s] == 0))  indx[s] <- 2 ## code for only l
          if (all(x[inds.cats.kl[[2]], s] == 0))  indx[s] <- 1   ## code for only k
        }
        indx
      })
      tmp <- lapply(seq_along(rho$constraints), function(p) {
        sapply(seq_len(NCOL(rho$constraints[[p]])), function(s)
          switch(inddouble[[p]][s] + 1,
            d_beta_rect_klp(k, l, p, s, indkl, rho) + d_beta_rect_klp(l, k, p=p,s=s, indkl, rho),
            d_beta_rect_klp(k, l, p, s, indkl, rho),
            d_beta_rect_klp(l, k, p, s, indkl, rho))
          )
      })
      dbeta[indkl, ] <- do.call("cbind", tmp)
    }
    ##################
    ## dcorr
    ##################
    if (npar.err > 0){
      dcorr <- matrix(0, ncol = npar.err, nrow = rho$n)
      covar <- attr(rho$error.structure, "covariate")
      dLdr <- d_corr_rect(Uk = rho$U[indkl, k], Ul = rho$U[indkl, l],
                          Lk = rho$L[indkl, k], Ll = rho$L[indkl, l],
                          r = rho$r, fun = rho$link$deriv.fun$dF2dr)
      if (rho$error.structure$name %in% c("cor_ar1", "cor_equi")) {
        lagabs <- switch(rho$error.structure$name,
                          "cor_ar1" = abs(comb[1] - comb[2]),
                          "cor_equi" = 1)
        rpowinvlag <- rho$r^(1/lagabs)
        salpha <- 0.5 * (log(1 + rpowinvlag) - log(1 - rpowinvlag))
        const <- exp(2 * salpha)/(exp(2 * salpha) + 1)^2  * 4
        dcorr[indkl, ] <- lagabs * rpowinvlag^(lagabs - 1) * dLdr * const * covar[indkl, ]
      } else {
        poslev <- which(sapply(combn(rho$ndim, 2, simplify=F),
          function(x) all(x == comb)))
        arr.ind <- cbind(which(indkl), (lev[indkl]-1) * (rho$ndim * (rho$ndim - 1)/2)  + poslev)
        dcorr[arr.ind] <- dLdr
      }
    }
    ##################
    ## dstddev
    ##################
    if (rho$error.structure$type == "covariance") {
      dstddev <- matrix(0, nrow = rho$n, ncol = rho$ndim * max(lev))
      poslevk <- (lev[indkl] - 1) * rho$ndim + k
      poslevl <- (lev[indkl] - 1) * rho$ndim + l
      arr.ind.k <- cbind(which(indkl), poslevk)
      arr.ind.l <- cbind(which(indkl), poslevl)
      dstddev[arr.ind.k] <-  1/rho$std.dev.mat[indkl, k] *
        d_stddev_rect(Uk = rho$U[indkl, k], Ul = rho$U[indkl, l],
          Lk = rho$L[indkl, k], Ll = rho$L[indkl, l], rho$r,
          d_biv_fun = rho$link$deriv.fun$dF2dx)
      dstddev[arr.ind.l] <-    1/rho$std.dev.mat[indkl, l] *
        d_stddev_rect(Uk = rho$U[indkl, l], Ul = rho$U[indkl, k],
          Lk = rho$L[indkl, l], Ll = rho$L[indkl, k], r = rho$r,
          d_biv_fun = rho$link$deriv.fun$dF2dx)
     dcorr[, (NCOL(dcorr) - rho$ndim * max(lev) + 1):NCOL(dcorr)] <- dstddev
    }

    h_list[[it]] <- rho$weights * 1/pr * cbind(dtheta, dbeta, dcorr)

  }
  ## matrix containing the gradients for each subject
  Vi <- Reduce("+", h_list)
  ## variability matrix
  V <- crossprod(Vi)
  ## Hessian matrix
  H <- Reduce("+", lapply(h_list, crossprod))
  list(V = V, H = H)
}
#############################################################
###### neg loglikelihood component for each subject i #######
######                 for numeric gradient           #######
#############################################################
transf_par_i <- function(par, rho, i) {
  par_sigma <- par[rho$npar.thetas + rho$npar.betas + seq_len(attr(rho$error.structure, "npar"))]
  sigmas <- build_error_struct(rho$error.structure, par_sigma)
  if (is.null(dim(sigmas$sdVec))){
    sdi <- sigmas$sdVec
  } else {
    sdi <- sigmas$sdVec[i, ]
  }
  par_beta <- par[rho$npar.thetas + seq_len(rho$npar.betas)]
  betatilde <- rho$constraints_mat %*% par_beta
  par_theta <- rho$transf_thresholds(par[seq_len(rho$npar.thetas)], rho, betatilde)
  thetatilde <- lapply(seq_len(rho$ndim), function(j)
    par_theta[[j]] + rho$thold_correction[[j]](betatilde, k = j, rho = rho))

  pred.upper  <- sapply(1:rho$ndim, function(j) {
   th_u <- c(thetatilde[[j]], rho$inf.value)[rho$y[i, j]]
   xbeta_u <- sum(rho$XcatU[[j]][i, ] * betatilde[rho$indjbeta[[j]]])
   th_u - xbeta_u - rho$offset[[j]]
  })/sdi
  pred.lower  <- sapply(1:rho$ndim, function(j) {
    th_l <- c(-rho$inf.value, thetatilde[[j]])[rho$y[i, j]]
    xbeta_l <- sum(rho$XcatL[[j]][i, ] * betatilde[rho$indjbeta[[j]]])
    th_l - xbeta_l - rho$offset[[j]]
  })/sdi
  list(U = pred.upper, L = pred.lower,
       corr_par = sigmas$rVec[i, , drop=F])
}
##############
neg_logPL_comp_i <- function(par, rho, i) {
  # transform parameters and get upper and lower bounds
  tmp <- transf_par_i(par, rho, i)
  U <- tmp$U
  L <- tmp$L
  r_mat <- tmp$corr_par
  q <- which(!is.na(rho$y[i, ]))
  if (length(q) == 1){
    pr <- rho$link$F_uni(U[q]) - rho$link$F_uni(L[q])
    logPLi <- rho$weights[i] * log(max(pr, .Machine$double.eps))
  } else {
    combis <- combn(q, 2)
    combis <- combis[,which((combis[2,] - combis[1,])  <= rho$PL.lag), drop = FALSE]
    logPLi <- 0
    dim(U) <- dim(L) <- c(1, length(U))
    for (h in seq_len(ncol(combis))) {
      r <- r_mat[, h]
      pr <- rho$link$F_biv_rect(U = U[,combis[, h], drop = F],
                        L = L[,combis[, h], drop = F],
                        r)
      logPLi <- logPLi + rho$weights[i] * log(max(pr, .Machine$double.eps))
  }
}
  -logPLi
}

transf_thresholds_fix2_firstlast_jac <- function(rho, j, gamma_j, i){
  recursive.theta <- function(i) {
    if (i == 0) 0
    else return ((exp(gamma_j[i]) + recursive.theta(i - 1))/(1 + exp(gamma_j[i])))
  }
    theta <- sapply(1:length(gamma_j), function(i)
      recursive.theta(i))
    theta[i]
}


transf_thresholds_fix2_first_jac <- function(rho,j,gamma_j,i){
      c(0, cumsum(c(1 ,exp(gamma_j))))[i+2]
}

make_jac_matrix <- function(rho) {
  par <- rho$optpar
  first.ind.theta <- sapply(rho$ind.thresholds, "[", 1)
  transf_thresholds_jac <- switch(rho$threshold,
                                  fix2firstlast = transf_thresholds_fix2_firstlast_jac,
                                  fix2first = transf_thresholds_fix2_first_jac)

  gamma <- par[1:rho$npar.thetas]
   if (rho$threshold == "flexible") {
     jac <- lapply((1:rho$ndim)[which(rho$npar.theta.opt > 0)], function(j){ #rho$npar.theta
       emat <- diag(rho$ntheta[j])
       if (ncol(emat) >= 2) {
         emat[,1] <- 1
         for (k in 2:ncol(emat))
           emat[(k:nrow(emat)), k] <-
             exp(gamma[(first.ind.theta[j]) + seq_len(rho$ntheta[j]-1)])[k - 1]
       }
       emat
       })
   } else {
     if (rho$threshold == "fix1first") {
       jac <- lapply((1:rho$ndim)[which(rho$npar.theta.opt > 0)], function(j){ #rho$npar.theta
         emat <- diag(rho$ntheta[j])
         if (ncol(emat) >= 2) {
           emat[,1] <- 1
           for (k in 2:ncol(emat))
             emat[(k:nrow(emat)), k] <-
               exp(gamma[(first.ind.theta[j]) + seq_len(rho$npar.theta.opt[j])-1])[k - 1] #rho$npar.theta
         }
         emat[-1,-1]
       })
     } else {
       jac <- lapply((1:rho$ndim)[which(rho$npar.theta.opt > 0)], function(j){ #rho$npar.theta
         gamma_j <- gamma[first.ind.theta[j] + seq_len(rho$npar.theta.opt[j]) - 1] #rho$npar.theta
         t(sapply(1:length(gamma_j),
                  function(i) grad(function(x) transf_thresholds_jac(rho, j, x, i), x=gamma_j)))
       })
     }
   }
  ## Jacobian for BETAS: no transform
  jac[sum(rho$npar.theta.opt > 0) + seq_len(rho$npar.betas)] <- 1
  ## Jacobian for ERROR STRUCTURE
  parsigma <- par[rho$npar.thetas + rho$npar.betas + seq_len(attr(rho$error.structure, "npar"))]
  corr.jac <- corr_jac(rho$error.structure, parsigma)
  jac[sum(rho$npar.theta.opt > 0) + rho$npar.betas + seq_along(corr.jac)] <- corr.jac #rho$npar.theta
  ## make Jacobian matrix
  J <- as.matrix(bdiag(jac))
  return(J)
}
