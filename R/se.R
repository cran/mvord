##########################################################
###### PL Standard Errors - no parallel    ###############
##########################################################

PL_se <- function(rho){
  if (is.null(rho$link$deriv.fun)) {
    par <- rho$optpar
    ## construct Jacobian
    J <- make_jac_matrix(rho)
    J.inv <- solve(J)
    cat("Computing variability and hessian matrix numerically ... \n")
    Vi_num <- matrix(0, ncol = length(par), nrow = rho$n)
    for (i in 1:rho$n) {
      if (i %% 100 == 0)  cat('Computed gradient for', i, 'out of', rho$n,'subjects\n')
      Vi_num[i, ] <- grad(function(par) neg_logPL_comp_i(par, rho, i), par, method = "Richardson")
    }
    cat("\n")
    rho$Vi_num <-  Vi_num %*% J.inv
    rho$V <- rho$n/(rho$n - length(par)) * crossprod(rho$Vi_num) # original variability matrix
    cat("\nComputing Hessian numerically ... \n")
    Ht <-  hessian(function(par) PLfun(par, rho), par,
                   method = "Richardson",
                   method.args=list(eps=1e-6)) # Fisher matrix H(Gamma transf)
    rho$H.inv <-   J %*%  solve(Ht) %*% t(J)
  } else {
    cat("Computing variability and hessian matrix analytically ... \n")
    derivs_for_se <- derivs_ana(rho)
    rho$V <- rho$n/(rho$n - NCOL(derivs_for_se$V)) * derivs_for_se$V  ## correct for degrees of freedom
    rho$H.inv <- solve(derivs_for_se$H)
  }
    rho$varGamma <- rho$H.inv %*% rho$V %*% rho$H.inv ## inverse godambe
    rho$seGamma <- sqrt(diag(rho$varGamma))
    cat("Done computing the standard errors!\n")
    rho$claic <- 2 * rho$objective + 2 * sum(diag(rho$V %*% rho$H.inv))
    rho$clbic <- 2 * rho$objective + log(rho$n) * sum(diag(rho$V %*% rho$H.inv))
    rho
}

deriv_corr_rect <- function(Uk, Ul, Lk, Ll, r, deriv_corr_fun) {
  -  (deriv_corr_fun(Uk, Ul, r) - deriv_corr_fun(Uk, Ll, r) -
      deriv_corr_fun(Lk, Ul, r) + deriv_corr_fun(Lk, Ll, r))
}

deriv_theta_rect <- function(Uk, Ul, Lk, Ll,
                             r,
                             Umatk, Lmatk,
                             deriv_biv_fun, sdfack) {
    ## derivatives of the rectangle probabilities wrt to thresholds
    UU <-  deriv_biv_fun(Uk, Ul, r)
    UL <-  deriv_biv_fun(Uk, Ll, r)
    LU <-  deriv_biv_fun(Lk, Ul, r)
    LL <-  deriv_biv_fun(Lk, Ll, r)
    - 1/sdfack * ((UU - UL) * Umatk  - (LU - LL) * Lmatk)
}
deriv_beta_rect <- function(Uk, Ul, Lk, Ll,
                            r,
                            XmatkU, XmatkL,
                            deriv_biv_fun, sdfack) {
    ## derivatives of the rectangle probabilities wrt to thresholds
    UU <-  deriv_biv_fun(Uk, Ul, r)
    UL <-  deriv_biv_fun(Uk, Ll, r)
    LU <-  deriv_biv_fun(Lk, Ul, r)
    LL <-  deriv_biv_fun(Lk, Ll, r)
    1/sdfack * ((UU - UL) * XmatkU - (LU - LL) * XmatkL)
}
deriv_stddev_rect <- function(Uk, Ul, Lk, Ll, r, deriv_biv_fun){
    UU <- deriv_biv_fun(Uk, Ul, r)
    UL <- deriv_biv_fun(Uk, Ll, r)
    LU <- deriv_biv_fun(Lk, Ul, r)
    LL <- deriv_biv_fun(Lk, Ll, r)
    (UU * Uk - UL * Uk - LU  * Lk + LL * Lk)
}

derivs_ana <- function(rho){
  ## function for analytic gradient and hessian
  par <- rho$optpar
  tmp <- transf_par(par, rho)
  U <- tmp$U
  L <- tmp$L
  std.dev.mat <- tmp$sd_mat
  if (is.null(dim(std.dev.mat)))
    std.dev.mat <- matrix(1, ncol = rho$ndim, nrow = rho$n)
  ## prepare contrast matrices for threshold parameters
  rho$B2 <- lapply(1:rho$ndim, function(j)
    1 * (col(matrix(0, rho$n, rho$ntheta[j] + 1)) ==
           c(unclass(rho$y[, j]))))

  rho$B1 <- lapply(1:rho$ndim, function(i) as.matrix(rho$B2[[i]][,-(rho$ntheta[i] + 1), drop = FALSE]))
  rho$B2 <- lapply(1:rho$ndim, function(i) as.matrix(rho$B2[[i]][,-1, drop = FALSE]))

  if(! rho$error.structure$name %in% c("cor_equi", "cor_ar1")){
     lev <- apply(attr(rho$error.structure, "covar"),1,
      function(x) which(x==1))
  } else {
    lev <- rep(1, rho$n)
  }
  npar.err <-  attr(rho$error.structure, "npar")
  covar <- attr(rho$error.structure, "covariate")

  npar.beta.opt <- sum(rho$npar.betas)#apply(rho$ind.coef, 1, function(x) sum(!is.na(x)))
  #pick.col.beta <- lapply(seq_len(nrow(rho$ind.coef)), function(i) which(!is.na(rho$ind.coef[i, ])))

  contrast <- as.matrix((!is.na(rho$y)) + 0)
  contrast_uni <- unique(unlist(apply(contrast, 1, function(x) {
      k <- which(x == 1) * (sum(x) == 1)
      k[k != 0]
  })))

  ## First take care of the univariate case (q_i = 1)
  #pr <- rho$link$F_uni(pred.upper[rho$ind_univ]) -
  #rho$link$F_uni(pred.lower[rho$ind_univ])
  #pr[pr < .Machine$double.eps] <- .Machine$double.eps


  if (length(contrast_uni) > 0) {
   h_list <- lapply(contrast_uni, function(k) {
    indk <-  !is.na(rho$y[, k]) & (rowSums(!is.na(rho$y)) == 1)
    pr <- rep(1, rho$n)
    pr[indk] <- rho$link$F_uni(U[indk, k]) - rho$link$F_uni(L[indk, k])
    pr[pr < .Machine$double.eps] <- .Machine$double.eps
    # dtheta
    dtheta <- matrix(0, nrow = rho$n, ncol = rho$npar.thetas)
    pick.col.theta <- switch(rho$threshold,
                         flexible      = seq_len(rho$ntheta[k]),
                         fix1first     = seq_len(rho$ntheta[k])[-1],
                         fix2first     = seq_len(rho$ntheta[k])[-c(1,2)],
                         fix2firstlast = seq_len(rho$ntheta[k] - 1)[-1])
    if (length(pick.col.theta) != 0) {
      # first indices of thresholds in parameter vector for each rater
      first.ind.theta <- sapply(rho$ind.thresholds, "[", 1)
      colposk <- first.ind.theta[k] + seq_len(rho$npar.theta[k]) - 1
      dtheta[indk, colposk] <- -  1/std.dev.mat[indk, k] *
         (rho$link$deriv.fun$dF1dx(U[indk, k]) * rho$B1[[k]][indk, pick.col.theta] -
          rho$link$deriv.fun$dF1dx(L[indk, k]) * rho$B2[[k]][indk, pick.col.theta])
    } else dtheta <- NULL

    # dbeta
    dbeta <- matrix(0, nrow = rho$n, ncol = rho$npar.betas)

    XtmpUk <- do.call("cbind", lapply(seq_along(rho$constraints), function(p) {
     sapply(seq_len(NCOL(rho$constraints[[p]])), function(s){
          bb<- rho$XcatU[[k]][indk, (p -1)*rho$ntheta[k] + seq_len(rho$ntheta[k])]
          bb %*% rho$constraints[[p]][rho$inds.cat[[k]],s]
    })}))
    XtmpLk <- do.call("cbind", lapply(seq_along(rho$constraints), function(p) {
     sapply(seq_len(NCOL(rho$constraints[[p]])), function(s){
          bb<- rho$XcatL[[k]][indk, (p -1)*rho$ntheta[k] + seq_len(rho$ntheta[k])]
          bb %*% rho$constraints[[p]][rho$inds.cat[[k]],s]
    })}))
   dbeta[indk, ] <-
       1/std.dev.mat[indk, k] *
      (rho$link$deriv.fun$dF1dx(U[indk, k]) * XtmpUk - rho$link$deriv.fun$dF1dx(L[indk, k]) * XtmpLk)
    # dcorr -- no correlation for univ case
    dcorr <- matrix(0, nrow = rho$n, ncol = npar.err)

    # stddev
    if (rho$error.structure$type == "covariance"){
      dstddev <- matrix(0, nrow = rho$n, ncol = rho$ndim * NCOL(covar))
      arr.ind.k <- cbind(which(indk), (lev - 1) * rho$ndim + k)
      dstddev[arr.ind.k] <-   1/std.dev.mat[indk, k] *
             (rho$link$deriv.fun$dF1dx(U[indk, k]) * U[indk, k] -
              rho$link$deriv.fun$dF1dx(L[indk, k]) * L[indk, k])
      dcorr[, (NCOL(dcorr) - rho$ndim * max(lev) + 1):NCOL(dcorr)] <- dstddev
    }
    rho$weights * 1/pr * cbind(dtheta, dbeta, dcorr)
  })
  } else {
    h_list <- NULL
  }
  #####################################
   ## take each possible pair (k, l)
  ######################################
  r_mat <- tmp$corr_par[, rho$dummy_pl_lag == 1, drop = F]

  it0 <- length(h_list)
  #it <- 1
  for (it in (it0 + seq_along(rho$combis))) {
    comb <- rho$combis[[it - it0]]
    ## for each pair make an indicator for each subject where the pair applies
    indkl <- rho$ind_kl[[it - it0]]

    k <- comb[1]
    l <- comb[2]

    ## correlation
    r <- r_mat[indkl, (it - it0)]
    ## pr_{kl}
    pr <- rep(1, rho$n)
    pr[indkl] <- rho$link$F_biv_rect(U = U[indkl, comb, drop = F],
                                L = L[indkl, comb, drop = F],
                                r = r)
    pr[pr < .Machine$double.eps] <- .Machine$double.eps

   ## vector h_kl will contain the gradient for all d log p_{kl}/d pars
   ## dtheta
   if (sum(rho$npar.theta.opt) > 0) {
   dtheta <- matrix(0, ncol = rho$npar.thetas,
                       nrow = rho$n)
   deriv_theta_rect_kl <- function(k, l) {
     pick.col.theta <- switch(rho$threshold,
                          flexible      = seq_len(rho$ntheta[k]),
                          fix1first     = seq_len(rho$ntheta[k])[-1],
                          fix2first     = seq_len(rho$ntheta[k])[-c(1,2)],
                          fix2firstlast = seq_len(rho$ntheta[k] - 1)[-1])
     if (length(pick.col.theta) > 0) {
     Uk <- U[indkl, k]
     Ul <- U[indkl, l]
     Lk <- L[indkl, k]
     Ll <- L[indkl, l]
     return(deriv_theta_rect(
                      Uk = Uk, Ul = Ul,
                      Lk = Lk, Ll = Ll,
                      r = r,
                      Umatk = rho$B1[[k]][indkl, pick.col.theta, drop = F],
                      Lmatk = rho$B2[[k]][indkl, pick.col.theta, drop = F],
                      deriv_biv_fun = rho$link$deriv.fun$dF2dx,
                      sdfack =  std.dev.mat[indkl, k]) )
    } else {
     return(NULL)
    }
   }
   # check in constraints if the threshold values are the same \theta_k = \theta_l
   dtheta[indkl, ] <-  do.call("cbind", lapply(unique(rho$threshold.constraints), function(j) {
     indjj <- (1:rho$ndim)[rho$threshold.constraints == j]
     indj <- indjj[indjj %in% comb]

     if (length(indj) == 1) {
      dth <- deriv_theta_rect_kl(indj, comb[comb != indj])
      } else {
      if (length(indj) == 2) {
        dth <- deriv_theta_rect_kl(k, l) +  deriv_theta_rect_kl(l, k)
      } else {
        dth <- matrix(0, nrow = sum(indkl),
          ncol = rho$npar.theta.opt[indjj[1]])
      }
     }
     return(dth)
   }))
   } else dtheta <- NULL
   ## dbeta
   dbeta <- matrix(0, ncol = rho$npar.betas, nrow = rho$n)
   ## for which covariates are there betas to be estimated
   # deriv_beta_rect_klp <- function(k, l, p, ind) {
   #   Uk <- U[ind, k]
   #   Ul <- U[ind, l]
   #   Lk <- L[ind, k]
   #   Ll <- L[ind, l]
   #   return(
   #   deriv_beta_rect(Uk = Uk, Ul = Ul,
   #                   Lk = Lk, Ll = Ll, r = r,
   #                    Xmatk =rho$x[[k]][ind,p],
   #                    deriv_biv_fun = rho$link$deriv.fun$dF2dx,
   #                    sdfack = std.dev.mat[ind, k]))
   # }
  deriv_beta_rect_klp2 <- function(k, l, p,s, ind) {
     Uk <- U[ind, k]
     Ul <- U[ind, l]
     Lk <- L[ind, k]
     Ll <- L[ind, l]
     bb<- rho$XcatL[[k]][ind, (p -1)*rho$ntheta[k] + seq_len(rho$ntheta[k])]
     bbbL<- bb %*% rho$constraints[[p]][rho$inds.cat[[k]],s]
     bb<- rho$XcatU[[k]][ind, (p -1)*rho$ntheta[k] + seq_len(rho$ntheta[k])]
     bbbU<- bb %*% rho$constraints[[p]][rho$inds.cat[[k]],s]
     return(
     deriv_beta_rect(Uk = Uk, Ul = Ul,
                     Lk = Lk, Ll = Ll, r = r,
                     XmatkU =bbbU, XmatkL =bbbL,
                     deriv_biv_fun = rho$link$deriv.fun$dF2dx,
                     sdfack = std.dev.mat[ind, k]))
   }

   #rho$npar.betas
   inds.cats.kl <- rho$inds.cat[c(k,l)]
   #for X1
   inddouble<-lapply(rho$constraints, function(x){
   indx <- rep(0, NCOL(x))
   for (s in seq_len(NCOL(x))){
   if (all(x[inds.cats.kl[[1]], s] == 0))  indx[s] <- 2 ## code for only l
    if (all(x[inds.cats.kl[[2]], s] == 0))  indx[s] <- 1   ## code for only k

    }
   as.character(indx)
   })

   dbeta[indkl, ] <- do.call("cbind", lapply(seq_along(rho$constraints), function(p) {
     sapply(seq_len(NCOL(rho$constraints[[p]])), function(s)
      switch(inddouble[[p]][s],
        "0" = deriv_beta_rect_klp2(k,l,p=p,s=s, indkl) +
              deriv_beta_rect_klp2(l,k,p=p,s=s, indkl),
        "1" =  deriv_beta_rect_klp2(k,l,p=p,s=s, indkl),
        "2" = deriv_beta_rect_klp2(l,k,p=p,s=s, indkl) ))
   } ))

   # for (p in seq_len(NCOL(rho$coef.ind[[1]]))) {

   #    betaskl <- sapply(rho$coef.ind[c(k,l)], function(x)
   #      x[indkl, p])
   #    indx <- apply(betaskl, 2, function(x) !all(is.na(x)))
   #    if (sum(indx) == 1) {
   #      arr.ind.single <- cbind(which(indkl), betaskl[,indx])
   #      dbeta[arr.ind.single] <- deriv_beta_rect_klp(c(k, l)[indx],
   #        c(k, l)[!indx], p, which(indkl))

   #    } else {
   #       if (sum(indx) == 2) {
   #        ind.double <- betaskl[,1] == betaskl[,2]
   #        ind.single <- betaskl[,1] != betaskl[,2]
   #        arr.ind.double <- cbind(which(indkl)[ind.double],
   #          betaskl[ind.double,1])
   #        dbeta[arr.ind.double] <-
   #          deriv_beta_rect_klp(k, l, p, which(indkl)[ind.double]) +
   #          deriv_beta_rect_klp(l, k, p, which(indkl)[ind.double])

   #    arr.ind.single.k <- cbind(which(indkl)[ind.single], betaskl[ind.single,1])
   #    arr.ind.single.l <- cbind(which(indkl)[ind.single], betaskl[ind.single,2])

   #    dbeta[arr.ind.single.k] <-
   #      deriv_beta_rect_klp(k, l, p,which(indkl)[ind.single])

   #    dbeta[arr.ind.single.l] <-
   #      deriv_beta_rect_klp(l, k, p, which(indkl)[ind.single])
   #    }
   # }
   # }

  ## dcorr
  dcorr <- matrix(0, ncol = npar.err, nrow = rho$n)
  if (npar.err > 0){
  covar <- attr(rho$error.structure, "covariate")
  dLdr <- deriv_corr_rect(Uk = U[indkl, k], Ul = U[indkl, l],
    Lk = L[indkl, k], Ll = L[indkl, l], r = r,
    deriv_corr_fun = rho$link$deriv.fun$dF2dr)
  if (rho$error.structure$name == "cor_ar1") {
    rpowinvlag <- r^(1/abs(comb[1] - comb[2]))
    xbeta <- 0.5 * (log(1 + rpowinvlag) - log(1 - rpowinvlag))
    dcorr[indkl, ] <- abs(comb[1] - comb[2]) * rpowinvlag^(abs(comb[1] - comb[2]) - 1) * dLdr * exp(2 * xbeta)/(exp(2 * xbeta) + 1)^2  * 4 * covar[indkl, ]
  } else {
    if (rho$error.structure$name == "cor_equi") {
      xbeta <- 0.5 * (log(1 + r) - log(1 - r))
      dcorr[indkl, ] <- dLdr * exp(2 * xbeta)/(exp(2 * xbeta) + 1)^2  * 4 * covar[indkl, ]
    } else {
      poslev <- which(sapply(combn(rho$ndim, 2, simplify=F), function(x) all(x==comb)))
      arr.ind <- cbind(which(indkl), (lev[indkl]-1) * (rho$ndim * (rho$ndim - 1)/2)  + poslev)
      dcorr[arr.ind] <- dLdr
    }
  }
  ## dstddev
  if (rho$error.structure$type == "covariance") {
    dstddev <- matrix(0, nrow = rho$n,
      ncol = rho$ndim * max(lev))
    poslevk <- (lev[indkl] - 1) * rho$ndim + k
    poslevl <- (lev[indkl] - 1) * rho$ndim + l
    arr.ind.k <- cbind(which(indkl), poslevk)
    arr.ind.l <- cbind(which(indkl), poslevl)
    dstddev[arr.ind.k] <-  1/std.dev.mat[indkl, k] *
      deriv_stddev_rect(Uk = U[indkl, k], Ul = U[indkl, l],
        Lk = L[indkl, k], Ll = L[indkl, l], r,
        deriv_biv_fun = rho$link$deriv.fun$dF2dx)

    dstddev[arr.ind.l] <-    1/std.dev.mat[indkl, l] *
      deriv_stddev_rect(Uk = U[indkl, l], Ul = U[indkl, k],
        Lk = L[indkl, l], Ll = L[indkl, k], r,
        deriv_biv_fun = rho$link$deriv.fun$dF2dx)
   dcorr[, (NCOL(dcorr) - rho$ndim * max(lev) + 1):NCOL(dcorr)] <- dstddev
  }
}
  h_list[[it]] <- rho$weights * 1/pr * cbind(dtheta, dbeta, dcorr)
  }
  ## matrix containing the gradients for each subject
  Vi <- Reduce("+", h_list)
  ## variability matrix
  V <- crossprod(Vi)
  ## Hessian matrix
  H <- Reduce("+", lapply(h_list, crossprod))
  list(V=V, H = H)
}
#############################################################
###### neg loglikelihood component for each subject i #######
######                 for numeric gradient           #######
#############################################################
transf_par_i <- function(par, rho, i) {
  tparsigma <- par[rho$npar.thetas + rho$npar.betas + seq_len(attr(rho$error.structure, "npar"))]
  sigmas <- build_error_struct(rho$error.structure, tparsigma)
  if (is.null(dim(sigmas$sdVec))){ 
    sdi <- sigmas$sdVec
  } else {
    sdi <- sigmas$sdVec[i, ]
  }
  theta <- rho$transf_thresholds(par[seq_len(rho$npar.thetas)], rho)   #transform thresholds due to monotonicity
  par_beta <- par[rho$npar.thetas + seq(rho$npar.betas)]
  pred.fixedU  <- sapply(1:rho$ndim, function(j) {
     b <- lapply(seq_along(rho$constraints),function(p)
     rho$constraints[[p]][rho$inds.cat[[j]],,drop=F] %*%
     par_beta[rho$nbeta.first[p] + seq_len(rho$npar.beta[p]) - 1])
      (sum(rho$XcatU[[j]][i, ] * unlist(b)) + rho$offset[[j]])[i]
    })
  pred.fixedL  <- sapply(1:rho$ndim, function(j) {
     b <- lapply(seq_along(rho$constraints),function(p)
     rho$constraints[[p]][rho$inds.cat[[j]],,drop=F] %*%
     par_beta[rho$nbeta.first[p] + seq_len(rho$npar.beta[p]) - 1])
      sum(rho$XcatL[[j]][i, ] * unlist(b)) + rho$offset[[j]][i]
    })

 # beta <- lapply(seq_len(rho$ndim), function(j){
 #   sapply(seq_along(rho$coef.names), function(p) {
 #     ifelse(is.na(rho$coef.ind[[j]][,p]), 0, par_beta[rho$coef.ind[[j]][,p]]) #TODO: ?ifelse
 #   })
 # })
 # pred.fixed <- sapply(1:rho$ndim, function(j)
 #   (rowSums(rho$x[[j]] * beta[[j]]) + rho$offset[[j]])[i])
#  pred.fixed <- sapply(1:rho$ndim, function(j) rho$x[[j]][i, ] %*% beta[[j]])
  theta.lower <- sapply(1:rho$ndim, function(j) c(-rho$inf.value, theta[[j]])[rho$y[i, j]])
  theta.upper <- sapply(1:rho$ndim, function(j) c(theta[[j]], rho$inf.value)[rho$y[i, j]])
  pred.lower <- (theta.lower - pred.fixedL)/sdi
  pred.upper <- (theta.upper - pred.fixedU)/sdi
  list(U = pred.upper, L = pred.lower,
       corr_par = sigmas$rVec[i, , drop=F])
}
##############
neg_logPL_comp_i <- function(par, rho, i) {
  # transform parameters and get upper and lower bounds
  tmp <- transf_par_i(par, rho, i)
  U <- tmp$U; L <- tmp$L;
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
