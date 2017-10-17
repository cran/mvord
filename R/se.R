##########################################################
###### PL Standard Errors - no parallel    ###############
##########################################################

PL_se <- function(rho){
  ## analytic
  cat("Computing variability and hessian matrix analytically ... \n")
  derivs_for_se <- derivs_ana(rho)
  rho$H.inv <- solve(derivs_for_se$H)
  rho$V <- rho$n/(rho$n - NCOL(derivs_for_se$V)) * derivs_for_se$V  ## see maop code, correct for degrees of freedom
  rho$varGamma <- rho$H.inv %*% rho$V %*% rho$H.inv ## inverse godambe
  rho$seGamma <- sqrt(diag(rho$varGamma))
  rho$claic <- 2 * rho$objective + 2 * sum(diag(rho$V %*% rho$H.inv))
  rho$clbic <- 2 * rho$objective + log(rho$n) * sum(diag(rho$V %*% rho$H.inv))
  rho
}

deriv_biv_norm <- function(A, B, r){
  dnorm(A) * pnorm((B - r * A)/sqrt(1 - r^2))
}

deriv_corr_norm <- function(A, B, r){
  1/(2 * pi * sqrt(1 - r^2)) *
    exp(-(A^2 - 2 * r * A * B + B^2)/(2 * (1 - r^2)))
}

deriv_biv_t <- function(A, B, r, df){
  mu_c <- r * A
  sigma_c <- sqrt((df + A^2)/(df + 1) * (1 - r^2))
  df_c <- df + 1
  dt(A, df = df) * pt((B - mu_c)/sigma_c, df = df_c)
}

deriv_corr_t <- function(A, B, r, df){
  1/(2 * pi * sqrt(1 - r^2)) *
    (1 + (A^2 - 2 * r * A * B + B^2)/(df * (1 - r^2)))^(- df/2)
}

deriv_biv_t_copula <- function(A, B, r, df, inf.value){
  newA <- qt(plogis(A), df = df)
  newB <- qt(plogis(B), df = df)
  newA[newA > inf.value] <- inf.value
  newA[newA < - inf.value] <- - inf.value
  newB[newB > inf.value] <- inf.value
  newB[newB < -inf.value] <- - inf.value
  mu_c <- r * newA
  sigma_c <- sqrt((df + newA^2)/(df + 1) * (1 - r^2))
  df_c <- df + 1
  dlogis(A) * pt((newB - mu_c)/sigma_c, df = df_c)
}

deriv_corr_t_copula <- function(A, B, r, df, inf.value){
  ## add transformation
  newA <- qt(plogis(A), df = df)
  newB <- qt(plogis(B), df = df)
  newA[newA > inf.value] <- inf.value
  newA[newA < - inf.value] <- - inf.value
  newB[newB > inf.value] <- inf.value
  newB[newB < - inf.value] <- - inf.value
  1/(2 * pi * sqrt(1 - r^2)) *
    (1 + (newA^2 - 2 * r * newA * newB + newB^2)/(df * (1 - r^2)))^(- df/2)
}

deriv_corr_rect <- function(U_kl, L_kl, r, deriv_corr_fun) {
  -  (deriv_corr_fun(A = U_kl[, 1], B = U_kl[, 2], r) -
      deriv_corr_fun(A = U_kl[, 1], B = L_kl[, 2], r) -
      deriv_corr_fun(A = L_kl[, 1], B = U_kl[, 2], r) +
      deriv_corr_fun(A = L_kl[, 1], B = L_kl[, 2], r))
}

deriv_theta_rect <- function(k, l,
                             U, L, r,
                             Umatk, Lmatk,
                             deriv_biv_fun, sdfack) {
    ## derivatives of the rectangle probabilities wrt to thresholds
    UU <-  deriv_biv_fun(U[, k], U[, l], r)
    UL <-  deriv_biv_fun(U[, k], L[, l], r)
    LU <-  deriv_biv_fun(L[, k], U[, l], r)
    LL <-  deriv_biv_fun(L[, k], L[, l], r)

    - 1/sdfack * (UU * Umatk - UL * Umatk - LU * Lmatk + LL * Lmatk)
}
deriv_beta_rect <- function(k, l,
                            U, L, r,
                            Xmatk,
                            deriv_biv_fun, sdfack) {
    ## derivatives of the rectangle probabilities wrt to thresholds
    UU <-  deriv_biv_fun(U[, k], U[, l], r)
    UL <-  deriv_biv_fun(U[, k], L[, l], r)
    LU <-  deriv_biv_fun(L[, k], U[, l], r)
    LL <-  deriv_biv_fun(L[, k], L[, l], r)
    1/sdfack * (UU - UL - LU + LL) * Xmatk
}
deriv_stddev_rect <- function(k, l, U, L, r, deriv_biv_fun){
    UU <- deriv_biv_fun(U[, k], U[, l], r)
    UL <- deriv_biv_fun(U[, k], L[, l], r)
    LU <- deriv_biv_fun(L[, k], U[, l], r)
    LL <- deriv_biv_fun(L[, k], L[, l], r)
    (UU * U[, k] - UL * U[, k] - LU  * L[, k] + LL * L[, k])
}

set_deriv_functions <- function(rho) {
  rho$pfun <- switch(rho$link$name,
                     mvprobit = pnorm,
                     mvlogit  = plogis) # function for univariate probabilities
  rho$bivpfun <- switch(rho$link$name,
                     mvprobit = function(U, L, r) rectbiv.norm.prob(U, L, r),
                     mvlogit  = function(U, L, r) {
                       U <- qt(plogis(U), df = rho$link$df)
                       L <- qt(plogis(L), df = rho$link$df)
                       U[U > rho$inf.value] <- rho$inf.value
                       L[L < -rho$inf.value] <- - rho$inf.value
                       sapply(seq_len(nrow(U)), function(i)
                          biv.nt.prob2(df = rho$link$df,
                                       lower = L[i, ],
                                       upper = U[i, ],
                                       r     = r[i]))}) # function for bivariate probabilities

  rho$dfun <- switch(rho$link$name,
                     mvprobit = dnorm,
                     mvlogit  = dlogis) # univariate density
  rho$deriv_biv_fun <- switch(rho$link$name,
                              mvprobit = deriv_biv_norm,
                              mvlogit  = function(A, B, r) deriv_biv_t_copula(A, B, r,
                                                                              df = rho$link$df,
                                                                              inf.value = rho$inf.value))
  rho$deriv_corr_fun <- switch(rho$link$name,
                               mvprobit = deriv_corr_norm,
                               mvlogit  = function(A, B, r) deriv_corr_t_copula(A, B, r,
                                                                                df = rho$link$df,
                                                                                inf.value = rho$inf.value))
  rho
}

derivs_ana <- function(rho){
  ## function for analytic gradient and hessian
  rho <- set_deriv_functions(rho)
  par <- rho$optpar

  tmp <- rho$transf.par(par, rho)
  U <- tmp$U
  L <- tmp$L
  sigmas <- tmp$sigmas;
  ## prepare contrast matrices for threshold parameters
  rho$B2 <- lapply(1:rho$ndim, function(j)
    1 * (col(matrix(0, rho$n, rho$ntheta[j] + 1)) ==
           c(unclass(rho$y[, j]))))
  rho$B1 <- lapply(1:rho$ndim, function(i) as.matrix(rho$B2[[i]][,-(rho$ntheta[i] + 1), drop = FALSE]))
  rho$B2 <- lapply(1:rho$ndim, function(i) as.matrix(rho$B2[[i]][,-1, drop = FALSE]))

  if(! rho$error.structure$type %in% c("corEqui", "corAR1")){
     lev <- match(rho$error.structure$x, rho$error.structure$levels)
  } else {
    lev <- rep(1, rho$n)
  }

  if (rho$error.structure$type == "covGeneral") {
    std.dev <- tmp$std.dev;
  } else {
    std.dev <- rep(list(rep(1, rho$ndim)), rho$ncor.levels)
  }
  std.dev.mat <- do.call("rbind",  std.dev[lev])

  npar.beta.opt <- apply(rho$ind.coef, 1, function(x) sum(!is.na(x)))
  pick.col.beta <- lapply(seq_len(nrow(rho$ind.coef)), function(i)
    which(!is.na(rho$ind.coef[i, ])))

  contrast <- as.matrix((!is.na(rho$y)) + 0)
  contrast_uni <- unique(unlist(apply(contrast, 1, function(x) {
      k <- which(x == 1) * (sum(x) == 1)
      k[k != 0]
  })))

  ## First take care of the univariate case (q_i = 1)
  if (length(contrast_uni) > 0) {
   h_list <- lapply(contrast_uni, function(k) {
    indk <-  !is.na(rho$y[, k]) & (rowSums(!is.na(rho$y)) == 1)
    pr <- rep(1, rho$n)
    pr[indk] <- rho$pfun(U[indk, k]) - rho$pfun(L[indk, k])
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
         (rho$dfun(U[indk, k]) * rho$B1[[k]][indk, pick.col.theta] -
          rho$dfun(L[indk, k]) * rho$B2[[k]][indk, pick.col.theta])
    } else dtheta <- NULL

    # dbeta
    dbeta <- matrix(0, nrow = rho$n, ncol = rho$npar.betas)
    first.ind.beta <- apply(getInd.coef(rho$coef.constraints, rho$coef.values), 1, min, na.rm  = T)
    colposk <- first.ind.beta[k]  + seq_len(npar.beta.opt[k]) - 1
    dbeta[indk, colposk] <-  1/std.dev.mat[indk, k] *
      (rho$dfun(U[indk, k]) - rho$dfun(L[indk, k])) * rho$x[[k]][indk, pick.col.beta[[k]]]
    # dcorr -- no correlation for univ case
    dcorr <- matrix(0, nrow = rho$n, ncol = rho$npar.cor * rho$ncor.levels)

    # stddev
    if (rho$error.structure$type == "covGeneral"){
      dstddev <- matrix(0, nrow = rho$n, ncol = rho$npar.cor.sd * rho$ncor.levels)
      arr.ind.k <- cbind(which(indk), (lev - 1) * rho$npar.cor.sd + k)
      dstddev[arr.ind.k] <-  1/std.dev.mat[indk, k] *
             (rho$dfun(U[indk, k]) * U[indk, k] -
              rho$dfun(L[indk, k]) * L[indk, k])
    } else dstddev <- NULL

    rho$weights * 1/pr * cbind(dtheta, dbeta, dcorr, dstddev)
  })
  } else {
    h_list <- NULL
  }
  ## then the bivariates
  combis <- combn(rho$ndim, 2, simplify = F)
  combis <- combis[sapply(combis, function(x) x[2] - x[1] <= rho$PL.lag)]
   #####################################
   ## take each possibly pair (k, l)
  ######################################
  it0 <- length(h_list)
  for (it in (it0 + seq_len(length(combis)))) {
     comb <- combis[[it - it0]]
     k <- comb[1]
     l <- comb[2]
     ## for each pair make an indicator for each subject where the pair applies
     indkl <-  (rowSums(!is.na(rho$y[, comb])) == 2)


     ## pr_{kl}
    r <- switch(rho$error.structure$type,
              corGeneral = sapply(sigmas[lev[indkl]],'[', comb[1], comb[2]),
              covGeneral = sapply(sigmas[lev[indkl]],'[', comb[1], comb[2]),
              corEqui    = sigmas[indkl, 1],
              corAR1     = sapply(sigmas[indkl],'[', comb[1], comb[2]))

    pr <- rep(1, rho$n)
    pr[indkl] <- rho$bivpfun(U = U[indkl, comb, drop = F],
                             L = L[indkl, comb, drop = F],
                             r = r)
    pr[pr < .Machine$double.eps] <- .Machine$double.eps

   ## vector h_kl will contain the gradient for all d log p_{kl}/d pars
   ##
   ## dtheta
   if (sum(rho$npar.theta.opt) > 0) {
   dtheta <- matrix(0, ncol = rho$npar.thetas,
                       nrow = rho$n)
   rho$deriv_theta_rect_kl <- function(k, l) {
     pick.col.theta <- switch(rho$threshold,
                          flexible      = seq_len(rho$ntheta[k]),
                          fix1first     = seq_len(rho$ntheta[k])[-1],
                          fix2first     = seq_len(rho$ntheta[k])[-c(1,2)],
                          fix2firstlast = seq_len(rho$ntheta[k] - 1)[-1])
     if (length(pick.col.theta) > 0) {
     return(deriv_theta_rect(k, l,
                      U = U[indkl, , drop = F], L = L[indkl, ,drop = F ],
                      r = r,
                      Umatk = rho$B1[[k]][indkl, pick.col.theta, drop = F],
                      Lmatk = rho$B2[[k]][indkl, pick.col.theta, drop = F],
                      deriv_biv_fun = rho$deriv_biv_fun,
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
      dth <- rho$deriv_theta_rect_kl(indj, comb[comb != indj])
      } else {
      if (length(indj) == 2) {
        dth <- rho$deriv_theta_rect_kl(k, l) +  rho$deriv_theta_rect_kl(l, k)
      } else {
        dth <- matrix(0, nrow = sum(indkl),
          ncol = rho$npar.theta.opt[indjj[1]])
      }
     }
     return(dth)
   }))
   } else dtheta <- NULL
   ## dbeta
   dbeta <- matrix(0, ncol = rho$npar.betas,
                            nrow = rho$n)

   ## for which covariates are there betas to be estimated
   active_p <- which(apply(rho$ind.coef, 2, function(x) !all(is.na(x))))

   rho$deriv_beta_rect_klp <- function(k, l, p) {
     deriv_beta_rect(k, l,
                      U = U[indkl, ], L = L[indkl, ], r = r,
                      Xmatk = rho$x[[k]][indkl, p],
                      deriv_biv_fun = rho$deriv_biv_fun,
                      sdfack = std.dev.mat[indkl, k])
   }
   for (p in active_p) {
     dbeta[indkl, unique(rho$ind.coef[!is.na(rho$ind.coef[, p]), p])] <-
     do.call("cbind", lapply(unique(rho$coef.constraints[!is.na(rho$coef.constraints[,p]),p]), function(j){
     indjj <- (1:rho$ndim)[rho$coef.constraints[, p] == j]
     indj <- indjj[indjj %in% comb]
      if (length(indj) == 0) {
      db <- rep(0, sum(indkl))
     }
     if (length(indj) == 2) {
      db <-  rho$deriv_beta_rect_klp(k, l, p) + rho$deriv_beta_rect_klp(l, k, p)
     }
     if (length(indj) == 1) db <- rho$deriv_beta_rect_klp(indj, comb[comb!=indj], p)
     return(db)
   }))

   }
  ## dcorr
  dcorr <- matrix(0, ncol = rho$ncor.levels * rho$npar.cor,
                            nrow = rho$n)
  if (rho$error.structure$type == "corAR1") {
    rpowinvlag <- r^(1/abs(comb[1] - comb[2]))
    dLdr <- abs(comb[1] - comb[2]) * rpowinvlag^(abs(comb[1] - comb[2]) - 1) *
                  deriv_corr_rect(U_kl = U[indkl, comb, drop = F],
                                  L_kl = L[indkl, comb, drop = F],
                                  r = r,
                                  deriv_corr_fun = rho$deriv_corr_fun)

    xbeta <- 0.5 * (log(1 + rpowinvlag) - log(1 - rpowinvlag))
    dcorr[indkl, ] <- dLdr * exp(2 * xbeta)/(exp(2 * xbeta) + 1)^2  * 4 * rho$error.structure$x[indkl, ]
  } else {
    if (rho$error.structure$type == "corEqui") {
      dLdr <- deriv_corr_rect(U_kl = U[indkl, comb],
                              L_kl = L[indkl, comb],
                              r = r,
                              deriv_corr_fun = rho$deriv_corr_fun)
      xbeta <- 0.5 * (log(1 + r) - log(1 - r))
      dcorr[indkl, ] <- dLdr * exp(2 * xbeta)/(exp(2 * xbeta) + 1)^2  * 4 * rho$error.structure$x[indkl, ]
    } else {
      poslev <- which(sapply(combn(rho$ndim, 2, simplify=F), function(x) all(x==comb)))

      arr.ind <- cbind(which(indkl), (lev[indkl]-1) * rho$npar.cor + poslev)
      dcorr[arr.ind] <- deriv_corr_rect(U_kl = U[indkl, comb, drop = F],
                                        L_kl = L[indkl, comb, drop = F],
                                        r = r,
                                        deriv_corr_fun = rho$deriv_corr_fun)
    }
  }
  ## dstddev
  if (rho$error.structure$type == "covGeneral") {
    dstddev <- matrix(0, nrow = rho$n, ncol = rho$npar.cor.sd * rho$ncor.levels)
    poslevk <- (lev[indkl] - 1) * rho$npar.cor.sd + k
    poslevl <- (lev[indkl] - 1) * rho$npar.cor.sd + l
    arr.ind.k <- cbind(which(indkl), poslevk)
    arr.ind.l <- cbind(which(indkl), poslevl)
    dstddev[arr.ind.k] <- 1/std.dev.mat[indkl, k] *
      deriv_stddev_rect(k, l,
                         U = U[indkl, , drop = F],
                         L = L[indkl, , drop = F], r,
                         deriv_biv_fun = rho$deriv_biv_fun)

    dstddev[arr.ind.l] <-  1/std.dev.mat[indkl, l] *
       deriv_stddev_rect(l, k,
                         U = U[indkl, , drop = F],
                         L = L[indkl, , drop = F], r,
                         deriv_biv_fun = rho$deriv_biv_fun)

  } else dstddev <- NULL
  h_list[[it]] <- rho$weights * 1/pr * cbind(dtheta, dbeta, dcorr, dstddev)
  }
  ## matrix containing the gradients for each subject
  Vi <- Reduce("+", h_list)
  ## variability matrix
  V <- crossprod(Vi)
  ## Hessian matrix
  H <- Reduce("+", lapply(h_list, crossprod))
  list(V = V, H = H)
}
