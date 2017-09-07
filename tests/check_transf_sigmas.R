library(mvord)

#check z2r
mvord:::check(identical(mvord:::z2r(355),1))
mvord:::check(identical(mvord:::z2r(0),(exp(0)-1)/(1+exp(0))))
mvord:::check(identical(mvord:::z2r(2),(exp(4)-1)/(1+exp(4))))

rho <- list()
rho$ndim <- 5
rho$ncor.levels <- 1
rho$npar.cor <- 1
par.sigma <- 2

sigma <- diag(rho$ndim)
sigma[lower.tri(sigma)]   <- mvord:::z2r(par.sigma)^sequence((rho$ndim-1):1)
sigma <- sigma + t(sigma) - diag(diag(sigma))

rho$npar.cor <- length(par.sigma)
rho$error.structure$x <- c()
rho$error.structure$x <- matrix(rep(1,10), ncol = 1)

mvord:::check(identical(mvord:::transf.sigmas.corAR1(par.sigma, rho), lapply(1:10, function(i) sigma)))

# rho$ncor.levels <- 2
# rho$npar.cor <- 1
# par.sigma <- c(2,0)
#
# sigma <- diag(rho$ndim)
# sigma[lower.tri(sigma)]   <- z2r(par.sigma[1])^sequence((rho$ndim-1):1)
# sigma <- sigma + t(sigma) - diag(diag(sigma))
#
# sigma2 <- diag(rho$ndim)
# sigma2[lower.tri(sigma2)]   <- z2r(par.sigma[2])^sequence((rho$ndim-1):1)
# sigma2 <- sigma2 + t(sigma2) - diag(diag(sigma2))
#
# check(identical(transf.sigmas.corAR1(par.sigma, rho), list(sigma, sigma2)))



par.sigma <- c(1,2,-3)
rho$npar.cor <- length(par.sigma)
rho$error.structure$x <-NULL
rho$error.structure$x <- cbind(1:10,11:20,21:30)
mvord:::check(identical(mvord:::transf.sigmas.corEqui(par.sigma, rho), matrix(rep(-1,10), ncol = 1)))

par.sigma <- c(0.1,0.2,-0.3)
rho$npar.cor <- length(par.sigma)
rho$error.structure$x <- cbind(1:10,11:20,21:30)
mvord:::check(identical(mvord:::transf.sigmas.corEqui(par.sigma, rho), matrix(rep(mvord:::z2r(-4),10), ncol = 1)))



#transf.sigmas.spheric
rho <- list()
rho$ndim <- 3
rho$ncor.levels <- 1
rho$npar.cor <- 3
rho$num.sigmas <- 1
par.sigma <- c(0.5, 1, 2)
s <-  mvord:::transf.sigmas.spheric(par.sigma, rho)

mvord:::check(all.equal(mvord:::backtransf.sigmas(s[[1]]), par.sigma))


rho$ndim <- 5
rho$ncor.levels <- 1
rho$npar.cor <- 10
rho$num.sigmas <- 1
par.sigma <- c(rep(0.5,5),1:5)
s <-  mvord:::transf.sigmas.spheric(par.sigma, rho)

mvord:::check(all.equal(mvord:::backtransf.sigmas(s[[1]]), par.sigma))
