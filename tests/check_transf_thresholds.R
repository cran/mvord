library(mvord)
rho <- list()
rho$ndim <- 5
rho$threshold.values <- list(c(NA),
                            c(NA,NA,NA),
                            c(NA,NA,NA),
                            c(NA,NA,NA,NA,NA),
                            c(NA,NA))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$ind.thresholds <- list(1,2:4,5:7,8:12,13:14)

theta <- c(1,0,1,2,1,1,1,-5:-1, -10,-3)

mvord:::check(all.equal(mvord:::transf.thresholds.flexible(theta, rho), list(1,
                                                             c(0, exp(1), exp(1) + exp(2)),
                                                             c(1,1 + exp(1), 1+ exp(1) + exp(1)),
                                                             c(-5, -5 + exp(-4), -5 + exp(-4) + exp(-3), -5 + exp(-4) + exp(-3) +exp(-2), -5 + exp(-4) + exp(-3) + exp(-2) + exp(-1)),
                                                             c(-10, -10 + exp(-3)))))

rho$threshold.values <- list(1,
                             c(2,NA,NA),
                             c(3,NA,NA),
                             c(4,NA,NA,NA,NA),
                             c(-9,NA))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ind.thresholds <- list(c(),1:2,3:4,5:8,9)
rho$threshold.values.fixed <- lapply(1:rho$ndim, function(j){ind <- !is.na(rho$threshold.values[[j]])
rho$threshold.values[[j]][ind]})

theta <- c(1,0,1,2,1,1,-3,-10, -5)

mvord:::check(all.equal(mvord:::transf.thresholds.fix1.first(theta, rho), list(1,
                                                               c(2, 2 + exp(1), 2 + exp(1) + exp(0)),
                                                               c(3, 3 + exp(1), 3 + exp(1)+ exp(2)),
                                                               c(4, 4+ exp(1), 4+ exp(1)+ exp(1), 4+ exp(1)+ exp(1)+ exp(-3),4+ exp(1)+ exp(1)+ exp(-3)+ exp(-10)),
                                                               c(-9, -9 + exp(-5)))))


rho$threshold.values <- list(1,
                             c(2,NA,4),
                             c(3,NA,4),
                             c(4,NA,NA,NA,10),
                             c(-9,-6))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ind.thresholds <- list(c(),1,2,3:5,c())
rho$threshold.values.fixed <- lapply(1:rho$ndim, function(j){ind <- !is.na(rho$threshold.values[[j]])
rho$threshold.values[[j]][ind]})

gamma <- c(1,0,1,2,-10)

rho$first.ind.theta <- c(NA,1,2,3,NA)

#TODO
# check(all.equal(transf.thresholds.fix2.firstlast(gamma, rho), list(1,
#                                                                    c(2, (exp(1) + 1 )/exp(1) , 4),
#                                                                    c(3, , 4),
#                                                                    c(4, , , ,10),
#                                                                    c(-9, -6))))



rho$threshold.values <- list(1,
                             c(2,4,NA),
                             c(3,4,NA),
                             c(4,10,NA,NA,NA),
                             c(-9,-6))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ind.thresholds <- list(c(),1,2,3:5,c())
rho$threshold.values.fixed <- lapply(1:rho$ndim, function(j){ind <- !is.na(rho$threshold.values[[j]])
rho$threshold.values[[j]][ind]})

rho$first.ind.theta <- c(NA,1,2,3,NA)

gamma <- c(1,0,1,2,-10)

mvord:::check(all.equal(mvord:::transf.thresholds.fix2.first(gamma, rho), list(1,
                                                               c(2, 4, 4 + exp(1)),
                                                               c(3, 4, 4 + exp(0)),
                                                               c(4,10, 10 + exp(1), 10 + exp(1) + exp(2),10 + exp(1) + exp(2) + exp(-10)),
                                                               c(-9, -6))))
