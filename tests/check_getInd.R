library(mvord)
rho <- list()
rho$ndim <- 5

##getInd.coef
coef.constraints <- cbind(1:5,rep(1,5), c(1,NA,1,NA,NA), c(rep(2,4),3))
coef.values <- matrix(NA, nrow = rho$ndim, ncol = 4)
coef.values[is.na(coef.constraints)] <- 0

mvord:::check(identical(mvord:::getInd.coef(coef.constraints, coef.values), rbind(1:4,c(5,2,NA,4),c(6,2,3,4),c(7,2,NA,4),c(8,2,NA,9))))

##getInd.thresholds.flexible
threshold.constraints <- c(1,2,2,3,4)
rho$threshold.values <- list(NA,
                             c(NA,NA,NA),
                             c(NA,NA,NA),
                             c(NA,NA,NA,NA,NA),
                             c(NA,NA))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ncat <- sapply(rho$threshold.values, length) + 1

mvord:::check(all.equal(mvord:::getInd.thresholds.flexible(threshold.constraints,rho), list(1,2:4,2:4,5:9,10:11)))
threshold.constraints <- c(1,2,3,4,5)
mvord:::check(all.equal(mvord:::getInd.thresholds.flexible(threshold.constraints,rho), list(1,2:4,5:7,8:12,13:14)))

threshold.constraints <- c(1,2,2,3,4)
rho$threshold.values <- list(NA,
                             c(NA,NA,NA),
                             c(NA,NA,NA),
                             c(NA,NA,NA,NA,NA),
                             c(1,2))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ncat <- sapply(rho$threshold.values, length) + 1

mvord:::check(all.equal(mvord:::getInd.thresholds.flexible(threshold.constraints,rho), list(1,2:4,2:4,5:9,integer(0))))

##getInd.thresholds.fix2
threshold.constraints <- c(1,2,2,3,4)
rho$threshold.values <- list(1,
                             c(1,2,NA),
                             c(1,2,NA),
                             c(1,2,NA,NA,NA),
                             c(1,2))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ncat <- sapply(rho$threshold.values, length) + 1

mvord:::check(all.equal(mvord:::getInd.thresholds.fix2(threshold.constraints,rho), list(integer(0),1,1,2:4, integer(0))))
threshold.constraints <- c(1,2,3,4,5)
mvord:::check(all.equal(mvord:::getInd.thresholds.fix2(threshold.constraints,rho), list(integer(0),1,2,3:5, integer(0))))

threshold.constraints <- c(1,2,2,3,4)
rho$threshold.values <- list(1,
                             c(1,NA,2),
                             c(1,NA,2),
                             c(1,NA,NA,NA,2),
                             c(1,2))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ncat <- sapply(rho$threshold.values, length) + 1

mvord:::check(all.equal(mvord:::getInd.thresholds.fix2(threshold.constraints,rho), list(integer(0),1,1,2:4, integer(0))))
threshold.constraints <- c(1,2,3,4,5)
mvord:::check(all.equal(mvord:::getInd.thresholds.fix2(threshold.constraints,rho), list(integer(0),1,2,3:5, integer(0))))
threshold.constraints <- c(1,2,2,4,5)
mvord:::check(all.equal(mvord:::getInd.thresholds.fix2(threshold.constraints,rho), list(integer(0),1,1,2:4, integer(0))))



##getInd.thresholds.fix1
threshold.constraints <- c(1,2,2,3,4)
rho$threshold.values <- list(1,
                             c(1,NA,NA),
                             c(1,NA,NA),
                             c(1,NA,NA,NA,NA),
                             c(1,NA))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ncat <- sapply(rho$threshold.values, length) + 1

mvord:::check(all.equal(mvord:::getInd.thresholds.fix1(threshold.constraints,rho), list(integer(0),1:2,1:2,3:6, 7)))
threshold.constraints <- c(1,2,3,4,5)
mvord:::check(all.equal(mvord:::getInd.thresholds.fix1(threshold.constraints,rho), list(integer(0),1:2,3:4,5:8,9)))

threshold.constraints <- c(1,2,2,3,5)
rho$threshold.values <- list(1,
                             c(1,NA,NA),
                             c(1,NA,NA),
                             c(1,NA,NA,NA,NA),
                             c(1,NA))
rho$ntheta <- sapply(rho$threshold.values, length)
rho$npar.theta <- sapply(1:rho$ndim, function(j) sum(is.na(rho$threshold.values[[j]])))
rho$ncat <- sapply(rho$threshold.values, length) + 1

mvord:::check(all.equal(mvord:::getInd.thresholds.fix1(threshold.constraints,rho), list(integer(0),1:2,1:2,3:6, 7)))

