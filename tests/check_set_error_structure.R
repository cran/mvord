library(mvord)

df <- cbind.data.frame(1:10,11:20,21:30,as.factor(c(rep(1,4),rep(2,3),rep(3,3))))
names(df) <- c("X1", "X2", "X3","sec")
ndim <- 5
data.x <- lapply(seq_len(ndim), function(j) df)

## --------------- CHECK set.error.structure ---------------------------------------------
#corGeneral(~1)
mvord:::check(identical(mvord:::set.error.structure(corGeneral(~1), data.x, ndim)$x, factor(rep(1,10))))
#corGeneral(~sec)
mvord:::check(identical(mvord:::set.error.structure(corGeneral(~sec), data.x, ndim)$x, factor(c(rep(1,4),rep(2,3),rep(3,3)))))
#covGeneral(~1)
mvord:::check(identical(mvord:::set.error.structure(covGeneral(~1), data.x, ndim)$x, factor(rep(1,10))))
#covGeneral(~sec)
mvord:::check(identical(mvord:::set.error.structure(covGeneral(~sec), data.x, ndim)$x, factor(c(rep(1,4),rep(2,3),rep(3,3)))))
#equicorrelation(~1)
tmp <- matrix(1, nrow = 10)
rownames(tmp) <- 1:10
colnames(tmp) <- "(Intercept)"
attr(tmp, "assign") <- 0
mvord:::check(all.equal(mvord:::set.error.structure(corEqui(~1), data.x, ndim)$x, tmp))
#equicorrelation(~X1)
tmp <- matrix(c(rep(1,10),1:10), nrow = 10, ncol = 2)
rownames(tmp) <- 1:10
colnames(tmp) <- c("(Intercept)", "X1")
attr(tmp, "assign") <- c(0,1)
mvord:::check(all.equal(mvord:::set.error.structure(corEqui(~X1), data.x, ndim)$x, tmp))
#equicorrelation(~X1+X3)
tmp <- matrix(c(rep(1,10),1:10, 21:30), nrow = 10, ncol = 3)
rownames(tmp) <- 1:10
colnames(tmp) <- c("(Intercept)", "X1", "X3")
attr(tmp, "assign") <- c(0,1,2)
mvord:::check(all.equal(mvord:::set.error.structure(corEqui(~X1 + X3), data.x, ndim)$x, tmp))
#AR1(~1)
tmp <- matrix(1, nrow = 10)
rownames(tmp) <- 1:10
colnames(tmp) <- "(Intercept)"
attr(tmp, "assign") <- 0
mvord:::check(all.equal(mvord:::set.error.structure(corAR1(~1), data.x, ndim)$x, tmp))
#corGeneral(~sec)
tmp <- matrix(c(rep(1,10),c(0,0,0,0,1,1,1,0,0,0), c(0,0,0,0,0,0,0,1,1,1)), nrow = 10, ncol = 3)
rownames(tmp) <- 1:10
colnames(tmp) <- c("(Intercept)", "sec2", "sec3")
attr(tmp, "assign") <- c(0,1,1)
mvord:::check(all.equal(mvord:::set.error.structure(corAR1(~sec), data.x, ndim)$x, tmp))

#------------------------------------------------------------------------------------
df <- cbind.data.frame(1:10,11:20,21:30,as.factor(c(rep("Z",4),rep("X",3),rep("Y",3))))
names(df) <- c("X1", "X2", "X3","sec")
ndim <- 5
data.x <- lapply(seq_len(ndim), function(j) df)

#corGeneral(~sec)
mvord:::check(identical(mvord:::set.error.structure(corGeneral(~sec), data.x, ndim)$x, factor(c(rep("Z",4),rep("X",3),rep("Y",3)))))

