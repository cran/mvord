### R code from vignette source 'vignette_mvord.Rnw'

###################################################
### code chunk number 1: vignette_mvord.Rnw:55-56
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: vignette_mvord.Rnw:61-65
###################################################
library("mvord")
data("data_cr_panel")
data("data_cr")
cache <- TRUE


###################################################
### code chunk number 3: vignette_mvord.Rnw:813-815
###################################################
data("data_mvord_toy", package = "mvord")
str(data_mvord_toy)


###################################################
### code chunk number 4: vignette_mvord.Rnw:818-822
###################################################
data_toy_long <- cbind.data.frame(i = rep(1:100,2),
  j = rep(1:2,each = 100), Y = c(data_mvord_toy$Y1, data_mvord_toy$Y2),
  X1 = rep(data_mvord_toy$X1, 2), X2 = rep(data_mvord_toy$X2, 2),
  f1 = rep(data_mvord_toy$f1, 2), f2 = rep(data_mvord_toy$f2, 2))


###################################################
### code chunk number 5: vignette_mvord.Rnw:830-831
###################################################
str(data_toy_long)


###################################################
### code chunk number 6: vignette_mvord.Rnw:836-837 (eval = FALSE)
###################################################
## res <- mvord(formula = MMO(Y, i, j) ~ 0 + X1 + X2, data = data_toy_long)


###################################################
### code chunk number 7: vignette_mvord.Rnw:839-851
###################################################
FILE <- "res1.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
res <- mvord(formula = MMO(Y, i, j) ~ 0 + X1 + X2, data = data_toy_long)
res <- mvord:::reduce_size.mvord(res)
  save(res, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}


###################################################
### code chunk number 8: vignette_mvord.Rnw:950-951 (eval = FALSE)
###################################################
## res <- mvord(formula = MMO2(Y1, Y2) ~ 0 + X1 + X2, data = data_mvord_toy)


###################################################
### code chunk number 9: vignette_mvord.Rnw:953-965
###################################################
FILE <- "res2.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
res <- mvord(formula = MMO2(Y1, Y2) ~ 0 + X1 + X2, data = data_mvord_toy)
res <- mvord:::reduce_size2.mvord(res)
  save(res, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}


###################################################
### code chunk number 10: vignette_mvord.Rnw:1328-1329
###################################################
names_constraints(formula = Y ~ 0 + X1 + X2 + f2, data = data_mvord_toy)


###################################################
### code chunk number 11: vignette_mvord.Rnw:1352-1354
###################################################
formula <- MMO2(Y1, Y2) ~ 1 + X1 : X2 + f1 + f2 * X1
names_constraints(formula, data = data_mvord_toy)


###################################################
### code chunk number 12: predict_prob
###################################################
predict(res, subjectID =  1:6)


###################################################
### code chunk number 13: predict_cumprob
###################################################
predict(res, type = "cum.prob", subjectID =  1:6)


###################################################
### code chunk number 14: predict_class
###################################################
predict(res, type = "class", subjectID =  1:6)


###################################################
### code chunk number 15: vignette_mvord.Rnw:1576-1579
###################################################
data("data_cr", package = "mvord")
head(data_cr, n = 3)
str(data_cr, vec.len = 2.9)


###################################################
### code chunk number 16: plot_data
###################################################
op <- par(mfrow = c(1,4),
          oma = c(0,1.1,0,0),
          mar = c(2,3,5,1))
cexf <- 1.8
barplot(table(data_cr$rater1),  ylim = c(0, 500), las=1, main="rater1",
        cex.lab = cexf, cex.names = cexf, cex.main = 2, cex.axis = cexf, col=rgb(0.2,0.4,0.6,0.6))
barplot(table(data_cr$rater2), ylim = c(0, 500),las=1, main="rater2",
        cex.lab = cexf, cex.names = cexf, cex.main = 2, cex.axis = cexf, col=rgb(0.2,0.4,0.6,0.6))
barplot(table(data_cr$rater3), ylim = c(0, 500),las=1,main="rater3",
        cex.lab = cexf, cex.names = cexf, cex.main = 2, cex.axis = cexf, col=rgb(0.2,0.4,0.6,0.6))
barplot(table(data_cr$rater4), las=1, ylim = c(0, 500), main="rater4",
        cex.lab = cexf, cex.names = cexf, cex.main = 2, cex.axis = cexf, col=rgb(0.2,0.4,0.6,0.6))
par(op)


###################################################
### code chunk number 17: vignette_mvord.Rnw:1617-1619 (eval = FALSE)
###################################################
## res_cor_probit_simple <- mvord(formula = MMO2(rater1, rater2, rater3,
##   rater4) ~ 0 + LR + LEV + PR + RSIZE + BETA, data = data_cr)


###################################################
### code chunk number 18: res_cor_probit_simple
###################################################
FILE <- "res_cor_probit_simple.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
res_cor_probit_simple <- mvord(
    formula =
    MMO2(rater1, rater2, rater3, rater4) ~ 0 + LR + LEV + PR + RSIZE + BETA, data = data_cr)
res_cor_probit_simple <- mvord:::reduce_size.mvord(res_cor_probit_simple)
  save(res_cor_probit_simple, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}


###################################################
### code chunk number 19: summary_res_cor_probit_simple
###################################################
summary(res_cor_probit_simple, call = FALSE)


###################################################
### code chunk number 20: vignette_mvord.Rnw:1667-1668
###################################################
thresholds(res_cor_probit_simple)


###################################################
### code chunk number 21: vignette_mvord.Rnw:1671-1672
###################################################
coef(res_cor_probit_simple)


###################################################
### code chunk number 22: vignette_mvord.Rnw:1676-1677
###################################################
error_structure(res_cor_probit_simple)[[11]]


###################################################
### code chunk number 23: vignette_mvord.Rnw:1712-1717 (eval = FALSE)
###################################################
## res_cor_logit <- mvord(formula = MMO2(rater1, rater2, rater3, rater4) ~
##   0 + LR + LEV + PR + RSIZE + BETA, data = data_cr, link = mvlogit(),
##   coef.constraints = cbind(LR = c(1, 1, 1, 1), LEV = c(1, 2, 3, 4),
##     PR = c(1, 1, 1, 1), RSIZE = c(1, 1, 1, 2), BETA = c(1, 1, 2, 3)),
##   threshold.constraints = c(1, 1, 2, 3))


###################################################
### code chunk number 24: vignette_mvord.Rnw:1720-1741
###################################################
FILE <- "res_cor_logit.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
res_cor_logit <- mvord(formula = MMO2(rater1, rater2, rater3, rater4) ~
    0 + LR + LEV + PR + RSIZE + BETA, data = data_cr, link = mvlogit(),
  coef.constraints = cbind(
    c(1,1,1,1),
    c(1,2,3,4),
    c(1,1,1,1),
    c(1,1,1,2),
    c(1,1,2,3)),
    threshold.constraints = c(1, 1, 2, 3))
res_cor_logit <- mvord:::reduce_size2.mvord(res_cor_logit)
  save(res_cor_logit, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }

}


###################################################
### code chunk number 25: vignette_mvord.Rnw:1746-1747
###################################################
summary(res_cor_logit, call = FALSE)


###################################################
### code chunk number 26: vignette_mvord.Rnw:1770-1771
###################################################
constraints(res_cor_logit)$BETA


###################################################
### code chunk number 27: plot_fit1
###################################################
cols <- rev(colorspace::sequential_hcl(round(200),
                                   h = 260, c = c(80, 0),
                                   l = c(30, 90),
                                   power = 0.7))
 scatterplot.mvord <- function(tab,
                               zlim = NULL,
                               col = cols,
                               xlab = NULL, ylab = NULL, main = NULL,
                               percent = FALSE,
                               col.one.to.one.line=grey(0.4),
                               col.bar.legend=TRUE,
                               ...){
  if(percent == "all") tab <- tab/sum(tab)*100
  if(percent == "row") tab <- sweep(tab, 1, rowSums(tab), "/")
  if(percent == "col") tab <- sweep(tab, 2, colSums(tab), "/")

  if(percent %in% c("all", "row", "col")){  zlim = c(0,100)
    tab <- round(tab*100,4)
  }

  tab[tab==0] <- NA

  if (is.null(zlim))  zlim <- range(tab, na.rm=T)

    plot.seq.x <- seq_len(nrow(tab))
    plot.seq.y <- seq_len(ncol(tab))
    labels.x <- rownames(tab)
    labels.y <- colnames(tab)

    if(is.null(xlab)) xlab <- ""
    if(is.null(ylab)) ylab <- ""

  image(x=plot.seq.x, y=plot.seq.y, z=tab, zlim=zlim, col=col,
        xlab= "", ylab= "",
        main=main, axes = FALSE,
        xlim = c(min(plot.seq.x) - 1,max(plot.seq.x) + 1),
        ylim = c(min(plot.seq.y) - 1,max(plot.seq.y) + 1), ...)
  axis(1, at = plot.seq.x, line = 0.5, labels = labels.x)
  axis(2, at = plot.seq.y, line = 0.5, labels = labels.y, las = 1)
  title(xlab= xlab)
  title(ylab= ylab, line = 4)

  if (!is.null(col.one.to.one.line))
    segments(min(plot.seq.x) - 0.5, min(plot.seq.y) - 0.5,
          max(plot.seq.x) + 0.5, max(plot.seq.y) + 0.5, lty = 3,
          col=col.one.to.one.line)

  starting.par.settings <- par(no.readonly = TRUE)
    mai <- par("mai")
    fin <- par("fin")
    x.legend.fig <- c(1 - (mai[4]/fin[1]), 1)
    y.legend.fig <- c(mai[1]/fin[2], 1 - (mai[3]/fin[2]))
    x.legend.plt <- c(x.legend.fig[1] + (0.08 * (x.legend.fig[2] -
        x.legend.fig[1])), x.legend.fig[2] - (0.6 * (x.legend.fig[2] -
        x.legend.fig[1])))
    y.legend.plt <- y.legend.fig
    cut.pts <- seq(zlim[1], zlim[2], length = length(col) + 1)
    z <- (cut.pts[1:length(col)] + cut.pts[2:(length(col) + 1)])/2
    par(new = TRUE, pty = "m", plt = c(x.legend.plt, y.legend.plt))
    image(x = 1, y = z, z = matrix(z, nrow = 1, ncol = length(col)),
        col = col, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    axis(4, mgp = c(3, 0.2, 0), las = 2, cex.axis = 0.8, tcl = -0.1)
    box()
    mfg.settings <- par()$mfg
    par(starting.par.settings)
    par(mfg = mfg.settings, new = FALSE)
}

op <- par(mfrow = c(2,2),
          oma = c(1,1,0,0),
          mar = c(4,5,2,3))
op <- par(mfrow = c(2,2),
          oma = c(0,0,0,0),
          mar = c(4,5,2,3))
scatterplot.mvord(
    table(res_cor_logit$rho$y[,1], marginal_predict(res_cor_logit, type = "class")[,1]),
                  main = "rater 1", ylab = "predicted", xlab = "observed", percent = "row")#row of table
scatterplot.mvord(
    table(res_cor_logit$rho$y[,2], marginal_predict(res_cor_logit, type = "class")[,2]),
                  main = "rater 2", ylab = "predicted", xlab = "observed", percent = "row")
scatterplot.mvord(
    table(res_cor_logit$rho$y[,3], marginal_predict(res_cor_logit, type = "class")[,3]),
                  main = "rater 3", ylab = "predicted", xlab = "observed", percent = "row")
scatterplot.mvord(
    table(res_cor_logit$rho$y[,4], marginal_predict(res_cor_logit, type = "class")[,4]),
                  main = "rater 4", ylab = "predicted", xlab = "observed", percent = "row")
par(op)


###################################################
### code chunk number 28: vignette_mvord.Rnw:1884-1893
###################################################
BICvec <- c(BIC(res_cor_probit_simple), BIC(res_cor_logit))
AICvec <- c(AIC(res_cor_probit_simple), AIC(res_cor_logit))
loglikvec <- c(logLik(res_cor_probit_simple), logLik(res_cor_logit))
tab <- cbind(loglikvec, BICvec, AICvec)
colnames(tab) <- c("logLik", "BIC", "AIC")
rownames(tab) <- c("Example 1", "Example 2")
print(xtable::xtable(tab),
   only.contents = TRUE, math.style.negative = TRUE,
   include.colnames = FALSE)


###################################################
### code chunk number 29: vignette_mvord.Rnw:1906-1909
###################################################
data("data_cr_panel")
str(data_cr_panel, vec.len = 3)
head(data_cr_panel, n = 3)


###################################################
### code chunk number 30: vignette_mvord.Rnw:1963-1968 (eval = FALSE)
###################################################
## res_AR1_probit <- mvord(formula = MMO(rating, firm_id, year) ~ LR + LEV +
##   PR + RSIZE + BETA, error.structure = cor_ar1(~ BSEC), link = mvprobit(),
##   data = data_cr_panel, coef.constraints = c(rep(1, 4), rep(2, 4)),
##   threshold.constraints = rep(1, 8), threshold.values = rep(list(c(0, NA,
##     NA, NA)),8), control = mvord.control(solver = "BFGS"))


###################################################
### code chunk number 31: vignette_mvord.Rnw:1970-1991
###################################################
FILE <- "res_AR1_probit.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
res_AR1_probit <- mvord(
  formula = MMO(rating, firm_id, year) ~ LR + LEV + PR + RSIZE +  BETA,
  data = data_cr_panel,
  error.structure = cor_ar1(~ BSEC),
  coef.constraints = c(rep(1,4), rep(2,4)),
  threshold.constraints = c(rep(1,8)),
  threshold.values = rep(list(c(0,NA,NA,NA)),8),
  link = mvprobit(),
  control = mvord.control(solver = "BFGS",  solver.optimx.control = list(trace = TRUE)))
   res_AR1_probit <- mvord:::reduce_size.mvord(res_AR1_probit)
  save(res_AR1_probit, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }

}


###################################################
### code chunk number 32: vignette_mvord.Rnw:1997-1998
###################################################
summary(res_AR1_probit, short = TRUE, call = FALSE)


###################################################
### code chunk number 33: vignette_mvord.Rnw:2004-2005
###################################################
error_structure(res_AR1_probit)


###################################################
### code chunk number 34: vignette_mvord.Rnw:2008-2009
###################################################
head(error_structure(res_AR1_probit, type = "corr"), n = 3)


###################################################
### code chunk number 35: vignette_mvord.Rnw:2012-2013
###################################################
head(error_structure(res_AR1_probit, type = "sigmas"), n = 1)


