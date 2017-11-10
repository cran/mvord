### R code from vignette source 'vignette_mvord.Rnw'

###################################################
### code chunk number 1: vignette_mvord.Rnw:79-80
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: vignette_mvord.Rnw:84-89
###################################################
library("mvord")
data("data_cr_panel")
data("data_cr_mvord")
data("data_cr_mvord2")
cache <- TRUE


###################################################
### code chunk number 3: vignette_mvord.Rnw:1170-1176
###################################################
n<-3
data <- cbind.data.frame("Y" = ordered(c("A", "B", "A")),
  X1 = rnorm(n), X2 = rnorm(n),
  X3 = as.factor(c("c1", "c2", "c3")),
  X4 = rnorm(n), X5 = rnorm(n))
data


###################################################
### code chunk number 4: vignette_mvord.Rnw:1180-1182
###################################################
formula <- Y ~ 1 + X1 : X2 + X3 + X4 + X3 * X5
names_constraints(formula, data)


###################################################
### code chunk number 5: vignette_mvord.Rnw:1185-1187
###################################################
formula <- Y ~ 0 + X1 : X2 + X3 + X4 + X3 * X5
names_constraints(formula, data)


###################################################
### code chunk number 6: vignette_mvord.Rnw:1559-1561
###################################################
head(data_cr_mvord, n = 3)
str(data_cr_mvord, vec.len = 3)


###################################################
### code chunk number 7: vignette_mvord.Rnw:1564-1566
###################################################
by(data_cr_mvord,  data_cr_mvord$rater_id,
    function(x) table(x$rating))


###################################################
### code chunk number 8: vignette_mvord.Rnw:1624-1628
###################################################
covar_names <- c("ICR", "LR", "LEV1", "LEV2", "PR", "lRSIZE", "lSYSR")
 data_cr_mvord_scaled <- do.call("rbind.data.frame",
   by(data_cr_mvord, data_cr_mvord$rater_id,
     function(x){x[, covar_names] <- scale(x[, covar_names]); x}))


###################################################
### code chunk number 9: vignette_mvord.Rnw:1631-1642 (eval = FALSE)
###################################################
## res_cor_logit <- mvord(
##   formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
##   data = data_cr_mvord_scaled, error.structure = cor_general(~ 1),
##   link = mvlogit(), index = c("firm_id", "rater_id"),
##   response.names = c("R1", "R2", "R3", "R4"),
##   response.levels = list(rev(LETTERS[1:6]), rev(LETTERS[1:6]),
##     rev(LETTERS[7:13]), rev(LETTERS[14:15])),
##   coef.constraints = cbind(c(1, NA, 1, NA), c(NA, NA, NA, 1),
##     c(1, 1, 1, NA), c(1, 2, 3, 4),  c(1, 1, 1, 4), c(1, 2, 3, 4),
##     c(NA, NA, NA, 1)), threshold.constraints = c(1, 1, 2, 3),
##   solver = "newuoa")


###################################################
### code chunk number 10: vignette_mvord.Rnw:1645-1676
###################################################
FILE <- "res_cor_logit.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
    res_cor_logit <- mvord(
      formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
      error.structure = cor_general(~ 1),
      link = mvlogit(),
      data = data_cr_mvord_scaled,
      index = c("firm_id", "rater_id"),
      response.names = c("R1", "R2", "R3", "R4"),
      response.levels = list(rev(LETTERS[1:6]),
                         rev(LETTERS[1:6]),
                         rev(LETTERS[7:13]),
                         rev(LETTERS[14:15])),
      coef.constraints = cbind(c(1, NA, 1, NA),
                               c(NA, NA, NA, 1),
                               c(1, 1, 1, NA),
                               c(1, 2, 3, 4),
                               c(1, 1, 1, 4),
                               c(1, 2, 3, 4),
                               c(NA, NA, NA, 1)),
      threshold.constraints = c(1, 1, 2, 3),
      solver = "newuoa")
  save(res_cor_logit, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }

}


###################################################
### code chunk number 11: vignette_mvord.Rnw:1680-1681
###################################################
summary(res_cor_logit, call = FALSE)


###################################################
### code chunk number 12: vignette_mvord.Rnw:1684-1685
###################################################
print(res_cor_logit, call = FALSE)


###################################################
### code chunk number 13: vignette_mvord.Rnw:1688-1689 (eval = FALSE)
###################################################
## summary(res_cor_logit, short = FALSE, call = FALSE)


###################################################
### code chunk number 14: vignette_mvord.Rnw:1692-1693
###################################################
thresholds(res_cor_logit)


###################################################
### code chunk number 15: vignette_mvord.Rnw:1696-1697
###################################################
coef(res_cor_logit)


###################################################
### code chunk number 16: vignette_mvord.Rnw:1700-1701
###################################################
get_error_struct(res_cor_logit)[["11"]]


###################################################
### code chunk number 17: vignette_mvord.Rnw:1711-1713
###################################################
head(data_cr_mvord2, n = 3)
str(data_cr_mvord2, vec.len = 2)


###################################################
### code chunk number 18: vignette_mvord.Rnw:1716-1717
###################################################
data_cr_mvord2[, covar_names] <- scale(data_cr_mvord2[, covar_names])


###################################################
### code chunk number 19: vignette_mvord.Rnw:1720-1727 (eval = FALSE)
###################################################
## res_cor_logit <- mvord2(
##   formula = cbind(R1, R2, R3, R4) ~ 0 + ICR + LR + LEV1 + LEV2 + PR +
##     lRSIZE + lSYSR, error.structure = cor_general(~ 1), link = mvlogit(),
##   data = data_cr_mvord2, coef.constraints = cbind(c(1, NA, 1, NA),
##     c(NA, NA, NA, 1), c(1, 1, 1, NA), c(1, 2, 3, 4), c(1, 1, 1, 4),
##     c(1, 2, 3, 4), c(NA, NA, NA, 1)),
##   threshold.constraints = c(1, 1, 2, 3))


###################################################
### code chunk number 20: vignette_mvord.Rnw:1734-1736
###################################################
str(data_cr_panel, vec.len = 3)
head(data_cr_panel, n = 3)


###################################################
### code chunk number 21: vignette_mvord.Rnw:1739-1740
###################################################
summary(rowSums(with(data_cr_panel, table(firm_id, year))))


###################################################
### code chunk number 22: vignette_mvord.Rnw:1743-1744
###################################################
with(data_cr_panel, table(year))


###################################################
### code chunk number 23: vignette_mvord.Rnw:1761-1762
###################################################
levels(data_cr_panel$rating)


###################################################
### code chunk number 24: vignette_mvord.Rnw:1787-1790
###################################################
data_cr_panel_scaled <- do.call("rbind.data.frame",
  by(data_cr_panel, data_cr_panel$year,
    function(x){x[, covar_names] <- scale(x[, covar_names]); x}))


###################################################
### code chunk number 25: vignette_mvord.Rnw:1793-1800 (eval = FALSE)
###################################################
## res_AR1_probit <- mvord(
##   formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
##   error.structure = cor_ar1(~ BSEC), link = mvprobit(),
##   index = c("firm_id", "year"), data = data_cr_panel_scaled,
##   response.levels = rep(list(levels(data_cr_panel$rating)), 8),
##   response.names = paste0("year", 3:10), coef.constraints = c(rep(1, 3),
##     rep(2, 5)), threshold.constraints = rep(1, 8), solver = "BFGS")


###################################################
### code chunk number 26: vignette_mvord.Rnw:1802-1823
###################################################
FILE <- "res_AR1_probit.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
    res_AR1_probit <- mvord(
      formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
      error.structure = cor_ar1(~ BSEC),
      link = mvprobit(),
      data = data_cr_panel_scaled,
      index = c("firm_id", "year"),
      response.names = paste0("year", 3:10),
      response.levels = rep(list(levels(data_cr_panel$rating)), 8),
      coef.constraints =  c(rep(1, 3),  rep(2, 5)),
      threshold.constraints = rep(1,8))
  save(res_AR1_probit, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }

}


###################################################
### code chunk number 27: vignette_mvord.Rnw:1827-1828
###################################################
summary(res_AR1_probit, short = TRUE, call = FALSE, digits = 6)


###################################################
### code chunk number 28: vignette_mvord.Rnw:1831-1832
###################################################
print(res_AR1_probit, call = FALSE, digits = 4)


###################################################
### code chunk number 29: vignette_mvord.Rnw:1835-1836 (eval = FALSE)
###################################################
## summary(res_AR1_probit, short = FALSE, call = FALSE)


###################################################
### code chunk number 30: vignette_mvord.Rnw:1839-1840
###################################################
thresholds(res_AR1_probit)


###################################################
### code chunk number 31: vignette_mvord.Rnw:1843-1844
###################################################
coef(res_AR1_probit)


###################################################
### code chunk number 32: vignette_mvord.Rnw:1847-1848
###################################################
get_error_struct(res_AR1_probit)


###################################################
### code chunk number 33: vignette_mvord.Rnw:1851-1852
###################################################
head(get_error_struct(res_AR1_probit, type = "corr"), n = 3)


###################################################
### code chunk number 34: vignette_mvord.Rnw:1855-1856
###################################################
head(get_error_struct(res_AR1_probit, type = "sigmas"), n = 1)


