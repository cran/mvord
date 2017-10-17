### R code from vignette source 'vignette_mvord.Rnw'

###################################################
### code chunk number 1: vignette_mvord.Rnw:77-78
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: vignette_mvord.Rnw:82-87
###################################################
library("mvord")
data("data_cr_panel")
data("data_cr_mvord")
data("data_cr_mvord2")
cache <- TRUE


###################################################
### code chunk number 3: vignette_mvord.Rnw:675-693 (eval = FALSE)
###################################################
## mvord(formula,
##       data,
##       error.structure = corGeneral(~ 1),
##       link = mvprobit(),
##       index = NULL,
##       response.names = NULL,
##       response.levels = NULL,
##       threshold.constraints = NULL,
##       threshold.values = NULL,
##       coef.constraints = NULL,
##       coef.values = NULL,
##       weights = NULL,
##       se = TRUE,
##       start.values = NULL,
##       solver = "BFGS",
##       PL.lag = NULL,
##       control = list(maxit=200000, trace = 1, kkt = FALSE)
## )


###################################################
### code chunk number 4: vignette_mvord.Rnw:709-711
###################################################
index <- c("subject_index", "multiple_measurement_index")
index


###################################################
### code chunk number 5: vignette_mvord.Rnw:724-726
###################################################
response.names <- c("Y1", "Y2", "Y3", "Y4")
response.names


###################################################
### code chunk number 6: vignette_mvord.Rnw:729-734
###################################################
response.levels <-  list(Y1 = rev(LETTERS[1:6]),
                         Y2 = rev(LETTERS[1:6]),
                         Y3 = rev(LETTERS[7:13]),
                         Y4 = c("O", "N"))
response.levels


###################################################
### code chunk number 7: vignette_mvord.Rnw:746-747
###################################################
formula = Y ~ 0 + X1 + ... + Xp


###################################################
### code chunk number 8: vignette_mvord.Rnw:753-754
###################################################
formula = Y ~ 1 + X1 + ... + Xp


###################################################
### code chunk number 9: vignette_mvord.Rnw:757-758
###################################################
formula = Y ~ X1 + ... + Xp


###################################################
### code chunk number 10: vignette_mvord.Rnw:768-769
###################################################
link = mvprobit()


###################################################
### code chunk number 11: vignette_mvord.Rnw:774-775
###################################################
link = mvlogit(df = 8L)


###################################################
### code chunk number 12: vignette_mvord.Rnw:806-807
###################################################
error.structure = corGeneral(~ 1)


###################################################
### code chunk number 13: vignette_mvord.Rnw:811-812
###################################################
error.structure = corEqui(~ 1)


###################################################
### code chunk number 14: vignette_mvord.Rnw:816-817
###################################################
error.structure = corAR1(~ 1)


###################################################
### code chunk number 15: vignette_mvord.Rnw:829-830
###################################################
error.structure = covGeneral(~ 1)


###################################################
### code chunk number 16: vignette_mvord.Rnw:844-845
###################################################
error.structure = corGeneral(~ f)


###################################################
### code chunk number 17: vignette_mvord.Rnw:849-850
###################################################
error.structure = corEqui(~ S1 + ... + Sm)


###################################################
### code chunk number 18: vignette_mvord.Rnw:854-855
###################################################
error.structure =  corAR1(~ S1 + ... + Sm)


###################################################
### code chunk number 19: vignette_mvord.Rnw:865-866
###################################################
error.structure = covGeneral(~ f)


###################################################
### code chunk number 20: vignette_mvord.Rnw:893-896
###################################################
threshold.constraints <- c(1, 1, 2, 3)
names(threshold.constraints) <- paste0("Y", 1:4)
threshold.constraints


###################################################
### code chunk number 21: vignette_mvord.Rnw:970-976
###################################################
threshold.values <- list(c(-4, NA, NA, NA, NA, NA),
                         c(-4, NA, NA, NA, NA, NA),
                         c(-5, NA, NA, NA, NA, NA, NA),
                         c(0))
names(threshold.values) <- paste0("Y", 1:4)
threshold.values


###################################################
### code chunk number 22: vignette_mvord.Rnw:997-1000
###################################################
coef.constraints <- c(1, 1, 2, 3)
names(coef.constraints) <- paste0("Y", 1:4)
coef.constraints


###################################################
### code chunk number 23: vignette_mvord.Rnw:1019-1029
###################################################
coef.constraints <- cbind(c(1, 2, 3, 4),
                          c(1, 1, 1, 2),
                          c(NA, NA, NA, 1),
                          c(1, 1, 1, NA),
                          c(NA, NA, NA, NA),
                          c(1, 2, 3, 4),
                          c(1, 2, 3, 4))
rownames(coef.constraints) <- paste0("Y", 1:4)
colnames(coef.constraints) <- paste0("X", 1:7)
coef.constraints


###################################################
### code chunk number 24: vignette_mvord.Rnw:1049-1059
###################################################
coef.values <- cbind(c(NA, NA, NA, NA),
                     c(NA, NA, NA, NA),
                     c(0, 0, 0, NA),
                     c(NA, NA, NA, 0),
                     c(2, 2, 2, 2),
                     c(NA, NA, NA, NA),
                     c(NA, NA, NA, NA))
rownames(coef.values) <- paste0("Y", 1:4)
colnames(coef.values) <- paste0("X", 1:7)
coef.values


###################################################
### code chunk number 25: vignette_mvord.Rnw:1076-1077
###################################################
data_x <- cbind.data.frame("Y" = "A", X1 = 1, X2 = 2, X3 = 3, X4 =4, X5 = 5, X6 = 6, X7 = 7)


###################################################
### code chunk number 26: vignette_mvord.Rnw:1079-1081
###################################################
formula <- Y ~ 0 + X1 : X2 + X3 + X4 + X5 + X6 * X7
colnames(model.matrix(formula, data = data_x))


###################################################
### code chunk number 27: vignette_mvord.Rnw:1102-1107 (eval = FALSE)
###################################################
## solver = function(starting.values, objFun, control){
##   optRes <- solver.function(...)
##   list(optpar = optRes$optpar,
##        objvalue = optRes$objvalue)
## }


###################################################
### code chunk number 28: vignette_mvord.Rnw:1127-1134
###################################################
start.values = list(theta = list(c(-3, -1, 0, 0.5, 2.5),
                                 c(-3, -1, 0, 0.5, 2, 3.5),
                                 c(0)),
                    beta = list(c(0.05, -0.05, -0.8, 1, 0.2),
                                c(-0.5, 0.2),
                                c(-0.3, 0.3),
                                c(0.5, -1.1, 0.7, 0.3, -1.2)))


###################################################
### code chunk number 29: vignette_mvord.Rnw:1190-1204 (eval = FALSE)
###################################################
## mvord2(formula,
##        data,
##        error.structure = corGeneral(~ 1),
##        link = mvprobit(),
##        coef.constraints = NULL,
##        coef.values = NULL,
##        threshold.constraints = NULL,
##        threshold.values = NULL,
##        weights = NULL,
##        se = TRUE,
##        start.values = NULL,
##        solver = "BFGS",
##        PL.lag = NULL,
##        control = list(maxit = 200000, trace = 1, kkt = FALSE))


###################################################
### code chunk number 30: vignette_mvord.Rnw:1212-1213
###################################################
formula <- cbind(Y1, ..., Yq) ~ 0 + X1 + ... + Xp


###################################################
### code chunk number 31: vignette_mvord.Rnw:1423-1425
###################################################
head(data_cr_mvord, n = 3)
str(data_cr_mvord, vec.len = 3)


###################################################
### code chunk number 32: vignette_mvord.Rnw:1428-1430
###################################################
by(data_cr_mvord,  data_cr_mvord$rater_id,
    function(x) table(x$rating))


###################################################
### code chunk number 33: vignette_mvord.Rnw:1433-1435
###################################################
formula <- rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR
formula


###################################################
### code chunk number 34: vignette_mvord.Rnw:1438-1440
###################################################
index <- c("firm_id", "rater_id")
index


###################################################
### code chunk number 35: vignette_mvord.Rnw:1443-1445
###################################################
response.names <- c("R1", "R2", "R3", "R4")
response.names


###################################################
### code chunk number 36: vignette_mvord.Rnw:1448-1454
###################################################
response.levels <-  list(rev(LETTERS[1:6]),
                         rev(LETTERS[1:6]),
                         rev(LETTERS[7:13]),
                         rev(LETTERS[14:15]))
names(response.levels) <- response.names
response.levels


###################################################
### code chunk number 37: vignette_mvord.Rnw:1462-1465
###################################################
 threshold.constraints <- c(1, 1, 2, 3)
 names(threshold.constraints) <- response.names
 threshold.constraints


###################################################
### code chunk number 38: vignette_mvord.Rnw:1468-1479
###################################################
coef.constraints <- cbind(c(1, NA, 1, NA),
                          c(NA, NA, NA, 1),
                          c(1, 1, 1, NA),
                          c(1, 2, 3, 4),
                          c(1, 1, 1, 4),
                          c(1, 2, 3, 4),
                          c(NA, NA, NA, 1))
rownames(coef.constraints) <- response.names
colnames(coef.constraints) <- c("ICR", "LR", "LEV1", "LEV2",
                           "PR", "lRSIZE", "lSYSR")
coef.constraints


###################################################
### code chunk number 39: vignette_mvord.Rnw:1482-1493
###################################################
coef.values <- cbind(c(NA, 0, NA, 0),
                     c(0, 0, 0, NA),
                     c(NA, NA, NA, 0),
                     c(NA, NA, NA, NA),
                     c(NA, NA, NA, NA),
                     c(NA, NA, NA, NA),
                     c(0, 0, 0, NA))
rownames(coef.values) <- response.names
colnames(coef.values) <- c("ICR", "LR", "LEV1", "LEV2",
                           "PR", "lRSIZE", "lSYSR")
coef.values


###################################################
### code chunk number 40: vignette_mvord.Rnw:1505-1506
###################################################
link <- mvlogit()


###################################################
### code chunk number 41: vignette_mvord.Rnw:1509-1511
###################################################
error.structure <- corGeneral(~ 1)
error.structure


###################################################
### code chunk number 42: vignette_mvord.Rnw:1514-1518
###################################################
covar_names <- c("ICR", "LR", "LEV1", "LEV2", "PR", "lRSIZE", "lSYSR")
 data_cr_mvord_scaled <- do.call("rbind.data.frame",
   by(data_cr_mvord, data_cr_mvord$rater_id,
     function(x){x[, covar_names] <- scale(x[, covar_names]); x}))


###################################################
### code chunk number 43: vignette_mvord.Rnw:1521-1541 (eval = FALSE)
###################################################
## res_cor_logit <- mvord(
##   formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
##   data = data_cr_mvord_scaled,
##   error.structure = corGeneral(~ 1),
##   link = mvlogit(),
##   index = c("firm_id", "rater_id"),
##   response.names = c("R1", "R2", "R3", "R4"),
##   response.levels = list(rev(LETTERS[1:6]),
##                          rev(LETTERS[1:6]),
##                          rev(LETTERS[7:13]),
##                          rev(LETTERS[14:15])),
##   coef.constraints = cbind(c(1, NA, 1, NA),
##                            c(NA, NA, NA, 1),
##                            c(1, 1, 1, NA),
##                            c(1, 2, 3, 4),
##                            c(1, 1, 1, 4),
##                            c(1, 2, 3, 4),
##                            c(NA, NA, NA, 1)),
##   threshold.constraints = c(1, 1, 2, 3),
##   solver = "newuoa")


###################################################
### code chunk number 44: vignette_mvord.Rnw:1544-1575
###################################################
FILE <- "res_cor_logit.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
    res_cor_logit <- mvord(
      formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
      error.structure = corGeneral(~ 1),
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
### code chunk number 45: vignette_mvord.Rnw:1579-1580
###################################################
summary(res_cor_logit, call = FALSE)


###################################################
### code chunk number 46: vignette_mvord.Rnw:1583-1584
###################################################
print(res_cor_logit, call = FALSE)


###################################################
### code chunk number 47: vignette_mvord.Rnw:1587-1588 (eval = FALSE)
###################################################
## summary(res_cor_logit, short = FALSE, call = FALSE)


###################################################
### code chunk number 48: vignette_mvord.Rnw:1591-1592
###################################################
thresholds(res_cor_logit)


###################################################
### code chunk number 49: vignette_mvord.Rnw:1595-1596
###################################################
coef(res_cor_logit)


###################################################
### code chunk number 50: vignette_mvord.Rnw:1599-1600
###################################################
get.error.struct(res_cor_logit)


###################################################
### code chunk number 51: vignette_mvord.Rnw:1610-1612
###################################################
head(data_cr_mvord2, n = 3)
str(data_cr_mvord2, vec.len = 2)


###################################################
### code chunk number 52: vignette_mvord.Rnw:1615-1616
###################################################
data_cr_mvord2[, covar_names] <- scale(data_cr_mvord2[, covar_names])


###################################################
### code chunk number 53: vignette_mvord.Rnw:1619-1633 (eval = FALSE)
###################################################
## res_cor_logit <- mvord2(
##   formula = cbind(R1, R2, R3, R4) ~ 0 + ICR + LR + LEV1 + LEV2 + PR +
##                                         lRSIZE + lSYSR,
##   error.structure = corGeneral(~ 1),
##   link = mvlogit(),
##   data = data_cr_mvord_scaled,
##   coef.constraints = cbind(c(1, NA, 1, NA),
##                            c(NA, NA, NA, 1),
##                            c(1, 1, 1, NA),
##                            c(1, 2, 3, 4),
##                            c(1, 1, 1, 4),
##                            c(1, 2, 3, 4),
##                            c(NA, NA, NA, 1)),
##   threshold.constraints = c(1, 1, 2, 3))


###################################################
### code chunk number 54: vignette_mvord.Rnw:1640-1642
###################################################
str(data_cr_panel, vec.len = 3)
head(data_cr_panel, n = 3)


###################################################
### code chunk number 55: vignette_mvord.Rnw:1645-1646
###################################################
summary(rowSums(with(data_cr_panel, table(firm_id, year))))


###################################################
### code chunk number 56: vignette_mvord.Rnw:1649-1650
###################################################
with(data_cr_panel, table(year))


###################################################
### code chunk number 57: vignette_mvord.Rnw:1654-1656
###################################################
formula <- rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR
formula


###################################################
### code chunk number 58: vignette_mvord.Rnw:1659-1661
###################################################
index <- c("firm_id", "year")
index


###################################################
### code chunk number 59: vignette_mvord.Rnw:1665-1667
###################################################
response.names <- paste0("year", 3:10)
response.names


###################################################
### code chunk number 60: vignette_mvord.Rnw:1670-1671
###################################################
levels(data_cr_panel$rating)


###################################################
### code chunk number 61: vignette_mvord.Rnw:1674-1678
###################################################
response.levels <- rep(list(levels(data_cr_panel$rating)),
                       length(response.names))
names(response.levels) <- response.names
response.levels


###################################################
### code chunk number 62: vignette_mvord.Rnw:1683-1686
###################################################
threshold.constraints <- rep(1, length(response.names))
names(threshold.constraints) <- response.names
threshold.constraints


###################################################
### code chunk number 63: vignette_mvord.Rnw:1691-1694
###################################################
coef.constraints <- c(rep(1, 3),  rep(2, 5))
names(coef.constraints) <- response.names
coef.constraints


###################################################
### code chunk number 64: vignette_mvord.Rnw:1697-1699
###################################################
error.structure <- corAR1(~ BSEC)
error.structure


###################################################
### code chunk number 65: vignette_mvord.Rnw:1703-1706
###################################################
data_cr_panel_scaled <- do.call("rbind.data.frame",
  by(data_cr_panel, data_cr_panel$year,
    function(x){x[, covar_names] <- scale(x[, covar_names]); x}))


###################################################
### code chunk number 66: vignette_mvord.Rnw:1709-1720 (eval = FALSE)
###################################################
## res_AR1_probit <- mvord(
##     formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
##     index = c("firm_id", "year"),
##     data = data_cr_panel_scaled,
##     response.levels = rep(list(levels(data_cr_panel$rating)), 8),
##     response.names = paste0("year", 3:10),
##     link = mvprobit(),
##     error.structure = corAR1(~ BSEC),
##     coef.constraints = c(rep(1, 3),  rep(2, 5)),
##     threshold.constraints = rep(1, 8),
##     solver = "BFGS")


###################################################
### code chunk number 67: vignette_mvord.Rnw:1722-1743
###################################################
FILE <- "res_AR1_probit.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
    res_AR1_probit <- mvord(
      formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
      error.structure = corAR1(~ BSEC),
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
### code chunk number 68: vignette_mvord.Rnw:1747-1748
###################################################
summary(res_AR1_probit, short = TRUE, call = FALSE, digits = 6)


###################################################
### code chunk number 69: vignette_mvord.Rnw:1751-1752
###################################################
print(res_AR1_probit, call = FALSE, digits = 4)


###################################################
### code chunk number 70: vignette_mvord.Rnw:1755-1756 (eval = FALSE)
###################################################
## summary(res_AR1_probit, short = FALSE, call = FALSE)


###################################################
### code chunk number 71: vignette_mvord.Rnw:1759-1760
###################################################
thresholds(res_AR1_probit)


###################################################
### code chunk number 72: vignette_mvord.Rnw:1763-1764
###################################################
coef(res_AR1_probit)


###################################################
### code chunk number 73: vignette_mvord.Rnw:1767-1768
###################################################
get.error.struct(res_AR1_probit)


###################################################
### code chunk number 74: vignette_mvord.Rnw:1771-1772
###################################################
head(get.error.struct(res_AR1_probit, type = "corr"), n = 3)


###################################################
### code chunk number 75: vignette_mvord.Rnw:1775-1776
###################################################
head(get.error.struct(res_AR1_probit, type = "sigmas"), n = 1)


