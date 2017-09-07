### R code from vignette source 'vignette_mvord.Rnw'

###################################################
### code chunk number 1: vignette_mvord.Rnw:79-85
###################################################
options(width=65, prompt = "R> ", continue = "+  ", useFancyQuotes = FALSE)
cache <- TRUE
library(mvord)
data(data_cr_panel)
data(data_cr_multord)
data(data_cr_multord2)


###################################################
### code chunk number 2: vignette_mvord.Rnw:561-578 (eval = FALSE)
###################################################
## multord(formula,
##         error.structure = corGeneral(~1),
##         link = c("probit", "logit"),
##         data,
##         index = NULL,
##         response.names = NULL,
##         response.levels = NULL,
##         coef.constraints = NULL,
##         coef.values = NULL,
##         threshold.constraints = NULL,
##         threshold.values = NULL,
##         weights = NULL,
##         se = TRUE,
##         start.values = NULL,
##         solver = "BFGS",
##         control = list(maxit=200000, trace = 1, kkt = FALSE)
## )


###################################################
### code chunk number 3: vignette_mvord.Rnw:589-591
###################################################
head(data_cr_multord, n = 3)
str(data_cr_multord, vec.len = 3)


###################################################
### code chunk number 4: vignette_mvord.Rnw:610-612
###################################################
index <- c("firm_id", "rater_id")
index


###################################################
### code chunk number 5: vignette_mvord.Rnw:623-625
###################################################
response.names <- c("R1", "R2", "R3", "R4")
response.names


###################################################
### code chunk number 6: vignette_mvord.Rnw:634-640
###################################################
response.levels <-  list(rev(LETTERS[1:6]),
                         rev(LETTERS[1:6]),
                         rev(LETTERS[7:13]),
                         rev(LETTERS[14:15]))
names(response.levels) <- response.names
response.levels


###################################################
### code chunk number 7: vignette_mvord.Rnw:677-678
###################################################
formula <- rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR


###################################################
### code chunk number 8: vignette_mvord.Rnw:684-685
###################################################
formula <- rating ~ 1 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR


###################################################
### code chunk number 9: vignette_mvord.Rnw:688-689
###################################################
formula <- rating ~ ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR


###################################################
### code chunk number 10: vignette_mvord.Rnw:708-709
###################################################
error.structure <- corGeneral(~ 1)


###################################################
### code chunk number 11: vignette_mvord.Rnw:712-713
###################################################
error.structure = corGeneral(~ f)


###################################################
### code chunk number 12: vignette_mvord.Rnw:719-720
###################################################
error.structure <- corEqui(~ S1 + ... + Sm)


###################################################
### code chunk number 13: vignette_mvord.Rnw:726-727
###################################################
error.structure = corAR1(~ 1)


###################################################
### code chunk number 14: vignette_mvord.Rnw:730-731
###################################################
error.structure = corAR1(~ S1 + ... + Sm)


###################################################
### code chunk number 15: vignette_mvord.Rnw:738-739
###################################################
error.structure = covGeneral(~ 1)


###################################################
### code chunk number 16: vignette_mvord.Rnw:742-743
###################################################
error.structure = covGeneral(~ f)


###################################################
### code chunk number 17: vignette_mvord.Rnw:800-803
###################################################
threshold.constraints <- c(1, 1, 2, 3)
names(threshold.constraints) <- response.names
threshold.constraints


###################################################
### code chunk number 18: vignette_mvord.Rnw:880-886
###################################################
threshold.values <- list(c(-4, NA, NA, NA, NA, NA),
                         c(-4, NA, NA, NA, NA, NA),
                         c(-5, NA, NA, NA, NA, NA, NA),
                         c(0))
names(threshold.values) <- response.names
threshold.values


###################################################
### code chunk number 19: vignette_mvord.Rnw:944-947
###################################################
coef.constraints <- c(1, 1, 2, 3)
names(coef.constraints) <- response.names
coef.constraints


###################################################
### code chunk number 20: vignette_mvord.Rnw:974-985
###################################################
coef.constraints = cbind(c(1, NA, 1, NA),
                         c(NA, NA, NA, 1),
                         c(1, 1, 1, NA),
                         c(1, 2, 3, 4),
                         c(1, 1, 1, 4),
                         c(1, 2, 3, 4),
                         c(NA, NA, NA, 1))
rownames(coef.constraints) <- response.names
colnames(coef.constraints) <- c("ICR", "LR", "LEV1", "LEV2", "PR",
                                "lRSIZE", "lSYSR")
coef.constraints


###################################################
### code chunk number 21: vignette_mvord.Rnw:1027-1038
###################################################
coef.values <- cbind(c(NA, NA, NA, NA),
                     c(NA, NA, NA, NA),
                     c(0, 0, 0, NA),
                     c(NA, NA, NA, 0),
                     c(2, 2, 2, 2),
                     c(NA, NA, NA, NA),
                     c(NA, NA, NA, NA))
rownames(coef.values) <- response.names
colnames(coef.values) <- c("ICR", "LR", "LEV1", "LEV2", "PR",
                           "lRSIZE", "lSYSR")
coef.values


###################################################
### code chunk number 22: vignette_mvord.Rnw:1048-1050
###################################################
formula <- rating ~ 0 + ICR : LR + LEV1 + LEV2 + PR + lRSIZE * lSYSR
colnames(model.matrix(formula, data = data_cr_multord))


###################################################
### code chunk number 23: vignette_mvord.Rnw:1080-1085 (eval = FALSE)
###################################################
## solver = function(starting.values, objFun, control){
##   optRes <- solver.function(...)
##   list(optpar = , optRes$optpar, # a vector of length equal to number of parameters to optimize
##        objvalue = optRes$objvalue) # value of objective function
## }


###################################################
### code chunk number 24: vignette_mvord.Rnw:1094-1101
###################################################
start.values = list(theta = list(c(-3,-1,0,0.5,2.5),
                                 c(-3,-1,0,0.5,2,3.5),
                                 c(0)),
                    beta = list(c(0.05,-0.05,-0.8,1,0.2),
                                c(-0.5,0.2),
                                c(-0.3,0.3),
                                c(0.5,-1.1,0.7,0.3,-1.2)))


###################################################
### code chunk number 25: vignette_mvord.Rnw:1145-1158 (eval = FALSE)
###################################################
## multord2(formula,
##          error.structure = corGeneral(~1),
##          data,
##          link = c("probit", "logit"),
##          coef.constraints = NULL,
##          coef.values = NULL,
##          threshold.constraints = NULL,
##          threshold.values = NULL,
##          weights = NULL,
##          se = TRUE,
##          start.values = NULL,
##          solver = "BFGS",
##          control = list(maxit = 200000, trace = 1, kkt = FALSE))


###################################################
### code chunk number 26: vignette_mvord.Rnw:1172-1173
###################################################
head(data_cr_multord2, n = 3)


###################################################
### code chunk number 27: vignette_mvord.Rnw:1177-1178
###################################################
formula <- cbind(R1, R2, R3) ~ 0 + X1 + ... + Xp


###################################################
### code chunk number 28: vignette_mvord.Rnw:1215-1217
###################################################
head(data_cr_multord, n = 3)
str(data_cr_multord, vec.len = 3)


###################################################
### code chunk number 29: vignette_mvord.Rnw:1220-1222
###################################################
nfirms <- length(unique(data_cr_multord$firm_id))
table(data_cr_multord$rater_id)/nfirms


###################################################
### code chunk number 30: vignette_mvord.Rnw:1224-1225
###################################################
tab <- round(table(data_cr_multord$rater_id)/nfirms * 100)


###################################################
### code chunk number 31: vignette_mvord.Rnw:1231-1233
###################################################
by(data_cr_multord,  data_cr_multord$rater_id,
    function(x) table(x$rating))


###################################################
### code chunk number 32: vignette_mvord.Rnw:1237-1239
###################################################
formula <- rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR
formula


###################################################
### code chunk number 33: vignette_mvord.Rnw:1242-1244
###################################################
index <- c("firm_id", "rater_id")
index


###################################################
### code chunk number 34: vignette_mvord.Rnw:1247-1249
###################################################
response.names <- c("R1", "R2", "R3", "R4")
response.names


###################################################
### code chunk number 35: vignette_mvord.Rnw:1252-1258
###################################################
response.levels <-  list(rev(LETTERS[1:6]),
                         rev(LETTERS[1:6]),
                         rev(LETTERS[7:13]),
                         rev(LETTERS[14:15]))
names(response.levels) <- response.names
response.levels


###################################################
### code chunk number 36: vignette_mvord.Rnw:1266-1269
###################################################
 threshold.constraints <- c(1,1,2,3)
 names(threshold.constraints) <- response.names
 threshold.constraints


###################################################
### code chunk number 37: vignette_mvord.Rnw:1272-1283
###################################################
coef.constraints = cbind(c(1,NA,1,NA),
                         c(NA,NA,NA,1),
                         c(1,1,1,NA),
                         c(1,2,3,4),
                         c(1,1,1,4),
                         c(1,2,3,4),
                         c(NA,NA,NA,1))
rownames(coef.constraints) <- response.names
colnames(coef.constraints) <- c("ICR", "LR", "LEV1", "LEV2",
                           "PR", "lRSIZE", "lSYSR")
coef.constraints


###################################################
### code chunk number 38: vignette_mvord.Rnw:1286-1297
###################################################
coef.values <- cbind(c(NA, 0, NA, 0),
                     c(0, 0, 0, NA),
                     c(NA, NA, NA, NA),
                     c(NA, NA, NA, 0),
                     c(NA, NA, NA, NA),
                     c(NA, NA, NA, NA),
                     c(0, 0, 0, NA))
rownames(coef.values) <- response.names
colnames(coef.values) <- c("ICR", "LR", "LEV1", "LEV2",
                           "PR", "lRSIZE", "lSYSR")
coef.values


###################################################
### code chunk number 39: vignette_mvord.Rnw:1310-1311
###################################################
link <- "logit"


###################################################
### code chunk number 40: vignette_mvord.Rnw:1314-1316
###################################################
error.structure <- corGeneral(~ 1)
error.structure


###################################################
### code chunk number 41: vignette_mvord.Rnw:1333-1337
###################################################
covar_names <- c("ICR", "LR","LEV1","LEV2", "PR","lRSIZE","lSYSR")
data_cr_multord_scaled <- do.call("rbind.data.frame",
  by(data_cr_multord, data_cr_multord$rater_id,
    function(x){x[, covar_names] <- scale(x[, covar_names]); x}))


###################################################
### code chunk number 42: vignette_mvord.Rnw:1342-1361 (eval = FALSE)
###################################################
## res_cor_logit <- multord(
##   formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
##   error.structure = corGeneral(~ 1),
##   link = "logit",
##   data = data_cr_multord_scaled,
##   index = c("firm_id", "rater_id"),
##   response.names = c("R1", "R2", "R3", "R4"),
##   response.levels = list(rev(LETTERS[1:6]),
##                          rev(LETTERS[1:6]),
##                          rev(LETTERS[7:13]),
##                          rev(LETTERS[14:15])),
##   coef.constraints = cbind(c(1,NA,1,NA),
##                            c(NA,NA,NA,1),
##                            c(1,1,1,NA),
##                            c(1,2,3,4),
##                            c(1,1,1,4),
##                            c(1,2,3,4),
##                            c(NA,NA,NA,1)),
##   threshold.constraints = c(1,1,2,3))


###################################################
### code chunk number 43: vignette_mvord.Rnw:1364-1394
###################################################
FILE <- "res_cor_logit.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
    res_cor_logit <- multord(
      formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
      error.structure = corGeneral(~ 1),
      link = "logit",
      data = data_cr_multord_scaled,
      index = c("firm_id", "rater_id"),
      response.names = c("R1", "R2", "R3", "R4"),
      response.levels = list(rev(LETTERS[1:6]),
                         rev(LETTERS[1:6]),
                         rev(LETTERS[7:13]),
                         rev(LETTERS[14:15])),
      coef.constraints = cbind(c(1,NA,1,NA),
                               c(NA,NA,NA,1),
                               c(1,1,1,NA),
                               c(1,2,3,4),
                               c(1,1,1,4),
                               c(1,2,3,4),
                               c(NA,NA,NA,1)),
      threshold.constraints = c(1,1,2,3))
  save(res_cor_logit, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }

}


###################################################
### code chunk number 44: vignette_mvord.Rnw:1399-1400
###################################################
summary(res_cor_logit, call = FALSE)


###################################################
### code chunk number 45: vignette_mvord.Rnw:1403-1404
###################################################
print(res_cor_logit, call = FALSE)


###################################################
### code chunk number 46: vignette_mvord.Rnw:1407-1408
###################################################
summary(res_cor_logit, short = FALSE, call = FALSE)


###################################################
### code chunk number 47: vignette_mvord.Rnw:1411-1412
###################################################
thresholds(res_cor_logit)


###################################################
### code chunk number 48: vignette_mvord.Rnw:1415-1416
###################################################
coef(res_cor_logit)


###################################################
### code chunk number 49: vignette_mvord.Rnw:1419-1420
###################################################
get.error.struct(res_cor_logit)


###################################################
### code chunk number 50: vignette_mvord.Rnw:1431-1433
###################################################
head(data_cr_multord2, n = 3)
str(data_cr_multord2, vec.len = 2)


###################################################
### code chunk number 51: vignette_mvord.Rnw:1437-1438
###################################################
data_cr_multord2[, covar_names] <- scale(data_cr_multord2[, covar_names])


###################################################
### code chunk number 52: vignette_mvord.Rnw:1441-1455 (eval = FALSE)
###################################################
## res_cor_logit <- multord(
##   formula = cbind(R1, R2, R3, R4) ~ 0 + ICR + LR + LEV1 + LEV2 + PR +
##                                         lRSIZE + lSYSR,
##   error.structure = corGeneral(~ 1),
##   link = "logit",
##   data = data_cr_multord_scaled,
##   coef.constraints = cbind(c(1,NA,1,NA),
##                            c(NA,NA,NA,1),
##                            c(1,1,1,NA),
##                            c(1,2,3,4),
##                            c(1,1,1,4),
##                            c(1,2,3,4),
##                            c(NA,NA,NA,1)),
##   threshold.constraints = c(1,1,2,3))


###################################################
### code chunk number 53: vignette_mvord.Rnw:1462-1464
###################################################
str(data_cr_panel, vec.len = 3)
head(data_cr_panel, n = 3)


###################################################
### code chunk number 54: vignette_mvord.Rnw:1467-1468
###################################################
summary(rowSums(with(data_cr_panel, table(firm_id, year))))


###################################################
### code chunk number 55: vignette_mvord.Rnw:1471-1472
###################################################
with(data_cr_panel, table(year))


###################################################
### code chunk number 56: vignette_mvord.Rnw:1476-1478
###################################################
formula <- rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR
formula


###################################################
### code chunk number 57: vignette_mvord.Rnw:1482-1484
###################################################
index <- c("firm_id", "year")
index


###################################################
### code chunk number 58: vignette_mvord.Rnw:1489-1491
###################################################
response.names <- paste0("year", 3:10)
response.names


###################################################
### code chunk number 59: vignette_mvord.Rnw:1495-1496
###################################################
levels(data_cr_panel$rating)


###################################################
### code chunk number 60: vignette_mvord.Rnw:1499-1503
###################################################
response.levels <- rep(list(levels(data_cr_panel$rating)),
                       length(response.names))
names(response.levels) <- response.names
response.levels


###################################################
### code chunk number 61: vignette_mvord.Rnw:1509-1512
###################################################
threshold.constraints <- rep(1, length(response.names))
names(threshold.constraints) <- response.names
threshold.constraints


###################################################
### code chunk number 62: vignette_mvord.Rnw:1517-1520
###################################################
coef.constraints = c(rep(1, 3),  rep(2, 5))
names(coef.constraints) <- response.names
coef.constraints


###################################################
### code chunk number 63: vignette_mvord.Rnw:1523-1525
###################################################
error.structure = corAR1(~ BSEC)
error.structure


###################################################
### code chunk number 64: vignette_mvord.Rnw:1531-1534
###################################################
data_cr_panel_scaled <- do.call("rbind.data.frame",
  by(data_cr_panel, data_cr_panel$year,
    function(x){x[, covar_names] <- scale(x[, covar_names]); x}))


###################################################
### code chunk number 65: vignette_mvord.Rnw:1536-1547 (eval = FALSE)
###################################################
## res_AR1_probit <- multord(
##     formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
##     index = c("firm_id", "year"),
##     data = data_cr_panel_scaled,
##     response.levels = rep(list(levels(data_cr_panel$rating)), 8),
##     response.names = paste0("year", 3:10),
##     link = "probit",
##     error.structure = corAR1(~ BSEC),
##     coef.constraints = c(rep(1, 3),  rep(2, 5)),
##     threshold.constraints = rep(1, 8),
##     solver = "BFGS")


###################################################
### code chunk number 66: vignette_mvord.Rnw:1549-1570
###################################################
FILE <- "res_AR1_probit.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
    res_AR1_probit <- multord(
      formula = rating ~ 0 + ICR + LR + LEV1 + LEV2 + PR + lRSIZE + lSYSR,
      error.structure = corAR1(~ BSEC),
      link = "probit",
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
### code chunk number 67: vignette_mvord.Rnw:1575-1576
###################################################
summary(res_AR1_probit, short = TRUE, call = FALSE, digits = 6)


###################################################
### code chunk number 68: vignette_mvord.Rnw:1579-1580
###################################################
print(res_AR1_probit, call = FALSE, digits = 4)


###################################################
### code chunk number 69: vignette_mvord.Rnw:1583-1584 (eval = FALSE)
###################################################
## summary(res_AR1_probit, short = FALSE, call = FALSE)


###################################################
### code chunk number 70: vignette_mvord.Rnw:1587-1588
###################################################
thresholds(res_AR1_probit)


###################################################
### code chunk number 71: vignette_mvord.Rnw:1591-1592
###################################################
coef(res_AR1_probit)


###################################################
### code chunk number 72: vignette_mvord.Rnw:1595-1596
###################################################
get.error.struct(res_AR1_probit)


###################################################
### code chunk number 73: vignette_mvord.Rnw:1599-1600
###################################################
head(get.error.struct(res_AR1_probit, type = "corr"), n = 3)


###################################################
### code chunk number 74: vignette_mvord.Rnw:1604-1605
###################################################
head(get.error.struct(res_AR1_probit, type = "sigmas"), n = 1)


