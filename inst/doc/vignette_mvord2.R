## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=FALSE------------------------------------------------------------
library(mvord)

## -----------------------------------------------------------------------------
data("data_cr", package = "mvord")
str(data_cr, vec.len = 3)
head(data_cr, n = 3)

## ----eval = FALSE-------------------------------------------------------------
#  res_cor_probit_2raters <- mvord(formula = MMO2(rater1, rater2) ~ 0 + LR + LEV + PR + RSIZE + BETA,
#                                 threshold.constraints = c(1, 1),
#                                 data = data_cr)

## ----echo = FALSE, results = 'hide', eval = TRUE------------------------------
cache <- TRUE
FILE <- "res_cor_probit_2raters.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
res_cor_probit_2raters <- mvord(formula = MMO2(rater1, rater2) ~ 0 + LR + LEV + PR + RSIZE + BETA,
                               threshold.constraints = c(1, 1),
                               data = data_cr)
res_cor_probit_2raters <- mvord:::reduce_size.mvord(res_cor_probit_2raters)
  save(res_cor_probit_2raters, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}

## -----------------------------------------------------------------------------
summary(res_cor_probit_2raters)

## -----------------------------------------------------------------------------
coef(res_cor_probit_2raters)

## ----eval = FALSE-------------------------------------------------------------
#  res_cor_logit_3raters <- mvord(formula = MMO2(rater1, rater2, rater 3) ~ 0 + LR + LEV + PR + RSIZE + BETA,
#                                 coef.constraints = c(1, 1, 1),
#                                 data = data_cr,
#                                 link = mvlogit())

## ----echo = FALSE, results = 'hide', eval = TRUE------------------------------
cache <- TRUE
FILE <- "res_cor_logit_3raters.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
res_cor_logit_3raters <- mvord(formula = MMO2(rater1, rater2, rater3) ~ 0 + LR + LEV + PR + RSIZE + BETA,
                               coef.constraints = c(1, 1, 1),
                               data = data_cr,
                               link = mvlogit())
res_cor_logit_3raters <- mvord:::reduce_size.mvord(res_cor_logit_3raters)
  save(res_cor_logit_3raters, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}

## -----------------------------------------------------------------------------
summary(res_cor_logit_3raters)

## -----------------------------------------------------------------------------
thresholds(res_cor_logit_3raters)

## ----eval = FALSE-------------------------------------------------------------
#  res_cor_probit_simple <- mvord(formula = MMO2(rater1, rater2, rater3, rater4) ~ 0 + LR + LEV + PR + RSIZE + BETA, data = data_cr)

## ----echo = FALSE, results = 'hide', eval = TRUE------------------------------
cache <- TRUE
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

## -----------------------------------------------------------------------------
summary(res_cor_probit_simple)

## -----------------------------------------------------------------------------
thresholds(res_cor_probit_simple)

## -----------------------------------------------------------------------------
coef(res_cor_probit_simple)

## -----------------------------------------------------------------------------
error_structure(res_cor_probit_simple)[[11]]

## ----eval = FALSE-------------------------------------------------------------
#  res_cor_logit <- mvord(formula = MMO2(rater1, rater2, rater3, rater4) ~
#      0 + LR + LEV + PR + RSIZE + BETA, data = data_cr, link = mvlogit(),
#      coef.constraints = cbind(LR = c(1, 1, 1, 1),
#                               LEV = c(1, 2, 3, 4),
#                               PR = c(1, 1, 1, 1),
#                               RSIZE = c(1, 1, 1, 2),
#                               BETA = c(1, 1, 2, 3)),
#      threshold.constraints = c(1, 1, 2, 3))

## ----echo = FALSE, results = 'hide', eval = TRUE------------------------------
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
#res_cor_logit <- mvord:::reduce_size2.mvord(res_cor_logit)
  save(res_cor_logit, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }

}

## -----------------------------------------------------------------------------
summary(res_cor_logit)

## -----------------------------------------------------------------------------
constraints(res_cor_logit)

## -----------------------------------------------------------------------------
BIC(res_cor_probit_simple, res_cor_logit)

## -----------------------------------------------------------------------------
AIC(res_cor_probit_simple)
AIC(res_cor_logit)

## -----------------------------------------------------------------------------
logLik(res_cor_probit_simple)
logLik(res_cor_logit)

## -----------------------------------------------------------------------------
mp <- marginal_predict(res_cor_logit, type = "class")

## -----------------------------------------------------------------------------
table(data_cr$rater1, mp$rater1)
table(data_cr$rater2, mp$rater2)
table(data_cr$rater3, mp$rater3)
table(data_cr$rater4, mp$rater4)

## ----eval = FALSE-------------------------------------------------------------
#  jp <- joint_probabilities(res_cor_logit,
#                            response.cat = data_cr[,1:4],
#                            type = "prob")

## ----echo = FALSE, results = 'hide', eval = TRUE------------------------------
FILE <- "jp.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
jp <- predict(res_cor_logit, type = "class")
  save(jp, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}

## -----------------------------------------------------------------------------
table(data_cr$rater1, jp$rater1)
table(data_cr$rater2, jp$rater2)
table(data_cr$rater3, jp$rater3)
table(data_cr$rater4, jp$rater4)

## -----------------------------------------------------------------------------
data("data_cr_panel", package = "mvord")
str(data_cr_panel, vec.len = 3)
head(data_cr_panel, n = 3)

## ----eval=F-------------------------------------------------------------------
#  res_AR1_probit <- mvord(formula = MMO(rating, firm_id, year) ~ LR + LEV +
#    PR + RSIZE + BETA,
#    error.structure = cor_ar1(~ BSEC), link = mvprobit(),
#    data = data_cr_panel,
#    coef.constraints = c(rep(1, 4), rep(2, 4)),
#    threshold.constraints = rep(1, 8),
#    threshold.values = rep(list(c(0, NA, NA, NA)),8),
#    control = mvord.control(solver = "BFGS"))

## ----echo = FALSE, results = 'hide', eval = TRUE------------------------------
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
  control = mvord.control(solver = "BFGS",  
                          solver.optimx.control = list(trace = TRUE)))
   res_AR1_probit <- mvord:::reduce_size.mvord(res_AR1_probit)
  save(res_AR1_probit, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}

## -----------------------------------------------------------------------------
summary(res_AR1_probit, short = TRUE, call = FALSE)

## -----------------------------------------------------------------------------
error_structure(res_AR1_probit)

## -----------------------------------------------------------------------------
head(error_structure(res_AR1_probit, type = "corr"), n = 3)

## -----------------------------------------------------------------------------
head(error_structure(res_AR1_probit, type = "sigmas"), n = 1)

## ----include=FALSE------------------------------------------------------------
# load("data_SRHS_long.rda")

## ----eval=TRUE, include=TRUE--------------------------------------------------
data(data_SRHS_long, package = "LMest")

## -----------------------------------------------------------------------------
str(data_SRHS_long)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  res_srhs <- mvord(formula = MMO(srhs, id, t) ~ 0 + factor(gender) +
#  	factor(race) + factor(education) + age,
#  	data = data_SRHS_long,
#  	threshold.constraints = rep(1, 8),
#  	coef.constraints = rep(1, 8),
#  	error.structure = cor_ar1(~ 1), link = mvlogit(),
#  	PL.lag = 2)

## ----include=FALSE------------------------------------------------------------
FILE <- "res_srhs.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
    res_srhs <- mvord(formula = MMO(srhs, id, t) ~ 0 +  factor(gender) +
   	    factor(race) + factor(education) + age,
                  data = data_SRHS_long,
                  link = mvlogit(),
                  threshold.constraints = rep(1, 8),
                  coef.constraints = rep(1, 8),
                  error.structure = cor_ar1(~1), PL.lag = 2)
   res_srhs <- mvord:::reduce_size.mvord(res_srhs)
   save(res_srhs, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}

## -----------------------------------------------------------------------------
unique(error_structure(res_srhs, type = "corr"))

## -----------------------------------------------------------------------------
summary(res_srhs, call = FALSE)

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  ## links are broken
#  N <- "http://www-math.bgsu.edu/~albert/ord_book/Chapter5/essay_data/N.dat"
#  X <- "http://www-math.bgsu.edu/~albert/ord_book/Chapter5/essay_data/X.dat"
#  y  <- read.delim(url(N), header = F, sep = "")
#  wl <- read.delim(url(X), header = F, sep = "")[,2]
#  essay_data <- cbind.data.frame(y, wl)
#  colnames(essay_data)[1:5] <- paste0("Judge", 1:5)
#  save(essay_data, file =  "essay_data.rda")

## ----include=TRUE-------------------------------------------------------------
data("essay_data", package = "mvord")

## -----------------------------------------------------------------------------
head(essay_data)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  res_essay_0 <- mvord(
#    formula = MMO2(Judge1, Judge2, Judge3, Judge4, Judge5) ~ -1,
#    data = essay_data, threshold.constraints = rep(1, 5),
#    coef.constraints = rep(1, 5))

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
FILE <- "res_essay.rda"
if (cache & file.exists(FILE)) {
  load(FILE)
} else {
  if (cache) {
    res_essay_wl <- mvord(
      formula = MMO2(Judge1, Judge2, Judge3, Judge4, Judge5) ~ 0 + wl,
      data = essay_data, threshold.constraints = rep(1, 5),
      coef.constraints = rep(1, 5))
    res_essay_wl <- mvord:::reduce_size2.mvord(res_essay_wl)
    res_essay_0 <- mvord(
      formula = MMO2(Judge1, Judge2, Judge3, Judge4, Judge5) ~ -1,
      data = essay_data, threshold.constraints = rep(1, 5),
      coef.constraints = rep(1,5))
    res_essay_0 <- mvord:::reduce_size.mvord(res_essay_0)
    save(res_essay_0, res_essay_wl, file  = FILE)
  } else {
      if(file.exists(FILE)) file.remove(FILE)
  }
}

## -----------------------------------------------------------------------------
summary(res_essay_0, call = FALSE)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  res_essay_wl <- mvord(
#    formula = MMO2(Judge1, Judge2, Judge3, Judge4, Judge5) ~ 0 + wl,
#    data = essay_data, threshold.constraints = rep(1, 5),
#    coef.constraints = rep(1, 5))

## -----------------------------------------------------------------------------
summary(res_essay_wl, call = FALSE)

## -----------------------------------------------------------------------------
agree_prob_list <- lapply(1:10, function(i)
  joint_probabilities(res_essay_wl, rep(i, 5)))
agree_prob <- Reduce("+", agree_prob_list)
summary(agree_prob)

## ----fig.align='center', fig.width=6, fig.height=6----------------------------
plot(essay_data$wl, agree_prob,
  xlab = "word length", ylab = "probability of agreement")

