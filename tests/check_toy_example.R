library(mvord)
data(data_toy_example)
tolerance <- 1e-6

# convert data_toy_example into long format
df <- cbind.data.frame("i" = rep(1:100,2), "j" = rep(1:2,each = 100),
                       "Y" = c(data_toy_example$Y1,data_toy_example$Y2),
                       "X1" = rep(data_toy_example$X1,2),
                       "X2" = rep(data_toy_example$X2,2),
                       "f1" = factor(sample(rep(data_toy_example$Y2,2)), ordered =F),
                       "f2" = factor(rep(data_toy_example$Y1,2), ordered=F))
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = T,
                     error.structure = cor_general(~1),
                     threshold.constraints = c(1,1),
                     coef.constraints = c(1,1),
                     contrasts = list(f1 = function(x)
                      contr.treatment(nlevels(df$f1), base = 1),
                      f2 = "contr.sum"))

res.summary <- summary(res, short = FALSE)
# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.96257384204389418, 1.03347470659308210,-0.96257384204389418, 1.03347407659308210), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.63801536536015713, -0.42672812065735988), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.8542626591759872), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.16613898748900424, 0.15004418316690710, 0.16613898748900424, 0.15004418316690710), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.13576680048134798, 0.13643622562495589), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.062442298836962326), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -134.90867383218699, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 280.34366345384768, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 294.05508548536392, tolerance = tolerance))


res2 <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = T,
                     error.structure = cor_general(~1),
                     threshold.constraints = c(1,1),
                     coef.constraints = list(matrix(rep(1,4), ncol = 1), matrix(rep(1,4), ncol = 1)))

mvord:::check(all.equal(res$beta, res2$beta, tolerance = tolerance))
mvord:::check(all.equal(res$sebeta, res2$sebeta, tolerance = tolerance))

#######################################################################
## cor_general(~factor)
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = T,
                     error.structure = cor_general(~f2),
                     threshold.constraints = c(1,1),
                     coef.constraints = c(1,1),
                     contrasts = list(f2 = "contr.sum"))

res.summary <- summary(res, short = FALSE)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate,
  c(-0.905536238229823,1.00422647477676,-0.905536238229823,1.00422647477676)))

mvord:::check(all.equal(res.summary$coefficients$Estimate,
  c(0.647946752758975,-0.428945695428613), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate,
  c(0.736674877333851,0.918294104589989,0.837114099387819), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`,
  c(0.182656232144459,0.174670667614416,0.182656232144459,0.174670667614416)))

mvord:::check(all.equal(res.summary$coefficients$`Std. Error`,
  c(0.141534034219262,0.139898136273291), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`,
  c(0.178240140406125,0.0688621486547878,0.131030217690603), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -134.1704617831153427687, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 283.3946870070909085371, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 303.0034948586141467786, tolerance = tolerance))

##################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvlogit(df = 8L),
                     solver = "newuoa",
                     se = T,
                     error.structure = cor_general(~1),
                     threshold.constraints = c(1,1),
                     coef.constraints = c(1,1))
res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
# mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-1.6170817306633420, 1.7855897338762188, -1.6170817306633420, 1.7855897338762188), tolerance = tolerance))
# mvord:::check(all.equal(res.summary$coefficients$Estimate, c(1.07242200717115987, 1.07242200717115987, -0.76715925377701732, -0.76715925377701732), tolerance = tolerance))
# mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.85317690560688531), tolerance = tolerance))
# mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.28997405649500174, 0.27427389826231802, 0.28997405649500174, 0.27427389826231802), tolerance = tolerance))
# mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.24111402270822993, 0.24111402270822993, 0.24156664773225886, 0.24156664773225886), tolerance = tolerance))
# mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.063316529381183581), tolerance = tolerance))
# mvord:::check(all.equal(logLik(res), -135.41665313840898, tolerance = tolerance))
# mvord:::check(all.equal(AIC(res), 281.35962206629165, tolerance = tolerance))
# mvord:::check(all.equal(BIC(res), 295.07104409780789, tolerance = tolerance))

##################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = TRUE,
                     error.structure = cov_general(~1),
                     threshold.constraints = c(1,1),
                     threshold.values = list(c(-1,NA),
                                             c(-1,NA)),
                     coef.constraints = c(1,1))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-1.0000000000000000, 1.0826480184519838,-1.0000000000000000, 1.0826480184519838), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.68836802258273944, -0.45397153898690201), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.85615798942939858,1.00191295742275210,1.08981621557349184), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.00000000000000000, 0.24925279863048969, 0.00000000000000000, 0.24925279863048969), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.15128077416900992, 0.15703117160571514), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.061820808778450094, 0.185179969647227688, 0.197587693964367678), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -134.63911116138209, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 282.04417976957268, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 298.67292563758178, tolerance = tolerance))
########################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = TRUE,
                     error.structure = cor_equi(~1),
                     threshold.constraints = c(1,1),
                     coef.constraints = cbind(c(1,1),c(1,2)))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.96275842449933668, 1.03387690191526760,-0.96275842449933668, 1.03387690191526760), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.63820742953523402, -0.44676677963058165,-0.40749970376598349), tolerance = tolerance))
# mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.85458123266967545), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.16703843305279975, 0.15203099683306531, 0.16703843305279975, 0.15203099683306531), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.13670473419831372,  0.15900889650026340, 0.13850979700325503), tolerance = tolerance))
# mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.062626544056546274), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -134.84323213339533, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 282.45242171359916, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 299.08116758160827, tolerance = tolerance))

res2 <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                      data = df,
                      index = c("i", "j"),
                      link = mvprobit(),
                      solver = "BFGS",
                      se = TRUE,
                      error.structure = cor_equi(~1),
                      threshold.constraints = c(1,1),
                      coef.constraints = list(X2 = cbind(c(1,1,0,0), c(0,0,1,1)), 
                        X1 = matrix(rep(1,4), ncol = 1)))

mvord:::check(all.equal(res$beta, res2$beta, tolerance = tolerance))
mvord:::check(all.equal(res$sebeta, res2$sebeta, tolerance = tolerance))

########################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = TRUE,
                     error.structure = cor_ar1(~ 1 + X1),
                     threshold.constraints = c(1,1),
                     coef.constraints = c(1,1))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.95722934559150896, 1.03746387407236318,-0.95722934559150896, 1.03746387407236318), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.65303962839195517, -0.42272946633706560), tolerance = tolerance))
#mvord:::check(all.equal(res.summary$error.structure$Estimate, c(1.2989148879266874,0.2935093715838244), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.16558281182128490, 0.15306970925575175, 0.16558281182128490, 0.15306970925575175), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.13584033372479276, 0.13820232270551500), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.24517467376804233, 0.29416990342543725), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -133.9388276754043, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 280.6436127976171, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 297.2723586656262, tolerance = tolerance))

res2 <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                      data = df,
                      index = c("i", "j"),
                      link = mvprobit(),
                      solver = "BFGS",
                      se = TRUE,
                      error.structure = cor_ar1(~1 + X1),
                      threshold.constraints = c(1,1),
                      coef.constraints = list(matrix(rep(1,4), ncol = 1), matrix(rep(1,4), 
                        ncol = 1)))

mvord:::check(all.equal(res$beta, res2$beta, tolerance = tolerance))
mvord:::check(all.equal(res$sebeta, res2$sebeta, tolerance = tolerance))

########################################################################################
res <- mvord:::mvord2(formula = cbind(Y1,Y2) ~ 0 + X1 + X2,
                      data = data_toy_example,
                      link = mvprobit(),
                      solver = "BFGS",
                      se = TRUE,
                      error.structure = cor_general(~1),
                      threshold.constraints = c(1,1),
                      coef.constraints = cbind(c(1,2),c(NA,1)))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.89893338192154260, 0.98413232708128517,-0.89893338192154260, 0.98413232708128517), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.681942721708264754, 0.468374979841733929, -0.052177869325230393), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.89039189201689062), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.16246078115908485, 0.15981676680681894, 0.16246078115908485, 0.15981676680681894), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.174961798042564121, 0.157105841207838853, 0.089797619171645041), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.063174936125465944), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -137.64946154566053, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 288.06488053812956, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 304.69362640613866, tolerance = tolerance))


res2 <- mvord:::mvord2(formula = cbind(Y1,Y2) ~ 0 + X1 + X2,
                       data = data_toy_example,
                       link = mvprobit(),
                       solver = "BFGS",
                       se = TRUE,
                       error.structure = cor_general(~1),
                       threshold.constraints = c(1,1),
                      coef.constraints = list(X1 = cbind(c(1,1,0,0), c(0,0,1,1)), 
                        X2 = matrix(c(rep(0,2),rep(1,2)), ncol = 1)))

mvord:::check(all.equal(res$beta, res2$beta, tolerance = tolerance))
mvord:::check(all.equal(res$sebeta, res2$sebeta, tolerance = tolerance))

########################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + offset(X2),
                      data = df,
                      index = c("i", "j"),
                      link = mvprobit(),
                      solver = "BFGS",
                      se = TRUE,
                      error.structure = cor_general(~1),
                      threshold.constraints = c(1,2),
                      coef.constraints = list(X1 = cbind(c(1,1,0,0), c(0,0,1,1))))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.76726530179547259, 0.89190466943429281,-0.79675332855226777, 0.87294046792542679), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.53370079897652545,0.34385571539923593), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.94080863143351701), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.12023340254630158, 0.13058943089325223, 0.11172347047030670, 0.13662237869977656), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.13636427290941133, 0.10634167445546971), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.033697157785564119), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -194.32584685564191, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 403.70545715214405, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 423.31426500366729, tolerance = tolerance))

res2 <- mvord:::mvord(formula = Y ~ 0 + X1,
                     offset = list(df$X2[1:100], df$X2[101:200]),
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = TRUE,
                     error.structure = cor_general(~1),
                     threshold.constraints = c(1,2),
                     coef.constraints = list(X1 = cbind(c(1,1,0,0), c(0,0,1,1))))

mvord:::check(all.equal(res$beta, res2$beta, tolerance = tolerance))
mvord:::check(all.equal(res$sebeta, res2$sebeta, tolerance = tolerance))

res3 <- mvord:::mvord2(formula = cbind(Y1,Y2) ~ 0 + X1 + offset(X2),
                       data = data_toy_example,
                       link = mvprobit(),
                       solver = "BFGS",
                       se = TRUE,
                       error.structure = cor_general(~1),
                       threshold.constraints = c(1,2),
                       coef.constraints = list(X1 = cbind(c(1,1,0,0), c(0,0,1,1))))
mvord:::check(all.equal(res$beta, res3$beta, tolerance = tolerance))
mvord:::check(all.equal(res$sebeta, res3$sebeta, tolerance = tolerance))

########################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
              data = df,
              index = c("i", "j"),
              link = mvlogit(),
              solver = "newuoa",
              se = TRUE,
              error.structure = cor_general(~1),
              threshold.constraints = c(1,1),
              coef.constraints = cbind(c(NA, NA), c(1,2)),
              coef.values = cbind(c(1, 1), c(NA,NA)))
#rho <- res$rho


res.summary <- summary(res, short = FALSE)

options(digits = 22)

mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-1.5832887068318791, 1.7586425059282578,-1.5832887068318791, 1.7586425059282578), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(-0.77306090515002379,-0.72430328395818655), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.85589444067598619), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.25851084470435, 0.242515931175915, 0.25851084470435, 0.242515931175915), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.267328102851796, 0.244039846158968), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.0605992051275919), tolerance = 0.00001))
mvord:::check(all.equal(logLik(res)[[1]], -135.52109964954849, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 281.56851508857068, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 295.27993712008691, tolerance = tolerance))

# res1 <- mvord:::mvord(formula = Y ~ 1 + X1 + X2,
#                       data = df,
#                       index = c("i", "j"),
#                       link = mvprobit(),
#                       solver = "BFGS",
#                       se = TRUE,
#                       error.structure = cor_equi(~1),
#                       threshold.constraints = c(1,1),
#                       coef.constraints = list(Intercept = matrix(rep(1,6), ncol = 1),
#                                               X2 = cbind(c(1,1,1,0,0,0), c(0,0,0,1,1,1)), X1 = matrix(rep(1,6), ncol = 1)))


########################################################################################
df$X3 <- cut(df$X2, c(-Inf, -0.2, 0.2, Inf))

res <- mvord:::mvord(formula = Y ~ 0 + X1 + X3,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = T,
                     error.structure = cor_general(~1),
                     threshold.constraints = c(1,1),
                     coef.constraints = c(1,1))
res.summary <- summary(res, short = FALSE)

options(digits = 22)

mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-1.21272652121924729,  0.75845553347339068, -1.21272652121924729,  0.75845553347339068), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.582352398722169484,  0.065952799481200336, -0.639869020298003632), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.85797312289027627), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.21131283795030667, 0.18860708674851231, 0.21131283795030667, 0.18860708674851231), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.13387326172482344, 0.39424777181603660, 0.23959456368744839), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.06239272825287346), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -136.05262199097416, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 284.87120142875682, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 301.49994729676592, tolerance = tolerance))
########################################################################################
res <- mvord:::mvord(formula = Y ~ 1 + X1 * X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = T,
                     error.structure = cor_general(~1),
                     threshold.constraints = c(1,1),
                     threshold.values = list(c(-1,NA),
                                             c(-1,NA)),
                     coef.constraints = c(1,1))
res.summary <- summary(res, short = TRUE)

options(digits = 22)

mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-1.0000000000000000,  1.0026913095517882), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c( -0.021928099468482259,  0.621615617919327845, -0.427632640476335146, -0.102565004643414323), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.85355560703961098), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.00000000000000000, 0.22781186888156521), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.16709863517554593, 0.13853938614861874, 0.14066642505159824, 0.15037979977593394), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.063097735777156036), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -134.6255603063936, tolerance = tolerance))
mvord:::check(all.equal(AIC(res), 282.01707805959569, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 298.64582392760479, tolerance = tolerance))
########################################################################################
df_NA <- df[-c(1,90:110),]


res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df_NA,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = T,
                     error.structure = cor_general(~1),
                     threshold.constraints = c(1,2)
                     #coef.constraints = list(X1 = cbind(c(1,1,1,0,0,0), c(0,0,0,1,1,1)))
                     )

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate,
  c(-1.0201257709778766002984,  1.1417947563798713783711,-0.9066208754867860486470,  0.9994625771009618686591), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(  0.8373293289440481013131,  0.4982163010003868297559, -0.4474052843372895438279, -0.3539081075852134050663),
 tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.9106443920801793323605), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.1999665948025287354195, 0.2237925964931104239053, 0.1905970002366945137418,
0.1740301425566555815205), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c( 0.1889858954424657733195, 0.1663043722901739185360, 0.1876367292206014836253,
0.1448961134964274755088), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.07055712765440583233989), tolerance = tolerance))
mvord:::check(all.equal(logLik(res)[[1]], -119.4526280847876478219, tolerance = tolerance))
mvord:::check(all.equal(AIC(res),  258.7052561695753070126, tolerance = tolerance))
mvord:::check(all.equal(BIC(res), 284.3969426859077316294, tolerance = tolerance))
