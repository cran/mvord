library(mvord)
data(data_toy_example)
tolerance <- 0.000001

# convert data_toy_example into long format
df <- cbind.data.frame("i" = rep(1:100,2), "j" = rep(1:2,each = 100),
                       "Y" = c(data_toy_example$Y1,data_toy_example$Y2),
                       "X1" = rep(data_toy_example$X1,2),
                       "X2" = rep(data_toy_example$X2,2))

res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
             index = c("i", "j"),
             link = mvprobit(),
             solver = "BFGS",
             se = TRUE,
             error.structure = corGeneral(~1),
             threshold.constraints = c(1,1),
             coef.constraints = c(1,1))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.96257384204389418, 1.03347470659308210,-0.96257384204389418, 1.03347407659308210), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.63801536536015713, 0.63801536536015713,-0.42672812065735988,-0.42672812065735988), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.8542626591759872), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.16613898748900424, 0.15004418316690710, 0.16613898748900424, 0.15004418316690710), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.13576680048134798, 0.13576680048134798, 0.13643622562495589, 0.13643622562495589), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.062442298836962326), tolerance = tolerance))
mvord:::check(all.equal(mvord:::logPL(res), -134.90867383218699, tolerance = tolerance))
mvord:::check(all.equal(mvord:::claic(res), 280.34366345384768, tolerance = tolerance))
mvord:::check(all.equal(mvord:::clbic(res), 294.05508548536392, tolerance = tolerance))

##################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvlogit(df = 7),
                     solver = "newuoa",
                     se = FALSE,
                     error.structure = corGeneral(~1),
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
# mvord:::check(all.equal(mvord:::logPL(res), -135.41665313840898, tolerance = tolerance))
# mvord:::check(all.equal(mvord:::claic(res), 281.35962206629165, tolerance = tolerance))
# mvord:::check(all.equal(mvord:::clbic(res), 295.07104409780789, tolerance = tolerance))

##################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = TRUE,
                     error.structure = covGeneral(~1),
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
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.68836802258273944, 0.68836802258273944,-0.45397153898690201,-0.45397153898690201), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.85615798942939858,1.00191295742275210,1.08981621557349184), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.00000000000000000, 0.24925279863048969, 0.00000000000000000, 0.24925279863048969), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.15128077416900992, 0.15128077416900992, 0.15703117160571514, 0.15703117160571514), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.061820808778450094, 0.185179969647227688, 0.197587693964367678), tolerance = tolerance))
mvord:::check(all.equal(mvord:::logPL(res), -134.63911116138209, tolerance = tolerance))
mvord:::check(all.equal(mvord:::claic(res), 282.04417976957268, tolerance = tolerance))
mvord:::check(all.equal(mvord:::clbic(res), 298.67292563758178, tolerance = tolerance))
########################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = TRUE,
                     error.structure = corEqui(~1),
                     threshold.constraints = c(1,1),
                     coef.constraints = cbind(c(1,1),c(1,2)))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.96275842449933668, 1.03387690191526760,-0.96275842449933668, 1.03387690191526760), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.63820742953523402, 0.63820742953523402,-0.44676677963058165,-0.40749970376598349), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.85458123266967545), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.16703843305279975, 0.15203099683306531, 0.16703843305279975, 0.15203099683306531), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.13670473419831372, 0.13670473419831372, 0.15900889650026340, 0.13850979700325503), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.062626544056546274), tolerance = tolerance))
mvord:::check(all.equal(mvord:::logPL(res), -134.84323213339533, tolerance = tolerance))
mvord:::check(all.equal(mvord:::claic(res), 282.45242171359916, tolerance = tolerance))
mvord:::check(all.equal(mvord:::clbic(res), 299.08116758160827, tolerance = tolerance))

########################################################################################
res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = TRUE,
                     error.structure = corAR1(~1 + X1),
                     threshold.constraints = c(1,1),
                     coef.constraints = c(1,1))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.95722934559150896, 1.03746387407236318,-0.95722934559150896, 1.03746387407236318), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.65303962839195517, 0.65303962839195517,-0.42272946633706560,-0.42272946633706560), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(1.2989148879266874,0.2935093715838244), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.16558281182128490, 0.15306970925575175, 0.16558281182128490, 0.15306970925575175), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.13584033372479276, 0.13584033372479276, 0.13820232270551500, 0.13820232270551500), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.24517467376804233, 0.29416990342543725), tolerance = tolerance))
mvord:::check(all.equal(mvord:::logPL(res), -133.9388276754043, tolerance = tolerance))
mvord:::check(all.equal(mvord:::claic(res), 280.6436127976171, tolerance = tolerance))
mvord:::check(all.equal(mvord:::clbic(res), 297.2723586656262, tolerance = tolerance))

########################################################################################
res <- mvord:::mvord2(formula = cbind(Y1,Y2) ~ 0 + X1 + X2,
                      data = data_toy_example,
                      link = mvprobit(),
                      solver = "BFGS",
                      se = TRUE,
                      error.structure = corGeneral(~1),
                      threshold.constraints = c(1,1),
                      coef.constraints = cbind(c(1,2),c(NA,1)))

res.summary <- summary(res, short = FALSE)

options(digits = 22)

# paste(format(res.summary$thresholds$Estimate), collapse = ",")
# paste(format(res.summary$coefficients$Estimate), collapse = ",")
# paste(format(res.summary$error.structure$Estimate), collapse = ",")
mvord:::check(all.equal(res.summary$thresholds$Estimate, c(-0.89893338192154260, 0.98413232708128517,-0.89893338192154260, 0.98413232708128517), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$Estimate, c(0.681942721708264754, 0.468374979841733929, 0.000000000000000000,-0.052177869325230393), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$Estimate, c(0.89039189201689062), tolerance = tolerance))
mvord:::check(all.equal(res.summary$thresholds$`Std. Error`, c(0.16246078115908485, 0.15981676680681894, 0.16246078115908485, 0.15981676680681894), tolerance = tolerance))
mvord:::check(all.equal(res.summary$coefficients$`Std. Error`, c(0.174961798042564121, 0.157105841207838853, 0.000000000000000000, 0.089797619171645041), tolerance = tolerance))
mvord:::check(all.equal(res.summary$error.structure$`Std. Error`, c(0.063174936125465944), tolerance = tolerance))
mvord:::check(all.equal(mvord:::logPL(res), -137.64946154566053, tolerance = tolerance))
mvord:::check(all.equal(mvord:::claic(res), 288.06488053812956, tolerance = tolerance))
mvord:::check(all.equal(mvord:::clbic(res), 304.69362640613866, tolerance = tolerance))

