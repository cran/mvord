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

df_NA <- df[-c(1,90:110),]


res <- mvord:::mvord(formula = Y ~ 0 + X1 + X2,
                     data = df_NA,
                     index = c("i", "j"),
                     link = mvprobit(),
                     solver = "BFGS",
                     se = T,
                     error.structure = cor_general(~1),
                     threshold.constraints = c(1,2),
                     coef.constraints = c(1,1))



print(res)
summary(res)
print(summary(res))

coef(res)
thresholds(res)
AIC(res)
BIC(res)


logLik(res)
nobs(res)
vcov(res)
terms(res)
model.matrix(res)


fitted(res)

constraints(res)

names_constraints(Y ~ 0 + X1 + X2, df_NA)

#predict functions
marginal.predict(res, type = "prob", subjectID = c(2,5,32,88))
marginal.predict(res, type = "class", subjectID = c(2,5,32,88)+2)
marginal.predict(res, type = "class")
marginal.predict(res, type = "pred", subjectID = c(2,5,32,88))
marginal.predict(res, type = "pred", subjectID = c(2,5,32,88)+4)
marginal.predict(res, type = "cum.prob", subjectID = c(2,5,32,88)+8)
predict(res, type = "prob", subjectID = c(3,6,33,55,90))
predict(res, type = "class.max", subjectID = c(3,6,33,55,90)+1)
predict(res, type = "cum.prob", subjectID = c(3,6,33,55,90)+2)
get.prob(res, response.cat = c(1,2))
get.prob(res, response.cat = c(3,3), subjectID = 22:33)
get.prob(res, response.cat = res$rho$y)

get_error_struct(res)

