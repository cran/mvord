#' @rdname mvord-package
#' @title Multivariate Ordinal Regression Models in R.
#' @description  The R package mvord implements composite likelihood
#' estimation in the class of multivariate ordinal regression models with probit and logit link.
#' A flexible modeling framework for multiple ordinal measurements on
#' the same subject is set up, which takes into consideration the
#' dependence among the multiple observations by employing different
#' error structures.
#' Heterogeneity in the error structure across the subjects can be
#' accounted for by the package, which allows for covariate dependent
#' error structures.
#' In addition, regression coefficients and threshold parameters are
#' varying across the multiple response dimensions in the default
#' implementation. However, constraints can be defined by the user if a
#' reduction of the parameter space is desired.
#' @details see \code{\link{mvord}}, \code{\link{mvord2}}
#' @name mvord-package
#' @aliases mvord-package
NULL

#' @title Simulated panel of credit ratings
#'
#' @description A data set containing simulated credit ratings assigned by one rater and simulated perfomance measures for firms in different years.
#'
#' \itemize{
#'   \item \code{year} year index
#'   \item \code{rating} credit ratings
#'   \item \code{firm_id} firm index
#'   \item \code{ICR} interest coverage ratio, which measures how well the interest expenses can be
#' covered from the free operating cash-flow of a company
#'   \item \code{LR} liquidity ratio, relating the cash held by a company to the current liabilities
#'   \item \code{LEV1} leverage ratio relating debt to earnings before interest and taxes
#'   \item \code{LEV2} leverage ratio measuring the percentage of debt in the long-term capital of a firm
#'   \item \code{PR} profitability ratio measuring return on capital
#'   \item \code{lRSIZE} log of relative size of the company in the market
#'   \item \code{lSYSR} log of a measure of systematic risk
#'   \item \code{BSEC} business sector of a firm (factor with 8 levels)
#' }
#'
#' @name data_cr_panel
#' @docType data
# #' @keywords datasets
#' @usage data(data_cr_panel)
#' @format A data frame with 11431 rows and 11 variables
NULL

#' Simulated credit ratings
#'
#' A data set containing simulated credit ratings and simulated perfomance measures from four raters.
#'
#' \itemize{
#'   \item \code{rating} credit ratings
#'   \item \code{firm_id} firm index
#'   \item \code{rater_id} rater index
#'   \item \code{ICR} interest coverage ratio, which measures how well the interest expenses can be
#' covered from the free operating cash-flow of a company
#'   \item \code{LR} liquidity ratio, relating the cash held by a company to the current liabilities
#'   \item \code{LEV1} leverage ratio relating debt to earnings before interest and taxes
#'   \item \code{LEV2} leverage ratio measuring the percentage of debt in the long-term capital of a firm
#'   \item \code{PR} profitability ratio measuring return on capital
#'   \item \code{lRSIZE} log of relative size of the company in the market
#'   \item \code{lSYSR} log of a measure of systematic risk
#'   \item \code{BSEC} business sector of a firm (factor with 8 levels)
#' }
#'
#' @name data_cr_mvord
#' @docType data
# #' @keywords datasets
#' @usage data(data_cr_mvord)
#' @format A data frame with 4566 rows and 11 variables
NULL

#' Simulated credit ratings
#'
#' A data set containing simulated credit ratings and simulated perfomance measures from four raters.
#'
#' \itemize{
#'   \item \code{rating} credit ratings
#'   \item \code{firm_id} firm index
#'   \item \code{rater_id} rater index
#'   \item \code{ICR} interest coverage ratio, which measures how well the interest expenses can be
#' covered from the free operating cash-flow of a company
#'   \item \code{LR} liquidity ratio, relating the cash held by a company to the current liabilities
#'   \item \code{LEV1} leverage ratio relating debt to earnings before interest and taxes
#'   \item \code{LEV2} leverage ratio measuring the percentage of debt in the long-term capital of a firm
#'   \item \code{PR} profitability ratio measuring return on capital
#'   \item \code{lRSIZE} log of relative size of the company in the market
#'   \item \code{lSYSR}log of a measure of systematic risk
#'   \item \code{BSEC} business sector of a firm (factor with 8 levels)
#' }
#'
#' @name data_cr_mvord2
#' @docType data
# #' @keywords datasets
#' @usage data(data_cr_mvord2)
#' @format A data frame with 1665 rows and 13 variables
NULL

#' Simulated credit ratings
#'
#' A simulated data set where three different raters (\code{rater1, rater2} and \code{rater3})
#'  assign ordinal ratings on different firms. \code{rater3} uses a different rating scale
#'   compared to \code{rater1} and \code{rater2}. The IDs for each subject \eqn{i} of the \eqn{n = 1000}
#' firms are stored in the column \code{firm_id}.
#'
#' \itemize{
#'   \item \code{firm_id} firm index
#'   \item \code{rater1}  ordinal rating outcome of rater 1
#'   \item \code{rater2}  ordinal rating outcome of rater 2
#'   \item \code{rater3}  ordinal rating outcome of rater 3
#'   \item \code{X1} covariate X1
#'   \item \code{X2} covariate X2
#'   \item \code{X3} covariate X3
#'   \item \code{X4} covariate X4
#'   \item \code{X5} covariate X5
#'   \item \code{X6} covariate X6 (factor)
#' }
#' @name data_mvord2
#' @docType data
# #' @keywords datasets
#' @usage data(data_mvord2)
#' @format A data frame with 1000 rows and 10 variables
NULL

#' Simulated panel of credit ratings
#'
#' A simulated data set where one rater assigns ratings over the years \eqn{2001} to \eqn{2010} for a set of firms.
#' The IDs for each subject \eqn{i} of the \eqn{n = 1000} firms are stored in the column \code{firm_id}.
#' The year of the rating observation is stored in the column \code{year}.
#' The ordinal ratings are provided in the column \code{rating} and all the covariates in the remaining columns.
#'
#' \itemize{
#'   \item \code{firm_id} firm index
#'   \item \code{year}  year index (2001 - 2010)
#'   \item \code{rating} ordinal credit ratings
#'   \item \code{X1} covariate X1
#'   \item \code{X2} covariate X2
#'   \item \code{X3} covariate X3
#'   \item \code{X4} covariate X4
#'   \item \code{X5} covariate X5
#'   \item \code{X6} covariate X6 (factor)
#' }
#' @name data_mvord_panel
#' @docType data
# #' @keywords datasets
#' @usage data(data_mvord_panel)
#' @format A data frame with 10000 rows and 9 variables
NULL


#' Simulated credit ratings
#'
#' A simulated data set where three different raters (\code{rater1, rater2} and \code{rater3})
#'  assign ordinal ratings on different firms.
#'  \code{rater3} uses a different rating scale compared to \code{rater1} and \code{rater2},
#'  i.e. the number of threshold categories is different.
#' For each firm we simulate
#'  five different covariates \code{X1, ..., X5} from a standard normal distribution. Additionally, each firm is randomly assigned
#'  to a business sector (sector \code{X}, \code{Y} or \code{Z}), captured by the covariate \code{X6}. Furthermore, we simulate
#'  multivariate normally distributed errors. For a given set of parameters we obtain the three rating variables for
#'  each firm by slotting the latent scores according to the corresponding threshold parameters.
#' The IDs for each subject \eqn{i} of the \eqn{n = 1000}
#' firms are stored in the column \code{firm_id}. The IDs of the raters are stored
#' in the column \code{rater_id}. The ordinal ratings are provided in the column \code{rating} and all the covariates in the remaining columns.
#' Overall, the data set has 3000 rows, for each of the \eqn{n = 1000} firms it has three rating observations.
#'
#' \itemize{
#'   \item \code{firm_id} firm index
#'   \item \code{rater_id}  rater index
#'   \item \code{rating} ordinal credit ratings
#'   \item \code{X1} covariate X1
#'   \item \code{X2} covariate X2
#'   \item \code{X3} covariate X3
#'   \item \code{X4} covariate X4
#'   \item \code{X5} covariate X5
#'   \item \code{X6} covariate X6 (factor)
#' }
#'
#' @name data_mvord
#' @docType data
# #' @keywords datasets
#' @usage data(data_mvord)
#' @format A data frame with 3000 rows and 9 variables
NULL

#' Data set toy example
#'
#' A data set containing two simulated ordinal responses with three categories and two covariates \code{X1} and \code{X2}.
#'
#' \itemize{
#'   \item \code{Y1} ordinal outcome \code{Y1} (three categories)
#'   \item \code{Y2} ordinal outcome \code{Y2} (three categories)
#'   \item \code{X1} covariate \code{X1}
#'   \item \code{X2} covariate \code{X2}
#' }
#'
#' @name data_toy_example
#' @docType data
# #' @keywords datasets
#' @usage data(data_toy_example)
#' @format A data frame with 100 rows and 4 variables
NULL

##IMPORTS
#' @importFrom optimx optimx
#' @importFrom stats model.frame model.matrix coef as.formula cov2cor dnorm dt pnorm pt terms.formula plogis dlogis qt
#' @importFrom pbivnorm pbivnorm
#' @importFrom MASS polr
#' @importFrom utils combn data write.table
#' @importFrom mnormt sadmvn sadmvt


#############################################################################################
#' @title Multivariate Ordinal Regression Models.
#'
#' @description
#' \code{mvord} is used to estimate multivariate ordinal regression models. Different model types are implemented and can be chosen by
#' the use of different \code{\link{error.structures}}. Constraints on the threshold as well as on the regression parameters can be imposed.
#'
#' @details
#' \describe{
#'   \item{\code{data}}{
#' We use the long format for the input of \code{data}, where each row contains a subject
#' index \eqn{i} (\code{firm_id}), a multiple measurement index \eqn{j} (\code{rater_id}), an ordinal response
#' (\code{rating}) and all the covariates (\code{X1, ..., Xp}). This long format data stucture
#' is internally transformed to matrix of covariates \eqn{Y} and a list of covariate matrices
#' \eqn{X_j} for all \eqn{j \in J} by a matching according to the subject index \eqn{i} and the multiple
#' measurement index \eqn{j}, which are passed by an optional argument \code{index}. This is
#' usually performed by a character vector of length two specifying the column names
#' of the subject index and the multiple measurement index in \code{data}. For the data set in \code{\link{data_mvord}} we have:
#'
#'    \code{index = c("firm_id", "rater_id")}
#'
#' The default value of \code{index} is \code{NULL} assuming that the first column of \code{data}
#' contains the subject index and the second column the multiple measurement index.
#' (Note that if the covariates have different scale, the estimation is prone to numerical instabilities.
#' In such a case one could standardize the covariates \eqn{x_{ij}}.)
#'
#' If specific constraints are imposed on the parameter set, a well defined index \eqn{j \in J}
#' for the multiple measurements is needed. Therefore, a vector \code{response.names}
#' is used to define the index number of the multiple measurement.
#'
#'     \code{response.names = c("rater1", "rater2", "rater3")}
#'
#' The default value of \code{response.names} is \code{NULL} giving the natural ordering
#' of the levels of the factor variable for all the multiple measurements.
#' The ordering of \code{response.names} always specifies the index of the
#' multiple measurement unit \eqn{j \in J}. This ordering is essential when
#' putting constraints on the parameters and when setting \code{response.levels}.
#'
#' \preformatted{response.levels = list(c("G","F","E", "D", "C", "B", "A"),
#'                        c("G","F","E", "D", "C", "B", "A"),
#'                        c("O","N","M","L", "K", "J", "I", "H"))}
#'
#' If the categories differ across multiple measurements (either the number of categories or the category labels)
#' one needs to specify the \code{response.levels} explicitly. This is performed by a list
#' of length \eqn{J} (number of multiple measurements), where each element contains
#' the names of the levels of the ordered categories in ascending or descending order.}
#'
#' \item{\code{formula}}{
#' The ordinal responses \eqn{Y} (\code{rating}) are passed by a \code{formula} object.
#' Intercepts can be included or excluded in the model depending on the model paramterization:
#' \itemize{
#' \item {Model without intercept:}
#'
#' If the intercept should be removed the \code{formula} for a given response (\code{rating})
#' and covariates (\code{X1} to \code{Xp}) has the following form:
#'
#'      \code{formula = rating ~ 0 + X1 + ... + Xp}.
#'
#' \item {Model with intercept:}
#'
#' If one wants to include an intercept in the model, there are two equivalent possibilities
#' to set the model \code{formula}. Either one inludes the intercept explicitly by:
#'
#'     \code{formula = rating ~ 1 + X1 + ... + Xp},
#'
#' or by
#'
#'   \code{formula = rating ~ X1 + ... + Xp}.
#' }
#'   }
#'   \item{\code{error.structure}}{
#'  We allow for different error structures depending on the model parameterization:
#'\itemize{
#'   \item {Correlation:}
#'   \itemize{
#'   \item \code{corGeneral}
#' The most common parameterization is the general correlation matrix.
#'
#'  \code{error.structure = corGeneral(~ 1)}
#'
#' This parameterization can be extended by allowing a factor dependent
#' correlation structure, where the correlation of each subject \eqn{i} depends
#' on a given subject-specific factor \code{f}. This factor \code{f} is not allowed to vary
#' across multiple measurements \eqn{j} for the same subject \eqn{i} and due to numerical
#' constraints only up to maximum 30 levels are allowed.
#'
#'       \code{error.structure = corGeneral(~ f)}
#'
#'   \item \code{corEqui}
#' A covariate dependent equicorrelation structure, where the correlations
#' are equal across all \eqn{J} dimensions and depend on subject-specific covariates \code{S1, ..., Sm}.
#' It has to be noted that these covariates \code{S1, ..., Sm} are not allowed to vary across
#'  multiple measurements \eqn{j} for the same subject \eqn{i}.
#'
#'          \code{error.structure = corEqui(~ S1 + ... + Sm)}
#'
#'   \item \code{corAR1}
#' In order to account for some heterogeneity the \eqn{AR(1)} error structure
#' is allowed to depend on covariates \code{X1, ..., Xp} that are constant
#' over time for each subject \eqn{i}.
#'
#'       \code{error.structure = corAR1(~ S1 + ... + Sm)}
#'}
#'
#'
#'\item {Covariance:}
#'\itemize{
#'\item \code{covGeneral}
#'
#' In case of a full variance-covariance parameterization the standard parameterization
#'  with a full variance-covariance is obtained by:
#'
#'  \code{error.structure = covGeneral(~ 1)}
#'
#'  This parameterization can be extended to the factor dependent covariance structure,
#'   where the covariance of each subject depends on a given factor \code{f}:
#'
#'  \code{error.structure = covGeneral(~ f)}
#'   }
#'   }
#'   }


#'
#'   \item{\code{coef.constraints}}{
#'   In order to achieve a more flexible framework we allow for constraints on the regression
#'    coefficients. These constraints can be specified by a vector or a matrix \cr
#'    \code{coef.constraints}.
#'     First, a simple and less flexible way by specifying a vector \cr
#'     \code{coef.constraints}
#'      of dimension \eqn{J}. The ordering \eqn{j \in J} of the responses is given by \code{response.names}.
#'      This vector is allocated in the following way:
#' The first element of the vector \code{coef.constraints} gets a value of 1. If the coefficients
#'  of the multiple measurement \eqn{j = 2} should be equal to the coefficients of the first dimension (\eqn{j=1}) again
#'   a value of 1 is set. If the coefficients should be different to the coefficients of the first dimension
#'   a value of 2 is set. In analogy, if the coefficients of dimensions two and three
#'    should be the same one sets both values to 2 and if they should be different,
#'     a value of 3 is set. Constraints on the regression coefficients of the remaining multiple measurements are set analogously.
#'
#'  \code{coef.constraints <- c(1,1,2,3)}
#'
#'  This vector \code{coef.constraints} sets the coefficients of the first two raters equal
#'  \deqn{\beta_{1\cdot} = \beta_{2\cdot}}
#'  A more flexible way to specify constraints on the regression coefficients is a matrix with \eqn{J} rows and \eqn{p} columns,
#'   where each column specifies constraints on one of the \eqn{p} coefficients in the same way as above.
#'    In addition, a value of \code{NA} excludes a corresponding coefficient (meaning it should be fixed to zero).
#'
#'    \preformatted{coef.constraints <- cbind(c(1,2,3,4), c(1,1,1,2), c(NA,NA,NA,1),
#'                           c(1,1,1,NA), c(1,2,3,4), c(1,2,3,4))}
#'
#'        This matrix \code{coef.constraints} gives the following constraints:
#'\itemize{
#'  \item \eqn{\beta_{12} = \beta_{22} = \beta_{32}}
#'    \item \eqn{\beta_{13} = 0}
#'    \item \eqn{\beta_{23} = 0}
#'    \item \eqn{\beta_{33} = 0}
#'    \item \eqn{\beta_{44} = 0}
#'    \item \eqn{\beta_{14} = \beta_{24} = \beta_{34}}
#'}
#'}
#'
#'
#'   \item{\code{coef.values}}{
#'   In addition, specific values on regression coefficients can be set in the matrix \cr
#'   \code{coef.values}.
#'    Parameters are removed if the value is set to zero (default for \code{NA}'s in \cr
#'    \code{coef.constraints})
#'     or to some fixed value. If constraints on parameters are set, these dimensions need to have
#'      the same value in \code{coef.values}. Again each column corresponds to one regression coefficient.
#'
#'  Together with the \code{coef.constraints} from above we impose:
#'
#'    \preformatted{coef.constraints <- cbind(c(1,2,2), c(1,1,2), c(NA,1,2),
#'                           c(NA,NA,NA), c(1,1,2))}
#'
#'  \preformatted{coef.values <- cbind(c(NA,NA,NA), c(NA,NA,NA), c(0,NA,NA),
#'                      c(1,1,1), c(NA,NA,NA))}
#'Interaction terms
#'
#'When constraints on the regression coefficient should be specified in models with interaction terms,
#' the \code{coef.constraints} matrix has to be expanded manually. In case of interaction terms
#'  (specified either by \code{X1 + X2 + X1:X2} or equivalently by \code{X1*X2}), one additional
#'   column at the end of \code{coef.constraints} for the interaction term has to be specified for
#'    numerical variables. For interaction terms including factor variables suitably more columns have
#'     to be added to the \code{coef.constraints} matrix.
#' }
#'
#'
#'   \item{\code{threshold.constraints}}{
#'   Similarly, constraints on the threshold parameters can be imposed by a vector of positive integers,
#'    where dimensions with equal threshold parameters get the same integer. When restricting the thresholds of two
#'     outcome dimensions to be the same, one has to be careful that the number of categories in
#'      the two outcome dimensions must be the same. In our example with \eqn{J=4} different outcomes we impose:
#'
#'  \code{threshold.constraints <- c(1,1,2)}
#'
#'    gives the following restrictions:
#'  \itemize{
#'  \item \eqn{\bm\theta_{1} = \bm\theta_{2}}
#'  \item \eqn{\bm\theta_{3}} arbitrary.
#' }
#' }
#'

#'   \item{\code{threshold.values}}{
#'   In addition, threshold parameter values can be specified by \code{threshold.values}
#'    in accordance with identifiability constraints. For this purpose we use a \code{list}
#'     with \eqn{J} elements, where each element specifies the constraints of the particular
#'      dimension by a vector of length of the number of threshold parameters (number of categories - 1).
#'      A number specifies a threshold parameter to a specific value and \code{NA} leaves the parameter flexible.
#'       For \code{\link{data_mvord}} we have

#' \preformatted{threshold.constraints <- NULL}
#'
#' \preformatted{threshold.values <- list(c(-4,NA,NA,NA,NA,4.5),
#'                          c(-4,NA,NA,NA,NA,4.5),
#'                          c(-5,NA,NA,NA,NA,NA,4.5))}
#' }
#' }
#'
#' @return The function \code{mvord} returns an object of \code{\link{class}} \code{"mvord"}.
#'
#' The functions \code{summary} and \code{print} are used to display the results.
#' The function \code{coef} extracts the regression coefficients, a function \code{thresholds} the threshold coefficients
#' and the function \cr
#' \code{get.error.struct} returns the estimated parameters of the corresponding error structure.
#'
#' An object of \code{\link{class}} \code{"mvord"} is a list containing the following components:
#'
#' \itemize{
#'  \item{\code{beta}}{
#'
#'  a named \code{\link{matrix}} of regression coefficients}
#'  \item{\code{theta}}
#'
#'  a named \code{\link{list}}{ of threshold parameters}
#'   \item{\code{error.struct}}{
#'
#'     a named \code{\link{list}} of correlation (covariance) matrices,
#'   or a vector of coefficients in the \cr
#'   \code{corEqui()} and \code{corAR1()} error structures}
#'   \item{\code{sebeta}}{
#'
#'     a named \code{\link{matrix}} of the standard errors of the regression coefficients}
#'   \item{\code{setheta}}{
#'
#'     a named \code{\link{list}} of the standard errors of the threshold parameters}
#'   \item{\code{seerror.struct}}{
#'
#'     a named \code{\link{list}} of the standard errors of the correlation (covariance) matrices,
#'   or a vector of coefficients in the \code{error.structure = corEqui} setting}
#'   \item{\code{rho}}{
#'
#'     a \code{\link{list}} of all objects that are used in \code{mvord()}}
#' }
#'
#' @seealso %\code{\link{predict.mvord}},
#' \code{\link{print.mvord}}, \code{\link{summary.mvord}}, \code{\link{coef.mvord}},
#'  \code{\link{thresholds.mvord}}, \code{\link{get.error.struct.mvord}}, \code{\link{logPL}},
#'  \code{\link{claic}}, \code{\link{clbic}},
#'  \code{\link{data_cr_panel}},\code{\link{data_cr_mvord}}, \code{\link{data_cr_mvord2}},
#'  \code{\link{data_mvord_panel}},\code{\link{data_mvord}}, \code{\link{data_mvord2}}
#'
#'
#' @param formula an object of class \code{\link{formula}} of the form \code{y ~ X1 + ... + Xp}.
#' @param data \code{\link{data.frame}} containing a subject index, an index for the multiple measurements,
#' an ordinal response \code{y} and covariates \code{X1, ..., Xp}.
#' @param index (optional) argument to specify the column names of the subject index and the multiple measurement index
#' by a vector \cr
#' \code{c("subject", "multiple_measurement")} in \code{data}.
#' The default value of \code{index} is \code{NULL} assuming that the first column of \code{data} contains
#' the subject index and the second column the multiple measurement index.
#' @param response.levels (optional) \code{\link{list}} of length equal to the number of multiple measurements to specify the category labels
#' in case of varying categories across multiple measurements
#' @param response.names (optional) \code{\link{vector}} of the labels of the multiple measurement index in order to
#' specify the ordering of the responses which is essential when setting constraints on the model parameters.
#'  The default value of \code{response.names} is \code{NULL} giving the natural ordering of the levels of the factor variable
#'  of multiple measurements.
#' @param link specifies the link function by \code{mvprobit()} (multivariate normally distributed errors)
#' or \code{mvlogit(df = 8)} (multivariate logistically distributed errors), where \code{df} specifies the degrees of freedom of the t copula.
#' @param error.structure different \code{\link{error.structures}}: general correlation structure (default)\cr
#' \code{corGeneral(~1)},
#' general covariance structure \code{covGeneral(~1)}, factor dependent correlation structure \code{covGeneral(~f)},
#' factor dependent covariance structure \code{covGeneral(~f)}, covariate dependent equicorrelation structure \cr
#' \code{corEqui(~S)},
#' AR(1) correlation structure \code{corAR1(~1)} or a covariate dependent \cr
#' AR(1) correlation structure \code{corAR1(~S)}.
#' See \code{\link{error.structures}} or 'Details'.
#' @param weights (optional) column name of subject-specific weights in \code{data} which need to be
#' constant across multiple measurements. Negative weights are not allowed.
#' @param coef.constraints \code{\link{vector}} or \code{\link{matrix}} of constraints on the regression coefficients. See 'Details'.
#' @param coef.values \code{\link{matrix}} setting fixed values on the regression coefficients. See 'Details'.
#' @param threshold.constraints \code{\link{vector}} of constraints on the threshold parameters. See 'Details'.
#' @param threshold.values \code{\link{list}} of (optional) fixed values for the threshold parameters. See 'Details'.
#' @param se logical, if \code{TRUE} standard errors are computed.
#' @param start.values vector of (optional) starting values.
#' @param solver character string containing the name of the applicable solver of \code{\link{optimx}} (default is \code{"BFGS"})
#'  or wrapper function for user defined solver.
#' @param PL.lag specifies the time lag of the pairs in the pairwise likelihood approach to be optimized.
#' @param control a list of control arguments. See \code{\link{optimx}}.
# #' Only applicable with \code{error.structure = corAR1}.
#'
#'
#' @examples
#' library(mvord)
#'
#' #toy example
#' data(data_toy_example)
#'
#' # convert data_toy_example into long format
#' df <- cbind.data.frame("i" = rep(1:100,2), "j" = rep(1:2,each = 100),
#'                        "Y" = c(data_toy_example$Y1,data_toy_example$Y2),
#'                        "X1" = rep(data_toy_example$X1,2),
#'                        "X2" = rep(data_toy_example$X2,2))
#'
#' res <- mvord(formula = Y ~ 0 + X1 + X2,
#'                data = df,
#'                index = c("i", "j"),
#'                link = mvprobit(),
#'                solver = "BFGS",
#'                se = TRUE,
#'                error.structure = corGeneral(~1),
#'                threshold.constraints = c(1,1),
#'                coef.constraints = c(1,1))
#' print(res)
#' summary(res)
#' thresholds(res)
#' coefficients(res)
#' get.error.struct(res)
#'
#' ## examples
#' #load data
#' data(data_mvord)
#' head(data_mvord)
#'
#' #-------------
#' # corGeneral
#' #-------------
#' \donttest{
#' # approx 1 min
#' res_cor <- mvord(formula = rating ~ 0 + X1 + X2 + X3 + X4 + X5,
#' #formula ~ 0 ... without intercept
#'                index = c("firm_id", "rater_id"),
#' #not necessary if firm_id is first column and rater is second column in data
#'                data = data_mvord, #choose data
#'                response.levels = list(c("G","F","E", "D", "C", "B", "A"),
#'                                       c("G","F","E", "D", "C", "B", "A"),
#'                                       c("O","N","M","L", "K", "J", "I", "H")),
#' #list for each rater;
#' #need to be set if specific levels/labels are desired (not in natural ordering)
#'                response.names = c("rater1", "rater2", "rater3"),
#' # set if not all raters are used and specifies ordering
#'                link = mvprobit(), #mvprobit() or mvlogit()
#'                error.structure = corGeneral(~1), #different error structures
#'                coef.constraints = cbind(c(1,2,2),
#'                                         c(1,1,2),
#'                                         c(NA,1,2),
#'                                         c(NA,NA,NA),
#'                                         c(1,1,2)),#either a vector or a matrix
#'                coef.values = cbind(c(NA,NA,NA),
#'                                    c(NA,NA,NA),
#'                                    c(0,NA,NA),
#'                                    c(1,1,1),
#'                                    c(NA,NA,NA)),
#' #matrix (possible if coef.constraints is a matrix)
#'                threshold.constraints = c(1,1,2),
#'                solver = "BFGS") #BFGS is faster
#' print(res_cor)
#' summary(res_cor)
#' thresholds(res_cor)
#' coefficients(res_cor)
#' get.error.struct(res_cor)
#'
#' #-------------
#' # covGeneral
#' #-------------
#' #approx 4 min
#' res_cov <- mvord(formula = rating ~ 1 + X1 + X2 + X3 + X4 + X5,
#' #formula ~ 0 ... without intercept
#'             index = c("firm_id", "rater_id"),
#' #not necessary if firm_id is first column and rater is second column in data
#'             data = data_mvord, #choose data
#'             response.levels = list(c("G","F","E", "D", "C", "B", "A"),
#'                                    c("G","F","E", "D", "C", "B", "A"),
#'                                    c("O","N","M","L", "K", "J", "I", "H")),
#' #list for each rater;
#' #need to be set if specific levels/labels are desired
#'             response.names = c("rater1", "rater2", "rater3"),
#' # set if not all raters are used and specifies ordering
#'             link = mvprobit(), #mvprobit() or mvlogit()
#'             error.structure = covGeneral(~1), #different error structures
#'             threshold.constraints = NULL, #vector
#'             threshold.values = list(c(-4,NA,NA,NA,NA,4.5),
#'                                     c(-4,NA,NA,NA,NA,4),
#'                                     c(-5,NA,NA,NA,NA,NA,4.5)),
#' #list for each rater
#'             solver = "newuoa") #does not converge with BFGS

#' print(res_cov)
#' summary(res_cov)
#' thresholds(res_cov)
#' coefficients(res_cov)
#' get.error.struct(res_cov)
#'
#'
#' #-------------
#' # corAR1
#' #-------------
#' #approx 4min
#' data(data_mvord_panel)
#' head(data_mvord_panel)
#' mult.obs <- 5
#' res_AR1 <- mvord(formula = rating ~ 0 + X1 + X2 + X3 + X4 + X5,
#' #formula ~ 0 ... without intercept
#'            index = c("firm_id", "year"),
#' #not necessary if firm_id is first column and rater is second column in data
#'            data = data_mvord_panel, #choose data
#'            response.levels = rep(list(c("G","F","E", "D", "C", "B", "A")), mult.obs),
#' #list for each rater;
#' #need to be set if specific levels/labels are desired (not in natural ordering)
#'            response.names = c("year3", "year4", "year5", "year6", "year7"),
#' # set if not all raters are used and specifies ordering
#'            link = mvprobit(), #mvprobit() or mvlogit()
#'            error.structure = corAR1(~1), #different error structures
#'            threshold.constraints = c(1,1,1,2,2),
#'            coef.constraints = c(1,1,1,2,2),
#'            solver = "BFGS")
#' print(res_AR1)
#' summary(res_AR1)
#' thresholds(res_AR1)
#' coefficients(res_AR1)
#' get.error.struct(res_AR1)
#' get.error.struct(res_AR1, type = "corr")
#' }
#'
#' @name mvord
#' @export
mvord <- function(formula,
                  error.structure = corGeneral(~1),
                  link = mvprobit(),
                  data,
                  index = NULL,
                  response.names = NULL,
                  response.levels = NULL,
                  coef.constraints = NULL,
                  coef.values = NULL,
                  threshold.constraints = NULL,
                  threshold.values = NULL,
                  weights = NULL, #TODO
                  se = TRUE,
                  start.values = NULL,
                  solver = "BFGS",
                  PL.lag = NULL,
                  control = list(maxit=200000, trace = 1, kkt = FALSE)
){
  #check arguments
  rho <- list()
  rho$function.name <- "mvord"
  rho$link <- link
  rho$se <- se
  rho$start.values <- start.values
  rho$solver <- solver
  rho$threshold.constraints <- threshold.constraints
  rho$threshold.values <- threshold.values
  rho$coef.constraints <- coef.constraints
  rho$coef.values <- coef.values
  rho$formula <- formula
  rho$PL.lag <- PL.lag
  rho$control <- control
  rho$mc <- match.call(expand.dots = FALSE)
  rho$index <- index
  rho$weights.name <- weights
  rho$response.levels <- response.levels
  rho$response.names <- response.names
  rho$error.structure.input <- error.structure
  nm <- names(as.list(rho$mc))

  if(!"data" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)
  if(!"link" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)
  if(!"formula" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)

  rho$response.name <- all.vars(rho$formula[[2]])
  if(!is.null(rho$response.names)){
    if(is.null(rho$index)) rho$index[1:2] <- c(1,2)
    if(!all(rho$response.names %in% unique(data[,rho$index[2]]))){
    warning("response.names do not all exist in data.", call.=FALSE)
    }}
  if(length(rho$response.name) > 1) stop("only one response needed", call.=FALSE)
  rho$x.names <- c(all.vars(rho$formula[[3]]),
                   all.vars(rho$error.structure.input$formula[[2]]))
  if(is.null(rho$index)) index <- colnames(data)[1:2]
  if(!all(rho$index %in% colnames(data))) stop("index names do not match with column names of data", call.=FALSE)
  data.mvord <- mvord.data(data, rho$index, rho$response.name, unique(c(rho$x.names, rho$weights.name)),
                               y.levels = rho$response.levels, response.names = rho$response.names)

  rho$y <- data.mvord$y
  rho$y.names <- colnames(rho$y)
  rho$ndim <- ncol(rho$y)
  rho$x <- lapply(1:rho$ndim, function(j) {
    tmp <- model.matrix(as.formula(paste0("~",deparse(rho$formula[[3]]))), data.mvord$x[[j]])
    attribute <- attr(tmp, "assign")
    tmp <- tmp[match(rownames(data.mvord$x[[j]]),rownames(tmp)),]
    rownames(tmp) <- rownames(data.mvord$x[[j]])
    attr(tmp, "assign") <- attribute
    tmp
  })
  if (is.null(rho$weights.name)) {
    rho$weights <- rep(1, nrow(rho$y))#matrix(1, nrow = rho$n, ncol = rho$ndim)
  } else {
    tmp <- sapply(data.mvord$x, function(j) as.numeric(j[,rho$weights.name]))
    rho$weights <- apply(tmp,1,function(x) unique(x[!is.na(x)]))
    if(is.list(rho$weights)) stop("Weights need to be constant across multiple measurements", call.=FALSE)
    if(any(rho$weights < 0)) stop("Weights must be non-negative", call.=FALSE)
  }

  rho$error.structure <- set.error.structure(rho$error.structure.input, data.mvord$x, rho$ndim)
  if(!is.null(rho$PL.lag) && rho$error.structure$type != "corAR1") stop("Use PL.lag only with corAR1 error.structure", call.=FALSE)
  if(!is.null(rho$PL.lag) && rho$PL.lag <= 0) stop("PL.lag must be greater than 0", call.=FALSE)

  rho$intercept <- ifelse(attr(terms.formula(rho$formula), "intercept") == 1, TRUE, FALSE)

  if(is.null(rho$coef.constraints)) rho$coef.constraints <- matrix(1:rho$ndim, ncol = ncol(rho$x[[1]]), nrow = rho$ndim)
  if(is.vector(rho$coef.constraints)) rho$coef.constraints <- matrix(rho$coef.constraints,
                                                                     ncol = length(rho$x.names) + (rho$intercept),
                                                                     nrow = rho$ndim)

  if(rho$intercept){
    rho$coef.constraints <- rho$coef.constraints[,attr(rho$x[[1]], "assign") + 1]
  } else rho$coef.constraints <- rho$coef.constraints[,attr(rho$x[[1]], "assign")]

  if(is.null(rho$coef.values)){
    rho$coef.values <- matrix(NA, ncol = ncol(rho$coef.constraints), nrow = nrow(rho$coef.constraints))
    rho$coef.values[is.na(rho$coef.constraints)] <- 0 #default 0
  } else {
    if(rho$intercept){
      rho$coef.values <- rho$coef.values[,attr(rho$x[[1]], "assign") + 1]
    } else rho$coef.values <- rho$coef.values[,attr(rho$x[[1]], "assign")]
  }
  rho$intercept.type <- ifelse(rho$intercept == FALSE, "fixed", ifelse(any(is.na(rho$coef.values[,1])), "flexible", "fixed"))

  mvord.fit(rho)
}
#-----------------------------------------------------------------------------------------------------------------
# mvord2
#-----------------------------------------------------------------------------------------------------------------
#' @title Multivariate Ordinal Regression Models with Subject Specific Covariates
#'
#' @description
#' \code{mvord2} fits a ordinal regression model for the case where the covariates do not vary\cr over the responses dimensions.
#'
#' @param formula a \code{\link{formula}} object for multivariate responses in the form of\cr
#' \code{cbind(Y1, ..., Yj) ~ X1 + ... + Xp}.
#'        Responses need to be ordered factors.
#' @param error.structure different \code{\link{error.structures}}: general correlation structure (default)\cr
#' \code{corGeneral(~1)},
#' general covariance structure \code{covGeneral(~1)}, factor dependent correlation structure \code{covGeneral(~f)},
#' factor dependent covariance structure \code{covGeneral(~f)}, covariate dependent equicorrelation structure \cr
#' \code{corEqui(~S)},
#' AR(1) correlation structure \code{corAR1(~1)} or a covariate dependent \cr
#' AR(1) correlation structure \code{corAR1(~S)}.
#' See \code{\link{error.structures}} or 'Details'.
#' @param data \code{\link{data.frame}} containing the ordinal observations and the covariates to be used in the model
#' @param link specifies the link function by \code{mvprobit()} (multivariate normally distributed errors)
#' or \code{mvlogit(df = 8)} (multivariate logistically distributed errors), where \code{df} specifies the degrees of freedom of the t copula.
#' @param coef.constraints \code{\link{vector}} or \code{\link{matrix}} of constraints on coefficients. See 'Details'.
#' @param coef.values \code{\link{matrix}} setting fixed values on the regression coefficients. See 'Details'.
#' @param threshold.constraints \code{\link{vector}} of constraints on thresholds. See 'Details'.
#' @param threshold.values (optional) \code{\link{list}} of fixed values for threshold parameters. See 'Details'.
#' @param weights (optional) column name of subject-specific weights in \code{data} which need to be
#' constant across multiple measurements. Negative weights are not allowed.
#' @param se logical, if \code{TRUE} standard errors are computed.
#' @param start.values vector of (optional) starting values.
#' @param solver character string containing the name of the applicable solver of \code{\link{optimx}} (default is \code{"BFGS"})
#' or wrapper function for user defined solver.
#' @param PL.lag specifies the time lag of the pairs in the pairwise likelihood approach to be optimized.
#' @param control a list of control arguments. See \code{\link{optimx}}.
#' @details see vignette or \code{\link{mvord}}
#' @examples
#' library(mvord)
#'
#' ## toy example
#' data(data_toy_example)
#'
#' res <- mvord2(formula = cbind(Y1,Y2) ~ 0 + X1 + X2,
#'                 data = data_toy_example,
#'                 link = mvprobit(),
#'                 solver = "BFGS",
#'                 se = TRUE,
#'                 error.structure = corGeneral(~1),
#'                 threshold.constraints = c(1,1),
#'                 coef.constraints = c(1,1))
#' print(res)
#' summary(res)
#' thresholds(res)
#' coefficients(res)
#' get.error.struct(res)
#'
#' #load data
#' data(data_mvord2)
#' head(data_mvord2)
#'
#' #-------------
#' # corGeneral
#' #-------------
#' \donttest{
#' #approx 1 min
#' res_cor <- mvord2(formula = cbind(rater1, rater2, rater3) ~ 0 + X1 + X2 + X3 + X4 + X5,
#' #formula ~ 0 ... without intercept
#'                data = data_mvord2, #choose data
#'                link = mvprobit(), #mvprobit() or mvlogit()
#'                error.structure = corGeneral(~1), #different error structures
#'                coef.constraints = cbind(c(1,2,2),
#'                                         c(1,1,2),
#'                                         c(NA,1,2),
#'                                         c(NA,NA,NA),
#'                                         c(1,1,2)),#either a vector or a matrix
#'                coef.values = cbind(c(NA,NA,NA),
#'                                    c(NA,NA,NA),
#'                                    c(0,NA,NA),
#'                                    c(1,1,1),
#'                                    c(NA,NA,NA)),
#'                                    #matrix (possible if coef.constraints is a matrix)
#'                threshold.constraints = c(1,1,2),
#'                solver = "BFGS")
#' print(res_cor)
#' summary(res_cor)
#' thresholds(res_cor)
#' coefficients(res_cor)
#' get.error.struct(res_cor)
#' }
#'
#' @name mvord2
#' @export
mvord2 <- function(formula,
                   data,
                   error.structure = corGeneral(~1),
                   link = mvprobit(),
                   coef.constraints = NULL,
                   coef.values = NULL,
                   threshold.constraints = NULL,
                   threshold.values = NULL,
                   weights = NULL,
                   se = TRUE,
                   start.values = NULL,
                   solver = "BFGS",
                   PL.lag = NULL,
                   control = list(maxit = 200000, trace = 1, kkt = FALSE)){
  #check arguments
  rho <- list()
  rho$function.name <- "mvord2"
  rho$link <- link
  rho$se <- se
  rho$start.values <- start.values
  rho$solver <- solver
  rho$threshold.constraints <- threshold.constraints
  rho$threshold.values <- threshold.values
  rho$coef.constraints <- coef.constraints
  rho$coef.values <- coef.values
  rho$formula <- formula
  rho$mc <- match.call(expand.dots = FALSE)
  nm <- names(as.list(rho$mc))
  rho$control <- control
  rho$PL.lag <- PL.lag
  rho$error.structure.input <- error.structure
  rho$weights.name <- weights
  if(!"data" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)
  if(!"link" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)
  if(!"formula" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)

  #CALL mvord2
  rho$y.names <- all.vars(rho$formula[[2]])
  rho$y <- data[,rho$y.names]
  rho$ndim <- ncol(rho$y)
  rho$x <- lapply(1:rho$ndim, function(j) model.matrix(rho$formula,
	model.frame(rho$formula, data, na.action=function(x)x)))

  rho$x.names <- c(all.vars(rho$formula[[3]]), all.vars(rho$error.structure.input$formula[[2]]))
  rho$y.names <- colnames(rho$y)
  rho$ndim <- ncol(rho$y)

  if (is.null(rho$weights.name)) {
    rho$weights <- rep(1, nrow(rho$y))
  } else {
    rho$weights <- data[,rho$weights.name]
    if(any(rho$weights < 0)) stop("Weights must be non-negative", call.=FALSE)
  }

  rho$error.structure <- set.error.structure.mvord2(rho$error.structure.input, data)
  rho$intercept <- ifelse(attr(terms.formula(rho$formula), "intercept") == 1, TRUE, FALSE)

  if(is.null(rho$coef.constraints)) rho$coef.constraints <- matrix(1:rho$ndim, ncol = length(rho$x.names) + (rho$intercept), nrow = rho$ndim)
  if(is.vector(rho$coef.constraints)) rho$coef.constraints <- matrix(rho$coef.constraints,
                                                                     ncol = length(rho$x.names) + (rho$intercept),
                                                                     nrow = rho$ndim)

  if(rho$intercept){
    rho$coef.constraints <- rho$coef.constraints[,attr(rho$x[[1]], "assign") + 1]
  } else rho$coef.constraints <- rho$coef.constraints[,attr(rho$x[[1]], "assign"), drop = F]

  if(is.null(rho$coef.values)){
    rho$coef.values <- matrix(NA, ncol = ncol(rho$coef.constraints), nrow = nrow(rho$coef.constraints))
    rho$coef.values[is.na(rho$coef.constraints)] <- 0 #default 0
  } else {
    if(rho$intercept){
      rho$coef.values <- rho$coef.values[,attr(rho$x[[1]], "assign") + 1]
    } else rho$coef.values <- rho$coef.values[,attr(rho$x[[1]], "assign"), drop = F]
  }
  rho$intercept.type <- ifelse(rho$intercept == FALSE, "fixed", ifelse(any(is.na(rho$coef.values[,1])), "flexible", "fixed"))

  mvord.fit(rho)
}
#-----------------------------------------------------------------------------------------------------------------
#
#-----------------------------------------------------------------------------------------------------------------
mvord.finalize <- function(rho){
  est <- list()
  est$theta <- rho$transf.thresholds(rho$optpar[seq_len(rho$npar.thetas)], rho)
  names(est$theta) <- rho$y.names
  for(j in 1:rho$ndim){
    names(est$theta[[j]]) <- get.labels.theta(rho,j)
  }
  if (rho$se){
    tmp <- rho$threshold.values
    est$setheta <- lapply(1:rho$ndim, function(j){
      if(rho$threshold.constraints[j] %in% rho$threshold.constraints[seq_len(j-1)]){#check threshold.constraints
        tmp[[j]][!is.na(tmp[[j-1]])] <- 0
        tmp[[j]][is.na(tmp[[j-1]])] <- rho$seGamma[rho$ind.thresholds[[j-1]]]
      } else{
        tmp[[j]][!is.na(tmp[[j]])] <- 0
        tmp[[j]][is.na(tmp[[j]])] <- rho$seGamma[rho$ind.thresholds[[j]]]
      }
      tmp[[j]]
    })

    names(est$setheta) <- rho$y.names
    for(j in 1:rho$ndim){
      names(est$setheta[[j]]) <- get.labels.theta(rho,j)
    }
  }
  #if corEqui else...
  if(rho$error.structure$type %in% c("corAR1")){
    est$alpha <- rho$optpar[rho$npar.thetas + rho$npar.betas + seq_len(rho$npar.sigmas)]
    names(est$alpha) <- colnames(rho$error.structure$x)
    est$z <- rho$error.structure$x %*% est$alpha
    colnames(est$z) <- "Fisher-z Score"
    est$error.struct <- est$alpha
    est$r <- z2r(est$z)
    colnames(est$r) <- "Correlation"
    } else if(rho$error.structure$type %in% c("corEqui")){
      est$alpha <- rho$optpar[rho$npar.thetas + rho$npar.betas + seq_len(rho$npar.sigmas)]
      names(est$alpha) <- colnames(rho$error.structure$x)
      est$z <- rho$error.structure$x %*% est$alpha
      colnames(est$z) <- "Fisher-z Score"
      est$r <- z2r(est$z)#TODO ???rho$transf.sigmas(est$alpha,rho)
      colnames(est$r) <- "Correlation"
      est$error.struct <- est$alpha
  } else {
    #spherical parametrization of correlation matrix
    if (rho$error.structure$type == "covGeneral"){
      exp.par.sd <- exp(rho$optpar[rho$npar.thetas + rho$npar.betas + rho$npar.cor * rho$ncor.levels + seq_len(rho$npar.cor.sd * rho$ncor.levels)])
      sigmas <- rho$transf.sigmas(rho$optpar[rho$npar.thetas + rho$npar.betas + seq_len(rho$npar.cor * rho$ncor.levels)],rho, exp.par.sd)
    } else {
      sigmas <- rho$transf.sigmas(rho$optpar[rho$npar.thetas + rho$npar.betas + seq_len(rho$npar.cor * rho$ncor.levels)],rho)
    }
    sigmas <- lapply(1:rho$ncor.levels, function(j){
      rownames(sigmas[[j]]) <- colnames(sigmas[[j]]) <- rho$y.names
      sigmas[[j]]
    })
    if(length(sigmas) > 1) names(sigmas) <- rho$error.structure$levels
    est$error.struct <- sigmas
  }
    if(rho$se){
      est$seerror.struct <- rho$seGamma[rho$npar.thetas + rho$npar.betas + seq_len(rho$npar.sigmas)] #seq_len(rho$npar.cor * rho$ncor.levels)]
    }
  par.beta <- rho$optpar[rho$npar.thetas + seq_len(rho$npar.betas)]
  est$beta <- sapply(1:ncol(rho$coef.constraints), function(j){
    sapply(1:nrow(rho$coef.constraints), function(i,j) ifelse(is.na(rho$ind.coef[i,j]), rho$coef.values[i,j], par.beta[rho$ind.coef[i, j]]), j)
  })
  colnames(est$beta) <- colnames(rho$x[[1]])
  rownames(est$beta) <- rho$y.names
  if(rho$se){
    se.beta <- rho$seGamma[rho$npar.thetas + seq_len(rho$npar.betas)]
    est$sebeta <- sapply(1:ncol(rho$coef.constraints), function(j){
      sapply(1:nrow(rho$coef.constraints), function(i,j) ifelse(is.na(rho$ind.coef[i,j]), 0, se.beta[rho$ind.coef[i, j]]), j)
    })
    colnames(est$sebeta) <- colnames(rho$x[[1]])
    rownames(est$sebeta) <- rho$y.names
  }
  est
}

#' @title Print Method for Multivariate Ordinal Regression Models.
#' @description Prints thresholds, regression coefficients
#'  and parameters of the error structure of class \code{"mvord"}.
#' @param x object of class \code{"mvord"}
#' @param call displays function call if \code{TRUE}
#' @param ... further arguments passed to or from other methods.
# #' @noRd
# #' @rdname mvord
#' @method print mvord
#' @export
# #' @exportMethod  print mvord
print.mvord <- function(x, call = TRUE, ...){
  if(call){
  cat("\nCall:\n",
      paste(deparse(x$rho$mc), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  }
  cat("Thresholds:\n")
  print(x$theta, ...)

  cat("Coefficients:\n")
  print(x$beta, ...)
  cat("\n")
  if(x$rho$error.structure$type %in% c("corGeneral", "covGeneral")){
    if(length(x$error.struct) == 1){
      cat("Sigma:\n")
      print(x$error.struct[[1]], names = FALSE, ...)
      } else {
        cat("Sigmas:\n")
        print(x$error.struct, names = FALSE, ...)
      }
  } else{
    if (x$rho$error.structure$formula == ~1) {
      cat("correlation parameter:\n")
      cat(x$r[1], ...)
    } else {
      cat("alpha parameters error.structure:\n")
      print(x$alpha, names = FALSE, ...)
    }
  }
  cat("\n")
  invisible(x)
}

#' @title Summary method for Multivariate Ordinal Regression Models.
#' @description Summary of thresholds, regression coefficients
#' and parameters of the error structure of class \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param short if \code{TRUE} short summary, otherwise extended summary
#' @param call displays function call if \code{TRUE}
#' @param ... further arguments passed to or from other methods.
# #' @rdname mvord
#' @method summary mvord
# #' @exportMethod  summary mvord
#' @export
summary.mvord <- function(object, short = TRUE, call = TRUE, ...){
  ntotal <- sum(object$rho$ntheta) + length(object$beta) + object$rho$npar.sigmas
  ## only for mvord2cor
  names.theta <- unlist(lapply(1:object$rho$ndim, function(j) paste(names(object$theta)[j], names(object$theta[[j]]), sep = " ")))
  names.beta <- unlist(lapply(1:ncol(object$beta), function(p){
    lapply(1:object$rho$ndim, function(j) paste(colnames(object$beta)[p], row.names(object$beta)[j], sep = " "))}))
  ind <- combn(1:object$rho$ndim,2)
  if(object$rho$error.structure$type == "covGeneral") sigmascor <- lapply(object$error.struct, cov2cor)
  ## if only one r, print r and its standard error
  if (object$rho$error.structure$type %in% c("corEqui", "corAR1") && object$rho$error.structure$formula == ~1){
    object$alpha <- object$r[1]
    names(object$alpha) <- "corr"
    ## dL/dr = dL/dalpha * dalpha/dr
    dadr <- 1/((1 + object$r[1]) * (1 - object$r[1]))
    object$seerror.struct <- object$seerror.struct/dadr
  }
  if (object$rho$error.structure$type %in% c("corGeneral", "covGeneral")) {
    if (object$rho$error.structure$formula == ~1) {
      ## correlation names
      names.corr <- paste("corr", sapply(seq_len(NCOL(ind)), function(j){
        paste(colnames(object$error.struct[[1]])[ind[1,j]], colnames(object$error.struct[[1]])[ind[2,j]], sep = " ")
            }), sep = " ")
      ## std deviation names
      names.sigma <- paste("sigma", colnames(object$error.struct[[1]]), sep = " ")
    } else { ## if factor dependent
      ## correlation names
      si <- object$error.struct
      names.corr.pair <- apply(ind, 2, function(i)
        paste(colnames(si[[1]])[i], collapse = " "))
      names.corr <- paste("corr", rep(names(si), each = NCOL(ind)),
            rep(names.corr.pair, length(si)))
      ## std deviation names
      names.sigma <- paste("sigma",  rep(names(si), each = NCOL(ind)),
                           rep(colnames(object$error.struct[[1]]), length(si)), sep = " ")
    }
  }
  names.sigma <- switch(object$rho$error.structure$type,
                        corGeneral = names.corr,
                        covGeneral = c(names.corr, names.sigma),
                        corEqui    =  names(object$alpha),
                        corAR1     =  names(object$alpha))

  coef <- switch(object$rho$error.structure$type,
                 corGeneral = as.matrix(c(unlist(object$theta), as.vector(object$beta),
                                          as.vector(sapply(seq_len(length(object$error.struct)), function(l){
                                            sapply(1:ncol(ind), function(j) object$error.struct[[l]][ind[1,j],ind[2,j]])}))), ncol = 1),
                 covGeneral = as.matrix(c(unlist(object$theta), as.vector(object$beta),
                                          as.vector(sapply(seq_len(length(object$error.struct)), function(l){
                                            sapply(1:ncol(ind), function(j) sigmascor[[l]][ind[1,j],ind[2,j]])})),
                                          as.vector(sapply(seq_len(length(object$error.struct)), function(l){
                                            sapply(1:object$rho$ndim, function(j) sqrt(object$error.struct[[l]][j,j]))})))
                                        , ncol = 1),
                 corEqui    = as.matrix(c(unlist(object$theta), as.vector(object$beta),object$alpha), ncol = 1),
                 corAR1     = as.matrix(c(unlist(object$theta), as.vector(object$beta),object$alpha), ncol = 1))
  rownames(coef) <- c(names.theta, names.beta, names.sigma)
  colnames(coef) <- c("Estimate")
  if(object$rho$se){
    tmp <- matrix(0, ntotal, 4,
                  dimnames = list(c(names.theta, names.beta, as.vector(names.sigma)),
                                  c("Estimate", "Std. Error", "z value", "Pr(>|z|)")))
    tmp[, 1] <- coef
    tmp[, 2] <- c(unlist(object$setheta), as.vector(object$sebeta), object$seerror.struct)
    tmp[, 3] <- tmp[, 1]/tmp[, 2]
    tmp[, 4] <- 2 * pnorm(abs(tmp[, 3]), lower.tail=FALSE)
    signif <- ifelse(tmp[,4] >=  0.1," ",
                     ifelse(tmp[,4] >=  0.05,".",
                            ifelse(tmp[,4] >=  0.01,"*",
                                   ifelse(tmp[,4] >=  0.001,"**","***"))))
    coef <- cbind.data.frame(tmp, signif)
    coef[tmp[, 2] == 0, 3] <- NA
    coef[tmp[, 2] == 0, 4] <- NA
    coef[tmp[, 2] == 0, 5] <- NA
  }
  mat <- cbind.data.frame(c("link",object$rho$link$name), c("threshold",object$rho$threshold),
                          c("nsubjects", object$rho$n), c("ndim", object$rho$ndim),
                          c("logPL", round(-object$rho$objective,2)),c("CLAIC", ifelse(object$rho$se,round(object$rho$claic,2),NA)),
                          c("CLBIC", ifelse(object$rho$se,round(object$rho$clbic,2),NA)),
                          c("fevals", if(is.null(object$rho$optRes$fevals)) NA else object$rho$optRes$fevals))
  summary.output <- list()
  if(call){
    summary.output$call <-  paste(deparse(object$rho$mc), sep = "\n", collapse = "\n")
    cat("\nCall: ",
        summary.output$call, "\n\n", sep = "")
  }
  cat("Formula: ")
  summary.output$formula <- print(object$rho$formula)
  cat("\n")
  summary.output$info <- format(mat, justify="right")
  write.table(summary.output$info, row.names = FALSE, col.names = FALSE, quote = FALSE)
  cat("\n")
  if(short){
    tmp.duplicated <- duplicated(lapply(object$theta, unname))
    len.thresh <- object$rho$ntheta
    lower.ind <- cumsum(c(1, len.thresh[-length(len.thresh)]))
    upper.ind <- cumsum(len.thresh)
    tmp.ind <- lapply(seq_len(length(len.thresh)), function(j) if(tmp.duplicated[j] == FALSE) seq(lower.ind[j], upper.ind[j]) else c())
    cat("Thresholds:\n")
    summary.output$thresholds <- print(coef[unlist(tmp.ind),], ...)

    cat("\nCoefficients:\n")
    tmp.duplicated <- duplicated(object$rho$ind.coef)
    ind.beta <- matrix(seq_len(length(names.beta)), ncol = ncol(object$rho$ind.coef), nrow = nrow(object$rho$ind.coef))
    tmp.ind <- length(names.theta) + as.vector(ind.beta[!tmp.duplicated,])
    summary.output$coefficients <- print(coef[tmp.ind,], ...)
  } else{
    cat("Thresholds:\n")
    summary.output$thresholds <- print(coef[seq_len(length(names.theta)),], ...)
    cat("\nCoefficients:\n")
    summary.output$coefficients <- print(coef[length(names.theta) + seq_len(length(names.beta)),], ...)
  }
  cat("\nError Structure:\n")
  summary.output$error.structure <- print(coef[length(names.theta) + length(names.beta) + seq_len(length(names.sigma)),], ...)
  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  class(summary.output) <- "summary.mvord"
  invisible(summary.output)
}

print.summary.mvord <- function(summary.output, ...){
  if(!is.null(summary.output)){
    cat("\nCall: ",
        summary.output$call, "\n\n", sep = "")
  }
  cat("Formula: ")
  print(summary.output$formula, ...)
  cat("\n")
  write.table(summary.output$info, row.names = FALSE, col.names = FALSE, quote = FALSE)
  cat("\n")
    cat("Thresholds:\n")
    print(summary.output$thresholds, ...)
    cat("\nCoefficients:\n")
    print(summary.output$coefficients, ...)
  cat("\nError Structure:\n")
  print(summary.output$error.structure, ...)
  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' @title Coefficients of Multivariate Ordinal Regression Models.
#' @description \code{coef} is a generic function which extracts
#'  regression coefficients from objects of class \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param ... further arguments passed to or from other methods.
# #' @noRd
# #' @export
# coef <- function(object, ...) UseMethod("coef")
# #' @rdname mvord
#' @method coef mvord
# ' @exportMethod coef mvord
#' @export
# #' @exportMethod  coef mvord
coef.mvord <- function(object, ...) object$beta

#' @title Thresholds of Multivariate Ordinal Regression Models.
#' @description
#' \code{thresholds} is a generic function which extracts threshold coefficients from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param ... further arguments passed to or from other methods.
# #' @rdname mvord
#' @export
thresholds <- function(object, ...) UseMethod("thresholds")
#' @rdname thresholds
#' @export
thresholds.mvord <- function(object, ...) object$theta

#' @title Extracts Error Structure of Multivariate Ordinal Regression Models.
#' @description
#' \code{get.error.struct} is a generic function which extracts the estimated error structure parameters from objects of class \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param type choose type \code{c("sigmas", "alpha", "corr", "z")}
#' @param ... further arguments passed to or from other methods.
#' @details \itemize{
#' \item{\code{sigmas}} {extracts the correlation/covariance matrices corresponding to each subject.
#'             Applicable in line with \code{corGeneral, covGeneral, corEqui, corAR1}}
#' \item{\code{alpha}} {extracts the parameters of covariate dependent error structure. Applicable in line with \code{corEqui, corAR1}}
#' \item{\code{corr}} {extracts the subject-specific correlation parameters. Applicable in line with \code{corEqui, corAR1}}
#' \item{\code{z}} {extracts the subject-specific Fisher-z score. Applicable in line with \code{corEqui, corAR1}}}
#' @export
get.error.struct <- function(object, type, ...) UseMethod("get.error.struct")
#' @rdname get.error.struct
#' @export
get.error.struct.mvord <- function(object, type = NULL, ...){
  if(is.null(type)){
  if(object$rho$error.structure$type %in% c("corGeneral", "covGeneral")){
    if(length(object$error.struct) == 1){
      object$error.struct[[1]]
    } else {
      object$error.struct
    }
  } else object$alpha
  } else{ #here for each i
    if(type == "sigmas"){
      #object$error.struct
      if(object$rho$error.structure$type %in% c("corGeneral", "covGeneral")){
      lev <- match(object$rho$error.structure$x, object$rho$error.structure$levels)
      sigmas <- lapply(seq_len(object$rho$n), function(i) object$error.struct[[lev[i]]])
      names(sigmas) <- rownames(object$rho$y)
      sigmas
      } else {sigmas <- r2sigma(object$r, object)
      names(sigmas) <- rownames(object$rho$y)
      sigmas
        }
    } else if(type == "corr"){
      object$r
    } else object[[type]]
  }
}


#' @title CLAIC of Multivariate Ordinal Regression Models.
#' @description
#' \code{claic} is a generic function which extracts the composite likelihood AIC from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @export
claic <- function(object) UseMethod("claic")
#' @rdname claic
#' @export
claic.mvord <- function(object) ifelse(object$rho$se, object$rho$claic, NA)


#' @title CLBIC of Multivariate Ordinal Regression Models.
#' @description
#' \code{clbic} is a generic function which extracts the composite likelihood BIC from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @export
clbic <- function(object) UseMethod("clbic")
#' @rdname clbic
#' @export
clbic.mvord <- function(object) ifelse(object$rho$se, object$rho$clbic, NA)


#' @title logPL of Multivariate Ordinal Regression Models.
#' @description
#' \code{logPL} is a generic function which extracts the log pairwise likelihood from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @export
logPL <- function(object) UseMethod("logPL")
#' @rdname logPL
#' @export
logPL.mvord <- function(object) as.numeric(-object$rho$objective)
