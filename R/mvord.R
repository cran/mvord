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
#' @importFrom stats formula update model.frame model.matrix coef as.formula cov2cor dnorm dt pnorm pt terms.formula plogis dlogis qt nobs predict terms printCoefmat model.offset na.omit
#' @importFrom pbivnorm pbivnorm
#' @importFrom MASS polr
#' @importFrom utils combn data write.table
#' @importFrom mnormt sadmvn sadmvt
#' @importFrom Matrix bdiag
#' @importFrom numDeriv grad hessian



#############################################################################################
#' @title Multivariate Ordinal Regression Models.
#'
#' @description
#' \code{mvord} is used to estimate multivariate ordinal regression models. Different model types are implemented and can be chosen by
#' the use of different \code{error.structures}. Constraints on the threshold as well as on the regression parameters can be imposed.
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
#'   \item \code{cor_general}
#' The most common parameterization is the general correlation matrix.
#'
#'  \code{error.structure = cor_general(~ 1)}
#'
#' This parameterization can be extended by allowing a factor dependent
#' correlation structure, where the correlation of each subject \eqn{i} depends
#' on a given subject-specific factor \code{f}. This factor \code{f} is not allowed to vary
#' across multiple measurements \eqn{j} for the same subject \eqn{i} and due to numerical
#' constraints only up to maximum 30 levels are allowed.
#'
#'       \code{error.structure = cor_general(~ f)}
#'
#'   \item \code{cor_equi}
#' A covariate dependent equicorrelation structure, where the correlations
#' are equal across all \eqn{J} dimensions and depend on subject-specific covariates \code{S1, ..., Sm}.
#' It has to be noted that these covariates \code{S1, ..., Sm} are not allowed to vary across
#'  multiple measurements \eqn{j} for the same subject \eqn{i}.
#'
#'          \code{error.structure = cor_equi(~ S1 + ... + Sm)}
#'
#'   \item \code{cor_ar1}
#' In order to account for some heterogeneity the \eqn{AR(1)} error structure
#' is allowed to depend on covariates \code{X1, ..., Xp} that are constant
#' over time for each subject \eqn{i}.
#'
#'       \code{error.structure = cor_ar1(~ S1 + ... + Sm)}
#'}
#'
#'
#'\item {Covariance:}
#'\itemize{
#'\item \code{cov_general}
#'
#' In case of a full variance-covariance parameterization the standard parameterization
#'  with a full variance-covariance is obtained by:
#'
#'  \code{error.structure = cov_general(~ 1)}
#'
#'  This parameterization can be extended to the factor dependent covariance structure,
#'   where the covariance of each subject depends on a given factor \code{f}:
#'
#'  \code{error.structure = cov_general(~ f)}
#'   }
#'   }
#'   }


#'
#'   \item{\code{coef.constraints}}{
#'   The package supports
#'   constraints on the regression coefficients. Firstly, the
#'   user can specify whether the regression coefficients should be equal
#'   across some or all response dimensions. Secondly, the values of some
#'   of the regression coefficients can be fixed.
#'
#'   As there is no unanimous way to specify such constraints, we offer
#'   two options. The first option is similar to the specification of constraints on the thresholds.
#'    The constraints can be specified in this case as a vector or matrix of integers,
#'     where coefficients getting same integer value are set equal.
#'   Values of the regression coefficients can be fixed through a matrix.
#'   Alternatively constraints on the regression coefficients can be specified
#'   by using the design employed by the \pkg{VGAM} package.
#'   The constraints in this setting are set through a named list,
#'   where each element of the list contains a matrix full-column rank.
#'   If the values of some regression coefficients should be fixed, offsets can be used.
#'   This design has the advantage that it supports
#'   constraints on outcome-specific as well as category-specific
#'   regression coefficients. While the first option has the advantage of requiring a more concise input,
#'    it does not support category-specific coefficients.
#'   The second option offers a more flexible design in this respect. For further information
#'   on the second option we refer to the vignette and to the documentation of \code{\link[VGAM]{vglm}}.
#'
#' Using the first option, constraints can be specified by a vector or a matrix \cr
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
#' \code{get_error_struct} returns the estimated parameters of the corresponding error structure.
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
#'   an object of class \code{\link{error_struct}} containing the parameters of the error
#'   structure}
#'   \item{\code{sebeta}}{
#'
#'     a named \code{\link{matrix}} of the standard errors of the regression coefficients}
#'   \item{\code{setheta}}{
#'
#'     a named \code{\link{list}} of the standard errors of the threshold parameters}
#'   \item{\code{seerror.struct}}{
#'
#'   a \code{vector} of standard errors for the parameters of the error structure}
#'   \item{\code{rho}}{
#'
#'     a \code{\link{list}} of all objects that are used in \code{mvord()}}
#' }
#'
#' @seealso %\code{\link{predict.mvord}},
#' \code{\link{print.mvord}}, \code{\link{summary.mvord}}, \code{\link{coef.mvord}},
#'  \code{\link{thresholds.mvord}}, \code{\link{get_error_struct.mvord}},
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
#' @param error.structure different \code{error.structures}: general correlation structure (default)\cr
#' \code{cor_general(~1)},
#' general covariance structure \code{cov_general(~1)}, factor dependent correlation structure \code{cov_general(~f)},
#' factor dependent covariance structure \code{cov_general(~f)}, covariate dependent equicorrelation structure \cr
#' \code{cor_equi(~S)},
#' AR(1) correlation structure \code{cor_ar1(~1)} or a covariate dependent \cr
#' AR(1) correlation structure \code{cor_ar1(~S)}.
#' See \code{\link{error_struct}} or 'Details'.
#' @param weights (optional) column name of subject-specific weights in \code{data} which need to be
#' constant across multiple measurements. Negative weights are not allowed.
#' @param offset this can be used to specify an a priori known component to be included in the linear predictor during fitting.
#'  This should be NULL or a numeric vector of length equal to the number of cases. One or more offset terms can be included
#'  in the formula instead or as well, and if more than one is specified their sum is used. See model.offset.
#' @param scale If \code{scale = TRUE}, the continuous covariates are standardized
#'  by substracting the mean and dividing by the standard deviation.
#'  This operation is performed for each repeated measurement before fitting.
#' @param coef.constraints \code{\link{vector}} or \code{\link{matrix}} of constraints on the regression coefficients. See 'Details'.
#' @param coef.values \code{\link{matrix}} setting fixed values on the regression coefficients. See 'Details'.
#' @param threshold.constraints \code{\link{vector}} of constraints on the threshold parameters. See 'Details'.
#' @param threshold.values \code{\link{list}} of (optional) fixed values for the threshold parameters. See 'Details'.
#' @param se logical, if \code{TRUE} standard errors are computed.
#' @param start.values vector of (optional) starting values.
#' @param solver character string containing the name of the applicable solver of \code{\link{optimx}} (default is \code{"newuoa"})
#'  or wrapper function for user defined solver.
#' @param PL.lag specifies the time lag of the pairs in the pairwise likelihood approach to be optimized.
#' @param control a list of control arguments. See \code{\link{optimx}}.
# #' Only applicable with \code{error.structure = cor_ar1}.
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
#'                error.structure = cor_general(~1),
#'                threshold.constraints = c(1,1),
#'                coef.constraints = c(1,1))
#' print(res)
#' summary(res)
#' thresholds(res)
#' coefficients(res)
#' get_error_struct(res)
#'
#' ## examples
#' #load data
#' data(data_mvord)
#' head(data_mvord)
#'
#' #-------------
#' # cor_general
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
#'                error.structure = cor_general(~1), #different error structures
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
#' get_error_struct(res_cor)
#'
#' #-------------
#' # cov_general
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
#'             error.structure = cov_general(~1), #different error structures
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
#' get_error_struct(res_cov)
#'
#'
#' #-------------
#' # cor_ar1
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
#'            error.structure = cor_ar1(~1), #different error structures
#'            threshold.constraints = c(1,1,1,2,2),
#'            coef.constraints = c(1,1,1,2,2),
#'            solver = "BFGS")
#' print(res_AR1)
#' summary(res_AR1)
#' thresholds(res_AR1)
#' coefficients(res_AR1)
#' get_error_struct(res_AR1)
#' get_error_struct(res_AR1, type = "corr")
#' }
#'
#' @name mvord
#' @export
mvord <- function(formula,
                  error.structure = cor_general(~1),
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
                  offset = NULL,
                  scale = FALSE,
                  se = TRUE,
                  start.values = NULL,
                  solver = "newuoa",
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
  rho$scale <- scale
  rho$offset <- offset
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
                   all.vars(formula(error.structure)[[2]]))
  if(is.null(rho$index)) index <- colnames(data)[1:2]
  if(!all(rho$index %in% colnames(data))) stop("index names do not match with column names of data", call.=FALSE)

  if(rho$scale) {
    data.classes <- sapply(data, class)
    scale_names <- rho$x.names[rho$x.names %in% names(data.classes[data.classes == "numeric"])]
    data <- do.call("rbind.data.frame",
                                    by(data, data[, rho$index[2]],
                                       function(x){x[, scale_names] <- scale(x[, scale_names]); x}))
  }

  data.mvord <- mvord_data(data, rho$index, rho$response.name, unique(c(rho$x.names, rho$weights.name)),
                               y.levels = rho$response.levels, response.names = rho$response.names)

  rho$rownames.constraints <- unlist(data.mvord$ylevels)
  rho$intercept <- ifelse(attr(terms.formula(rho$formula), "intercept") == 1, TRUE, FALSE)

  rho$y <- data.mvord$y
  rho$y.names <- colnames(rho$y)
  rho$ndim <- ncol(rho$y)
  rho$x <- lapply(1:rho$ndim, function(j) {
  	rhs.form <- as.formula(paste0("~",deparse(rho$formula[[3]])))
  	new.rhs.form <- update(rhs.form, ~ . + 1)
  	tmp <-  model.matrix(new.rhs.form, model.frame(new.rhs.form,  data.mvord$x[[j]],
      na.action = function(x) x))

    attribute <- attr(tmp, "assign")

    if(rho$intercept == FALSE){
    attribute <- attribute[-1]
    tmp <- tmp[,-1, drop = F]
    }

    tmp <- tmp[match(rownames(data.mvord$x[[j]]), rownames(tmp)), , drop = FALSE]
    rownames(tmp) <- rownames(data.mvord$x[[j]])
    attr(tmp, "assign") <- attribute
    tmp
  })
  if (is.null(rho$offset)) {
    rho$offset <- lapply(1:rho$ndim, function(j) {
      mf <- model.frame(as.formula(paste0("~",deparse(rho$formula[[3]]))), data.mvord$x[[j]],
                        na.action = function(x) x)
      mf[is.na(mf)] <- 0
      model.offset(mf)
  })
  }
  if (is.null(rho$weights.name)) {
    rho$weights <- rep(1, nrow(rho$y))
  } else {
    tmp <- sapply(data.mvord$x, function(j) as.numeric(j[,rho$weights.name]))
    rho$weights <- apply(tmp,1,function(x) unique(x[!is.na(x)]))
    if(is.list(rho$weights)) stop("Weights need to be constant across multiple measurements", call.=FALSE)
    if(any(rho$weights < 0)) stop("Weights must be non-negative", call.=FALSE)
  }
  ## initialize error structure
  rho$error.structure <- init_fun(error.structure, data.mvord)


  if(!is.null(rho$PL.lag) && rho$error.structure$name != "cor_ar1") stop("Use PL.lag only with cor_ar1 error.structure", call.=FALSE)
  if(!is.null(rho$PL.lag) && rho$PL.lag <= 0) stop("PL.lag must be greater than 0", call.=FALSE)


  if(is.null(rho$coef.constraints)) rho$coef.constraints <- matrix(1:rho$ndim, ncol = ncol(rho$x[[1]]), nrow = rho$ndim)

  if(is.list(rho$coef.constraints) && !is.null(rho$coef.values)) stop("This coef.constraints design requires offsets instead of coef.values.", call.=FALSE)

  if(!is.list(rho$coef.constraints)){
  if(is.vector(rho$coef.constraints)) rho$coef.constraints <- matrix(rho$coef.constraints,
                                                                     ncol = ncol(rho$x[[1]]),
                                                                     nrow = rho$ndim)

  if(is.null(rho$coef.values)){
    rho$coef.values <- matrix(NA, ncol = ncol(rho$coef.constraints), nrow = nrow(rho$coef.constraints))
    rho$coef.values[is.na(rho$coef.constraints)] <- 0 #default 0
  }
  #check if coef.values fit to coef.constraints
  check_args_coef(rho)
  # set coef.constraints NA where coef.values are set
  rho$coef.constraints[!is.na(rho$coef.values)] <- NA
  rho$intercept.type <- ifelse(rho$intercept == FALSE, "fixed", ifelse(any(is.na(rho$coef.values[,1])), "flexible", "fixed"))
  } else rho$intercept.type <- "fixed" #TODO ?

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
#' @param error.structure different \code{error.structures}: general correlation structure (default)\cr
#' \code{cor_general(~1)},
#' general covariance structure \code{cov_general(~1)}, factor dependent correlation structure \code{cov_general(~f)},
#' factor dependent covariance structure \code{cov_general(~f)}, covariate dependent equicorrelation structure \cr
#' \code{cor_equi(~S)},
#' AR(1) correlation structure \code{cor_ar1(~1)} or a covariate dependent \cr
#' AR(1) correlation structure \code{cor_ar1(~S)}.
#' See \code{\link{error_struct}} or 'Details'.
#' @param data \code{\link{data.frame}} containing the ordinal observations and the covariates to be used in the model
#' @param link specifies the link function by \code{mvprobit()} (multivariate normally distributed errors)
#' or \code{mvlogit(df = 8)} (multivariate logistically distributed errors), where \code{df} specifies the degrees of freedom of the t copula.
#' @param coef.constraints \code{\link{vector}} or \code{\link{matrix}} of constraints on coefficients. See 'Details'.
#' @param coef.values \code{\link{matrix}} setting fixed values on the regression coefficients. See 'Details'.
#' @param threshold.constraints \code{\link{vector}} of constraints on thresholds. See 'Details'.
#' @param threshold.values (optional) \code{\link{list}} of fixed values for threshold parameters. See 'Details'.
#' @param weights (optional) column name of subject-specific weights in \code{data} which need to be
#' constant across multiple measurements. Negative weights are not allowed.
#' @param offset this can be used to specify an a priori known component to be included in the linear predictor during fitting.
#'  This should be NULL or a numeric vector of length equal to the number of cases. One or more offset terms can be included
#'  in the formula instead or as well, and if more than one is specified their sum is used. See model.offset.
#' @param scale If \code{scale = TRUE}, the continuous covariates are standardized by substracting the mean and dividing by the standard deviation.
#'  This operation is performed for each repeated measurement before fitting.
#' @param se logical, if \code{TRUE} standard errors are computed.
#' @param start.values vector of (optional) starting values.
#' @param solver character string containing the name of the applicable solver of \code{\link{optimx}} (default is \code{"newuoa"})
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
#'                 error.structure = cor_general(~1),
#'                 threshold.constraints = c(1,1),
#'                 coef.constraints = c(1,1))
#' print(res)
#' summary(res)
#' thresholds(res)
#' coefficients(res)
#' get_error_struct(res)
#'
#' #load data
#' data(data_mvord2)
#' head(data_mvord2)
#'
#' #-------------
#' # cor_general
#' #-------------
#' \donttest{
#' #approx 1 min
#' res_cor <- mvord2(formula = cbind(rater1, rater2, rater3) ~ 0 + X1 + X2 + X3 + X4 + X5,
#' #formula ~ 0 ... without intercept
#'                data = data_mvord2, #choose data
#'                link = mvprobit(), #mvprobit() or mvlogit()
#'                error.structure = cor_general(~1), #different error structures
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
#' get_error_struct(res_cor)
#' }
#'
#' @name mvord2
#' @export
mvord2 <- function(formula,
                   data,
                   error.structure = cor_general(~1),
                   link = mvprobit(),
                   coef.constraints = NULL,
                   coef.values = NULL,
                   threshold.constraints = NULL,
                   threshold.values = NULL,
                   weights = NULL,
                   offset = NULL,
                   scale = FALSE,
                   se = TRUE,
                   start.values = NULL,
                   solver = "newuoa",
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
  rho$weights.name <- weights
  rho$scale <- scale
  rho$offset <- offset

  if(!"data" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)
  if(!"link" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)
  if(!"formula" %in% nm) stop("Model needs formula, data and link.", call.=FALSE)
  #CALL mvord2
  rho$y.names <- all.vars(rho$formula[[2]])
  rho$y <- data[,rho$y.names]
  rho$ndim <- ncol(rho$y)

  rho$x.names <- c(all.vars(rho$formula[[3]]), all.vars(formula(error.structure)[[2]]))

  if(rho$scale) {
    data.classes <- sapply(data, class)
    scale_names <- rho$x.names[rho$x.names %in% names(data.classes[data.classes == "numeric"])]
    data[, scale_names] <- scale(data[, scale_names])
  }

  rho$intercept <- ifelse(attr(terms.formula(rho$formula), "intercept") == 1, TRUE, FALSE)
  rho$x <- lapply(1:rho$ndim, function(j) {
  	rhs.form <- as.formula(paste0("~",deparse(rho$formula[[3]])))
  	new.rhs.form <- update(rhs.form, ~ . + 1)
  	tmp <-  model.matrix(new.rhs.form,
  		model.frame(new.rhs.form,  data, na.action = function(x) x))
    attribute <- attr(tmp, "assign")

    if(rho$intercept == FALSE){
    attribute <- attribute[-1]
    tmp <- tmp[,-1, drop = F]
    }

    tmp <- tmp[match(rownames(data), rownames(tmp)), , drop = FALSE]
    rownames(tmp) <- rownames(data)
    attr(tmp, "assign") <- attribute
    tmp
  })

 #  if(rho$intercept){
  #  rho$x <- lapply(1:rho$ndim, function(j) model.matrix(rho$formula,
#	    model.frame(rho$formula, data, na.action=function(x)x)))
#  } else {
 #   rho$x <- lapply(1:rho$ndim, function(j){
 #   	tmp <- model.matrix(update(rho$formula, ~ . + 1),
 #         model.frame(rho$formula, data, na.action=function(x)x))[,-1, drop = F]
 #
 #   })
 # }

  if (is.null(rho$offset)) {
    rho$offset <- lapply(1:rho$ndim, function(j) {
      mf <- model.frame(rho$formula, data, na.action = function(x) x)
      model.offset(mf)
    })
  }

  if (is.null(rho$weights.name)) {
    rho$weights <- rep(1, nrow(rho$y))
  } else {
    rho$weights <- data[,rho$weights.name]
    if(any(rho$weights < 0)) stop("Weights must be non-negative", call.=FALSE)
  }
  data.mvord2 <- list(y = data[, rho$y.names],
  	                  x = rep(list(data[, rho$x.names]), rho$ndim))

  rho$error.structure <- init_fun(error.structure, data.mvord2)


  if(is.null(rho$coef.constraints)) rho$coef.constraints <- matrix(1:rho$ndim,
                                                                   ncol = ncol(rho$x[[1]]),
                                                                   nrow = rho$ndim)

  if(is.list(rho$coef.constraints) && !is.null(rho$coef.values)) stop("This coef.constraints design requires offsets instead of coef.values.", call.=FALSE)

  if(!is.list(rho$coef.constraints)){
    if(is.vector(rho$coef.constraints)) rho$coef.constraints <- matrix(rho$coef.constraints,
                                                                       ncol = ncol(rho$x[[1]]),
                                                                       nrow = rho$ndim)

    if(is.null(rho$coef.values)){
      rho$coef.values <- matrix(NA, ncol = ncol(rho$coef.constraints), nrow = nrow(rho$coef.constraints))
      rho$coef.values[is.na(rho$coef.constraints)] <- 0 #default 0
    }
    #check if coef.values fit to coef.constraints
    check_args_coef(rho)
    # set coef.constraints NA  coef.values are set
    rho$coef.constraints[!is.na(rho$coef.values)] <- NA
    rho$intercept.type <- ifelse(rho$intercept == FALSE, "fixed", ifelse(any(is.na(rho$coef.values[,1])), "flexible", "fixed"))
  } else rho$intercept.type <- "fixed" #TODO ?

  mvord.fit(rho)
}
#-----------------------------------------------------------------------------------------------------------------
#
#-----------------------------------------------------------------------------------------------------------------
mvord_finalize <- function(rho){
  est <- list()

  ## Thresholds
  est$theta <- rho$transf_thresholds(rho$optpar[seq_len(rho$npar.thetas)], rho)
  names(est$theta) <- rho$y.names
  for(j in 1:rho$ndim){
    names(est$theta[[j]]) <- get_labels_theta(rho,j)
  }
  tmp <- rho$threshold.values
  ## Regression coefficients
  est$beta <- rho$optpar[rho$npar.thetas + seq_len(rho$npar.betas)]
  names(est$beta) <- unlist(lapply(1:length(rho$constraints), function(p) colnames(rho$constraints[[p]])))
  ## Error Structure
  tparsigma <- rho$optpar[rho$npar.thetas + rho$npar.betas +  seq_len(attr(rho$error.structure, "npar"))]
  est$error.struct <- finalize(rho$error.structure, tparsigma)

  if (rho$se) {
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
    names(est$setheta[[j]]) <- get_labels_theta(rho,j)
  }

  est$sebeta <- rho$seGamma[rho$npar.thetas + seq_len(rho$npar.betas)]
  names(est$sebeta) <- unlist(lapply(1:length(rho$constraints), function(p) colnames(rho$constraints[[p]])))

  est$seerror.struct <- rho$seGamma[rho$npar.thetas + rho$npar.betas +  seq_len(attr(rho$error.structure, "npar"))]
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
  cat("Parameters of the error structure:\n")
  print(attr(x$error.struct, "par"), ...)
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
  ntotal <-  sum(object$rho$ntheta) + length(object$beta) +
    attr(object$error.struct, "npar")

  names.theta <- unlist(lapply(1:object$rho$ndim, function(j) paste(names(object$theta)[j], names(object$theta[[j]]))))
  names.beta <- names(object$beta)
  ind <- combn(1:object$rho$ndim,2)

  names.sigma <- attr(object$error.struct, "parnames")

  coef <- as.matrix(c(unlist(object$theta), as.vector(object$beta),
  	attr(object$error.struct, "par")), ncol = 1)
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
    coef <- cbind.data.frame(tmp)
    coef[tmp[, 2] == 0, 3] <- NA
    coef[tmp[, 2] == 0, 4] <- NA
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
    tmp.ind <- lapply(seq_along(len.thresh), function(j) if(tmp.duplicated[j] == FALSE) seq(lower.ind[j], upper.ind[j]) else c())
    cat("Thresholds:\n")
    if(object$rho$se) {
      summary.output$thresholds <- printCoefmat(coef[unlist(tmp.ind),])
    } else summary.output$thresholds <- print(coef[unlist(tmp.ind),], ...)

    cat("\nCoefficients:\n")
    tmp.ind <- length(names.theta) + seq_along(names.beta)
    if(object$rho$se) {
      summary.output$coefficients <- printCoefmat(coef[unlist(tmp.ind),])
    } else summary.output$coefficients <- print(coef[unlist(tmp.ind),], ...)
  } else{
    cat("Thresholds:\n")
    if(object$rho$se) {
      summary.output$thresholds <- printCoefmat(coef[seq_along(names.theta),])
    } else summary.output$thresholds <- print(coef[seq_along(names.theta),], ...)

    cat("\nCoefficients:\n")
    if(object$rho$se) {
      summary.output$coefficients <- printCoefmat(coef[length(names.theta) + seq_along(names.beta),])
    } else summary.output$coefficients <- print(coef[length(names.theta) + seq_along(names.beta),], ...)
  }
  cat("\nError Structure:\n")
    if(object$rho$se) {
    summary.output$error.structure <- printCoefmat(coef[length(names.theta) + length(names.beta) + seq_along(names.sigma),])
  } else summary.output$error.structure <- print(coef[length(names.theta) + length(names.beta) + seq_along(names.sigma),], ...)
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
    if(ncol(summary.output$thresholds) > 1) printCoefmat(summary.output$thresholds) else print(summary.output$thresholds, ...)
    cat("\nCoefficients:\n")
    if(ncol(summary.output$coefficients) > 1) print(summary.output$coefficients) else print(summary.output$coefficients, ...)
  cat("\nError Structure:\n")
  if(ncol(summary.output$error.structure) > 1) printCoefmat(summary.output$error.structure) else print(summary.output$error.structure, ...)

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

# #' @title CLAIC of Multivariate Ordinal Regression Models.
# #' @description
# #' \code{claic} is a generic function which extracts the composite likelihood AIC from objects of class \cr
# #' \code{"mvord"}.
# #' @param object object of class \code{"mvord"}
# #' @export
claic <- function(object) UseMethod("claic")
# #' @rdname claic
# #' @export
claic.mvord <- function(object) ifelse(object$rho$se, object$rho$claic, NA)


# #' @title CLBIC of Multivariate Ordinal Regression Models.
# #' @description
# #' \code{clbic} is a generic function which extracts the composite likelihood BIC from objects of class \cr
# #' \code{"mvord"}.
# #' @param object object of class \code{"mvord"}
# #' @export
clbic <- function(object) UseMethod("clbic")
# #' @rdname clbic
# #' @export
clbic.mvord <- function(object) ifelse(object$rho$se, object$rho$clbic, NA)


# #' @title logPL of Multivariate Ordinal Regression Models.
# #' @description
# #' \code{logPL} is a generic function which extracts the log pairwise likelihood from objects of class \cr
# #' \code{"mvord"}.
# #' @param object object of class \code{"mvord"}
# #' @export
logPL <- function(object) UseMethod("logPL")
# #' @rdname logPL
# #' @export
logPL.mvord <- function(object) as.numeric(-object$rho$objective)


#object <- res_AR1_probit
#' @title nobs of Multivariate Ordinal Regression Models.
#' @description
#' \code{nobs} is a generic function which extracts the number of observations from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param ... further arguments passed to or from other methods.
#' @method nobs mvord
#' @export
nobs.mvord <- function(object, ...) object$rho$n

#' @title vcov of Multivariate Ordinal Regression Models.
#' @description
#' \code{vcov} is a generic function which extracts the Godambe information matrix from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param ... further arguments passed to or from other methods.
#' @method vcov mvord
#' @export
vcov.mvord <- function(object, ...) object$rho$varGamma

#' @title terms of Multivariate Ordinal Regression Models.
#' @description
#' \code{terms} is a generic function which can be used to extract terms from objects of class \cr
#' \code{"mvord"}.
#' @param x object of class \code{"mvord"}
#' @param ... further arguments passed to or from other methods.
#' @method terms mvord
#' @export
terms.mvord <- function(x, ...) terms(x$rho$formula)

#' @title model.matrix of Multivariate Ordinal Regression Models.
#' @description
#' \code{model.matrix} is a generic function which extracts the Godambe information matrix from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param ... further arguments passed to or from other methods.
#' @method model.matrix mvord
#' @export
model.matrix.mvord <- function(object, ...) object$rho$x

#' @title fitted of Multivariate Ordinal Regression Models.
#' @description
#' \code{fitted} is a generic function which extracts fitted probabilities for the observed categories from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param ... further arguments passed to or from other methods.
#' @method fitted mvord
#' @export
fitted.mvord <- function(object, ...) predict(object)

#' @title log Pairwise Likelihood of Multivariate Ordinal Regression Models.
#' @description
#' \code{logLik} is a generic function which extracts the log pairwise likelihood from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @param ... further arguments passed to or from other methods.
#' @method logLik mvord
#' @export
logLik.mvord <- function(object, ...) structure(-object$rho$objective,
                                                df = sum(diag(object$rho$V %*% object$rho$H.inv)), class = "logLik")

#' @title Regression constraints of Multivariate Ordinal Regression Models.
#' @description
#' \code{constraints} is a generic function which extracts the regression constraints from objects of class \cr
#' \code{"mvord"}.
#' @param object object of class \code{"mvord"}
#' @export
constraints <- function(object) UseMethod("constraints")
#' @rdname constraints
#' @export
constraints.mvord <- function(object) object$constraints


#' @title Names of regression coefficient constraints in mvord
#' @description
#' \code{names_constraints} is a function which extracts the names of the regression constraints based on the model \code{formula} and \code{data}.
#' @param formula model formula
#' @param data data set
#' @export
names_constraints <- function(formula, data){
  intercept <- ifelse(attr(terms.formula(formula), "intercept") == 1, TRUE, FALSE)
  nam <-  colnames(model.matrix(update(as.formula(paste0("~",deparse(formula[[3]]))), ~ . + 1), data))
  if(intercept) nam else nam[-1]
}
