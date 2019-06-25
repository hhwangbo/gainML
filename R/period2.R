#' Apply Period 2 Analysis
#'
#' Conducts period 2 analysis; uses the optimal set of variables obtained in the
#' period 1 analysis to predict the power output of REF and CTR-b turbines in
#' period 2.
#'
#' @param per1 A dataframe containing the period 1 data.
#' @param per2 A dataframe containing the period 2 data.
#' @param opt.cov A character vector indicating the optimal set of variables
#'   (obtained from the period 1 analysis).
#' @return The function returns a list of the following datasets. \describe{
#'   \item{\code{pred.REF}}{A dataframe including the period 2 prediction for
#'   the REF turbine.} \item{\code{pred.CTR}}{A dataframe including the period 2
#'   prediction for the CTR-b turbine.} }
#' @references H. Hwangbo, Y. Ding, and D. Cabezon, 'Machine Learning Based
#'   Analysis and Quantification of Potential Power Gain from Passive Device
#'   Installation,' arXiv:1906.05776 [stat.AP], Jun. 2019.
#'   \url{https://arxiv.org/abs/1906.05776}.
#' @examples
#' df.ref <- with(wtg, data.frame(time = time, turb.id = 1, wind.dir = D,
#'  power = y, air.dens = rho))
#' df.ctrb <- with(wtg, data.frame(time = time, turb.id = 2, wind.spd = V,
#'  power = y))
#' df.ctrn <- df.ctrb
#' df.ctrn$turb.id <- 3
#'
#' data <- arrange.data(df.ref, df.ctrb, df.ctrn, p1.beg = '2014-10-24',
#'  p1.end = '2014-10-25', p2.beg = '2014-10-25', p2.end = '2014-10-26',
#'  k.fold = 2)
#'
#' p1.res <- analyze.p1(data$train, data$test, ratedPW = 1000)
#' p2.res <- analyze.p2(data$per1, data$per2, p1.res$opt.cov)
#'
#' @export
#'
analyze.p2 <- function(per1, per2, opt.cov) {
    if (is.na(per1)[1]) {
        list(pred.REF = NA, pred.CTR = NA)
    } else {
        message("Period 2 Prediction")
        
        id.circ <- which(opt.cov %in% c("D", "hour"))
        
        message(" Period 2 Prediction - REF Model...")
        pred.ref <- pred.akern(per1$yr, as.matrix(per1[, opt.cov]), as.matrix(per2[, 
            opt.cov]), id.circ, k = "gcv", kernel = "gauss")
        
        message(" Period 2 Prediction - CTR-b Model...")
        pred.ctr <- pred.akern(per1$yb, as.matrix(per1[, opt.cov]), as.matrix(per2[, 
            opt.cov]), id.circ, k = "gcv", kernel = "gauss")
        
        res.ref <- per2[, c(opt.cov, "yr")]
        res.ref$pred <- pred.ref
        
        res.ctr <- per2[, c(opt.cov, "yb")]
        res.ctr$pred <- pred.ctr
        
        list(pred.REF = res.ref, pred.CTR = res.ctr)
    }
}
