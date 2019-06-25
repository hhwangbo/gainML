#' Analyze Potential Gain from Passive Device Installation on WTGs by Using a
#' Machine Learning-Based Tool
#'
#' Implements the gain analysis as a whole; this includes data arrangement,
#' period 1 analysis, period 2 analysis, and gain quantification.
#'
#' @details Builds a machine learning model for a REF turbine (device installed)
#'   and a baseline CTR turbine (CTR-b; without device installation and
#'   preferably closest to the REF turbine) by using data measurements from a
#'   neutral CTR turbine (CTR-n; without device installation). Gain is
#'   quantified by evaluating predictions from the machine learning models and
#'   their differences during two different time periods, namely, period 1
#'   (without device installation on the REF turbine) and period 2 (device
#'   installed on the REF turbine).
#'
#' @inheritParams arrange.data
#' @inheritParams quantify.gain
#' @note \itemize{ \item This function will execute four other functions in
#'   sequence, namely, \code{\link{arrange.data}}, \code{\link{analyze.p1}},
#'   \code{\link{analyze.p2}}, \code{\link{quantify.gain}}. \item A user can
#'   alternatively run the four funtions by calling them individually in
#'   sequence.}
#' @return The function returns a list of several objects (lists) that includes
#'   all the analysis results from all steps. \describe{ \item{\code{data}}{A
#'   list of arranged datasets including period 1 and period 2 data as well as
#'   \eqn{k}-folded training and test datasets generated from the period 1 data.
#'   See also \code{\link{arrange.data}}.} \item{\code{p1.res}}{A list
#'   containing period 1 analysis results. This includes the optimal set of
#'   predictor variables, period 1 prediction for the REF turbine and CTR-b
#'   turbine, the corresponding error measures such as RMSE and BIAS, and BIAS
#'   curves for both REF and CTR-b turbine models; see \code{\link{analyze.p1}}
#'   for the details.} \item{\code{p2.res}}{A list containing period 2 analysis
#'   results. This includes period 2 prediction for the REF turbine and CTR-b
#'   turbine. See also \code{\link{analyze.p2}}.} \item{\code{gain.res}}{A list
#'   containing gain quantification results. This includes effect curve, offset
#'   curve, and gain curve as well as the measures of effect (gain without
#'   offset), offset, and (the final) gain; see \code{\link{quantify.gain}} for
#'   the details.} }
#' @seealso \code{\link{arrange.data}}, \code{\link{analyze.p1}},
#'   \code{\link{analyze.p2}}, \code{\link{quantify.gain}}
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
#' # For Full Sector Analysis
#' res <- analyze.gain(df.ref, df.ctrb, df.ctrn, p1.beg = '2014-10-24',
#'  p1.end = '2014-10-25', p2.beg = '2014-10-25', p2.end = '2014-10-26',
#'  ratedPW = 1000, AEP = 300000, pw.freq = pw.freq, k.fold = 2)
#' # In practice, one may use annual data for each of period 1 and period 2 analysis.
#' # One may typically use k.fold = 5 or 10.
#'
#' # For Free Sector Analysis
#' free.sec <- list(c(310, 50), c(150, 260))
#'
#' res <- analyze.gain(df.ref, df.ctrb, df.ctrn, p1.beg = '2014-10-24',
#'  p1.end = '2014-10-25', p2.beg = '2014-10-25', p2.end = '2014-10-26',
#'  ratedPW = 1000, AEP = 300000, pw.freq = pw.freq, k.fold = 2,
#'  free.sec = free.sec)
#'
#' gain.res <- res$gain.res
#' gain.res$gain    #This will provide the final gain value.
#'
#' @export
#'
analyze.gain <- function(df1, df2, df3, p1.beg, p1.end, p2.beg, p2.end, ratedPW, 
    AEP, pw.freq, freq.id = 3, time.format = "%Y-%m-%d %H:%M:%S", k.fold = 5, col.time = 1, 
    col.turb = 2, bootstrap = NULL, free.sec = NULL, neg.power = FALSE) {
    
    data <- arrange.data(df1, df2, df3, p1.beg, p1.end, p2.beg, p2.end, time.format, 
        k.fold, col.time, col.turb, bootstrap, free.sec, neg.power)
    
    if (is.na(data$train)[1]) {
        gain <- NA
        gain.res <- list(gain = gain)
        list(gain.res = gain.res)
    } else {
        p1.res <- analyze.p1(data$train, data$test, ratedPW)
        p2.res <- analyze.p2(data$per1, data$per2, p1.res$opt.cov)
        
        gain.res <- quantify.gain(p1.res, p2.res, ratedPW, AEP, pw.freq, freq.id)
        
        list(data = data, p1.res = p1.res, p2.res = p2.res, gain.res = gain.res)
    }
}

