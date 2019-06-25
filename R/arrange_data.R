#' Split, Merge, and Filter Given Datasets for the Subsequent Analysis
#'
#' Generates datasets that consist of the measurements from REF, CTR-b, and
#' CTR-n turbines only. Filters the datasets by eliminating data points with a
#' missing measurement and those with negative power output (optional).
#' Generates training and test datasets for \eqn{k}-fold CV and splits the
#' entire data into period 1 data and period 2 data.
#'
#' @importFrom utils read.csv
#'
#' @param df1 A dataframe for reference turbine data. This dataframe must
#'   include five columns: timestamp, turbine id, wind direction, power output,
#'   and air density.
#' @param df2 A dataframe for baseline control turbine data. This dataframe must
#'   include four columns: timestamp, turbine id, wind speed, and power output.
#' @param df3 A dataframe for neutral control turbine data. This dataframe must
#'   include four columns and have the same structure with \code{df2}.
#' @param p1.beg A string specifying the beginning date of period 1. By default,
#'   the value needs to be specified in \samp{\%Y-\%m-\%d} format, for example,
#'   \code{'2014-10-24'}. A user can use a different format as long as it is
#'   consistent with the format defined in \code{time.format} below.
#' @param p1.end A string specifying the end date of period 1. For example, if
#'   the value is \code{'2015-10-24'}, data observed until
#'   \code{'2015-10-23 23:50:00'} would be considered for period 1.
#' @param p2.beg A string specifying the beginning date of period 2.
#' @param p2.end A string specifying the end date of period 2. Defined similarly
#'   as \code{p1.end}.
#' @param time.format A string describing the format of time stamps used in the
#'   data to be analyzed. The default value is \code{'\%Y-\%m-\%d \%H:\%M:\%S'}.
#' @param k.fold An integer defining the number of data folds for the period 1
#'   analysis and prediction. In the period 1 analysis, \eqn{k}-fold cross
#'   validation (CV) will be applied to choose the optimal set of covariates
#'   that results in the least prediction error. The value of \code{k.fold}
#'   corresponds to the \eqn{k} of the \eqn{k}-fold CV. The default value is 5.
#' @param col.time An integer specifying the column number of time stamps in
#'   wind turbine datasets. The default value is 1.
#' @param col.turb An integer specifying the column number of turbines' id in
#'   wind turbine datasets. The default value is 2.
#' @param bootstrap An integer indicating the current replication (run) number
#'   of bootstrap. If set to \code{NULL}, bootstrap is not applied. The default
#'   is \code{NULL}. A user is not recommended to set this value and directly
#'   run bootstrap; instead, use \code{\link{bootstrap.gain}} to run bootstrap.
#' @param free.sec A list of vectors defining free sectors. Each vector in the
#'   list has two scalars: one for starting direction and another for ending
#'   direction, ordered clockwise. For example, a vector of \code{c(310 , 50)}
#'   is a valid component of the list. By default, this is set to \code{NULL}.
#' @param neg.power Either \code{TRUE} or \code{FALSE}, indicating whether or
#'   not to use data points with a negative power output, respectively, in the
#'   analysis. The default value is \code{FALSE}, i.e., negative power output
#'   data will be eliminated.
#' @return The function returns a list of several datasets including the
#'   following. \describe{ \item{\code{train}}{A list containing \emph{k}
#'   datasets that will be used to \bold{train} the machine learning model.}
#'   \item{\code{test}}{A list containing \emph{k} datasets that will be used to
#'   \bold{test} the machine learning model.} \item{\code{per1}}{A dataframe
#'   containing the period 1 data.} \item{\code{per2}}{A dataframe containing
#'   the period 2 data.} }
#' @examples
#' df.ref <- with(wtg, data.frame(time = time, turb.id = 1, wind.dir = D, power = y,
#'  air.dens = rho))
#' df.ctrb <- with(wtg, data.frame(time = time, turb.id = 2, wind.spd = V, power = y))
#' df.ctrn <- df.ctrb
#' df.ctrn$turb.id <- 3
#'
#' # For Full Sector Analysis
#' data <- arrange.data(df.ref, df.ctrb, df.ctrn, p1.beg = '2014-10-24', p1.end = '2014-10-27',
#'  p2.beg = '2014-10-27', p2.end = '2014-10-30')
#'
#' # For Free Sector Analysis
#' free.sec <- list(c(310, 50), c(150, 260))
#' data <- arrange.data(df.ref, df.ctrb, df.ctrn, p1.beg = '2014-10-24', p1.end = '2014-10-27',
#'  p2.beg = '2014-10-27', p2.end = '2014-10-30', free.sec = free.sec)
#'
#' length(data$train) #This equals to k.
#' length(data$test)  #This equals to k.
#' head(data$per1)    #This shows the beginning of the period 1 dataset.
#' head(data$per2)    #This shows the beginning of the period 2 dataset.
#' @export
#'
arrange.data <- function(df1, df2, df3, p1.beg, p1.end, p2.beg, p2.end, time.format = "%Y-%m-%d %H:%M:%S", 
    k.fold = 5, col.time = 1, col.turb = 2, bootstrap = NULL, free.sec = NULL, neg.power = FALSE) {
    
    if (ncol(df1) != 5) {
        warning("The reference turbine dataset (df1) must have five columns.")
    } else if ((ncol(df2) != 4) | (ncol(df3) != 4)) {
        warning("The control turbine dataset (either df2 or df3) must have four columns.")
    } else {
        REF.id <- unique(df1$turb.id)[1]
        CTRb.id <- unique(df2$turb.id)[1]
        CTRn.id <- unique(df3$turb.id)[1]
        turb.id <- c(REF.id, CTRb.id, CTRn.id)
        
        leval <- list(df1, df2, df3)
        
        # Correct the column names
        for (j in 1:length(leval)) {
            col.id <- c(1:ncol(leval[[j]]))
            col.new <- c(col.time, col.turb, col.id[-which(col.id == col.time | col.id == 
                col.turb)])
            if (turb.id[j] == REF.id) 
                names(leval[[j]])[col.new] <- c("Time", "id", "D", "yr", "density") else if (turb.id[j] == CTRb.id) 
                names(leval[[j]])[col.new] <- c("Time", "id", "Vb", "yb") else names(leval[[j]])[col.new] <- c("Time", "id", "Vn", "yn")
        }
        
        # Correct the first row of the datasets that has a parsing error
        nchar.target <- nchar(as.character(leval[[1]]$Time[2]))
        ladj <- lapply(leval, function(data) {
            nchar.first <- nchar(as.character(data$Time[1]))
            if (nchar.first > nchar.target) {
                nchar.adj <- nchar.first - nchar.target + 1
                data$Time <- as.character(data$Time)
                data$Time[1] <- substr(data$Time[1], nchar.adj, nchar.first)
            }
            return(data)
        })
        
        
        ### MERGE DATA
        dfull <- merge(ladj[[1]][, -col.turb], ladj[[2]][, -col.turb], by = "Time", 
            all = T)
        if (length(ladj) > 2) {
            for (k in 3:length(ladj)) {
                dfull <- merge(dfull, ladj[[k]][, -col.turb], by = "Time", all = T)
            }
        }
        
        # Time information
        timeframe <- as.POSIXct(dfull$Time, format = time.format, tz = "GMT")
        p1.beg <- strptime(p1.beg, format = "%Y-%m-%d", tz = "GMT")
        p1.end <- strptime(p1.end, format = "%Y-%m-%d", tz = "GMT")
        p2.beg <- strptime(p2.beg, format = "%Y-%m-%d", tz = "GMT")
        p2.end <- strptime(p2.end, format = "%Y-%m-%d", tz = "GMT")
        
        # Time restriction
        dfull <- dfull[which(timeframe >= p1.beg & timeframe < (p2.end + 3600 * 24)), 
            ]
        
        # Other variable calculation
        dfull$dVb <- c(NA, round(diff(dfull$Vb), 2))
        dfull$dVn <- c(NA, round(diff(dfull$Vn), 2))
        time.full <- as.POSIXct(dfull$Time, format = time.format, tz = "GMT")
        dfull$hour <- as.numeric(format(time.full, format = "%H", usetz = F))
        
        
        ### FILTER DATA
        id.na <- which(apply(dfull, 1, function(row) any(is.na(row))))
        
        # Free Sector
        if (length(free.sec) > 0) {
            nonfr <- matrix(NA, nrow(dfull), length(free.sec))
            for (m in 1:length(free.sec)) {
                sec <- free.sec[[m]]
                if (sec[1] < sec[2]) 
                  nonfr[, m] <- (dfull$D < sec[1] | dfull$D > sec[2]) else nonfr[, m] <- (dfull$D < sec[1] & dfull$D > sec[2])
            }
            id.nonfr <- which(apply(nonfr, 1, function(x) all(x)))
            id.na <- sort(unique(c(id.na, id.nonfr)))
        }
        
        # Negative power outputs
        if (!neg.power) {
            id.col <- grep("y", names(dfull))
            id.elim <- which(apply(dfull[, id.col], 1, function(row) any(row < 0)))
            id.na <- sort(unique(c(id.na, id.elim)))
        }
        
        dcln <- dfull[-id.na, ]
        
        # Resampling the data
        if (!is.null(bootstrap)) {
            id.sp <- sample(1:nrow(dcln), nrow(dcln), replace = T)
            dcln <- dcln[id.sp, ]
        }
        
        
        ### PERIOD DATA
        time.cln <- as.POSIXct(dcln$Time, format = time.format, tz = "GMT")
        dp1 <- dcln[which(time.cln >= p1.beg & time.cln < (p1.end)), ]
        dp2 <- dcln[which(time.cln >= p2.beg & time.cln < (p2.end)), ]
        
        
        ### GENERATE k FOLDS
        dfold <- gen.fold(dp1, k.fold)
        ltrts <- gen.trts(dfold, k.fold)
        ltr <- ltrts$tr
        lts <- ltrts$ts
        
        list(train = ltr, test = lts, per1 = dp1, per2 = dp2)
    }
}



# Generate indices for k-fold
gen.fold <- function(data, k.fold) {
    index <- sample(1:nrow(data), nrow(data))
    n.fold <- round(nrow(data)/k.fold)
    data$fold <- 0
    for (i in 1:(k.fold - 1)) {
        data$fold[index[((i - 1) * n.fold + 1):(i * n.fold)]] <- i
    }
    data$fold[index[((k.fold - 1) * n.fold + 1):nrow(data)]] <- k.fold
    return(data)
}



# Split data into tr and ts by using fold indices
gen.trts <- function(data, k.fold) {
    data.ts <- rep(list(c()), k.fold)
    data.tr <- rep(list(c()), k.fold)
    
    for (i in 1:k.fold) {
        data.ts[[i]] <- data[which(data$fold == i), ]
        data.tr[[i]] <- data[which(data$fold != i), ]
    }
    
    list(tr = data.tr, ts = data.ts)
}
