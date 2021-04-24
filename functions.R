#
# Functions to read, transform data and produce time-series forecasts with the different methods.
#


require(magrittr)
require(ggplot2)
require(dplyr)
require(forecast)


defaultwindowsize=50

# scan a given path for the data (.probs) files it contains. An exclude.list file can be used to
# forbid certain files explicitly
getDataFilesInPath <- function(path) {
  datafiles <- dir(path=path, pattern=".probs$", recursive = T)
  datafiles <- paste(path, datafiles, sep = "/")

  exclusion_files <- dir(path=path, pattern="exclude.list", recursive = T)
  exclusion_files <- paste(rep(path, length(exclusion_files)), exclusion_files, sep = "/")
  for( e in exclusion_files )
  {
    if( file.exists(e) )
    {
      print(e)
      exclusions <- scan(file = e, what=character(), sep = "\n")
      excluded <- datafiles %in% exclusions
      datafiles <- datafiles[!excluded]
      print("Excluding %d instances from file e" %>% sprintf(length(excluded), e))
    }
  }

  datafiles
}

#
# converts a PROBS file into a data frame to work with.
#
probsFile2dataFrame <- function(filename, maxsize=1024) {
  df <- read.table(filename, quote="\"", col.names = c("Progress", "Visited", "Ssg")) %>%
    mutate(CumProgress=cumsum(Progress))
  df$LeafFreq <- (1:nrow(df) / df$Visited) - 1 / (2 * df$Visited)
  df$Leaves <- 1:nrow(df)
  df$UnsolvedNodes <- df$Visited - 2 * df$Leaves + 1
  df$Reso <- 2 ** ceiling(log2(ceiling(df$Leaves / maxsize)))

  df <- subset(df, df$Leaves %% df$Reso == 0)
  df
}

#
# converts a file name into a title for a plot (that contains the instance)
#
progressPlotTitle <- function(filename) {
  filename %>% basename() %>% gsub(".probs$", "", .) %>% gsub("\\.p_[uk]", "", .)
}


#
# make forecast for a target column and specified number of steps into the future. Different methods are available
#
makeForecast <- function(df, steps=100, method="ets", target="CumProgress", windowsize=defaultwindowsize, alphabeta=c(0.95,0.1)) {
  features <- list(
    CumProgress=list(
      bounds=c(0.0,1.0),
      increasing=TRUE
    ),
    LeafFreq=list(
      bounds=c(-0.5, 0.5),
      increasing=FALSE
    ),
    Visited=list(
      bounds=c(0,NULL),
      increasing=TRUE
    ),
    UnsolvedNodes=list(
      bounds=c(0,NULL),
      increasing=FALSE
    ),
    Ssg=list(
      bounds=c(0.0,1.0),
      increasing=FALSE
    )
  )
  if( method == "arima" && nrow(df) <= 2 )
    method <- "ets"
  if( method %in% c("ets", "des-paper") )
  {
    if( method == "ets" )
      alphabeta <- rep(NULL, 2)
    else
      alphabeta <- alphabeta

    mod <- ets(df[,target], model = "AAN",
               additive.only = TRUE,
               allow.multiplicative.trend = FALSE,
               alpha=alphabeta[1], beta = alphabeta[2],
               damped = FALSE)
    #print(mod)
    y <- forecast(mod, steps)
    result <- c(y$fitted, y$mean %>% as.numeric())

  } else if( method == "arima" ) {
    mod <- Arima(df[target], order = c(0,2,1))
    y <- forecast(mod, steps)
    result <- c(y$fitted, y$mean %>% as.numeric())

  } else if( method %in% c("w-linear", "w-quadratic")) {
    windowend <- nrow(df)
    # print(windowsize)
    if( windowsize <= nrow(df) )
    {
      windowstart <- nrow(df) - windowsize + 1
      ystart <- df[windowstart,target]
    } else {
      windowstart <- 0
      ystart <- 0
    }

    forecastk <- 1:(nrow(df) + steps)
    #print("Window start %d, end %d" %>% sprintf(windowstart, windowend))
    if( method == "w-linear" || nrow(df) <= 2 )
    {
      slope <- (df[windowend,target] - ystart) / (windowend - windowstart)
      intercept <- df[windowend,target] - slope * windowend
      #print(slope)
      result <- slope * forecastk + intercept
    } else {
      windowmid <- ceiling((windowend - windowstart) / 2)
      # print("Window mid %d" %>% sprintf(windowmid))
      v_start_end <- (df[windowend,target] - ystart) / (windowend - windowstart)
      v_start_mid <- (df[windowmid,target] - ystart) / (windowmid - windowstart)
      acc <- 2 * (v_start_end - v_start_mid) / (windowend - windowmid)
      vel <- v_start_mid - 0.5 * acc * (windowstart + windowmid)
      constant <- df[windowend,target] - vel * windowend - acc / 2 * windowend ** 2
      # print("Constant : %f, Acc: %f, Vel: %f" %>% sprintf(constant, acc, vel))
      result <- constant + (vel * forecastk + (acc / 2) * (forecastk ** 2))
    }
  }

  target.features <- features[[target]]
  target.bounds <- target.features$bounds

  #make the slope nondecreasing.
  if( target.features$increasing ) {
   slope <- pmax(1e-8, diff(result[nrow(df) + (0:steps)]))
   result[nrow(df) + (1:steps)] <- df[nrow(df), target] + cumsum(slope)
  }
  lower <- target.bounds[1]
  if(! is.na(lower))
    result <- pmax(result, lower)

  upper <- target.bounds[2]
  if(! is.na(upper))
    result <- pmin(result, upper)

  return(result)
}

# determine a split position based on progress
getProgressSplitPosition <- function(df, x) {
  trainEnd <- base::Position(function(y) y >= x, df$CumProgress)
  trainEnd <- max(min(3, nrow(df)), trainEnd)
  trainEnd
}

# compute prediction error
getPredictionError <- function(prediction, actual) {
  log2(prediction / actual)
}

# Search the data frame for subset of Progress levels that are reached
getProblemSpecificRellevels <- function(df, rellevels) {
  roundedprogress <- (df$CumProgress * 100) %>% as.integer(.) / 100
  indices <- lapply(rellevels, function(x) getProgressSplitPosition(df, x)) %>% unlist()
  indices <- indices[!is.na(indices)]
  indices <- indices[indices < nrow(df)]
  result <- roundedprogress[unique(indices)]
  # discard rellevels that are too big
  result[result <= max(rellevels)]
}

getLinearPrediction <- function(level, trend, targetlevel) {
  # return the number of steps until target level is reached from current trend
  h = ceiling((targetlevel - level) / trend)
  return(h)
}

# estimate termination of the search with a linear forecast of the number of steps until the target level is reached
estimateLinear <- function(training_data, method, target, windowsize, alphabeta) {
  # filter only the training observations.
  targetvalues <- list(
    CumProgress=1.0,
    LeafFreq=0.5,
    UnsolvedNodes=0.0,
    Ssg=0.0
  )

  f <- makeForecast(training_data,
                    steps = 1,
                    method = method,
                    target = target,
                    windowsize = windowsize,
                    alphabeta = alphabeta
  )
  level <- f[nrow(training_data)]
  trend <- f[length(f)] - f[length(f) - 1]
  # print(sprintf("Level %f, trend %f", level, trend))
  trainingResolution <- training_data$Reso[nrow(training_data)]
  estimation.no.resolution <- getLinearPrediction(level, trend, targetvalues[[target]])
  # print(estimation.no.resolution)

  # na's or inf's or trends that show into the wrong direction are treated separately. In this case, we
  # simply estimate that at least twice the number of nodes is necessary to complete the search.
  if( estimation.no.resolution < 0 || is.na(estimation.no.resolution) || is.infinite(estimation.no.resolution) )
  {
    estimation <- 2 * training_data$Visited[nrow(training_data)]
  }
  else {
    # use the estimation of the number of steps, consider also training resolution
    estimation.resolution <- training_data$Leaves[nrow(training_data)] + trainingResolution * estimation.no.resolution

    # multiply the estimated number of leaves at termination by 2 to get an estimation of the total tree size
    estimation <- 2 * estimation.resolution - 1
  }
  return(estimation)
}

# summarize estimation accuracy for all target time series at a specific progress level
summarize_one_forecast_linear <- function(df,
                             rellevel,
                             method="ets",
                             windowsize=defaultwindowsize,
                             alphabeta=c(0.95,0.1)) {

  # determine the position at which rellevel is reached
  trainEnd <- getProgressSplitPosition(df, rellevel)

  # filter only the training observations.
  trainingResolution <- df$Reso[trainEnd]

  df_train <- df[1:trainEnd,] %>% dplyr::filter( (Leaves %% trainingResolution == 0) )

  targetlist <- c("CumProgress", "LeafFreq", "UnsolvedNodes", "Ssg")

  estimations <- lapply(targetlist, function(x) estimateLinear(df_train, method = method, target = x, windowsize = windowsize, alphabeta = alphabeta))
  names(estimations) <- c("EstimProg", "EstimFreq", "EstimUns", "EstimSsg")
  estimations <- estimations %>% as.data.frame()
  actual <- df$Visited[nrow(df)]
  leaves=df$Leaves[trainEnd]
  errors <- getPredictionError(estimations, actual)
  names(errors) <- c("ErrorProg", "ErrorFreq", "ErrorUns", "ErrorSsg")
  errors <- errors %>% as.data.frame()
  cbind(
    estimations %>% as.data.frame(),
    errors,
    data.frame(Actual=actual,
               Current=df$Visited[trainEnd],
               Leaves=leaves
    )
  )
}





####################################################
# Functions for plotting                           #
####################################################

makeProgressSplitForecast <- function(df, x, method = "ets", target="CumProgress", windowsize=defaultwindowsize, alphabeta=c(0.95,0.1)) {
  trainEnd <- getProgressSplitPosition(df, x)
  f <- makeForecast(df[1:trainEnd,], nrow(df) - trainEnd, method = method, target = target, windowsize = windowsize, alphabeta = alphabeta)
  f
}

makeForecastDf <- function(df) {
  mylevels <- c(0.01,0.05, seq(0.1,0.5, by=0.2))
  splitPositions <- lapply(mylevels, function(x) getProgressSplitPosition(df, x)) %>% unlist()
  VisitedAtSplit <- df$Visited[splitPositions]
  VisitedAtSplitCol <- rep(VisitedAtSplit, each=nrow(df))
  forecastdf <- rbind(
    data.frame(Visited=rep(df$Visited, length(mylevels)),
               Leaves=rep(df$Leaves, length(mylevels)),
               LeafFreq=rep(df$LeafFreq, length(mylevels)),
               CumProgress=rep(df$CumProgress, length(mylevels)),
               VisitedAtSplit=VisitedAtSplitCol,
               Method=paste("A window (linear, w=50) ", rep(mylevels, each=nrow(df))),
               Forecast=do.call(c, lapply(mylevels,
                                          FUN = function(x) makeProgressSplitForecast(df, x, method = "w-linear"))
               ),
               ForecastUns=do.call(c, lapply(mylevels,
                                             FUN = function(x) makeProgressSplitForecast(df, x, method = "w-linear", target = "UnsolvedNodes")
               )
               ),
               ForecastFreq=do.call(c, lapply(mylevels,
                                              FUN= function(x) makeProgressSplitForecast(df, x, method = "w-linear", target = "LeafFreq")
               )
               )
    ),
    data.frame(Visited=rep(df$Visited, length(mylevels)),
               Leaves=rep(df$Leaves, length(mylevels)),
               LeafFreq=rep(df$LeafFreq, length(mylevels)),
               CumProgress=rep(df$CumProgress, length(mylevels)),
               VisitedAtSplit=VisitedAtSplitCol,
               Method=paste("DES (alpha,beta=0.15) ", rep(mylevels, each=nrow(df))),
               Forecast=do.call(c, lapply(mylevels,
                                          FUN = function(x) makeProgressSplitForecast(df, x, method = "des-paper"))
               ),
               ForecastUns=do.call(c, lapply(mylevels,
                                             FUN = function(x) makeProgressSplitForecast(df, x, method = "des-paper", target = "UnsolvedNodes"))
               ),
               ForecastFreq=do.call(c, lapply(mylevels,
                                              FUN= function(x) makeProgressSplitForecast(df, x, method = "des-paper", target = "LeafFreq")
               )
               )
    ),
    data.frame(Visited=rep(df$Visited, length(mylevels)),
               Leaves=rep(df$Leaves, length(mylevels)),
               LeafFreq=rep(df$LeafFreq, length(mylevels)),
               CumProgress=rep(df$CumProgress, length(mylevels)),
               VisitedAtSplit=VisitedAtSplitCol,
               Method=paste("ETS ", rep(mylevels, each=nrow(df))),
               Forecast=do.call(c, lapply(mylevels,
                                          FUN = function(x) makeProgressSplitForecast(df, x, method = "ets"))
               ),
               ForecastUns=do.call(c, lapply(mylevels,
                                             FUN = function(x) makeProgressSplitForecast(df, x, method = "ets", target = "UnsolvedNodes"))
               ),
               ForecastFreq=do.call(c, lapply(mylevels,
                                              FUN= function(x) makeProgressSplitForecast(df, x, method = "ets", target = "LeafFreq")
               )
               )
    )
  )
  forecastdf %>% mutate(Predicted=Visited > VisitedAtSplit)
}

progressPlot <- function(df, title, forecastdf = makeForecastDf(df), visibleforecasts=NULL) {
  maxunsolved <- max(df$UnsolvedNodes) + 1
  g<- ggplot(df, aes(Leaves, CumProgress, col="Progress")) +
    geom_line() +
    geom_line(aes(y=UnsolvedNodes / maxunsolved, col="Unsolved Nodes")) +
    geom_line(aes(y=LeafFreq, col="Leaf Frequency")) +
    ggtitle(title, subtitle = "Progress, Unsolved Nodes, Leaf Frequency") +
    xlab("Leaves (resolution: 1/%d)" %>% sprintf(df$Leaves[1])) + ylab("Progress/Leaf Frequency") +
    scale_y_continuous(sec.axis = sec_axis(~.*maxunsolved, name = "Unsolved Nodes"))

  g <- g + scale_color_manual("", values = c(#RColorBrewer::brewer.pal(nmethods, "RdYlGn"),
    "Blue", "Red", "Purple"))
  g
}

simultaneousplot <- function(df, errordf, title="Plot", target = "save") {
  g1 <- ggplot(df, aes(Leaves, CumProgress)) + geom_line() + ylab("Progress")
  g2 <- ggplot(df, aes(Leaves, LeafFreq)) + geom_line() + ylab("Leaf Frequency")
  g3 <- ggplot(df, aes(Leaves, UnsolvedNodes)) + geom_line() + ylab("Unsolved Nodes")

  actualnodes <- errordf$Actual[1]
  resolutioninv <- df$Leaves[1]
  maxnleaves <- df$Leaves[nrow(df)]

  g4 <- ggplot(errordf, aes(Leaves, y=EstimProg, col=Method)) + geom_line() + geom_point()  +
    geom_hline(yintercept = actualnodes) +
    theme(legend.position = "bottom") +
    xlim(c(0, maxnleaves)) + ylab("Estimation (Progress)")
  g5 <- ggplot(errordf, aes(Leaves, y=EstimFreq, col=Method)) + geom_line() + geom_point()  +
    geom_hline(yintercept = actualnodes) +
    theme(legend.position = "bottom") +
    xlim(c(0, maxnleaves)) +  ylab("Estimation (Leaf Frequency)")
  g6 <- ggplot(errordf, aes(Leaves, y=EstimUns, col=Method)) + geom_line() + geom_point()  +
    geom_hline(yintercept = actualnodes) + theme(legend.position = "bottom") +
    xlim(c(0, maxnleaves)) + ylab("Estimation (Unsolved Nodes)")

  if( target == "save" )
    gridExtra::arrangeGrob(g1,g2,g3,g4,g5,g6, nrow=2, padding = 1000)
  else
    gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6, nrow=2)
}
