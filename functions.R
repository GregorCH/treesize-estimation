#
# Functions to read, transform data and produce time-series forecasts with the different methods.
#


require(magrittr)
require(ggplot2)
require(dplyr)
require(forecast)


defaultwindowsize=50

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

probsFile2dataFrame <- function(filename, maxsize=1000) {
  df <- read.table(filename, quote="\"", col.names = c("Progress", "Visited")) %>%
    mutate(CumProgress=cumsum(Progress))
  df$LeafFreq <- (1:nrow(df) / df$Visited) - 1 / (2 * df$Visited)
  df$Leaves <- 1:nrow(df)
  df$UnsolvedNodes <- df$Visited - 2 * df$Leaves + 1
  if( nrow(df) > maxsize )
  {
    rate <- 2 ** ceiling(log2(nrow(df) / maxsize))
    # print(rate)
    index <- seq(rate,nrow(df), by = rate)
    df <- df[index,]
  }
  df
}

progressPlotTitle <- function(filename) {
  filename %>% basename() %>% gsub(".probs$", "", .) %>% gsub("\\.p_[uk]", "", .)
}


makeForecast <- function(df, steps=100, method="ets", target="CumProgress", windowsize=defaultwindowsize) {
  features <- list(
    CumProgress=list(
      bounds=c(0.0,1.0),
      increasing=TRUE
    ),
    LeafFreq=list(
      bounds=c(0.0, 0.5),
      increasing=FALSE
    ),
    Visited=list(
      bounds=c(0,NULL),
      increasing=TRUE
    ),
    UnsolvedNodes=list(
      bounds=c(0,NULL),
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
      alphabeta <- rep(0.15, 2)

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

makeSplitForecast <- function(df, x, method = "ets", target="CumProgress", windowsize=defaultwindowsize) {
  trainEnd <- ceiling(x * nrow(df))
  f <- makeForecast(df[1:trainEnd,], nrow(df) - trainEnd, method = method, target = target, windowsize = windowsize)
  f
}

getProgressSplitPosition <- function(df, x) {
  trainEnd <- base::Position(function(y) y >= x, df$CumProgress)
  trainEnd <- max(min(3, nrow(df)), trainEnd)
  trainEnd
}

makeProgressSplitForecast <- function(df, x, method = "ets", target="CumProgress", windowsize=defaultwindowsize) {
  trainEnd <- getProgressSplitPosition(df, x)
  f <- makeForecast(df[1:trainEnd,], nrow(df) - trainEnd, method = method, target = target, windowsize = windowsize)
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
    # data.frame(Visited=rep(df$Visited, length(mylevels)),
    #            Leaves=rep(df$Leaves, length(mylevels)),
    #            LeafFreq=rep(df$LeafFreq, length(mylevels)),
    #            CumProgress=rep(df$CumProgress, length(mylevels)),
    #            VisitedAtSplit=VisitedAtSplitCol,
    #            Method=paste("B window (linear, w=2) ", rep(mylevels, each=nrow(df))),
    #            Forecast=do.call(c, lapply(mylevels,
    #                                       FUN = function(x) makeProgressSplitForecast(df, x, method = "w-linear", windowsize = 2))
    #            ),
    #            ForecastUns=do.call(c, lapply(mylevels,
    #                                          FUN = function(x) makeProgressSplitForecast(df, x, method = "w-linear", target = "UnsolvedNodes", windowsize = 2)
    #            )
    #            ),
    #            ForecastFreq=do.call(c, lapply(mylevels,
    #                                           FUN= function(x) makeProgressSplitForecast(df, x, method = "w-linear", target = "LeafFreq", windowsize = 2)
    #            )
    #            )
    # ),
    # data.frame(Visited=rep(df$Visited, length(mylevels)),
    #            Leaves=rep(df$Leaves, length(mylevels)),
    #            Method=paste("B window (quadratic, w=50) ", rep(mylevels, each=nrow(df))),
    #            Forecast=do.call(c, lapply(mylevels,
    #                                     FUN = function(x) makeProgressSplitForecast(df, x, method = "w-quadratic"))
    #                           ),
    #                           ForecastUns=do.call(c, lapply(mylevels,
    #                                     FUN = function(x) makeProgressSplitForecast(df, x, method = "w-quadratic", target = "UnsolvedNodes"))
    #                           )
    #           ),
    # data.frame(Visited=rep(df$Visited, length(mylevels)),
    #            Method=paste("C Arima (0,2,1) ", rep(mylevels, each=nrow(df))),
    #            Forecast=do.call(c, lapply(mylevels,
    #                                     FUN = function(x) makeProgressSplitForecast(df, x, method = "arima"))
    #                           )
    #           ),
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

  # if( is.null(visibleforecasts) )
  #   visibleforecasts <- unique(forecastdf$Method)

  # if( length(visibleforecasts) > 5 ) {
  #   warning("Reducing %d visible forecasts to first five" %>% sprintf(length(visibleforecasts)))
  #   visibleforecasts <- visibleforecasts[1:5]
  # }

  # nmethods <- length(visibleforecasts)

  # g <- g + geom_line(data=subset(forecastdf, Method %in% visibleforecasts), linetype="dotdash", aes(Leaves, Forecast, col=Method %>% as.factor()))
  g <- g + scale_color_manual("", values = c(#RColorBrewer::brewer.pal(nmethods, "RdYlGn"),
                                             "Blue", "Red", "Purple"))
  g
}

simultaneousplot <- function(df, errordf, title="Plot") {
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

  gridExtra::arrangeGrob(g1,g2,g3,g4,g5,g6, nrow=2, padding = 1000)
}


summarizeForecastdf <- function(df, forecastdf=makeForecastDf(df)) {
  forecastdf %>% dplyr::filter(Predicted) %>% group_by(Method) %>% summarise(
    rmsd=((Forecast - CumProgress) ** 2) %>% mean() %>% sqrt(),
    maeProg=mean(abs(Forecast - CumProgress)),
    maeFreq=mean(abs(ForecastFreq - LeafFreq))
  )
}

getPredictionError <- function(prediction, actual) {
  log2(prediction / actual)
}

getPredictionsFromForecast <- function(forecastdf) {
  n <- nrow(forecastdf)
  # print(forecastdf)
  # make a prediction from the progress forecast
  if( max(forecastdf$Forecast) >= 1.0 )
  {
    leafidx <- Position(function(x) x == 1, forecastdf$Forecast)
    EstimProg <- 2 * forecastdf$Leaves[leafidx] - 1
  }
  else
  {
    if( n > 1 ) {
      prog1 <- forecastdf$Forecast[n - 1]
      leaves1 <- forecastdf$Leaves[n - 1]
    }
    else{
      prog1 <- 0
      leaves1 <- 0
    }
    slope <- (forecastdf$Forecast[n] - prog1) / (forecastdf$Leaves[n] - leaves1)
    # print(slope)
    estimleaves <-  forecastdf$Leaves[n] + (1.0 - forecastdf$Forecast[n]) / slope
    EstimProg <- 2 * estimleaves - 1
  }

  # make a prediction from the leaf frequency (not always possible)
  if( max(forecastdf$ForecastFreq) >= 0.5 )
  {
    leafidx <- Position(function(x) x >= 0.5, forecastdf$ForecastFreq)
    EstimFreq <- 2 * forecastdf$Leaves[leafidx] - 1
  }
  else
  {
    if( n > 1 ) {
      freq1 <- forecastdf$ForecastFreq[n - 1]
      leaves1 <- forecastdf$Leaves[n - 1]
    }
    else{
      freq1 <- 0
      leaves1 <- 0
    }

    slope <- (forecastdf$ForecastFreq[n] - freq1) / (forecastdf$Leaves[n] - leaves1)
    if( slope > 0)
    {
      estimleaves <- forecastdf$Leaves[n] + (0.5 - forecastdf$ForecastFreq[n]) / slope
      EstimFreq <- 2 * estimleaves - 1
    }
    else
    {
      EstimFreq <- 2 * forecastdf$VisitedAtSplit[1]
    }
  }
  # make a prediction from the ressource forecast
  resourcelinfunc <- forecastdf$ForecastUns
  if( min(resourcelinfunc) <= 0 ) {
    leafidx <- Position(function(x) x <= 0.0, resourcelinfunc)
    EstimUns <- 2 * forecastdf$Leaves[leafidx] - 1
  }
  else
  {
    if( n > 1 ) {
      resourcelinfunc1 <- resourcelinfunc[n - 1]
      leaves1 <- forecastdf$Leaves[n - 1]
    }
    else{
      resourcelinfunc1 <- 0
      leaves1 <- 0
    }
    slope <- (resourcelinfunc[n] - resourcelinfunc1) / (forecastdf$Leaves[n] - leaves1)
    # print(slope)
    if( slope < 0 )
    {
      estimleaves <- (0.0 - resourcelinfunc[n]) / slope + forecastdf$Leaves[n]
      EstimUns <- 2 * estimleaves - 1
    }
    else
      EstimUns <- 2 * forecastdf$VisitedAtSplit[1]
  }



  actual <- forecastdf$Visited[n]
  return(data.frame(EstimProg=EstimProg,
                    EstimFreq=EstimFreq,
                    EstimUns=EstimUns,
                    ErrorProg=getPredictionError(EstimProg, actual),
                    ErrorFreq=getPredictionError(EstimFreq, actual),
                    ErrorUns=getPredictionError(EstimUns, actual)
                    )
         )
}

getProblemSpecificRellevels <- function(df, rellevels) {
  roundedprogress <- (df$CumProgress * 100) %>% as.integer(.) / 100
  indices <- lapply(rellevels, function(x) getProgressSplitPosition(df, x)) %>% unlist()
  indices <- indices[!is.na(indices)]
  indices <- indices[indices < nrow(df)]
  result <- roundedprogress[unique(indices)]
  # discard rellevels that are too big
  result[result <= max(rellevels)]
}



summarize_one_forecast <- function(df, rellevel, method="ets", windowsize=defaultwindowsize) {
  trainEnd <- getProgressSplitPosition(df, rellevel)
  d1 <- data.frame(Visited=df$Visited,
             Leaves=df$Leaves,
             LeafFreq=df$LeafFreq,
             CumProgress=df$CumProgress,
             VisitedAtSplit=rep(df$Visited[trainEnd], nrow(df)),
             Method=method,
             Forecast=makeProgressSplitForecast(df, rellevel, method = method, windowsize = windowsize),
             ForecastUns=makeProgressSplitForecast(df, rellevel, method = method, target = "UnsolvedNodes", windowsize = windowsize),
             ForecastFreq=makeProgressSplitForecast(df, rellevel, method = method, target = "LeafFreq", windowsize = windowsize)
  )

  d1 <- d1 %>% mutate(Predicted=Visited > VisitedAtSplit)

  testdatarange <- (trainEnd + 1):nrow(df)
  testdf <- df[testdatarange,]
  testd1 <- d1[testdatarange,]
  # print(testdatarange)
  cbind(
    summarizeForecastdf(testdf, testd1),
    getPredictionsFromForecast(testd1),
    data.frame(Actual=df$Visited[nrow(df)],
               Current=df$Visited[trainEnd],
               Leaves=df$Leaves[trainEnd])
  )
}
