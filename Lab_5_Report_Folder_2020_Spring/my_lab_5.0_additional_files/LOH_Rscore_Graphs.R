# SDT_Eight_Graphs.R
##
# Lewis O. Harvey, Jr.
# Department of Psychology and Neuroscience
# University of Colorado Boulder
# Boulder, Colorado, USA
#
# 27 January 2019
#
# This R file defines eight plot functions to create signal detection graphs using
# a data.frame created by reading in a *_grf.txt file created by RscorePlus.
# e.g., df_grf <- read.grfn("my_file_grf.txt")
#       plot_proc(df_grf)

# 1. plot_proc <- function(df_grf, ...)      probability ROC curves
# 2. plot_zroc <- function(df_grf, ...)      z-score ROC curves
# 3. plot_freq <- function(df_grf, ...)      predicted vs observed rating frequencies
# 4. plot_prob <- function(df_grf, ...)      predicted vs observed rating probabilities
# 5. plot_pcoc <- function(df_grf, ...)      probability critical operating characteristic
# 6. plot_resd <- function(df_grf, ...)      resituals (observed - predicted) probabilities
# 7. plot_post <- function(df_grf, ...)      posterior probabilities of hit for each rating
# 8. plot_pdfs <- function(df_grf, ...)      probability density functions of fit model
# 9. plot_allg <- function(df.grf, ...)      plots all eight graphs in one figure
# A. read_grfn <- function(grf_txt)          returns a data frame from the *_grf.txt file
# B. read_altn <- function(alt_txt)          returns a data frame from the *_alt.txt file

# Use with SDT_Plot.R or SDT_Plot.Rmd or in your one script to create graphs
# The snippets below are keyed to the examples in the RscorePlus User's Manual
#
# History
#
# 5 December 2015
# Added check for AIC in RscorePlus graphics file. If present, include AIC in plots.
#
# 3 January 2016
# Added 0,1 lines to the ROC box
#
# 31 August 2016
# Changed the color of the noise distribution to red.
#
# 8 September 2016
# Fixed color of points to match curves in ROCs
#
# 11 September 2016
# fixed criterion marker heights
#
# 4 September 2017
# Round AIC to 1 decimal point
# Added optional Az arguement for plot1()
#
# 14 September 2017
# Added main title and aic arguements to some plot commands
#
# 4 February 2018
# Use legend() command to add info to plots
#
# 23 February 2018
# Made code suitable for use with Rmarkdown
# Added input grf file data frame arguement
# Changed the names of the plot functions to be more informative
#
# 16 May 2018
# Changed function names: replaced "." with "_"
#
# 13 June 2018
# Add code to rscore package
#
# 12 August 2018
# Fixed error in file name checks in read_altn() and read_grfn()
#
# 27 January 2019
# Changed the color pallet
#
# To create any graph from the graphical output of RscorePlus there are three
# steps to follow:
#
# 1. Read the contents of the RscorePlus *_grf file into an R data frame;
# 2. Select the subset of data to plot, typically a single data set;
# 3. Issue plot commands using the appropriate pairs of variables.
#
# For multiple data sets from one RscoreRun, this script loops through the data
# sets one at a time using variable sn (set number).
# Hitting return in the console window advances to the next data set.
#
# Note for plot_proc(), plot_zroc() and plot_distributions()
# Older versions of RscorePlus did not write the AIC value to the grf file;
# It is included as an optional arguement in all the plot functions;
# The value you pass will be printed in the legend
#
# The area under the ROC A-sub-z and the detection index d-sub-a are also
# not included in the grf file;
# One or both values may be passed in the call for printing in the legend.

#######################################################################
# Define the eight plot functions
# The plots are implimented as functions so that they can be
# individually plotted (e.g., plot1(set_number, sig0, sig1))
# The set number allows you to plot a sigle data set in a graphics file
# that contains more than one RscorePlus analysis. Some of the plot
# commands additionally allow you to plot single pairs of signal
# conditons (e.g., plot1() and plot2() that draw ROC functions)

# color plalette for plotting the different signal conditions
colorsForPlots <- c("black", rainbow(16, start = 0.7, end = 0.2))
colorsForPlots <- c("blue", rainbow(16))
colorForS0 <- "black"
colorForXc <- "gray"

# 1. plot_proc()
# ----------------------------------------------------
# Observed and Predicted ROC (probability coordinates)
# sn -> data set number
# indx -> for plotting a single ROC (x-axis)
# indy -> for plotting a single ROC (y-axis)
plot_proc <- function(df_grf,
                      data_set = 1,
                      signal_0 = 0,
                      signal_1 = 1,
                      main_title = NA,
                      aic = NA,
                      da = NA,
                      Az = NA) {

    main_title_flag <- !is.na(main_title)
    sn <- data_set
    indx <- signal_0
    indy <- signal_1
    ndata_sets <- length(levels(as.factor(df_grf$data_set)))

    # make sure sn is within range in data frame
    if(sn < 0 || sn > ndata_sets) sn <- 1

    df <- subset(df_grf, data_set == sn)	# select a data set
    nsig <- df$nsig[1]						# number of signals
    nrat <- df$nrat[1]						# number of ratings
    ncrt <- nrat - 1						# number of criteria
    name <- df[1, 1]                        # heading
    model_type <- df$Model[1]				# get the model type


    if ("aic" %in% colnames(df)) aic <- df$aic[1] else aic <- NA

    # make sure the offsets (indx and indy) are in the range of nsig
    if (indx < 0 || indx > nsig-1) indx <- 0
    if (indy < 0 || indy > nsig-1) indx <- 1

    ind0 <- match("Obs_pYs0",  names(df))	# find s0 observed p(Y|s) column
    ind1 <- match("Pred_pYs0", names(df))	# find s0 predicted p(Y|s) column

    # plot s0,s1 ROC
    # set up the graph
    if(main_title_flag) {
        main <- main_title
    } else {
        main <- paste("Data Set ", sn, ": ", name, "\n",
                      "Observed and Predicted ROC\n",
                      model_type, " model")
    }
    par(pty="s")							# make plot square
    ob0 <- ind0 + indx
    ob1 <- ind0 + indy
    plot(0, 0, type = "n",
         pch = 19,
         xlim = c(0, 1),
         ylim = c(0, 1),
         xlab = "False Alarm Rate (Probability)",
         ylab = "Hit Rate (Probability)",
         main = main)

    lines(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0), lty = "solid", col = "gray")
    lines(c(0.0, 0.5), c(1.0, 0.5), lty = "solid", col = "gray")
    lines(c(0.0, 1.0), c(0.0, 1.0), lty = "solid", col = "gray")

    # plot the ROCs relative to s0
    for (i in 1:(nsig-1)){
        ob0 <- ind0 + indx
        ob1 <- ind0 + indx + i
        points(df[, ob1] ~ df[, ob0],
               type = "p",
               col = colorsForPlots[i],
               pch = 19)
        pr0 <- ind1 + indx
        pr1 <- ind1 + indx + i
        lines(df[, pr0], df[, pr1], lwd = 2, col = colorsForPlots[i])
    }

    # make da character string for legend
    if (!is.na(da) && nsig == 2)
        str_da <- paste("da =", round(da, 2)) else str_da <- ""

    # make Az character string for legend
    if (!is.na(Az) && nsig == 2)
        str_Az <- paste("Az =", round(Az, 2)) else str_Az <- ""

    # make aic character string for legend
    if (!is.na(aic))
        str_aic <- paste("AIC =", round(aic, 2)) else str_aic <- ""

    # add legend
    legend("bottomright", cex = 0.6, bty = "n",
           c(str_da, str_Az, str_aic))

    par(pty="m")
}

# 2. plot_zroc()
# ----------------------------------------------------
# Observed vs. Predicted ROC (z-score probability coordinates)
# sn -> data set number
# indx -> for plotting a single ROC (x-axis)
# indy -> for plotting a single ROC (y-axis)
plot_zroc <- function(df_grf,
                      data_set = 1,
                      signal_0 = 0,
                      signal_1 = 1,
                      main_title = NA,
                      aic = NA,
                      da = NA,
                      Az = NA) {

    main_title_flag <- !is.na(main_title)
    sn <- data_set
    indx <- signal_0
    indy <- signal_1
    ndata_sets <- length(levels(as.factor(df_grf$data_set)))

    # make sure sn is within range in data frame
    if(sn < 0 || sn > ndata_sets) sn <- 1

    df <- subset(df_grf, data_set == sn)	# select a data set
    nsig <- df$nsig[1]						# number of signals
    nrat <- df$nrat[1]						# number of ratings
    ncrt <- nrat - 1						# number of criteria
    name <- df[1, 1]                        # heading
    model_type <- df$Model[1]				# get the model type
    if ("aic" %in% colnames(df)) aic <- df$aic[1] else aic <- NA

    # make sure the offsets (indx and indy) are in the range of nsig
    if (indx < 0 || indx > nsig-1) indx <- 0
    if (indy < 0 || indy > nsig-1) indx <- 1

    ind0 <- match("Obs_zYs0", names(df))	# find s0 observed z(Y|s) column
    ind1 <- match("Pred_zYs0", names(df))	# find s0 predicted z(Y|s) column
    lim <- 1.1 * max(abs(df[, ind0:(ind0+nsig-1)]), na.rm = TRUE)

    # plot s0,s1 ROC
    # set up the graph
    if(main_title_flag) {
        main <- main_title
    } else {
        main <- paste("Data Set ", sn, ": ", name, "\n",
                      "Observed and Predicted z-score ROC\n",
                      model_type, " model")
    }
    par(pty="s")	# make square
    ob0 <- ind0 + indx
    ob1 <- ind0 + indy
    plot(0, 0, type = "n",
         pch = 19,
         xlim = c(-lim, lim),
         ylim = c(-lim, lim),
         xlab = "False Alarm Rate (z-score)",
         ylab = "Hit Rate (z-score)",
         main = main)
    abline(0, 1, col = "gray", lty = "solid")
    lines(c(-lim, 0.0), c(lim, 0.0), lty = "solid", col = "gray")

    # plot the ROCs (s2...)
    for (i in 1:(nsig-1)){
        ob0 <- ind0 + indx
        ob1 <- ind0 + indx + i
        points(df[, ob1] ~ df[, ob0],
               type = "p",
               col = colorsForPlots[i],
               pch = 19)
        pr0 <- ind1 + indx
        pr1 <- ind1 + indx + i
        lines(df[, pr0], df[, pr1], lwd = 2, col = colorsForPlots[i])
    }

    # make da character string for legend
    if (!is.na(da) && nsig == 2)
        str_da <- paste("da =", round(da, 2)) else str_da <- ""

    # make Az character string for legend
    if (!is.na(Az) && nsig == 2)
        str_Az <- paste("Az =", round(Az, 2)) else str_Az <- ""

    # make aic character string for legend
    if (!is.na(aic))
        str_aic <- paste("AIC =", round(aic, 2)) else str_aic <- ""

    # add legend
    legend("bottomright", cex = 0.6, bty = "n",
           c(str_da, str_Az, str_aic))

    par(pty="m")
}

# 3. plot_freq()
# ----------------------------------------------------
# Observed vs. Predicted Response Frequencies
plot_freq <- function(df_grf,
                      data_set = 1,
                      main_title = NA,
                      aic = NA) {

    main_title_flag <- !is.na(main_title)
    sn <- data_set
    df <- subset(df_grf, data_set == sn)	# select a data set
    nsig <- df$nsig[1]						# number of signals
    nrat <- df$nrat[1]						# number of ratings
    ncrt <- nrat - 1						# number of criteria
    name <- df[1, 1]                        # heading
    model_type <- df$Model[1]				# get the model type
    iob <- match("Obs_Freq0",  names(df))	# find observed s0 response frequency column
    ipr <- match("Pred_Freq0", names(df))	# find predicted s0 response frequency column
    max_freq <- 1.2 * max(df[, c(iob:(iob+nsig-1), (ipr:(ipr+nsig-1)))], na.rm = TRUE)
    if ("aic" %in% colnames(df)) aic <- df$aic[1] else aic <- NA

    par(pty="s")                            # make graph square
    # set up the graph
    if(main_title_flag) {
        main <- main_title
    } else {
        main <- paste("Data Set ", sn, ": ", name, "\n",
                      "Observed vs. Predicted Response Frequencies\n",
                      model_type, " model")
    }
    plot(0, 0, type = "n", pch = 19,
         xlim = c(0, max_freq),
         ylim = c(0, max_freq),
         xlab = "Observed Rating Frequencies",
         ylab = "Predicted Rating Frequencies",
         main = main)

    # plot the response frequencies
    for (i in 1:nsig){
        points(df[, (iob+i-1)], df[, (ipr+i-1)],
               type = "p",
               pch = 19,
               col = c(colorForS0, colorsForPlots)[i])
    }
    abline(0, 1, col = "gray", lty = "solid")
    par(pty="m")

    # make aic character string for legend
    if (!is.na(aic))
        str_aic <- paste("AIC =", round(aic, 2)) else str_aic <- ""

    # add legend
    legend("bottomright", cex = 0.6, bty = "n",
           c(str_aic))
}

# 4. plot_prob()
# ----------------------------------------------------
# Observed vs. Predicted Response Probabilities
plot_prob <- function(df_grf,
                      data_set = 1,
                      main_title = NA,
                      aic = NA) {

    main_title_flag <- !is.na(main_title)
    sn <- data_set
    df <- subset(df_grf, data_set == sn)	# select a data set
    nsig <- df$nsig[1]						# number of signals
    nrat <- df$nrat[1]						# number of ratings
    ncrt <- nrat - 1						# number of criteria
    name <- df[1, 1]                        # heading
    model_type <- df$Model[1]				# get the model type
    iob <- match("Obs_Prob0",  names(df))   # find observed s0 response probability column
    ipr <- match("Pred_Prob0", names(df))	# find predicted s0 response probability column
    max_freq <- 1.2 * max(df[, c(iob:(iob+nsig-1), (ipr:(ipr+nsig-1)))], na.rm = TRUE)
    if ("aic" %in% colnames(df)) aic <- df$aic[1] else aic <- NA

    par(pty="s")                            # make graph square
    # set up the graph
    if(main_title_flag) {
        main <- main_title
    } else {
        main <- paste("Data Set ", sn, ": ", name, "\n",
                      "Observed vs. Predicted Response Probabilities\n",
                      model_type, " model")
    }
    plot(0, 0, type = "n", pch = 19,
         xlim = c(0, max_freq),
         ylim = c(0, max_freq),
         xlab = "Observed Rating Probabilities",
         ylab = "Predicted Rating Probabilities",
         main = main)

    # plot the response frequencies
    for (i in 1:nsig){
        points(df[, (iob+i-1)], df[, (ipr+i-1)],
               type = "p",
               pch = 19,
               col = c(colorForS0, colorsForPlots)[i])
    }
    abline(0, 1, col = "gray", lty = "solid")

    # make aic character string for legend
    if (!is.na(aic))
        str_aic <- paste("AIC =", round(aic, 2)) else str_aic <- ""

    # add legend
    legend("bottomright", cex = 0.6, bty = "n",
           c(str_aic))

    par(pty="m")
}

# 5. plot_pcoc()
# ----------------------------------------------------
# Observed and Predicted critical operating characteristic
plot_pcoc <- function(df_grf,
                      data_set = 1,
                      main_title = NA,
                      aic = NA) {

    main_title_flag <- !is.na(main_title)
    sn <- data_set
    df <- subset(df_grf, data_set == sn)		# select a data set
    nsig <- df$nsig[1]						    # number of signals
    nrat <- df$nrat[1]						    # number of ratings
    ncrt <- nrat - 1						    # number of criteria
    name <- df[1, 1]                            # heading
    model_type <- df$Model[1]			    	# get the model type
    if ("aic" %in% colnames(df)) aic <- df$aic[1] else aic <- NA
    ind0 <- match("Obs_pYs1", names(df))		# find the observed hit rate
    ind1 <- match("Obs_pHitXc1", names(df))		# find the observed posterior of hit
    ind2 <- match("Pred_pYs1", names(df))		# find the predicted hit rate
    ind3 <- match("Pred_pHitXc1", names(df))	# find the predicted posterior of hit

    par(pty="s")	# make square

    # plot s1 posterior
    # set up the graph
    if(main_title_flag) {
        main <- main_title
    } else {
        main <- paste("Data Set ", sn, ": ", name, "\n",
                      "Critical Operating Characteristic\n",
                      model_type, " model")
    }
    plot(df[, ind1] ~ df[, ind0],
         type = "p", pch = 19,
         xlim = c(0, 1),
         ylim = c(0, 1),
         xlab = "Hit Rate (Probability)",
         ylab = "Posterior Probability of a Hit",
         main = main)
    lines(df[, ind3] ~ df[, ind2], lwd = 2)
    lines(c(0, 1), c(0.9, 0.9), col = "gray")
    text(0.8, 0.92, "Beyond Reasonable Doubt", cex = 0.6)

    inter <- approx(df[, ind2], df[, ind3], n=1000)
    ind4 <- match(TRUE, inter$y < 0.9)

    if (!is.na(ind4)) {
        xc <- inter$x[ind4]
        yc <- inter$y[ind4]
        lines(c(xc, xc), c(0, yc), col = "gray")
        text(xc-.025, .2, "Critical Hit Rate", cex = 0.6, srt = 90)
    }

    # plot the remaining posterior probabilities (s2...)
    if (nsig > 2){
        for (i in 1:(nsig-1)){
            points(df[, ind0+i], df[, ind1+i], type = "p",
                   pch = 19)
            lines(df[, ind2+i], df[, ind3+i], lwd = 2, col = colorsForPlots[i])
            inter <- approx(df[, ind2+i], df[, ind3+i], n=1000)
            ind4 <- match(TRUE, inter$y < 0.9)
            if (!is.na(ind4)) {
                xc <- inter$x[ind4]
                yc <- inter$y[ind4]
                lines(c(xc, xc), c(0, yc), col = colorsForPlots[-1])
            }
        }
    }

    # make aic character string for legend
    if (!is.na(aic))
        str_aic <- paste("AIC =", round(aic, 2)) else str_aic <- ""

    # add legend
    legend("bottomright", cex = 0.6, bty = "n",
           c(str_aic))

    par(pty="m")
}

# 6. plot_resd()
# ----------------------------------------------------
# plot residuals of observed probabilites and predicted probabilities
plot_resd <- function(df_grf,
                      data_set = 1,
                      main_title = NA,
                      aic = NA) {

    main_title_flag <- !is.na(main_title)
    sn <- data_set
    df <- subset(df_grf, data_set == sn)	# select a data set
    nsig <- df$nsig[1]						# number of signals
    nrat <- df$nrat[1]						# number of ratings
    ncrt <- nrat - 1						# number of criteria
    name <- df[1, 1]                        # heading
    model_type <- df$Model[1]				# get the model type
    if ("aic" %in% colnames(df)) aic <- df$aic[1] else aic <- NA
    ndata <- nrat * nsig

    ind0 <- match("Obs_Prob0", names(df))		# find observed freqs
    ind1 <- match("Pred_Prob0", names(df))		# find predicted freqs

    obs <- df[!is.na(df[,ind0]), ind0]
    pre <- df[!is.na(df[,ind1]), ind1]
    resid <- obs - pre

    for(i in 2:nsig-1)
    {
        obs <- df[!is.na(df[,ind0+i]), ind0+i]
        pre <- df[!is.na(df[,ind1+i]), ind1+i]
        resid <- c(resid, (obs - pre))
    }
    ymax <- max(resid)
    ymin <- min(resid)
    yymx <- max(abs(ymax), abs(ymin))
    yymx <- yymx * 1.5
    # set up the graph
    if(main_title_flag) {
        main <- main_title
    } else {
        main <- paste("Data Set ", sn, ": ", name, "\n",
                      "Observed Minus Predicted Rating Probabilities\n",
                      model_type, " model")
    }
    plot(resid,
         pch = 19, col = "black", type='n',
         ylim = c(-yymx, yymx),
         xlab = "Rating",
         ylab = "Residual",
         main = main,
         xlim = c(1, ndata))

    points(resid[1:nrat], pch = 19, col = colorForS0)
    for (i in 1:(nsig-1))
    {
        i0 <- (nrat * i) + 1
        i1 <- i0 + nrat
        # note: the complex subscript on colors is to have all the points within each signal
        # condition be the same color, and to prevent having an index that is outside the
        # subscript bounds
        points(i0:i1, resid[i0:i1], pch = 19, col=colorsForPlots[((i%%length(colorsForPlots)))])
    }

    for (i in 1:nsig-1)
    {
        i0 <- (nrat * i) + 0.5
        abline(v=i0, col="gray")
    }
    abline(h=0.0, col="gray")

    # make aic character string for legend
    if (!is.na(aic))
        str_aic <- paste("AIC =", round(aic, 2)) else str_aic <- ""

    # add legend
    legend("bottomright", cex = 0.6, bty = "n",
           c(str_aic))
}

# 7. plot_post()
# ----------------------------------------------------
# Observed and Predicted posterior probability of signal given response
plot_post <- function(df_grf,
                      data_set = 1,
                      main_title = NA,
                      aic = NA) {

    main_title_flag <- !is.na(main_title)
    sn <- data_set
    df <- subset(df_grf, data_set == sn)	    # select a data set
    nsig <- df$nsig[1]							# number of signals
    nrat <- df$nrat[1]							# number of ratings
    ncrt <- nrat - 1							# number of criteria
    name <- df[1, 1]                            # heading
    model_type <- df$Model[1]			    	# get the model type
    if ("aic" %in% colnames(df)) aic <- df$aic[1] else aic <- NA
    ind0 <- match("Rating_Category", names(df))	# find the rating scale
    ind1 <- match("Obs_pSr1", names(df))		# find observed posterior prob of signal 1
    ind2 <- match("Pred_pSr1", names(df))		# find predicted posterior prob of signal 1

    par(pty="m")                                # don't make square

    # plot posterior probabilities of s1...sn
    # set up the graph
    if(main_title_flag) {
        main <- main_title
    } else {
        main <- paste("Data Set ", sn, ": ", name, "\n",
                      "Posterior Probability of Signal\n",
                      model_type, " model")
    }
    plot(0, 0, type = "n",
         xlim = c(1, nrat),
         ylim = c(0, 1),
         xlab = "Rating Categories",
         ylab = "Posterior Probability of Signal, P(s|R)",
         main = main)

    # plot the posterior probabilities (s2...)
    for (i in 0:(nsig-2)){
        points(df[, ind0], df[, ind1+i],
               type = "p",
               col = colorsForPlots[i+1],
               pch = 19)
        lines(df[, ind0], df[, ind2+i], lwd = 2, col = colorsForPlots[i+1])
    }

    # make aic character string for legend
    if (!is.na(aic))
        str_aic <- paste("AIC =", round(aic, 2)) else str_aic <- ""

    # add legend
    legend("bottomright", cex = 0.6, bty = "n",
           c(str_aic))

    par(pty="m")
}

# 8. plot_pdfs()
# ----------------------------------------------------
# Signal Detection Model with Decision Criteria
plot_pdfs <- function(df_grf,
                      data_set = 1,
                      main_title = NA,
                      aic = NA,
                      da = NA,
                      Az = NA) {
    sn <- data_set
    ndata_sets <- length(levels(as.factor(df_grf$data_set)))
    main_title_flag <- !is.na(main_title)

    # make sure sn is within range in data frame
    if(sn < 0 || sn > ndata_sets) sn <- 1

    df <- subset(df_grf, data_set == sn)	# select a data set
    nsig <- df$nsig[1]						# number of signals
    nrat <- df$nrat[1]						# number of ratings
    ncrt <- nrat - 1						# number of criteria
    name <- df[1, 1]                        # heading
    model_type <- df$Model[1]				# get the model type
    if ("aic" %in% colnames(df)) aic <- df$aic[1] else aic <- NA

    par(pty="m")

    # find the maximum density value for setting y-axis
    ind0 <- match("Pred_Density0", names(df))	# find s0 density column
    ind1 <- ind0 + nsig - 1
    max_density <- 1.2 * max(df[, ind0:ind1], na.rm = TRUE)
    min_density <- -0.03
    rng_density <- max_density - min_density
    min_criterion <- min(df$Xc, na.rm = TRUE)
    max_criterion <- max(df$Xc, na.rm = TRUE)
    sig_labels <- paste("s", 0:(nsig-1), sep = "")
    crit_density_offset <- rng_density * 0.05

    # set up the graph
    if(main_title_flag) {
        main <- main_title
    } else {
        main <- paste("Data Set ", sn, ": ", name, "\n",
                      "Signal Detection Model with Decision Criteria\n",
                      model_type, " model")
    }
    plot(0, 0, type = "n",
         lwd = 2,
         xlim = c(min(df$Xc, na.rm = TRUE), max_criterion),
         ylim = c(min_density, max_density),
         xlab = "Decision Axis",
         ylab = "Probability Density",
         main = main)

    # plot the probability density functions
    for (i in 1:nsig){
        lines(df[, ind0+i-1] ~ Xc,
              data = df,
              type = "l",
              lwd = 2,
              col = c(colorForS0, colorsForPlots)[i])
        # plot the signal label
        maxd <- max(df[, ind0+i-1], na.rm = TRUE)
        maxx <- df$Xc[match(maxd, df[, ind0+i-1])]
        text(x = maxx ,
             y = maxd,
             labels = sig_labels[i],
             pos = 3,
             offset = .9,
             col = c(colorForS0, colorsForPlots)[i], cex = 0.8)
    }
    abline(h = 0, col = "gray", lty = "solid")

    # extract the criterion coordinates
    crit <- subset(df, Criteria != is.na(Criteria))[c("Xc", "Xc_se", "Criteria")]
    xc <- df$Xc[1:ncrt]

    # plot the decision criterion markers
    for (i in 1:ncrt){
        segments(crit$Xc[(2*i)-1], crit$Criteria[(2*i)-1],
                 crit$Xc[2*i],     crit$Criteria[2*i] + crit_density_offset,
                 lwd = 2,
                 col = colorForXc)
    }

    # plot the standard error of each criterion marker
    for (i in 1:ncrt){
        segments(crit$Xc[2*i]-crit$Xc_se[2*i], crit$Criteria[2*i] + crit_density_offset,
                 crit$Xc[2*i]+crit$Xc_se[2*i], crit$Criteria[2*i] + crit_density_offset,
                 lwd = 2,
                 col = colorForXc)
    }

    # label the response categories
    xc_lab <- rep(0, nrat)
    if (nrat > 2){
        xc_lab[2:ncrt] <- xc[1:ncrt-1] + diff(xc)/2
        xc_lab[1] <- xc[1] - (diff(xc)/2)[1]
        xc_lab[nrat] <- xc[ncrt] + (diff(xc)/2)[ncrt-1]
    }
    else {
        xc_lab[1] <- xc[1] - 0.5
        xc_lab[nrat] <- xc[1] + 0.5
    }
    text(xc_lab, -rng_density * 0.025, labels = 1:nrat, cex = 0.7)

    # make da character string for legend
    if (!is.na(da) && nsig == 2)
        str_da <- paste("da =", round(da, 2)) else str_da <- ""

    # make Az character string for legend
    if (!is.na(Az) && nsig == 2)
        str_Az <- paste("Az =", round(Az, 2)) else str_Az <- ""

    # make aic character string for legend
    if (!is.na(aic))
        str_aic <- paste("AIC =", round(aic, 2)) else str_aic <- ""

    # add legend
    legend("topright", cex = 0.6, bty = "n",
           c(str_aic, str_da, str_Az))

    par(pty="m")
}

# 9. plot_allg()
# ----------------------------------------------------
# draw all eight plots in one panel
plot_allg <- function(df_grf,
                      sn = 1,
                      main_title = NA) {

    par (mfcol = c(4 ,2))
    plot_proc(df_grf, sn, main_title = main_title)
    plot_zroc(df_grf, sn, main_title = main_title)
    plot_freq(df_grf, sn, main_title = main_title)
    plot_prob(df_grf, sn, main_title = main_title)
    plot_pcoc(df_grf, sn, main_title = main_title)
    plot_resd(df_grf, sn, main_title = main_title)
    plot_post(df_grf, sn, main_title = main_title)
    plot_pdfs(df_grf, sn, main_title = main_title)
    par (mfcol = c(1 ,1))
}

# A. read_grfn <- function(grf_file_name)
read_grfn <- function(grf_file_name) {
    # check that the input file is an RscorePlus graphics file
    if (!(grepl("_grf.txt", grf_file_name))) {
        stop("The input file must be a grf file produced by RscorePlus")
    }

    # Read the RscorePlus *_grf_txt file into a data frame
    df_grf <- read.delim(grf_file_name, header = TRUE)  # reads tab delimited file
    return(df_grf)
}

# B_ read_altn <- function(alt_file_name)
read_altn <- function(alt_file_name) {
    # check that the input file is an RscorePlus alternative data file
    if (!(grepl("_alt.txt", alt_file_name))) {
        stop("The input file must be an alt file produced by RscorePlus")
    }

    # Read the RscorePlus *_alt_txt file into a data frame
    df_alt <- read.delim(alt_file_name, header = TRUE)		# reads tab delimited file
    return(df_alt)
}
