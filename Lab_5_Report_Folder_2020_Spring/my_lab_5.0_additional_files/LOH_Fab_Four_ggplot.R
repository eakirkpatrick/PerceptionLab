# Fab Four Plots in ggplot2
# PSYC 4165
# Lew Harvey
# 1 September 2019

# Generic generic strip and box plot main effects functions

# df = data frame with data to be plotted
# dv = dependent variable column name
# iv1 = independent variable 1 column name (usually a condition)
# iv2 = independent variable 2 column name (usually the subject ID)
# use_mu = flag to compute mean value of dv for each level of iv1
#           and each level of iv2 across all remain independent variable levels
# colo = colors for plotting
# xlab = x-axis label for plot
# ylab = y-axis label for plot
# main = main title for plot


plot_strip_main_effect <- function(df = NULL, dv = "?", iv1 = "?", iv2 = "subjectID",
                                   pt_size = 1,
                                   use_mu = FALSE,
                                   between = FALSE,
                                   col = "black",
                                   method = "jitter",
                                   jitter = 0.1,
                                   log = FALSE,
                                   xlab = "",
                                   ylab = "",
                                   main = "Strip Plot",
                                   ...) {

    plt <- ggplot(df, aes_string(x = iv1, y = dv, group = iv2)) +
        {if(log) coord_trans(y = "log10")} +
        {if(!between & !use_mu) geom_line(color = "gray")} +
        {if(!between &  use_mu) geom_line(color = "gray", stat = "summary", fun.y = "mean")} +

        {if(!between & !use_mu) geom_point(size = pt_size, alpha = 0.8)} +
        {if(!between &  use_mu) geom_point(stat = "summary", fun.y = "mean")} +

        {if(between & !use_mu) geom_point(color = col, size = pt_size)} +
        {if(between &  use_mu) geom_point(color = col, stat = "summary", fun.y = "mean")} +

        labs(x = xlab,
             y = ylab,
             title = main)

    return(plt)
}

plot_box_main_effect <- function(df = NULL, dv = "?", iv1 = "?", iv2 = "subj",
                                 use_mu = FALSE,
                                 col = "yellow",
                                 log = FALSE,
                                 xlab = "",
                                 ylab = "",
                                 main = "Box Plot",
                                 ...) {

    plt <- ggplot(df, aes_string(x = iv1, y = dv)) +
        {if(log) coord_trans(y = "log10")} +
        {if(!use_mu) geom_boxplot(fill = col)} +
        {if( use_mu) geom_boxplot(aes(stat = "summary", fun.y = "mean"), fill = col) }+
        labs(x = xlab,
             y = ylab,
             title = main)

    return(plt)
}

plot_box_effect_size <- function(bs = NULL,
                                 col = "yellow",
                                 log = "FALSE",
                                 xlab = "Main Effect",
                                 ylab = "Effect Size",
                                 main = "Effect Size",
                                 ...) {

    # insure that 0 is included in the plotted range
    ymin <- min(0, min(bs$t))
    ymax <- max(0, max(bs$t))

    df.t <- data.frame(t = bs$t)
    df.b <- data.frame(b = c(bs$t0, bs$bounds), x = -0.75,
                       label = c("Mean", "Lower CI", "Upper CI"),
                       nudge_y = c(0.1, -0.1, 0.1))

    plt <- ggplot(df.t, aes(y = t)) +
        geom_boxplot(fill = col) +
        lims(x = c(-1.2, 1), y = c(ymin, ymax)) +
        geom_hline(yintercept = c(bs$t0, bs$bounds), col = "gray") +
        geom_hline(yintercept = c(0), col = "red") +
        geom_text(data = df.b, aes(x = x, y = b, label = label), size = 3) +
        labs(x = xlab,
             y = ylab,
             title = main) +
        theme(axis.ticks.x = element_line(size = 0),
              axis.text.x = element_text(size = 0))

    return(plt)
}



# grid.arrange(p1, p2, p3, p4, ncol=2)
