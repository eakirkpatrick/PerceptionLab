
# This script loops through Rscore graphic file (grf)
# and creates a panel of eight graphs for each data set.
#
# Lewis O. Harvey, Jr.
# Department of Psychology and Neuroscience
# University of Colorado Boulder
# Boulder, Colorado, USA
#
# 11 September 2018
#######################################################################

# load SDT plot functions
source("LOH_Rscore_Graphs.R")

# choose the input file
fn <- file.choose()

# Read the RscorePlus *_grf.txt file into a data frame
df <- read_grfn(fn)

par(ask = FALSE)
for (sn in 1:length(levels(factor(df$data_set))))
{
    if (sn  > 1)  par(ask = TRUE)
    plot_allg(df, sn)
}
par(ask = FALSE)
