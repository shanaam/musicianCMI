#############################
## MUSICIAN CMI EXPERIMENT ##
#############################

#set the working directory to wherever this file is located
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#read in raw data files
rawDataStandard <- read.csv("data/standard_BrDIMeansZScores.csv")