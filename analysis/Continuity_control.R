library(grid)
library(ggplot2)
library(bootstrap)
library(lme4)
library(stringr)
library(plotrix)
library(reshape2)
library(plyr)

d1 <- read.csv("~/Desktop/disc_wl/data/control.csv")

# anonymize subject ids by giving them a value 1:num_subjects
anonymize.sids <- function(df, subject_column_label) {
  subj_col = which(names(df) == subject_column_label) # get workerid column index
  temp <- data.frame(workerid = unique(df[,subj_col])) # make new df of unique workerids
  temp$subid <- 1:length(unique(df[,subj_col])) # make list of subids
  index <- match(df[,subj_col], temp$workerid) 
  df$subids <- temp$subid[index]
  df[,subj_col] <- NULL 
  df$subids  = as.factor(df$subids)
  return(df)
}


##how often second toy is selected
d1$Answer.question = as.factor(d1$Answer.question)
d1$Input.start_side = as.factor(d1$Input.start_side)
d1$matchsecond <- d1$Answer.question != d1$Input.start_side


## aggregate by trial type and condition
agg.data <- aggregate(d1$matchsecond, list(d1$Input.type, d1$Input.condition), FUN=sum)
names(agg.data) <- c("expt","Location", "count")
agg.data.len <- aggregate(d1$matchsecond, list(d1$Input.type, d1$Input.condition ), FUN=length)
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total
agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)

## add some style elements for ggplot2
theme_set(theme_bw())
dodge <- position_dodge(width=0.5) 

##plot control data: 
ggplot(data=agg.data, aes(x=expt, y=prop.corr, fill=Location))+geom_bar(stat="identity", position=position_dodge())+ylab("Proportion of Second Toy Selections") +
xlab("Condition") +
#facet_wrap(~expt) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ geom_abline(intercept=.5,slope=0,lty=2)



