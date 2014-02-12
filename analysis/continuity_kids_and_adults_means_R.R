###################### averages by condition
	
rm(list=ls())
library(reshape)
library(bootstrap)
library(ggplot2)
library(binom)

######## VARIOUS PRELIMINARIES ##########
plot.style <- opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),
                   axis.line = theme_segment(colour="black",size=.5),
                   axis.ticks = theme_segment(size=.5),
                   axis.title.x = theme_text(vjust=-.5),
                   axis.title.y = theme_text(angle=90,vjust=0.25),
                   panel.margin = unit(1.5,"lines"))

theme_set(theme_bw())


theta <- function(x,xdata) {mean(xdata[x])}
ci.h <- function(x) {quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}
ci.l <- function(x) {mean(x) - quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}

####### BEGIN #######

data <- read.csv("/Users/Ali/new compy/Stanford/Research/Frank Lab/Experiments/Continuity/Continuity4_4 trials_CDM/Continuity4_pronouns_coding_corrected_eng2.csv")
mdata <- melt(data,id.vars=c("subID","DOT","DOB","Age","List", "agegroup", "condition"),
	measure.vars=c("corr1","corr2","corr3","corr4"))

mdata2 <- melt(data,id.vars=c("subID","DOT","DOB","Age","List","agegroup", "condition"),
	measure.vars=c("trial_type1","trial_type2","trial_type3","trial_type4"))

mdata$trial.type <- mdata2$value
mdata$correct <- mdata$value
mdata$trial.type <- factor(mdata$trial.type,levels=c("During A","After A", "During B", "After B"))

#### full analysis line plot ####
agg.data.s <- aggregate(correct ~ condition + agegroup + subID,data=mdata,mean)
agg.data <- aggregate(correct ~ condition + agegroup,data=agg.data.s,mean)
agg.data$ci.l <-  aggregate(correct ~ condition + agegroup,data=agg.data.s,ci.l)$correct
agg.data$ci.h <-  aggregate(correct ~ condition + agegroup,data=agg.data.s,ci.h)$correct

#agg.data$cond <- "After"
#agg.data$cond[grepl("During",agg.data$trial.type)] <- "During"
#agg.data$cond <- factor(agg.data$cond,levels=c("During","After"))

#agg.data$corr.side <- "A"
#agg.data$corr.side[grepl("B",agg.data$trial.type)] <- "B"
#agg.data$corr.side <- factor(agg.data$corr.side)

## add adults
## this code is TERRIBLE
agg.data <- rbind(agg.data,
                  as.character(c("During",8,.92,
                                 abs(binom.confint(44,48,.95,"bayes")[c("lower","upper")] - 0.92),
                                 "During")),
             
                  as.character(c("After",8,.605,
                                 abs(binom.confint(29,48,.95,"bayes")[c("lower","upper")] - .605),
                                 "After")))
                  
agg.data$correct <- as.numeric(agg.data$correct)
agg.data$ci.h <- as.numeric(agg.data$ci.h)
agg.data$ci.l <- as.numeric(agg.data$ci.l)

agg.data$agegroup.f <- factor(agg.data$agegroup)
levels(agg.data$agegroup.f) <- c("2 - 3 years","3 - 4 years","4 - 5 years","5 - 6 years","Adults")

agg.data$corr.side.noadults <- factor(agg.data$condition,levels=c("During", "After", "C", "D"))
agg.data$corr.side.noadults[agg.data$agegroup.f == "Adults" & agg.data$condition=="During"] <- "C"
agg.data$corr.side.noadults[agg.data$agegroup.f == "Adults" & agg.data$condition=="After"] <- "D"

## actually plot
quartz()
qplot(agegroup.f,correct,geom="line",group=corr.side.noadults,
	colour=condition,data=agg.data, xlab="Age",
      ylab="Proportion of Toy Selections Matching Trial Location ") + 
  scale_colour_manual(name="Trial Location",values=c("green","darkblue")) +
	geom_pointrange(aes(x=agegroup.f,y=correct, ymin=correct-ci.l,ymax=correct+ci.h),
                  position=position_dodge(width=.1)) + 
  geom_abline(intercept=.5,slope=0,lty=2) +
  plot.style
	


######### side by side 2 panel
qplot(agegroup.f,correct, facets=.~condition,geom="line",group=corr.side.noadults,
	colour=condition,data=agg.data, xlab="Age",
      ylab="Proportion of Toy Selections Matching Trial Location ") + 
  scale_colour_manual(name="Trial Location",values=c("magenta","darkblue")) +
	geom_pointrange(aes(x=agegroup.f,y=correct, ymin=correct-ci.l,ymax=correct+ci.h),
                  position=position_dodge(width=.1)) + 
  geom_abline(intercept=.5,slope=0,lty=2) +
  plot.style
	
	
	
	############# means ###############


agg.data <- aggregate(mdata$correct, list(mdata$condition, mdata$agegroup), FUN=sum)
agg.data.len <- aggregate(mdata$correct, list(mdata$condition, mdata$agegroup), FUN=length)
agg.data$x <- agg.data$x 
agg.data.len$x <- agg.data.len$x 

names(agg.data) <- c("condition", "agegroup", "count")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total

agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)




	

