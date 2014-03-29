#############################################
## analyze continuity data for disc_wl project
## ali horowitz, mike frank
##

rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R")

#############################################
## models of kid data first
##
d <- read.csv("data/all_data.csv")

## clean up data
d$agegroup <- as.factor(d$agegroup)
d$corr.side <- "A"
d$corr.side[grepl("B",d$trial.type)] <- "B"
d$corr.side <- factor(d$corr.side)
d$correct <- d$side == as.numeric(d$corr.side)-1

kids <- subset(d, agegroup != "adult")
kids$age <- as.numeric(as.character(kids$age)) # make age continuous again

# treat age as continuous, full model
lm.all <- glmer(side ~ condition * corr.side * age + (corr.side | subid),
	data=kids, family="binomial")	
summary(lm.all) # three-way interaction

# treat age as continuous, partial models
lm.during.cont <- glmer(side ~ age * corr.side + (corr.side | subid),
                  data=subset(kids, condition=="During"), 
                  family="binomial")
summary(lm.during.cont)

lm.after.cont <- glmer(side ~ age * corr.side + (corr.side | subid), 
                   data=subset(kids, condition=="After"),
                   family="binomial")
summary(lm.after.cont)

#############################################
## followup pairwise tests
##

agg.data.s <- aggregate(side ~ condition + corr.side + agegroup + subid,
                        data=d, mean)

ages <- levels(d$agegroup)
conds <- c("During","After")

for (a in ages) {
  for (cond in conds) {
    td <- subset(agg.data.s,agg.data.s$agegroup==a & condition == cond)
    x <- td$side[td$corr.side=="A"]
    y <- td$side[td$corr.side=="B"]
    
    w <- wilcox.test(x, y, paired=TRUE)
    t <- t.test(x, y, paired=TRUE)
    
    print(paste(a, "s, ", cond, ", Wilcox -- p: ",
                round(w$p.value,digits=3),
                ", stat: ", 
                round(w$statistic,digits=3),sep=""))
    print(paste(a, "s, ", cond, ", t-test -- p: ",
                round(t$p.value,digits=3),
                ", stat: ", 
                round(t$statistic,digits=3),
                ", param: ",
                t$parameter,sep=""))
  }
}

#############################################
## PLOT
##

mss <- aggregate(side ~ subid + agegroup + corr.side + condition, data=d, mean)
ms <- aggregate(side ~ agegroup + corr.side + condition, data=mss, mean)
ms$cil <- aggregate(side ~ agegroup + corr.side + condition, data=mss, ci.low)$side
ms$cih <- aggregate(side ~ agegroup + corr.side + condition, data=mss, ci.high)$side
ms$n <- aggregate(subid ~ agegroup + corr.side + condition, data=mss, n.unique)$subid

qplot(agegroup, side, colour=corr.side, facets=.~condition, 
      ymin=side - cil, ymax=side + cih, group=corr.side,
      position=position_dodge(width=.05),
      geom=c("pointrange","line"),
      data=ms)

