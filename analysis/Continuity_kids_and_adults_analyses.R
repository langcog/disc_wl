#############################################
## analyze continuity data for disc_wl project
## ali horowitz, mike frank
##

rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # github.com/langcog/Ranalysis

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

for (cond in conds) {
  for (a in ages) {
    td <- subset(agg.data.s, agg.data.s$agegroup==a & condition == cond)
    x <- td$side[td$corr.side=="A"]
    y <- td$side[td$corr.side=="B"]
    
    w <- wilcox.test(x, y, paired=TRUE)
    t <- t.test(x, y, paired=TRUE)
    
#     print(paste(a, "s, ", cond, ", Wilcox -- p: ",
#                 round(w$p.value,digits=3),
#                 ", stat: ", 
#                 round(w$statistic,digits=3),sep=""))
    print(paste(a, "s, ", cond, ", t-test -- p: ",
                round(t$p.value,digits=3),
                ", stat: ", 
                round(t$statistic,digits=3),
                ", param: ",
                t$parameter,sep=""))
  }
}

## t-tests for after-second toy trials against chance. 

t.test(subset(agg.data.s, agg.data.s$agegroup=="2" & 
                          condition == "After" & 
                          corr.side=="B")$side - .5)

t.test(subset(agg.data.s, agg.data.s$agegroup=="3" & 
                condition == "After" & 
                corr.side=="B")$side - .5)

t.test(subset(agg.data.s, agg.data.s$agegroup=="4" & 
                condition == "After" & 
                corr.side=="B")$side - .5)

t.test(subset(agg.data.s, agg.data.s$agegroup=="5" & 
                condition == "After" & 
                corr.side=="B")$side - .5)


t.test(subset(agg.data.s, agg.data.s$agegroup=="adult" & 
                condition == "After" & 
                corr.side=="B")$side - .5)

#############################################
## DESCRIPTIVES
##

mss <- aggregate(correct ~ subid + agegroup + condition, data=d, mean)
ms <- aggregate(correct ~ agegroup + condition, data=mss, mean)
ms$sd <- aggregate(correct ~ agegroup + condition, data=mss, sd)$correct


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


## barplot
levels(ms$condition) <- c("Experiment 1B","Experiment 1A")
ms$condition <- factor(ms$condition, levels=c("Experiment 1A","Experiment 1B"))
levels(ms$agegroup) <- c("2","3","4","5","Adults")

qplot(agegroup, side, fill=corr.side, facets=.~condition, 
      stat="identity",
      position="dodge",
      geom="bar",
      data=ms) + 
  geom_linerange(aes(ymin=side - cil, ymax=side + cih),
                 position=position_dodge(width=.9)) + 
  geom_hline(yintercept=.5, lty=2) +
  xlab("Age (Years)") +
  ylab("Proportion of Second Toy Selections") + 
  scale_fill_discrete(name="Name Location",
                       labels=c("First Toy","Second Toy"))
                       

## add gender for fun
mss <- aggregate(side ~ subid + agegroup + corr.side + condition + gender, data=d, mean)
ms <- aggregate(side ~ agegroup + corr.side + condition + gender, data=mss, mean)
ms$cil <- aggregate(side ~ agegroup + corr.side + condition + gender, data=mss, ci.low)$side
ms$cih <- aggregate(side ~ agegroup + corr.side + condition + gender, data=mss, ci.high)$side
ms$n <- aggregate(subid ~ agegroup + corr.side + condition + gender, data=mss, n.unique)$subid

qplot(agegroup, side, colour=corr.side, facets=gender~condition, 
      ymin=side - cil, ymax=side + cih, group=corr.side,
      position=position_dodge(width=.05),
      geom=c("pointrange","line"),
      data=ms)
