#############################################
## analyze continuity data for disc_wl project
## ali horowitz, mike frank
##

rm(list=ls())
library(reshape)
library(bootstrap)
library(ggplot2)
library(lme4)

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
 
# treat age as discrete, partial models
lm.during.disc <- glmer(side ~ agegroup * corr.side - 1 + (corr.side | subid),
                        data=subset(kids, condition=="During"), 
                        family="binomial")
summary(lm.during.disc)

lm.after.disc <- glmer(side ~ agegroup * corr.side + (corr.side | subid), 
                       data=subset(kids, condition=="After"),
                       family="binomial")
summary(lm.after.disc)


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





###2s
binom.test(17,32) 	#After A = 0.86
binom.test(22,32) 	#After B = 0.0501*
binom.test(11,32)	#During A = 0.1102
binom.test(19,32)	#During B = 0.3771

###3s
binom.test(17,32)	#After A = 0.86
binom.test(22,32)	#After B = 0.0501
binom.test(10,32)	#During A = 0.0501
binom.test(19,32)	#During B = 0.3771

###4s
binom.test(13,32)	#After A = 0.3771
binom.test(22,32)	#After B = 0.0501 
binom.test(10,32)	#During A = 0.0501
binom.test(26,32)	#During B = 0.000535

###5s
binom.test(10,32)	#After A = 0.0501
binom.test(18,32)	#After B = 0.5966
binom.test(6,32)	#During A = 0.000535
binom.test(30,32)	#During B = 2.463e-07

###adults
binom.test(10,24)	#After A = 0.5413
binom.test(15,24)	#After B = 0.3075
binom.test(2,24)	#During A = 3.588e-05
binom.test(22,24)	#During B = 3.588e-05







############# means ###############
agg.data <- aggregate(data$side, list(data$condition, data$agegroup), FUN=sum)
agg.data.len <- aggregate(data$side, list(data$condition, data$agegroup), FUN=length)
agg.data$x <- agg.data$x 
agg.data.len$x <- agg.data.len$x 

names(agg.data) <- c("condition", "agegroup", "count")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total

agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)






