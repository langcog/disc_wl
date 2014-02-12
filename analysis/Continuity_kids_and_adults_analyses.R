#########  analyze
rm(list=ls())
library(reshape)
library(bootstrap)
library(ggplot2)
library(lme4)

############ Kids only #############################

data <- read.csv("Continuity_kids.csv")


### LMER
data$agegroup <- as.factor(data$agegroup)
data$corr.side <- "A"
data$corr.side[grepl("B",data$trial.type)] <- "B"
data$corr.side <- factor(data$corr.side)

# treat age as continuous, full model
lm1 <- lmer(side ~ condition * corr.side * Age + (corr.side | subID),
	data=data,family="binomial")	




############ UPDATED mixed models

### LMER
data$agegroup <- as.factor(data$agegroup)
data$corr.side <- "A"
data$corr.side[grepl("B",data$trial.type)] <- "B"
data$corr.side <- factor(data$corr.side)
data$gender <- factor(data$gender)
data$correct <- data$side == as.numeric(data$corr.side)-1

# treat age as continuous, full model
lm.all.cont <- lmer(side ~ condition * corr.side * Age + (1 | subID), data=data, family="binomial")

lm.d.cont <- lmer(side ~ Age * corr.side + (1 | subID),data=subset(data,condition=="During"), family="binomial")

lm.a.cont <- lmer(side ~ Age * corr.side + (1 | subID), data=subset(data,condition=="After"),family="binomial")
 













######## Kids and adults #############################

data <- read.csv("~/new compy/Stanford/Research/Frank Lab/Experiments/Continuity/disc_wl/data/Continuity_kids_and_adults.csv")

data$agegroup <- as.factor(data$agegroup)
data$corr.side <- "A"
data$corr.side[grepl("B",data$trial.type)] <- "B"
data$corr.side <- factor(data$corr.side)
data$gender <- factor(data$gender)

data$correct <- data$side[1]



########## mixed model for "DURING"
during_only <- subset(data,condition=="During")

lm_during <- lmer(side ~ agegroup * corr.side - 1 + (corr.side | subID),data=during_only, family="binomial")



##########mixed model for "After"
after_only <- subset(data,condition=="After")

lm_after <- lmer(side ~ agegroup * corr.side - 1 +(corr.side | subID),data=after_only, family="binomial")



##### FOLLOWUP TESTS ######

####### diff across trial type within condition by age group?

agg.data.s <- aggregate(side ~ condition + corr.side + agegroup + subID,data=data,mean)

ages <- levels(data$agegroup)

for (a in ages) {
	durings <- subset(agg.data.s,agg.data.s$agegroup==a & condition == "During")
	afters <- subset(agg.data.s,agg.data.s$agegroup==a & condition == "After")

	print(paste("Age:",a))
	w1 <- wilcox.test(durings$side[durings$corr.side=="A"],
					  durings$side[durings$corr.side=="B"],paired=T)$p.value
	w2 <- wilcox.test(afters$side[afters$corr.side=="A"],
					  afters$side[afters$corr.side=="B"],paired=T)$p.value
	t1 <- t.test(durings$side[durings$corr.side=="A"],
					  durings$side[durings$corr.side=="B"],paired=T)$p.value
	t2 <- t.test(afters$side[afters$corr.side=="A"],
					  afters$side[afters$corr.side=="B"],paired=T)$p.value
	print(paste("During, Wilcox p: ",round(w1,digits=3),"t-test p:",round(t1,digits=3)))
	print(paste("After, Wilcox p: ",round(w2,digits=3),"t-test p:",round(t2,digits=3)))
}



########################## t values

for (a in ages) {
	durings <- subset(agg.data.s,agg.data.s$agegroup==a & condition == "During")
	afters <- subset(agg.data.s,agg.data.s$agegroup==a & condition == "After")

	print(paste("Age:",a))
	w1 <- wilcox.test(durings$side[durings$corr.side=="A"],
					  durings$side[durings$corr.side=="B"],paired=T)$statistic
	w2 <- wilcox.test(afters$side[afters$corr.side=="A"],
					  afters$side[afters$corr.side=="B"],paired=T)$statistic
	t1 <- t.test(durings$side[durings$corr.side=="A"],
					  durings$side[durings$corr.side=="B"],paired=T)$statistic
	t2 <- t.test(afters$side[afters$corr.side=="A"],
					  afters$side[afters$corr.side=="B"],paired=T)$statistic
	print(paste("During, Wilcox p: ",round(w1,digits=3),"t-test t:",round(t1,digits=3)))
	print(paste("After, Wilcox p: ",round(w2,digits=3),"t-test t:",round(t2,digits=3)))
}



########################## parameter aka degrees of freedom 

agg.data.s <- aggregate(side ~ condition + corr.side + agegroup + subID,data=data,mean)

ages <- levels(data$agegroup)

for (a in ages) {
	durings <- subset(agg.data.s,agg.data.s$agegroup==a & condition == "During")
	afters <- subset(agg.data.s,agg.data.s$agegroup==a & condition == "After")

	print(paste("Age:",a))
	w1 <- wilcox.test(durings$side[durings$corr.side=="A"],
					  durings$side[durings$corr.side=="B"],paired=T)$parameter
	w2 <- wilcox.test(afters$side[afters$corr.side=="A"],
					  afters$side[afters$corr.side=="B"],paired=T)$parameter
	t1 <- t.test(durings$side[durings$corr.side=="A"],
					  durings$side[durings$corr.side=="B"],paired=T)$parameter
	t2 <- t.test(afters$side[afters$corr.side=="A"],
					  afters$side[afters$corr.side=="B"],paired=T)$parameter
	print(paste("During, Wilcox p: ","t-test p:",round(t1,digits=3)))
	print(paste("After, Wilcox p: ","t-test p:",round(t2,digits=3)))
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






