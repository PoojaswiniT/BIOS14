library(sciplot) #for standard error calculation

dat = read.delim("C:/R data/BIOS14/tephritis.txt") #reading the datafile
dat_o = na.omit(dat) #removing NA from the data
names(dat) #to read names in the data file
head(dat) #to check the beggining of the data

#changing characters into factors
dat_o$Patry = as.factor(dat_o$Patry)
dat_o$Hostplant = as.factor(dat_o$Hostplant)
dat_o$Sex = as.factor(dat_o$Sex)
dat_o$Baltic = as.factor(dat_o$Baltic)

#fitting the data into a linear model, with Ovipositor length as a response variable
#Hostplant and Patry as combined effect variables

m1 = lm(dat_o$OL ~ dat_o$Hostplant*dat_o$Patry, data = dat)
m2 = lm(dat_o$OL ~ dat_o$Hostplant+dat_o$Patry, data = dat)
m3 = lm(dat_o$OL ~ dat_o$Hostplant, data = dat)
m4 = lm(dat_o$OL ~ dat_o$Patry, data = dat)
m5 = lm(dat_o$OL ~ 1, data = dat)

#AIC analysis
mlist = list(m1, m2, m3, m4,m5)
AICTab = AIC(m1, m2, m3, m4,m5)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

#summary
summary(m1)
#anova analysis for the best fit model
a = anova(m1)

#mean and standard error calculation

means = tapply(dat_o$OL, list(dat_o$Hostplant, dat_o$Patry), mean)
ses = tapply(dat_o$OL, list(dat_o$Hostplant, dat_o$Patry), se)

#mean Ovipositor Length

mean_ov = rowMeans(means) #calculates the mean ovipositor length across different levels of Patry for each level of Hostplant.
mean_ov1 = colMeans(means)

#in terms of percentage
perc = (mean_ov[2]-mean_ov[1])/mean_ov[2] *100
perc

perc2 = (mean_ov1[2]-mean_ov1[1])/mean_ov1[2] *100
perc2
#plotting the data

plot(c(0.97, 1.03), means[,1], ylim=c(1, 2), xlim=c(0.8, 2.2),
     xlab="Host Plant", 
     ylab="Ovipositor length",
     xaxt="n", las=1, pch=c(21,16), col="white") #xaxt hides the x- axis values
axis(1, 1:2, labels=c(expression(italic("C.Heterophyllum")), expression(italic("C.Oleraceum"))))

arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03), 
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03), 
       means[,2]+ses[,2], length=0.05, angle=90, code=3)

segments(0.97, means[1,1], 1.97, means[1,2], lty=2) #lty -2 is for dashed lines
segments(1.03, means[2,1], 2.03, means[2,2]) #default is solid 

points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 02.03), means[,2], pch=c(21, 16), bg="white")

legend("topright", c("Patry", "Allopatry", "Sympatry"), 
       bty="n", pch=c(NA,21,16))

