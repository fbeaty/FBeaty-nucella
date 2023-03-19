#Based on Katie Marshall's script

##now survival##
length <- meso_surv_1 %>% 
  rename("survival" = "Dead",
         "population" = "SR",
         "treatment" = "Treat",
         'meso' = "Tank") %>% 
  mutate(population = as.factor(population),
         treatment = as.numeric(as.character(treatment)))

str(length)
#population should be a factor

survival <- aggregate(length$survival > 0,by=list(length$population,length$treatment),FUN=sum,na.rm =T)
total <- aggregate(length$survival > -9,by=list(length$population,length$treatment),FUN=sum,na.rm =T)

names(survival)[3] <- "survived"
survival$total <- total$x
survival$prop.survived <- survival$survived/survival$total
survival$se.survived <- sqrt((survival$prop.survived*(1-survival$prop.survived)/survival$total))

#write.csv(survival,"aggregatedsurvival.csv")
str(survival)
#Now she's subsetting the data, so these won't necessasrily apply
no.var <- subset(survival,survival$Group.2 != "var")
survival$Group.2 <- as.numeric(as.character(survival$Group.2))

#make reed be Central Coast (cc) and tofino be Strait of Georgia (sog)
cc <- subset(survival,survival$Group.1 == "Central Coast")
sog <- subset(survival,survival$Group.1 == "Strait of Georgia")

cc <- cc[order(cc$Group.2),]
sog <- sog[order(sog$Group.2),]

plot(survival$prop.survived~survival$Group.2,ylim=c(0,1),col=c(cols[2],cols[1],cols[2],cols[1],cols[2],cols[1],cols[2],cols[1]),pch=16,cex=1.7,xlab="Temperature treatment (°C)",
     ylab="Survival",las=1,cex.lab=1.55,xlim=c(4,20))
lines(cc$Group.2,cc$prop.survived,col=cols[2],lwd=2,lty=2)
lines(sog$Group.2,sog$prop.survived,col=cols[1],lwd=2,lty=2)

segments(cc$Group.2,cc$prop.survived + cc$se.survived,cc$Group.2,cc$prop.survived - cc$se.survived)
arrows(cc$Group.2,cc$prop.survived+cc$se.survived,cc$Group.2,cc$prop.survived-cc$se.survived,angle=90,code=3,length=.2,col=cols[1],lwd=2)

segments(sog$Group.2,sog$prop.survived + sog$se.survived,sog$Group.2,sog$prop.survived - sog$se.survived)
arrows(sog$Group.2,sog$prop.survived+sog$se.survived,sog$Group.2,sog$prop.survived-sog$se.survived,angle=90,code=3,length=.2,col=cols[2],lwd=2)

abline(h= survival$prop.survived[7],col=cols[1],lty=2,lwd=2)
abline(h= survival$prop.survived[8],col=cols[2],lty=2,lwd=2)

abline(h=survival$prop.survived[7] + survival$se.survived[7],col=cols[1],lty=2)
abline(h=survival$prop.survived[7] - survival$se.survived[7],col=cols[1],lty=2)

abline(h=survival$prop.survived[8] + survival$se.survived[8],col=cols[2],lty=2)
abline(h=survival$prop.survived[8] - survival$se.survived[8],col=cols[2],lty=2)



survival$Group.2 <- factor(survival$Group.2)
cc <- subset(survival,survival$Group.1 == "Central Coast")
sog <- subset(survival,survival$Group.1 == "Strait of Georgia")

cc$Group.2 <- relevel(cc$Group.2,ref="12")
sog$Group.2 <- relevel(sog$Group.2,ref="12")

plot(cc$prop.survived~cc$Group.2,pch="n",cex=1.7,xlab="Temperature Acclimation ")

survival.glm <- glm(survival~population*treatment, data=length,family="binomial")
summary(survival.glm)
anova(survival.glm,test="Chisq")

fit <- glmer(survival ~ population*treatment + (1 | meso:treatment) + (1 | meso:population:treatment), family="binomial",data = length)
summary(fit)
anova(fit)
Anova(fit)

