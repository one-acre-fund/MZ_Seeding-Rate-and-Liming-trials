# Created by: Jean Honore NDORIMANA
# PURPOSE: Codes for travertine trial
# Prepared: October 2019

#Priliminary Codes
rm(list=ls()) # this clears the r environment
cat("\014") # this clears the screen

library(pacman)
library(tidyverse)
library(plyr)
suppressMessages(library(Rmisc))
library(multcomp)
library(reshape2)
library(foreign)
library(readxl)
library(car)
library(emmeans)
library(lsmeans)
library(yaml)
library(akima)
library(sjmisc)
library(goeveg)
library(ggpubr)
#Setting working Directories
#----------------------------
wd <- "C:/"
dd <- paste(wd, "Data", sep="/")
od <- paste(wd, "Output", sep="/")
list.files(wd)

d <- read.csv("19A-20B Travertine.csv", na.strings = c("NA", "#N/A","N/D"," ", "  ","#VALUE!","#REF!","#DIV/0!"))

#----Variables formatting------
d$cell <- as.factor(d$cell)
d$agzone <- as.factor(d$agzone)
d$Treatment <- as.factor(d$Treatment)
d$Block <- as.factor(d$Block)
d$Yield_t_ha <- as.numeric(d$Yield_t_ha)
d$Profit_USD_ha <- as.numeric(d$Profit_USD_ha)
##Interaction among DAP, travertine and AEZ in the first season of travertine application on maize yield
table(d$Treatment)
d$DAP <- NA
d$DAP <- ifelse(d$Treatment=="Compost_Fertilizer"|d$Treatment=="Compost_Fertilizer_Travertine_1.5"
                |d$Treatment=="Compost_Fertilizer_Travertine_2.5", "1", "0")
d$DAP <- ifelse(d$Treatment =="Compost_Fertlizer_Travertine_2.5", 1,d$DAP)
View(d)
d$Travertine <- NA
d$Travertine <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_1.5" |d$Treatment=="	Compost_Fertlizer_Travertine_2.5"
                       |d$Treatment=="Compost_Travertine_2.5", "1", "0")
d$Travertine <- ifelse(d$Treatment =="Compost_Fertlizer_Travertine_2.5", 1,d$Travertine)
View(d)
## Let us drop "Compost_Fertilizer_Travertine_1.5" since it has only one level
d1 <- d[!(d$Treatment=="Compost_Fertilizer_Travertine_1.5"),]
d1$DAP <- as.factor(d1$DAP)
d1$Travertine <- as.factor(d1$Travertine)
anova_i <- aov(Yield_t_ha~DAP*Travertine*agzone,data=d1)
summary(anova_i)
#DAP has influenced yield (p<0.0001), travertine did not (p=0.54), Agzone did (p<0.0000), no interaction
#between DAP and Trav (p=0.19), DAP and Agz interact (p=0.003), Trav and agz do not interact (p=0.61)
#DAP, trav, Agz do not interact (p=0.89)
#--------------Interaction plot---------------------
d1<-d
d1$Treatment1 <- NA
d1$Treatment1 <-ifelse(d1$Treatment=="Compost"|d1$Treatment=="Compost_Fertilizer", "No Travertine","Travertine_2.5")
table(d1$Treatment1)
interaction.plot(d1$DAP,d1$Treatment1,d1$Yield_t_ha,
                 fun = mean,
                 xlab="Fertilizer rate",ylab="Yield_tn_ha",
                 main=" 19A DAP X Travertine interaction",
                 ylim=c(3,5),trace.label="Treatment",type="b", col=c("blue4", "red4"),
                 pch=c(19,17,15),fixed=TRUE)
x$Treatment
x$pHi <- as.numeric(x$pHi)
x$Profit_USD_ha <- as.numeric(x$Profit_USD_ha)
x0 = x[x$Treatment=="Compost_Fertilizer"|x$Treatment=="Compost_Fertilizer_Travertine_2.5",]
xb = x[x$Treatment=="Compost_Fertilizer"|x$Treatment=="Compost_Fertilizer_Travertine_1.5",]
table(x0$Treatment)
table(xb$Treatment)
# Old formula
p <- ggplot(d1, aes(x = pHi, y = Profit_USD_ha, color=Treatment) ) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) #added SE by replacing se = FALSE with se = TRUE
p1 <- p + xlim(4.5, 6) + ylim(1500,2000)
print(p1 + labs( title= "1 season Profitable pH threshold", y="Profit", x = "pH"))

# New Formula
ggplot(x0, aes(pHi, Profit_USD_ha, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Profit") +
  ggtitle("19A Profitable pH threshold")
ggplot(xb, aes(pHi, Profit_USD_ha, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Profit") +
  ggtitle("19A Profitable pH threshold")

# Since the lines are crosscutting each other, this confirms that there is interaction effect between travertine and DAP

##Treatment AEZ interaction
anova_itaez <- aov(Yield_t_ha~Treatment*agzone,data=d)
summary(anova_itaez)
# there is no influence of agzone on treatments yield at p=0.12, however,
## Even though there is no interaction between treatments and agzone (p=0.12), let us do analysis by agzs
###Congo nile 19A ANOVA Yield
#subste database by Congo nile agz
list(d$agzone)
dc=d[d$agzone=="Congo Nile",]
View(dc)
table(dc$agzone)
aggregate(dc$Yield_t_ha,by=list(dc$Treatment),FUN=mean,na.rm=T)
anova_yieldc <- aov(Yield_t_ha~Treatment+cell+Block,data=dc)
summary(anova_yieldc)
tuk <- glht(anova_yieldc, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by lake kivu agz
list(d$agzone)
dk=d[d$agzone=="Lake Kivu",]
View(dk)
table(dk$agzone)
aggregate(dk$Yield_t_ha,by=list(dk$Treatment),FUN=mean,na.rm=T)
anova_yieldk <- aov(Yield_t_ha~Treatment+cell+Block,data=dk)
summary(anova_yieldk)
tuk <- glht(anova_yieldk, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Eastern Ridges agz
list(d$agzone)
de=d[d$agzone=="Eastern Ridges",]
View(de)
table(de$agzone)
aggregate(de$Yield_t_ha,by=list(de$Treatment),FUN=mean,na.rm=T)
anova_yielde <- aov(Yield_t_ha~Treatment+cell+Block,data=de)
summary(anova_yielde)
tuk <- glht(anova_yielde, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Volcanic cones agz
list(d$agzone)
dv=d[d$agzone=="Volcanic cones",]
View(dv)
table(dv$agzone)
aggregate(dv$Yield_t_ha,by=list(dv$Treatment),FUN=mean,na.rm=T)
anova_yieldv <- aov(Yield_t_ha~Treatment+cell+Block,data=dv)
summary(anova_yieldv)
tuk <- glht(anova_yieldv, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Buberuka agz
list(d$agzone)
db=d[d$agzone=="Buberuka highlands",]
View(db)
table(db$agzone)
aggregate(db$Yield_t_ha,by=list(db$Treatment),FUN=mean,na.rm=T)
anova_yieldb <- aov(Yield_t_ha~Treatment+cell+Block,data=db)
summary(anova_yieldb)
tuk <- glht(anova_yieldb, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
TukeyHSD(anova_yieldb)
###--------------------------------------------------------------------------------------------------
#-----genaral cost, yield, and profit analysis--------
#helping functions:
agg.stats <- function(x, y) {
  
  m=aggregate(x, by=list(y), FUN=mean, na.rm=T)
  ste = aggregate(x, by=list(y), function(z) {
    sqrt(var(z,na.rm=TRUE)/length(na.omit(z)))})
  out <-as.data.frame(paste(round(m$x,2), " (", round(ste$x,2), ")", sep=""))
  names(out) <- "stats"
  return(out)
}

comp.letters <- function(x) {
  tuk <- glht(x, linfct=mcp(Treatment="Tukey"))
  let <- cld(tuk, level=0.05)$mcletters[1]
  return(let$Letters)
}

# Cost,Yield,and profit analysis
#----Variables formatting------
d$agzone <- as.factor(d$agzone)
d$Treatment <- as.factor(d$Treatment)
d$Block <- as.factor(d$Block)
d$cell <- as.factor(d$cell)
d$Cost_USD_ha <- as.numeric(d$Cost_USD_ha)
d$Yield_t_ha <- as.numeric(d$Yield_t_ha)
d$Profit_USD_ha <- as.numeric(d$Profit_USD_ha)

Yield.residues <- function(x) {
  
  cost = agg.stats(d$Cost_USD_ha, d$Treatment)
  yield = agg.stats(d$Yield_t_ha, d$Treatment)
  profit = agg.stats(d$Profit_USD_ha, d$Treatment)
  
  #ANOVA - cost
  aov.cost <- aov(Cost_USD_ha ~ Treatment + agzone + Block, data=d)
  aov.cost.let <- comp.letters(aov.cost)
  
  #ANOVA - yield
  aov.yield <- aov(Yield_t_ha ~ Treatment + agzone + Block, data=d)
  #aov.yield <- aov(Yield_t_ha ~ treatment + agz, data=d)
  aov.yield.let <- comp.letters(aov.yield)
  
  #ANOVA - profit
  aov.profit <- aov(Profit_USD_ha ~ Treatment + agzone + Block, data=d)
  aov.profit.let <- comp.letters(aov.profit)
  
  # design results table
  output <- data.frame (
    N = aggregate(d$Treatment, by=list(d$Treatment), function(x) {length(x)})$x,
    treatment = unique(d$Treatment),
    cost = paste(cost$stats, aov.cost.let, sep = " "),
    yield = paste(yield$stats, aov.yield.let, sep = " "),
    profit = paste(profit$stats, aov.profit.let, sep = " ")
    
  )
  
  return(output)}

res1 = Yield.residues(d)
res1
#----- To arrange the yield by treatment in real order and calculate p-value of difference between treamnts---
aggregate(d$Cost_USD_ha,by=list(d$Treatment),FUN=mean,na.rm=TRUE)
aggregate(d$Yield_t_ha,by=list(d$Treatment),FUN=mean,na.rm=TRUE)
aggregate(d$Profit_USD_ha,by=list(d$Treatment),FUN=mean,na.rm=TRUE)
aov.cost <- aov(Cost_USD_ha ~ Treatment + agzone + Block, data=d)
aov.yield <- aov(Yield_t_ha ~ Treatment + agzone + Block, data=d)
aov.profit <- aov(Yield_t_ha ~ Treatment + agzone + Block, data=d)
summary(aov.cost)
summary(aov.yield)
summary(aov.profit)

#Let us see how soil pH affects the yield in two different categories
### Subsetting data by PH CATEGORY (>= 5.2 AND < 5.2) for 19A Database
View(d)
d$pH_range <- NA
d$pH_range <- ifelse(d$pHi >= "5.2" ,  "> 5.2", "< 5.2")
##co means combined; pH_range and Treatment
d$co <- NA
d$co <- ifelse(d$Treatment=="Compost"& d$pH_range=="> 5.2", "compost>", "compost<")
d$co <- ifelse(d$Treatment=="Compost_Fertilizer"& d$pH_range=="> 5.2", "Compost_Fertilizer>", d$co)
d$co <- ifelse(d$Treatment=="Compost_Fertilizer"& d$pH_range=="< 5.2", "Compost_Fertilizer<", d$co)

d$co <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_1.5"& d$pH_range=="> 5.2", 
               "Compost_Fertilizer_Travertine_1.5>", d$co)
d$co <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_1.5"& d$pH_range=="< 5.2", 
               "Compost_Fertilizer_Travertine_1.5<", d$co)

d$co <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_2.5"& d$pH_range=="> 5.2", 
               "Compost_Fertilizer_Travertine_2.5>", d$co)
d$co <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_2.5"& d$pH_range=="< 5.2", 
               "Compost_Fertilizer_Travertine_2.5", d$co)

d$co <- ifelse(d$Treatment=="Compost_Travertine_2.5"& d$pH_range=="> 5.2", "Compost_Travertine_2.5>", d$co)
d$co <- ifelse(d$Treatment=="Compost_Travertine_2.5"& d$pH_range=="< 5.2", "Compost_Travertine_2.5<", d$co)
table(d$co) #to check if created d$co has all expected variables
###Mode= COMPARING YIELD DIFFERENCE OF SAME TREATMENTS UNDER TWO PH CATEGORY###              
### ******************************************************************##
d$co <- as.factor(d$co)
#####Yield
aggregate(d$Yield_t_ha,by=list(d$co),FUN=mean,na.rm=T)
anova_model<- aov( Yield_t_ha~co,data=d)
summary(anova_model)
TukeyHSD(anova_model)
#####Profit
## <5.2
d1 <- d[!(d$pH_range=="> 5.2"),]
View(d1)
aggregate(d1$Profit_USD_ha,by=list(d1$co),FUN=mean,na.rm=T)
anova_model1<- aov(Profit_USD_ha~co,data=d1)
summary(anova_model1)
TukeyHSD(anova_model1)
tuk <- glht(anova_model1, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
### >5.2
d2 <- d[!(d$pH_range=="< 5.2"),]
View(d2)
aggregate(d2$Profit_USD_ha,by=list(d2$co),FUN=mean,na.rm=T)
anova_model2 <- aov(Profit_USD_ha~co,data=d2)
summary(anova_model2)
TukeyHSD(anova_model2)
tuk <- glht(anova_model2, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld

#-----------------------------------------------------------------------------------------------------
#------------19B"tab"-------------------------------------------
#---importing 19B data
#----Variables formatting------
d$agz <- as.factor(d$agz)
d$treatment <- as.factor(d$treatment)
d$Cell <- as.factor(d$Cell)
d$Block <- as.factor(d$Block)
d$Cost_USD_ha <- as.numeric(d$Cost_USD_ha)
d$Yield_t_ha <- as.numeric(d$Yield_t_ha)
d$Profit_USD_ha <- as.numeric(d$Profit_USD_ha)
##Interaction among DAP, travertine and AEZ in the second season of travertine application on maize yield
table(d$treatment)
d$DAP <- NA
d$DAP <- ifelse(d$treatment=="Compost_Fertilizer"|d$treatment=="Compost_Fertilizer_Travertine_1.5"
                |d$treatment=="Compost_Fertilizer_Travertine_2.5", "1", "0")
View(d)
d$travertine <- NA
d$travertine <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_1.5"|d$treatment=="Compost_Fertilizer_Travertine_2.5"
                       |d$treatment=="Compost_Travertine_2.5", "1", "0")
View(d)
## Let us drop "Compost_Fertilizer_Travertine_1.5" since it has only one level 
d1 <- d[!(d$treatment=="Compost_Fertilizer_Travertine_1.5"),]
View(d1)
d1$DAP <- as.factor(d1$DAP)
d1$travertine <- as.factor(d1$travertine)
anova_i <- aov(Yield_t_ha~DAP*travertine*agz,data=d1)
summary(anova_i)
#DAP has influenced yield (p<0.0001), travertine did (p<0.0001), Agzone did (p<0.0000), no interaction
#between DAP and Trav (p=0.51), DAP and Agz do not interact (p=0.39), Trav and agz interact (p=0.09)
#DAP, trav, Agz do not interact (p=0.94)
d1$treatment1 <- NA
d1$treatment1 <-ifelse(d1$treatment=="Compost"|d1$treatment=="Compost_Fertilizer", "No Travertine","Travertine_2.5")
table(d1$treatment1)
interaction.plot(d1$DAP,d1$treatment1,d1$Yield_t_ha,
                 fun = mean,
                 xlab="Fertilizer rate",ylab="Yield_tn_ha",
                 main=" 19B DAP X Travertine interaction",
                 ylim=c(1,1.6),trace.label="Treatment",type="b", col=c("blue4", "red4"),
                 pch=c(19,17,15),fixed=TRUE)
# Since the lines are not parallel, this further confirms that there is little interaction effect between travertine and DAP
###--------------------------------------------------------------------------------------------------
#-----genaral cost, yield, and profit analysis--------
#helping functions:
agg.stats <- function(x, y) {
  
  m=aggregate(x, by=list(y), FUN=mean, na.rm=T)
  ste = aggregate(x, by=list(y), function(z) {
    sqrt(var(z,na.rm=TRUE)/length(na.omit(z)))})
  out <-as.data.frame(paste(round(m$x,2), " (", round(ste$x,2), ")", sep=""))
  names(out) <- "stats"
  return(out)
}

comp.letters <- function(x) {
  tuk <- glht(x, linfct=mcp(treatment="Tukey"))
  let <- cld(tuk, level=0.05)$mcletters[1]
  return(let$Letters)
}

# Cost,Yield,and profit analysis
#----Variables formatting------
d$agz <- as.factor(d$agz)
d$treatment <- as.factor(d$treatment)
d$Block <- d$Block
d$Cell <- as.factor(d$Cell)
d$Cost_USD_ha <- as.numeric(d$Cost_USD_ha)
d$Yield_t_ha <- as.numeric(d$Yield_t_ha)
d$Profit_USD_ha <- as.numeric(d$Profit_USD_ha)

Yield.residues <- function(x) {
  
  cost = agg.stats(d$Cost_USD_ha, d$treatment)
  yield = agg.stats(d$Yield_t_ha, d$treatment)
  profit = agg.stats(d$Profit_USD_ha, d$treatment)
  
  #ANOVA - cost
  aov.cost <- aov(Cost_USD_ha ~ treatment + agz + Block, data=d)
  aov.cost.let <- comp.letters(aov.cost)
  
  #ANOVA - yield
  aov.yield <- aov(Yield_t_ha ~ treatment + agz + Block, data=d)
    aov.yield.let <- comp.letters(aov.yield)
  
  #ANOVA - profit
  aov.profit <- aov(Profit_USD_ha ~ treatment + agz + Block, data=d)
  aov.profit.let <- comp.letters(aov.profit)
  
  # design results table
  output <- data.frame (
    N = aggregate(d$treatment, by=list(d$treatment), function(x) {length(x)})$x,
    treatment = unique(d$treatment),
    cost = paste(cost$stats, aov.cost.let, sep = " "),
    yield = paste(yield$stats, aov.yield.let, sep = " "),
    profit = paste(profit$stats, aov.profit.let, sep = " ")
    
  )
  
  return(output)}

res1 = Yield.residues(d)
res1
#----- To arrange the yield by treatment in real order and calculate p-value of difference between treamnts---
aggregate(d$Cost_USD_ha,by=list(d$treatment),FUN=mean,na.rm=TRUE)
aggregate(d$Yield_t_ha,by=list(d$treatment),FUN=mean,na.rm=TRUE)
aggregate(d$Profit_USD_ha,by=list(d$treatment),FUN=mean,na.rm=TRUE)
aov.cost <- aov(Cost_USD_ha ~ treatment + agz + Block, data=d)
aov.yield <- aov(Yield_t_ha ~ treatment + agz + Block, data=d)
aov.profit <- aov(Yield_t_ha ~ treatment + agz + Block, data=d)
summary(aov.cost)
summary(aov.yield)
summary(aov.profit)
# we are starting to see the difference among treatments that received both fertilizer and travertine and 
# treatments tha received only fertilizer,let us do analysis by agzs
#---------Treatment AEZ interaction
anova_itaez <- aov(Yield_t_ha~treatment*agz,data=d)
summary(anova_itaez)
## Even though there is no interaction between treatments and agz (p=0.61), however, we saw 
#the difference among treatments that received both fertilizer and travertine and treatments that received
#only fertilizer,let us do analysis by agzs
###Congo nile 19B ANOVA Yield
#subste database by Congo nile agz
list(d$agz)
dc=d[d$agz=="Congo Nile",]
View(dc)
aggregate(dc$Yield_t_ha,by=list(dc$treatment),FUN=mean,na.rm=T)
anova_yield <- aov(Yield_t_ha~treatment+Cell,data=dc)
summary(anova_yield)
tuk <- glht(anova_yield, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Buberuka AEZ
list(d$agz)
db=d[d$agz=="Buberuka",]
View(db)
aggregate(db$Yield_t_ha,by=list(db$treatment),FUN=mean,na.rm=T)
anova_yield <- aov(Yield_t_ha~treatment+Cell,data=db)
summary(anova_yield)
tuk <- glht(anova_yield, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subset database by Lake Kivu AEZ
dl=d[d$agz=="Lake Kivu",]
View(dl)
aggregate(dl$Yield_t_ha,by=list(dl$treatment),FUN=mean,na.rm=T)
anova_yield <- aov(Yield_t_ha~treatment+Cell,data=dl)
summary(anova_yield)
tuk <- glht(anova_yield, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subset database by Lake Kivu AEZ
de=d[d$agz=="Eastern Ridges",]
View(de)
aggregate(de$Yield_t_ha,by=list(de$treatment),FUN=mean,na.rm=T)
anova_yield <- aov(Yield_t_ha~treatment+Cell,data=de)
summary(anova_yield)
tuk <- glht(anova_yield, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subset database by Lake Kivu AEZ
dvc=d[d$agz=="Volcanic cones",]
View(dvc)
aggregate(dvc$Yield_t_ha,by=list(dvc$treatment),FUN=mean,na.rm=T)
anova_yield <- aov(Yield_t_ha~treatment+Cell,data=dvc)
summary(anova_yield)
tuk <- glht(anova_yield, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld

##Let us see how different soil pH in two categories; >=5.2 and <5.2 affect beans yield in second season of travertine application
### Subsetting data by PH CATEGORY (>= 5.2 AND < 5.2) for 19B Database
View(d)
d$pH_range <- NA
d$pH_range <- ifelse(d$pHO >= "5.2" ,  "> 5.2", "< 5.2")
##co means combined; pH_range and Treatment
table(d$treatment)
d$co <- NA
d$co <- ifelse(d$treatment=="Compost"& d$pH_range=="> 5.2", "Compost>", "Compost<")
d$co <- ifelse(d$treatment=="Compost_Fertilizer"& d$pH_range=="> 5.2", "Compost_Fertilizer>", d$co)
d$co <- ifelse(d$treatment=="Compost_Fertilizer"& d$pH_range=="< 5.2", "Compost_Fertilizer<", d$co)

d$co <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_1.5"& d$pH_range=="> 5.2", 
               "Compost_Fertilizer_Travertine_1.5>", d$co)
d$co <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_1.5"& d$pH_range=="< 5.2", 
               "Compost_Fertilizer_Travertine_1.5<", d$co)


d$co <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_2.5"& d$pH_range=="> 5.2", 
               "Compost_Fertilizer_Travertine_2.5>", d$co)
d$co <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_2.5"& d$pH_range=="< 5.2", 
               "Compost_Fertilizer_Travertine_2.5<", d$co)


d$co <- ifelse(d$treatment=="Compost_Travertine_2.5"& d$pH_range=="> 5.2", "Compost_Travertine_2.5>", d$co)
d$co <- ifelse(d$treatment=="Compost_Travertine_2.5"& d$pH_range=="< 5.2", "Compost_Travertine_2.5<", d$co)
table(d$co) #to check if created d$co has all expected variables

#-----Mode= COMPARING YIELD DIFFERENCE OF SAME TREATMENTS UNDER TWO PH CATEGORY (<5.2 and >5.2--------#              
d$co <- as.factor(d$co)
#####Yield
aggregate(d$Yield_t_ha,by=list(d$co),FUN=mean,na.rm=T)
anova_model1<- aov( Yield_t_ha~co,data=d)
summary(anova_model1)
TukeyHSD(anova_model1)
#####Profit 
### <5.2
d1 <- d[!(d$pH_range=="> 5.2"),]
View(d1)
aggregate(d1$Profit_USD_ha,by=list(d1$co),FUN=mean,na.rm=T)
anova_model1<- aov(Profit_USD_ha~co,data=d1)
summary(anova_model1)
TukeyHSD(anova_model1)
tuk <- glht(anova_model1, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
### >5.2
d2 <- d[!(d$pH_range=="< 5.2"),]
View(d2)
aggregate(d2$Profit_USD_ha,by=list(d2$co),FUN=mean,na.rm=T)
anova_model2 <- aov(Profit_USD_ha~co,data=d2)
summary(anova_model2)
TukeyHSD(anova_model2)
tuk <- glht(anova_model2, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
####Soil pH and Yield relationship using regression equation#####
#Import 19A database
#subset database by "Compost_fertilizer" treatment
table(d$Treatment)
d1<-d[d$Treatment=="Compost_Fertilizer",]
table(d1$Treatment)                 
d1$pHi <- as.numeric(d1$pHi)
d1$Yield_t_ha <- as.numeric(d1$Yield_t_ha)
library(tigerstats)
lmGC(Yield_t_ha~pHi,data=d1,graph=TRUE)
#subset database by "Compost_Fertlizer_Travertine_2.5" treatment
d2<-d[d$Treatment=="Compost_Fertlizer_Travertine_2.5",]
table(d2$Treatment)                 
d2$Seasonal_effect <- as.numeric(d2$Seasonal_effect)
d2$Yield_t_ha <- as.numeric(d2$Yield_t_ha)
library(tigerstats)
lmGC(Yield_t_ha~Seasonal_effect,data=d2,graph=TRUE)
##Use of travertine-pH relationship
#subset database by "Compost_Fertlizer_Travertine_2.5" treatment to study travertine-pH relationship
d2<-d[d$Treatment=="Compost_Fertlizer_Travertine_2.5",]
table(d2$Treatment)                 
d2$pHi <- as.numeric(d2$pHi)
d2$pH19B <- as.numeric(d2$pH19B)
library(tigerstats)
lmGC(pH19B~pHi,data=d2,graph=TRUE)
##Use of travertine and Yield relationship
#subset database by "Compost_Fertlizer_Travertine_2.5" treatment 
d2<-d[d$Treatment=="Compost_Fertlizer_Travertine_2.5",]
table(d2$Treatment)                 
d2$pHi <- as.numeric(d2$pHi)
d2$Yield_t_ha <- as.numeric(d2$Yield_t_ha)
library(tigerstats)
lmGC(Yield_t_ha~pHi,data=d2,graph=TRUE)



#-----------------------------------------------------------------------------------------------------
#------------20A"tab"-------------------------------------------
#---importing 20A data
#----Variables formatting------
d$AEZ <- as.factor(d$AEZ)
d$Treatment <- as.factor(d$Treatment)
d$Block <- as.factor(d$Block)
d$Cell <- as.factor(d$Cell)
d$Cost_USD_ha <- as.numeric(d$Cost_USD_ha)
d$Yield_t_ha <- as.numeric(d$Yield_t_ha)
d$Profit_USD_ha <- as.numeric(d$Profit_USD_ha)
ggplot(d, aes(x=Treatment, y=Yield_t_ha)) + geom_boxplot()
##Interaction among DAP, travertine and AEZ in the third season of travertine application on maize yield
table(d$Treatment)
d$DAP <- NA
d$DAP <- ifelse(d$Treatment=="Compost_Fertilizer"|d$Treatment=="Compost_Fertilizer_Travertine_1.5"
                |d$Treatment=="Compost_Fertilizer_Travertine_2.5", "1", "0")
View(d)
d$Travertine <- NA
d$Travertine <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_1.5"|d$Treatment=="Compost_Fertilizer_Travertine_2.5"
                       |d$Treatment=="Compost_Travertine_2.5", "1", "0")
View(d)
## Let us drop "Compost_Fertilizer_Travertine_1.5" since it has only level 
d1 <- d[!(d$Treatment=="Compost_Fertilizer_Travertine_1.5"),]
View(d1)
d1$DAP <- as.factor(d1$DAP)
d1$Travertine <- as.factor(d1$Travertine)
anova_i <- aov(Yield_t_ha~DAP*Travertine*AEZ,data=d1)
summary(anova_i)
#DAP has influenced yield (p<0.0001), travertine did (p=0.19), Agzone did (p<0.0000), no interaction
#between DAP and Trav (p=0.86), DAP and Agz interact (p=0.007), Trav and agz interact (p=0.64)
#DAP, trav, Agz do not interact (p=0.58)
d1$Treatment1 <- NA
d1$Treatment1 <-ifelse(d1$Treatment=="Compost"|d1$Treatment=="Compost_Fertilizer", "No Travertine","Travertine_2.5")
table(d1$Treatment1)
interaction.plot(d1$DAP,d1$Treatment1,d1$Yield_t_ha,
                 fun = mean,
                 xlab="Fertilizer rate",ylab="Yield_tn_ha",
                 main=" 20A DAP X Travertine interaction",
                 ylim=c(3,5),trace.label="Treatment",type="b", col=c("blue4", "red4"),
                 pch=c(19,17,15),fixed=TRUE)
# Since the lines are parallel, this further confirms that there is no interaction effect between 
# travertine and DAP in third season after travertine application
##Treatment AEZ interaction
anova_itaez <- aov(Yield_t_ha~Treatment*AEZ,data=d)
summary(anova_itaez)
# Moreover there is a little interaction between treatments' yields and AEZ at p=0.08
###--------------------------------------------------------------------------------------------------
#-----genaral cost, yield, and profit analysis--------
#helping functions:
agg.stats <- function(x, y) {
  
  m=aggregate(x, by=list(y), FUN=mean, na.rm=T)
  ste = aggregate(x, by=list(y), function(z) {
    sqrt(var(z,na.rm=TRUE)/length(na.omit(z)))})
  out <-as.data.frame(paste(round(m$x,2), " (", round(ste$x,2), ")", sep=""))
  names(out) <- "stats"
  return(out)
}

comp.letters <- function(x) {
  tuk <- glht(x, linfct=mcp(Treatment="Tukey"))
  let <- cld(tuk, level=0.05)$mcletters[1]
  return(let$Letters)
}

# Cost,Yield,and profit analysis
#----Variables formatting------
d$AEZ <- as.factor(d$AEZ)
d$Treatment <- as.factor(d$Treatment)
d$Block <- as.factor(d$Block)
d$Cell <- as.factor(d$Cell)
d$Cost_USD_ha <- as.numeric(d$Cost_USD_ha)
d$Yield_t_ha <- as.numeric(d$Yield_t_ha)
d$Profit_USD_ha <- as.numeric(d$Profit_USD_ha)

Yield.residues <- function(x) {
  
  cost = agg.stats(d$Cost_USD_ha, d$Treatment)
  yield = agg.stats(d$Yield_t_ha, d$Treatment)
  profit = agg.stats(d$Profit_USD_ha, d$Treatment)
  
  #ANOVA - cost
  aov.cost <- aov(Cost_USD_ha ~ Treatment + AEZ + Block, data=d)
  aov.cost.let <- comp.letters(aov.cost)
  
  #ANOVA - yield
  aov.yield <- aov(Yield_t_ha ~ Treatment + AEZ + Block, data=d)
  #aov.yield <- aov(Yield_t_ha ~ treatment + agz, data=d)
  aov.yield.let <- comp.letters(aov.yield)
  
  #ANOVA - profit
  aov.profit <- aov(Profit_USD_ha ~ Treatment + AEZ + Block, data=d)
  aov.profit.let <- comp.letters(aov.profit)
  
  # design results table
  output <- data.frame (
    N = aggregate(d$Treatment, by=list(d$Treatment), function(x) {length(x)})$x,
    treatment = unique(d$Treatment),
    cost = paste(cost$stats, aov.cost.let, sep = " "),
    yield = paste(yield$stats, aov.yield.let, sep = " "),
    profit = paste(profit$stats, aov.profit.let, sep = " ")
    
  )
  
  return(output)}

res1 = Yield.residues(d)
res1
#----- To arrange the yield by treatment in real order and calculate p-value of difference between treamnts---
aggregate(d$Cost_USD_ha,by=list(d$Treatment),FUN=mean,na.rm=TRUE)
aggregate(d$Yield_t_ha,by=list(d$Treatment),FUN=mean,na.rm=TRUE)
aggregate(d$Profit_USD_ha,by=list(d$Treatment),FUN=mean,na.rm=TRUE)
aov.cost <- aov(Cost_USD_ha ~ Treatment + AEZ + Block, data=d)
aov.yield <- aov(Yield_t_ha ~ Treatment + AEZ + Block, data=d)
aov.profit <- aov(Yield_t_ha ~ Treatment + AEZ + Block, data=d)
summary(aov.cost)
summary(aov.yield)
summary(aov.profit)

## As there is little interaction of treatments and AEZ at p=0.08, will do the analysis by AEZs
###Congo nile 20A ANOVA Yield
#subste database by Congo nile AEZ
list(d$AEZ)
dc=d[d$AEZ=="Congo Nile",]
View(dc)
table(dc$AEZ)
aggregate(dc$Yield_t_ha,by=list(dc$Treatment),FUN=mean,na.rm=T)
anova_yieldc <- aov(Yield_t_ha~Treatment+Cell+Block,data=dc)
summary(anova_yieldc)
tuk <- glht(anova_yieldc, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by lake kivu AEZ
list(d$AEZ)
dk=d[d$AEZ=="Lake Kivu",]
View(dk)
table(dk$AEZ)
aggregate(dk$Yield_t_ha,by=list(dk$Treatment),FUN=mean,na.rm=T)
anova_yieldk <- aov(Yield_t_ha~Treatment+Cell+Block,data=dk)
summary(anova_yieldk)
tuk <- glht(anova_yieldk, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Eastern Ridges AEZ
list(d$AEZ)
de=d[d$AEZ=="Eastern Ridges",]
View(de)
table(de$AEZ)
aggregate(de$Yield_t_ha,by=list(de$Treatment),FUN=mean,na.rm=T)
anova_yielde <- aov(Yield_t_ha~Treatment+Cell+Block,data=de)
summary(anova_yielde)
tuk <- glht(anova_yielde, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Volcanic cones AEZ
list(d$AEZ)
dv=d[d$AEZ=="Volcanic cones",]
View(dv)
table(dv$AEZ)
aggregate(dv$Yield_t_ha,by=list(dv$Treatment),FUN=mean,na.rm=T)
anova_yieldv <- aov(Yield_t_ha~Treatment++Block,data=dv)
summary(anova_yieldv)
TukeyHSD(anova_yieldv)
tuk <- glht(anova_yieldv, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Buberuka AEZ
list(d$AEZ)
db=d[d$AEZ=="Buberuka",]
View(db)
table(db$AEZ)
aggregate(db$Yield_t_ha,by=list(db$Treatment),FUN=mean,na.rm=T)
anova_yieldb <- aov(Yield_t_ha~Treatment+Cell++Block,data=db)
summary(anova_yieldb)
tuk <- glht(anova_yieldb, linfct = mcp(Treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
### Subsetting data by PH CATEGORY (>= 5.2 AND < 5.2)
View(d)
d$pH_range <- NA
d$pH_range <- ifelse(d$pHi...19 >= "5.2" ,  "> 5.2", "< 5.2")
##co means combined; pH_range and Treatment
table(d$Treatment)
d$co <- NA
d$co <- ifelse(d$Treatment=="Compost"& d$pH_range=="> 5.2", "Compost>", "Compost<")
d$co <- ifelse(d$Treatment=="Compost_Fertilizer"& d$pH_range=="> 5.2", "Compost_Fertilizer>", d$co)
d$co <- ifelse(d$Treatment=="Compost_Fertilizer"& d$pH_range=="< 5.2", "Compost_Fertilizer<", d$co)

d$co <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_1.5"& d$pH_range=="> 5.2", 
               "Compost_Fertilizer_Travertine_1.5>", d$co)
d$co <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_1.5"& d$pH_range=="< 5.2", 
               "Compost_Fertilizer_Travertine_1.5<", d$co)


d$co <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_2.5"& d$pH_range=="> 5.2", 
               "Compost_Fertilizer_Travertine_2.5>", d$co)
d$co <- ifelse(d$Treatment=="Compost_Fertilizer_Travertine_2.5"& d$pH_range=="< 5.2", 
               "Compost_Fertilizer_Travertine_2.5<", d$co)


d$co <- ifelse(d$Treatment=="Compost_Travertine_2.5"& d$pH_range=="> 5.2", "Compost_Travertine_2.5>", d$co)
d$co <- ifelse(d$Treatment=="Compost_Travertine_2.5"& d$pH_range=="< 5.2", "Compost_Travertine_2.5<", d$co)
table(d$co) #to check if created d$co has all expected variables
###Mode= COMPARING YIELD DIFFERENCE OF SAME TREATMENTS UNDER TWO PH CATEGORY###              
### ******************************************************************##
d$co <- as.factor(d$co)
#####Yield
aggregate(d$Yield_t_ha,by=list(d$co),FUN=mean,na.rm=T)
anova_model<- aov( Yield_t_ha~co,data=d)
summary(anova_model)
TukeyHSD(anova_model)
tuk <- glht(anova_model, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#####Profit 
### <5.2
d1 <- d[!(d$pH_range=="> 5.2"),]
View(d1)
aggregate(d1$Profit_USD_ha,by=list(d1$co),FUN=mean,na.rm=T)
anova_model<- aov(Profit_USD_ha~co,data=d1)
summary(anova_model)
TukeyHSD(anova_model)
tuk <- glht(anova_model, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
### >5.2
d2 <- d[!(d$pH_range=="< 5.2"),]
View(d2)
aggregate(d2$Profit_USD_ha,by=list(d2$co),FUN=mean,na.rm=T)
anova_model2 <- aov(Profit_USD_ha~co,data=d2)
summary(anova_model2)
TukeyHSD(anova_model2)
tuk <- glht(anova_model2, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#-----------------------------------------------------------------------------------------------------
#------------20B"tab"-------------------------------------------
#---importing 20B data
#----Variables formatting------
d$agz <- as.factor(d$agz)
d$treatment <- as.factor(d$treatment)
d$Block <- as.factor(d$Block)
d$Cell <- as.factor(d$Cell)
d$Cost_USD_ha <- as.numeric(d$Cost_USD_ha)
d$Yield_t_ha <- as.numeric(d$Yield_t_ha)
d$Profit_USD_ha <- as.numeric(d$Profit_USD_ha)
##Interaction among DAP, travertine and AEZ in the third season of travertine application on maize yield
table(d$treatment)
d$DAP <- NA
d$DAP <- ifelse(d$treatment=="Compost_Fertilizer"|d$treatment=="Compost_Fertilizer_Travertine_1.5"
                |d$treatment=="Compost_Fertilizer_Travertine_2.5", "1", "0")
View(d)
d$travertine <- NA
d$travertine <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_1.5"|d$treatment=="Compost_Fertilizer_Travertine_2.5"
                       |d$treatment=="Compost_Travertine_2.5", "1", "0")
View(d)
## Let us drop "Compost_Fertilizer_Travertine_1.5" since it has only one level 
d1 <- d[!(d$treatment=="Compost_Fertilizer_Travertine_1.5"),]
View(d1)
d1$DAP <- as.factor(d1$DAP)
d1$travertine <- as.factor(d1$travertine)
anova_i <- aov(Yield_t_ha~DAP*travertine*agz,data=d1)
summary(anova_i)
#DAP has influenced yield (p<0.0001), travertine did (p=0.01), Agzone did (p<0.0000), no interaction
#between DAP and Trav (p=0.97), DAP and Agz interact (p=0.61), Trav and agz interact (p=0.33)
#DAP, trav, Agz do not interact (p=0.96)
d1$treatment1 <- NA
d1$treatment1 <-ifelse(d1$treatment=="Compost"|d1$treatment=="Compost_Fertilizer", "No Travertine","Travertine_2.5")
table(d1$treatment1)
interaction.plot(d1$DAP,d1$treatment1,d1$Yield_t_ha,
                 fun = mean,
                 xlab="Fertilizer rate",ylab="Yield_tn_ha",
                 main=" 20B DAP X Travertine interaction",
                 ylim=c(1,1.6),trace.label="Treatment",type="b", col=c("blue4", "red4"),
                 pch=c(19,17,15),fixed=TRUE)
# Since the lines are parallel, this further confirms that there is no interaction effect between travertine and DAP
##Treatment AEZ interaction
anova_itaez <- aov(Yield_t_ha~treatment*agz,data=d)
summary(anova_itaez)
## Even though there is no interaction between treatments and agz (p=0.93), let us do analysis by agzs
###Congo nile 20B ANOVA Yield
#subste database by Congo nile agz
list(d$agz)
dc=d[d$agz=="Congo Nile",]
View(dc)
table(dc$agz)
table(dc$treatment)
aggregate(dc$Yield_t_ha,by=list(dc$treatment),FUN=mean,na.rm=T)
anova_yieldc <- aov(Yield_t_ha~treatment+Cell+Block,data=dc)
summary(anova_yieldc)
tuk <- glht(anova_yieldc, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by lake kivu agz
list(d$agz)
dk=d[d$agz=="Lake Kivu",]
View(dk)
table(dk$agz)
table(dk$treatment)
aggregate(dk$Yield_t_ha,by=list(dk$treatment),FUN=mean,na.rm=T)
anova_yieldk <- aov(Yield_t_ha~treatment+Cell+Block,data=dk)
summary(anova_yieldk)
tuk <- glht(anova_yieldk, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Eastern Ridges agz
list(d$agz)
de=d[d$agz=="Eastern Ridges",]
View(de)
table(de$agz)
table(de$treatment)
aggregate(de$Yield_t_ha,by=list(de$treatment),FUN=mean,na.rm=T)
anova_yielde <- aov(Yield_t_ha~treatment+Cell+Block,data=de)
summary(anova_yielde)
tuk <- glht(anova_yielde, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Volcanic cones agz
list(d$agz)
dv=d[d$agz=="Volcanic cones",]
View(dv)
table(dv$agz)
table(dv$treatment)
aggregate(dv$Yield_t_ha,by=list(dv$treatment),FUN=mean,na.rm=T)
anova_yieldv <- aov(Yield_t_ha~treatment,data=dv)
summary(anova_yieldv)
tuk <- glht(anova_yieldv, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
#subste database by Buberuka agz
list(d$agz)
db=d[d$agz=="Buberuka",]
View(db)
table(db$agz)
table(db$treatment)
aggregate(db$Yield_t_ha,by=list(db$treatment),FUN=mean,na.rm=T)
anova_yieldb <- aov(Yield_t_ha~treatment+Cell+Block,data=db)
summary(anova_yieldb)
tuk <- glht(anova_yieldb, linfct = mcp(treatment = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
TukeyHSD(anova_yieldb)
###--------------------------------------------------------------------------------------------------
#-----genaral cost, yield, and profit analysis--------
#helping functions:
agg.stats <- function(x, y) {
  
  m=aggregate(x, by=list(y), FUN=mean, na.rm=T)
  ste = aggregate(x, by=list(y), function(z) {
    sqrt(var(z,na.rm=TRUE)/length(na.omit(z)))})
  out <-as.data.frame(paste(round(m$x,2), " (", round(ste$x,2), ")", sep=""))
  names(out) <- "stats"
  return(out)
}

comp.letters <- function(x) {
  tuk <- glht(x, linfct=mcp(treatment="Tukey"))
  let <- cld(tuk, level=0.05)$mcletters[1]
  return(let$Letters)
}

# Cost,Yield,and profit analysis
#----Variables formatting------
d$agz <- as.factor(d$agz)
d$treatment <- as.factor(d$treatment)
d$Block <- as.factor(d$Block)
d$Cell <- as.factor(d$Cell)
d$Cost_USD_ha <- as.numeric(d$Cost_USD_ha)
d$Yield_t_ha <- as.numeric(d$Yield_t_ha)
d$Profit_USD_ha <- as.numeric(d$Profit_USD_ha)

Yield.residues <- function(x) {
  
  cost = agg.stats(d$Cost_USD_ha, d$treatment)
  yield = agg.stats(d$Yield_t_ha, d$treatment)
  profit = agg.stats(d$Profit_USD_ha, d$treatment)
  
  #ANOVA - cost
  aov.cost <- aov(Cost_USD_ha ~ treatment + agz + Block, data=d)
  aov.cost.let <- comp.letters(aov.cost)
  
  #ANOVA - yield
  aov.yield <- aov(Yield_t_ha ~ treatment + agz + Block, data=d)
  #aov.yield <- aov(Yield_t_ha ~ treatment + agz, data=d)
  aov.yield.let <- comp.letters(aov.yield)
  
  #ANOVA - profit
  aov.profit <- aov(Profit_USD_ha ~ treatment + agz + Block, data=d)
  aov.profit.let <- comp.letters(aov.profit)
  
  # design results table
  output <- data.frame (
    N = aggregate(d$treatment, by=list(d$treatment), function(x) {length(x)})$x,
    treatment = unique(d$treatment),
    cost = paste(cost$stats, aov.cost.let, sep = " "),
    yield = paste(yield$stats, aov.yield.let, sep = " "),
    profit = paste(profit$stats, aov.profit.let, sep = " ")
    
  )
  
  return(output)}

res1 = Yield.residues(d)
res1
#----- To arrange the yield by treatment in real order and calculate p-value of difference between treamnts---
aggregate(d$Cost_USD_ha,by=list(d$treatment),FUN=mean,na.rm=TRUE)
aggregate(d$Yield_t_ha,by=list(d$treatment),FUN=mean,na.rm=TRUE)
aggregate(d$Profit_USD_ha,by=list(d$treatment),FUN=mean,na.rm=TRUE)
aov.cost <- aov(Cost_USD_ha ~ treatment + agz + Block, data=d)
aov.yield <- aov(Yield_t_ha ~ treatment + agz + Block, data=d)
aov.profit <- aov(Yield_t_ha ~ treatment + agz + Block, data=d)
summary(aov.cost)
summary(aov.yield)
summary(aov.profit)

##Let us see how different soil pH in two categories; >=5.5 and <5.5 affect beans yield in second season of travertine application
### Subsetting data by PH CATEGORY (>= 5.5 AND < 5.5) for 19B Database
View(d)
d$pH_range <- NA
d$pH_range <- ifelse(d$Baseline_pH >= "5.2" ,  "> 5.2", "< 5.2")
##co means combined; pH_range and Treatment
table(d$treatment)
d$co <- NA
d$co <- ifelse(d$treatment=="Compost"& d$pH_range=="> 5.2", "Compost>", "Compost<")
d$co <- ifelse(d$treatment=="Compost_Fertilizer"& d$pH_range=="> 5.2", "Compost_Fertilizer>", d$co)
d$co <- ifelse(d$treatment=="Compost_Fertilizer"& d$pH_range=="< 5.2", "Compost_Fertilizer<", d$co)

d$co <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_1.5"& d$pH_range=="> 5.2", 
               "Compost_Fertilizer_Travertine_1.5>", d$co)
d$co <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_1.5"& d$pH_range=="< 5.2", 
               "Compost_Fertilizer_Travertine_1.5<", d$co)


d$co <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_2.5"& d$pH_range=="> 5.2", 
               "Compost_Fertilizer_Travertine_2.5>", d$co)
d$co <- ifelse(d$treatment=="Compost_Fertilizer_Travertine_2.5"& d$pH_range=="< 5.2", 
               "Compost_Fertilizer_Travertine_2.5<", d$co)


d$co <- ifelse(d$treatment=="Compost_Travertine_2.5"& d$pH_range=="> 5.2", "Compost_Travertine_2.5>", d$co)
d$co <- ifelse(d$treatment=="Compost_Travertine_2.5"& d$pH_range=="< 5.2", "Compost_Travertine_2.5<", d$co)
table(d$co) #to check if created d$co has all expected variables

#-----Mode= COMPARING YIELD DIFFERENCE OF SAME TREATMENTS UNDER TWO PH CATEGORY (<5.5 and >5.5--------#              
d$co <- as.factor(d$co)
#####Yield
aggregate(d$Yield_t_ha,by=list(d$co),FUN=mean,na.rm=T)
anova_model1<- aov( Yield_t_ha~co,data=d)
summary(anova_model1)
TukeyHSD(anova_model1)
#####Profit 
### <5.2
d1 <- d[!(d$pH_range=="> 5.2"),]
View(d1)
aggregate(d1$Profit_USD_ha,by=list(d1$co),FUN=mean,na.rm=T)
anova_model1<- aov(Profit_USD_ha~co,data=d1)
summary(anova_model1)
TukeyHSD(anova_model1)
tuk <- glht(anova_model1, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
d1$treatment
d3 = d1[d1$treatment=="Compost_Fertilizer"|d1$treatment=="Compost_Fertilizer_Travertine_1.5",]
anova_model3<- aov(Profit_USD_ha~co,data=d3)
summary(anova_model3)
TukeyHSD(anova_model3)
tuk <- glht(anova_model3, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
d4 = d1[d1$treatment=="Compost_Fertilizer"|d1$treatment=="Compost_Fertilizer_Travertine_2.5",]
anova_model4<- aov(Profit_USD_ha~co,data=d4)
summary(anova_model4)
TukeyHSD(anova_model4)
tuk <- glht(anova_model4, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
### >5.2
d2 <- d[!(d$pH_range=="< 5.2"),]
View(d2)
aggregate(d2$Profit_USD_ha,by=list(d2$co),FUN=mean,na.rm=T)
anova_model2 <- aov(Profit_USD_ha~co,data=d2)
summary(anova_model2)
TukeyHSD(anova_model2)
tuk <- glht(anova_model2, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
d5 = d2[d2$treatment=="Compost_Fertilizer"|d2$treatment=="Compost_Fertilizer_Travertine_1.5",]
anova_model5<- aov(Profit_USD_ha~co,data=d5)
summary(anova_model3)
TukeyHSD(anova_model3)
tuk <- glht(anova_model5, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
d6 = d2[d2$treatment=="Compost_Fertilizer"|d2$treatment=="Compost_Fertilizer_Travertine_2.5",]
anova_model6<- aov(Profit_USD_ha~co,data=d6)
summary(anova_model6)
TukeyHSD(anova_model6)
tuk <- glht(anova_model6, linfct = mcp(co = "Tukey"))
tuk.cld <- cld(tuk)
tuk.cld
###--------------------------------------------------------------------------------------------------
#-----At which soil pH, below which travertine application is profitable-------- 
#--- import "accumulated profit" tab--------
###Two season threshold
x$pH <- as.numeric(x$pH)
x$Two_Accum_profit_usd_ha <- as.numeric(x$Two_Accum_profit_usd_ha)
x1 = x[x$Treatment=="Compost_Fertilizer"|x$Treatment=="Compost_Fertilizer_Travertine_2.5tn_ha",]
x2 = x[x$Treatment=="Compost_Fertilizer"|x$Treatment=="Compost_Fertlizer_Travertine_1.5tn_ha",]
table(x1$Treatment)
table(x2$Treatment)
# Old formula
p <- ggplot(d1, aes(x = pH, y = Two_Accum_profit_usd_ha, color=Treatment) ) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) #added SE by replacing se = FALSE with se = TRUE
p1 <- p + xlim(4, 6) + ylim(1500,2500)
print(p1 + labs( title= "2 seasons Profitable pH threshold", y="Accumulated Profit", x = "pH"))
## New Formula
ggplot(x1, aes(pH, Two_Accum_profit_usd_ha, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Acuumulated Profit") +
  ggtitle("2 season profitable pH threshold")
ggplot(x2, aes(pH, Two_Accum_profit_usd_ha, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Acuumulated Profit") +
  ggtitle("2 season profitable pH threshold")
###Three season threshold
x$pH <- as.numeric(x$pH)
x$Three_profit_usd_ha <- as.numeric(x$Three_profit_usd_ha)
x3 = x[x$Treatment=="Compost_Fertilizer"|x$Treatment=="Compost_Fertlizer_Travertine_2.5tn_ha",]
x4 = x[x$Treatment=="Compost_Fertilizer"|x$Treatment=="Compost_Fertlizer_Travertine_1.5tn_ha",]
table(x3$Treatment)
table(x4$Treatment)
# Old formula
p <- ggplot(d1, aes(x = pH, y = Three_profit_usd_ha, color=Treatment) ) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) #added SE by replacing se = FALSE with se = TRUE
p1 <- p + xlim(4, 6) + ylim(2000,4000)
print(p1 + labs( title= "3 seasons Profitable pH threshold", y="Accumulated Profit", x = "pH"))
## New Formula
ggplot(x3, aes(pH, Three_profit_usd_ha, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Acuumulated Profit") +
  ggtitle("3 season profitable pH threshold")
ggplot(x4, aes(pH, Three_profit_usd_ha, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Acuumulated Profit") +
  ggtitle("3 season profitable pH threshold")

#----all seasons-----Final----All AEZs---------------#
x$pH <- as.numeric(x$pH)
x$Four_profit_usd_ha <- as.numeric(x$Four_profit_usd_ha)
x5 = x[x$Treatment=="Compost_Fertilizer"|x$Treatment=="Compost_Fertlizer_Travertine_2.5tn_ha",]
x6 = x[x$Treatment=="Compost_Fertilizer"|x$Treatment=="Compost_Fertlizer_Travertine_1.5tn_ha",]
table(x5$Treatment)
table(x6$Treatment)
# Old formula
p <- ggplot(d1, aes(x = pH, y = Four_profit_usd_ha, color=Treatment) ) +
    geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) #added SE by replacing se = FALSE with se = TRUE
p1 <- p + xlim(4, 6) + ylim(5000,7000)
print(p1 + labs( title= "4 seasons Profitable pH threshold", y="Accumulated Profit", x = "pH"))

# New Formula
ggplot(x5, aes(pH, Four_profit_usd_ha, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Acuumulated Profit") +
  ggtitle("4 season profitable pH threshold")
ggplot(x6, aes(pH, Four_profit_usd_ha, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Acuumulated Profit") +
  ggtitle("4 season profitable pH threshold")

### Reltive yield of each treatment over environmental index (lm)##########
x$Treatment <- as.factor(x$Treatment)
x$Four_Yield_t_ha <- as.numeric(x$Four_Yield_t_ha)
x$RY <- as.numeric(x$RY)
x$pH <- as.numeric(x$pH)
xRY <- x[!(x$Treatment=="Compost"|x$Treatment=="Compost_Travertine_2.5tn_ha"),]
xRY$Treatment
p <- ggplot(xRY, aes(x = pH, y = RY, color=Treatment) ) +
    geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) #added SE by replacing se = FALSE with se = TRUE
p1 <- p + xlim(4, 6)
print(p1 + labs( title= "Relative yield increase pH threshold", y="Relative Yield", x = "pH"))
# New formula
ggplot(xRY, aes(pH, RY, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Soil pH") +
  ylab("Relative Yield") +
  ggtitle("Relative yield increase pH threshold")

### Histograms with all seasons
library(ggcharts)
library(tidytext)

d %>%
  filter(N_season %in% c(1, 2, 3, 4)) %>%
  bar_chart(x = Treatment, y = Yield_t_ha, facet = N_season, top_n = 5)

d %>%
  filter(N_season %in% c(1, 2, 3, 4)) %>%
  group_by(N_season) %>%
  top_n(5, Yield_t_ha) %>%
  ungroup() %>%
  mutate(Treatment = tidytext::reorder_within(Treatment, Yield_t_ha, N_season)) %>%
  ggplot(aes(Treatment, Yield_t_ha)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(N_season), scales = "free_y")
#Accumulated yield
ggcharts_set_theme("theme_hermit")
bar_chart(data = d, x = Yield_t_ha, y = Treatment)

###### Treatment Ranking
# Side-by-side bar chart, treatments comparison rank
d %>%
  ggplot(aes(`19A_treatment_comparison_rank`, fill = Treatment)) + 
  geom_bar(position = position_dodge()) +
  theme(legend.position = "right")
d %>%
  ggplot(aes(`19B_treatments_comparison_rank`, fill = treatment)) + 
  geom_bar(position = position_dodge()) +
  theme(legend.position = "right")
d %>%
  ggplot(aes(`20A_treatments_comparison_rank`, fill = Treatment)) + 
  geom_bar(position = position_dodge()) +
  theme(legend.position = "right")
d %>%
  ggplot(aes(`20B_treatments_comparison_rank`, fill = treatment)) + 
  geom_bar(position = position_dodge()) +
  theme(legend.position = "right")



#Anovas for cummulative yields--------------------

#2 seaons
lme2 = aov(Two_Accum_profit_usd_ha~ Treatment, data=d,na.action=na.exclude)
var_x = head(d$Treatment, levels=FALSE)[1]
print(anova(lme2))
out2 <- c(HSD.test(lme2,"Treatment", group=TRUE,
                   console=TRUE),   summary(lme2))
write.csv(out2$groups, file = paste(od,"Acc2.csv",sep=""))
#3 seaons
lme3 = aov(Three_profit_usd_ha~ Treatment, data=d,na.action=na.exclude)
var_x = head(d$Treatment, levels=FALSE)[1]
print(anova(lme3))
out3 <- c(HSD.test(lme3,"Treatment", group=TRUE,
                  console=TRUE),   summary(lme3))
write.csv(out3$groups, file = paste(od,"Acc3.csv",sep=""))

#4 seasons
lme4 = aov(Four_profit_usd_ha~ Treatment, data=d,na.action=na.exclude)
var_x = head(d$Treatment, levels=FALSE)[1]
print(anova(lme4))
out4 <- c(HSD.test(lme4,"Treatment", group=TRUE,
                   console=TRUE),   summary(lme4))
write.csv(out4$groups, file = paste(od,"Acc4.csv",sep=""))

--------------------------
  #pH thresholds 

df<-d
df<-filter(df,df$Treatment=="Compost_Fertilizer_Travertine_2.5tn_ha"|d$Treatment=="Compost_Fertilizer")
df$x<-df$pH
df$y<-df$Four_profit_usd_ha
g<-ggplot(df,aes(x,y,color=Treatment))
g<-g+geom_smooth(method="lm", size =1, se=FALSE)
g<-g+xlab("Soil pH")+ylab(bquote('Profit ('*USD~ha^-1*')'))+ scale_y_continuous(breaks=seq(0,6000,400))+scale_x_continuous(breaks=seq(0,8,0.3))
plot(g) 
ggsave(g, file=paste(od,"pHThresholdProf.png", sep="/"), width = 20, height = 10, units = "cm")



df<-d
df<-filter(df,df$Treatment=="Compost_Fertilizer_Travertine_2.5tn_ha"|d$Treatment=="Compost_Fertilizer")
df$x<-df$pH
df$y<-df$Four_profit_usd_ha
g<-ggplot(df,aes(x,y,color=Treatment))
g<-g+geom_smooth(method="lm", size =1, se=FALSE)
g<-g+xlab("Soil pH")+ylab(bquote('Profit ('*USD~ha^-1*')'))+ scale_y_continuous(breaks=seq(0,6000,400))+scale_x_continuous(breaks=seq(0,8,0.3))
plot(g) 
ggsave(g, file=paste(od,"pHThresholdProf3.png", sep="/"), width = 20, height = 10, units = "cm")

df<-d
df<-filter(df,df$Treatment=="Compost_Fertilizer_Travertine_2.5tn_ha"|d$Treatment=="Compost_Fertilizer")
df$x<-df$pH
df$y<-df$Two_Accum_profit_usd_ha
g<-ggplot(df,aes(x,y,color=Treatment))
g<-g+geom_smooth(method="lm", size =1, se=FALSE)
g<-g+xlab("Soil pH")+ylab(bquote('Profit ('*USD~ha^-1*')'))+ scale_y_continuous(breaks=seq(0,3000,100))+scale_x_continuous(breaks=seq(0,8,0.3))
plot(g) 
ggsave(g, file=paste(od,"pHThresholdProf2.png", sep="/"), width = 20, height = 10, units = "cm")

df<-d
df<-filter(df,df$Treatment=="Compost_Fertilizer_Travertine_2.5tn_ha"|d$Treatment=="Compost_Fertilizer")
df$x<-df$pH
df$y<-df$Profit_USD_ha_19A
g<-ggplot(df,aes(x,y,color=Treatment))
g<-g+geom_smooth(method="lm", size =1, se=FALSE)
g<-g+xlab("Soil pH")+ylab(bquote('Profit ('*USD~ha^-1*')'))+ scale_y_continuous(breaks=seq(0,2000,100))+scale_x_continuous(breaks=seq(0,8,0.3))
plot(g) 
ggsave(g, file=paste(od,"pHThresholdProf1.png", sep="/"), width = 20, height = 10, units = "cm")




