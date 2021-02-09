library(polynom)
library(easynls)
library(nlraa)
library(ggplot2)
library(agricolae)
library(MASS)
library(rpart)
library(plyr)
library(dplyr)
library(nlme)
library(lme4)
library(multcomp)
library(multcompView)
library(lsmeans)
library(polynom)
library(easynls)
library(nlraa)
library(insight)
library(coda)
library(languageR)
library(memisc)
library(pbkrtest)
library(lmerTest)

getwd()
wd <- "C:/"
dd <- paste(wd, "data", sep="/")
od <- paste(wd, "output/", sep="/")
md <- paste(wd, "market_prices", sep="/")
setwd(wd)

#Add relative yield column
bl_y_avg<-aggregate(Y~bl_id, data=data, mean)
colnames(bl_y_avg)<-c("bl_id","bl_y_avg")
data <- merge(data, bl_y_avg, by="bl_id")
data$RYR<-((data$Y -data$bl_y_avg)/ data$bl_y_avg)*100

#Plot Relative Yield as a function of plant density for all database
range <- unique(data$EI_range_id)
range_reg <- NULL
for (i in 1:length(levels(range))){
  data_rx <- data[data$EI_range_id  %in% range[i],]
  data_rx$x<-data_rx$plant_den
  data_rx$y<-data_rx$RYR
  formula<- y ~ poly(x, 2, raw = TRUE)
  avg_yield<-aggregate(Y~EI_range_id, data=data_rx, mean)
  g<-ggplot(data_rx,aes(x,y))
  g<-g+geom_point()+geom_smooth(method="lm",formula =formula, size =1, se=FALSE)+ggtitle(avg_yield[1,2]) 
  m <- lm(formula, data_rx)
  a<-unlist(summary(m))$coefficients3
  b<-unlist(summary(m))$coefficients2
  opt_d<--(b/(2*a))
  my.eq <- as.character(signif(as.polynomial(coef(m)), 3))
  label.text <- paste(gsub("x", "~italic(x)", my.eq, fixed = TRUE),
                      paste("italic(R)^2",  
                            format(summary(m)$r.squared, digits = 3), 
                            sep = "~`=`~"),
                      sep = "~~~~",
                              paste("italic(Opt_Dens)", format(opt_d, digits = 3), 
                                    sep = "~`=`~"))
  g<- g+annotate(geom = "text", x = 1, y = 40, label = label.text,  family = "serif", hjust = 0, parse = TRUE, size = 4)
  plot(g) 
  g<-g+xlab(bquote('Plant Density ('*plants~m^-2*')'))+ ylab("Relative Yield (%)")+ scale_y_continuous(breaks=seq(-30,30,5))+scale_x_continuous(breaks=seq(0,10,1))
plot(g) 
}
ggsave(g, file=paste0("NN.png"), width = 14, height = 10, units = "cm")

------------------------------------ 
#Plot the higuer density of each block, as a function of the plant environmental index
  
range2 <- unique(data$bl_id)
rank_reg <- NULL
  for (k in 1:length(levels(range2))){
    data_blx <- data[data$bl_id  %in% range2[k],]
    data_blx <-data_blx[order(data_blx$Y,decreasing=TRUE),]
    opt_d<-data_blx$plant_den[1]
    EI<- data_blx$bl_y_avg[1]
    reg_blx<- as.data.frame(cbind(range2[k],EI,opt_d))
    rank_reg <- rbind(rank_reg, reg_blx)
  }

colnames(rank_reg) <- c("BL","x","y")
str(rank_reg)
formula<- y ~ poly(x, 2, raw = TRUE)
g<-ggplot(rank_reg,aes(x,y))
g<-g+geom_point()+geom_smooth(method="lm",formula =formula, size =1, se=FALSE)
m <- lm(formula, rank_reg)
a<-unlist(summary(m))$coefficients3
b<-unlist(summary(m))$coefficients2
opt_d<--(b/(2*a))
my.eq <- as.character(signif(as.polynomial(coef(m)), 3))
label.text <- paste(gsub("x", "~italic(x)", my.eq, fixed = TRUE),
                    paste("italic(R)^2",  
                          format(summary(m)$r.squared, digits = 3), 
                          sep = "~`=`~"),
                    sep = "~~~~",
                    paste("italic(EI_at_Plateau)", format(opt_d, digits = 3), 
                          sep = "~`=`~"))
g<- g+annotate(geom = "text", x = 1, y = 10, label = label.text,  family = "serif", hjust = 0, parse = TRUE, size = 4)
g<-g+xlab(bquote('Environmental Index ('*t~ha^-1*')'))+ylab(bquote('Plant density ('*plants~m^-2*')'))+ scale_y_continuous(breaks=seq(0,10,1))+scale_x_continuous(breaks=seq(0,10,1))
plot(g) 
ggsave(g, file=paste0("NN.png"), width = 14, height = 10, units = "cm")
xlab(bquote('Environmental Index('*t~ha^-1*')'))

_________________________________________________________________________________________

# PROFIT

#Add relative prof column
bl_prof_avg<-aggregate(prof~bl_id, data=data, mean)
colnames(bl_prof_avg)<-c("bl_id","bl_prof_avg")
data <- merge(data, bl_prof_avg, by="bl_id")
bl_y_avg<-aggregate(Y~bl_id, data=data, mean)
colnames(bl_y_avg)<-c("bl_id","bl_y_avg")
data <- merge(data, bl_y_avg, by="bl_id")
data$Rprof<-((data$prof -data$bl_prof_avg)/ data$bl_prof_avg)*100

#Relative profit as a function of plant density
range <- unique(data$EI_range)
range_reg <- NULL
for (i in 1:length(levels(range))){
  data_rx <- data[data$EI_range  %in% range[i],]
  data_rx$x<-data_rx$plant_den
  data_rx$y<-data_rx$Rprof
  formula<- y ~ poly(x, 2, raw = TRUE)
  #lm(RYR~ poly(plant_den ,2), data=data_rx)
  g<-ggplot(data_rx,aes(x,y))
  g<-g+geom_point()+geom_smooth(method="lm",formula =formula, size =1, se=FALSE)+ggtitle(range[i]) 
  m <- lm(formula, data_rx)
  a<-unlist(summary(m))$coefficients3
  b<-unlist(summary(m))$coefficients2
  opt_d<--(b/(2*a))
  my.eq <- as.character(signif(as.polynomial(coef(m)), 3))
  label.text <- paste(gsub("x", "~italic(x)", my.eq, fixed = TRUE),
                      paste("italic(R)^2",  
                            format(summary(m)$r.squared, digits = 3), 
                            sep = "~`=`~"),
                      sep = "~~~~",
                      paste("italic(Opt_Dens)", format(opt_d, digits = 3), 
                            sep = "~`=`~"))
  g<- g+annotate(geom = "text", x = 1, y = 40, label = label.text,  family = "serif", hjust = 0, parse = TRUE, size = 4)
  plot(g) 
}
------------------------------------------------------------------
#All data analysis of plant density that maximizes profit on each block, as a function of environmental index of the block
  
range2 <- unique(data$bl_id)
rank_reg <- NULL
for (k in 1:length(levels(range2))){
  data_blx <- data[data$bl_id  %in% range2[k],]
  data_blx<- data_blx[order(data_blx$profit,decreasing=TRUE),]
  opt_d<-data_blx$plant_den[1]
  EI<- data_blx$bl_y_avg.x[1]
  reg_blx<- as.data.frame(cbind(range2[k],EI,opt_d))
  rank_reg <- rbind(rank_reg, reg_blx)
}
colnames(rank_reg) <- c("BL","EI","OD")
colnames(rank_reg) <- c("BL","x","y")
str(rank_reg)
formula<- y ~ poly(x, 2, raw = TRUE)

g<-ggplot(rank_reg,aes(x,y))
g<-g+geom_point()+geom_smooth(method="lm",formula =formula, size =1, se=FALSE)
m <- lm(formula, rank_reg)
a<-unlist(summary(m))$coefficients3
b<-unlist(summary(m))$coefficients2
opt_d<--(b/(2*a))
my.eq <- as.character(signif(as.polynomial(coef(m)), 3))
label.text <- paste(gsub("x", "~italic(x)", my.eq, fixed = TRUE),
                    paste("italic(R)^2",  
                          format(summary(m)$r.squared, digits = 3), 
                          sep = "~`=`~"),
                    sep = "~~~~",
                    paste("italic(EI_at_Plateau)", format(opt_d, digits = 3), 
                          sep = "~`=`~"))
g<- g+annotate(geom = "text", x = 1, y = 10, label = label.text,  family = "serif", hjust = 0, parse = TRUE, size = 4)
g<-g+xlab("Environmental Index (t/ha)")+ylab("Opt. PLant Density (pl/m-2)")+ scale_y_continuous(breaks=seq(0,10,1))+scale_x_continuous(breaks=seq(0,10,1))
plot(g) 
ggsave(g, file=paste0("NN.png"), width = 14, height = 10, units = "cm")

___

#ANOVAs

lme1 = aov(profit~ s_rate, data=data,na.action=na.exclude)
var_x = head(data$seed_id, levels=FALSE)[1]
print(anova(lme1))
out <- c(HSD.test(lme1,"s_rate", group=TRUE,
                  console=TRUE),   summary(lme1))

#Site x seeding rate interactions

m2<-lmer(yield ~ s_rate+site+s_rate:site+(1 |site:oaf_id), data=data)
summary(m2)
anova(m2)

_

