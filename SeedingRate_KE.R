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

data<- "KE_Dataset"
#In 2018 we onlu had Duma43, so most of the analysis is done with this variety alone. Then we do comparisons among varieties using 2019 trial.
data<-filter(data,data$seed_id == "scduma43")
#data<-filter(data,data$seed_id == "p3812w")

#Relative yield calculation
block_avg<-aggregate(Y~oaf_id, data=data, mean)
colnames(block_avg)<-c("oaf_id","EI")
data <- merge(data, block_avg, by="oaf_id")
data$RY<-((data$Y -data$EI)/ data$EI)*100

--------------------------------------------------------------
#Relative yield/profit as a function of seeding for all data
data_x <- data
data_x$x<-data_x$s_rate
data_x$y<-data_x$RY
n_plots<-length(data_x$oaf_id)
formula<- y ~ poly(x, 2, raw = TRUE)
g<-ggplot(data_x,aes(x,y,color=EI_qualy))
g<-g+geom_smooth(method="lm",formula =formula, size =1, se=FALSE)+ggtitle(paste0(response_variable," as a function of seeding rate")) 
m <- lm(formula, data_x)
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
                          sep = "~`=`~"),
                    paste("N",n_plots,sep = "~`=`~"))
g<-g+xlab(bquote('Opt. Seeding Rate ('*seeds~m^-2*')')) +ylab(paste0("Relative ",response_variable, "(%)"))+ scale_y_continuous(breaks=seq(-100,100,1))+scale_x_continuous(breaks=seq(0,10,0.3))
plot(g) 
ggsave(g, file=paste(od,"NN.png", sep = "/"), width = 14, height = 10, units = "cm")
---------------------------------------------------------------------  
  
#Cathegorize blocks according to EI, in 4 ranges

exp_EI_list<-data$EI_Y
cut(exp_EI_list,breaks = c(0,2.6,4.7,10))
data$EI_qualy<-cut(exp_EI_list,breaks = c(0,2.6,4.7,10))

#Yield/profit as a function of seeding rate for the 4 EI ranges

qualy_range <- unique(data$EI_qualy)
qualy_reg <- NULL
for (i in 1:length(levels(qualy_range))){
  data_n <- data[data$EI_qualy  %in% qualy_range[i],]
  data_n$x<-data_n$s_rate
  data_n$y<-data_n$RY
  n_plots<-length(data_n$oaf_id)
  formula<- y ~ poly(x, 2, raw = TRUE)
  avg_yield<-aggregate(Y ~EI_qualy, data=data_n, mean)
  g<-ggplot(data_n,aes(x,y))
  g<-g+geom_point()+geom_smooth(method="lm",formula =formula, size =1, se=FALSE)+ggtitle(paste("EI_range= ",qualy_range[i]," ; Avg Profit= ",format(avg_yield[1,2],digits=3)))
  m <- lm(formula, data_n)
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
                            sep = "~`=`~"),
                      paste("N",n_plots,sep = "~`=`~"))
  g<- g+annotate(geom = "text", x = 4, y = 80, label = label.text,  family = "serif", hjust = 0, parse = TRUE, size = 4)
  
  g<-g+xlab(bquote('Opt. Seeding Rate ('*seeds~m^-2*')'))+ylab(paste0("Relative ",response_variable, "(%)"))+ scale_y_continuous(breaks=seq(-80,80,10))+scale_x_continuous(breaks=seq(4,10,1))
  plot(g)
  plot_id<-paste0(qualy_range[i],"NN.png")
  ggsave(g, file=paste(od,plot_id, sep = "/"), width = 14, height = 10, units = "cm")
    }
------------------------------------------
#All data analysis of density at higuers yield/profit by plot as a function of EI
#Eliminate unbalanced blocks as would bring bias

range <- unique(data$oaf_id)
range<- as.factor(range)
rank_reg <- NULL
for (k in 1:length(levels(range))){
  data_blx <- data[data$oaf_id  %in% range[k],]
  data_blx <-data_blx[order(data_blx$Y,decreasing=TRUE),]
  opt_d<-data_blx$s_rate[1]
  EI<- data_blx$EI_Y[1]
  treatN<-length(data_blx$RY)
  site<-as.character(unique(data_blx$site))
  BL<-unique(data_blx$oaf_id)
  reg_blx<- as.data.frame(cbind(site,BL,EI,opt_d,treatN))
  rank_reg <- rbind(rank_reg, reg_blx)
}

colnames(rank_reg) <- c("site","BL","x","y","N")
rank_reg$x<-as.numeric(as.character(rank_reg$x))
rank_reg$y<-as.numeric(as.character(rank_reg$y))
rank_reg$N<-as.numeric(as.character(rank_reg$N))
rank_reg<-filter(rank_reg,rank_reg$N>=4)
str(rank_reg)
formula<-lm(y~ x, data=rank_reg)
anova<-anova(formula)
g<-ggplot(rank_reg,aes(x,y))
g<-g+geom_point()+geom_smooth(method="lm", size =1, se=FALSE)
m <- lm(formula, rank_reg)
my.eq <- as.character(m[["coefficients"]][["x"]])
label.text <- paste(paste("italic(R)^2", format(summary(m)$r.squared, digits = 3),
                          sep = "~`=`~"),
                          paste("pvalue",format(anova[1,5], digits=3),
                          sep = "~`=`~"),
                          sep = "~~~~")
g<- g+annotate("text", x = 1, y = 8, label = label.text,  family = "serif", hjust = 0, parse = TRUE, size = 4)
g<-g+xlab("Environmental Index (t ha-1)")+ylab("Opt. PLant Density (pl m-2)")+ scale_y_continuous(breaks=seq(0,10,1))+scale_x_continuous(breaks=seq(0,10,1))
plot(g) 
ggsave(g, file=paste(od,"2018_.png", sep="/"), width = 14, height = 10, units = "cm")
--------------------------------------------------
#Calculate the proportion of blocks where 6.66 outperforrm 5.33
  
dataf<-filter(data,data$EI_Y >=4.7) & data$EI_Y <=4.7)
dataf<-filter(dataf,dataf$s_rate >5 & dataf$s_rate<7)
range2 <- unique(dataf$oaf_id)
range2<- as.factor(range2)
rank_reg2 <- NULL
for (k in 1:length(levels(range2))){
  data_blx <- dataf[dataf$oaf_id  %in% range2[k],]
  data_blx <-data_blx[order(data_blx$Y,decreasing=TRUE),]
  opt_d<-data_blx$s_rate[1]
  EI<- data_blx$EI_Y[1]
  treatN<-length(data_blx$RY)
  site<-as.character(unique(data_blx$site))
  BL<-unique(data_blx$oaf_id)
  reg_blx<- as.data.frame(cbind(site,BL,EI,opt_d,treatN))
  rank_reg2 <- rbind(rank_reg2, reg_blx)
}
sum(rank_reg2$opt_d == '6.66')
sum(rank_reg2$opt_d == '5.33')

----------------------------------------------------
#Yield as finction of EI for each seeding rate
  
data$x<-data$EI_Y
data$y<-data$profit
data$s_rate<-as.factor(data$s_rate)
formula<-lm(y~ x, data=data)
anova<-anova(formula)
g<-ggplot(data,aes(x,y,color=s_rate))
g<-g+geom_point()+geom_smooth(method="lm", size =1, se=FALSE)
m_4<- lm(formula, filter(data,data$s_rate==4.44))
m_5<- lm(formula, filter(data,data$s_rate==5.33))
m_6<- lm(formula, filter(data,data$s_rate==6.66))
m_7<- lm(formula, filter(data,data$s_rate==7.14))
pv_4<-anova(m_4)[1,5]
pv_5<-anova(m_5)[1,5]
pv_6<-anova(m_6)[1,5]
pv_7<-anova(m_7)[1,5]
x_4<- m_4[["coefficients"]][["x"]]
x_5<- m_5[["coefficients"]][["x"]]
x_6<- m_6[["coefficients"]][["x"]]
x_7<- m_7[["coefficients"]][["x"]]
label.text_4 <- paste(paste("SR_4.4","italic(R)^2", format(summary(m_4)$r.squared, digits = 2),
                          sep = "~`=`~"),
                    paste("b",format(x_4, digits = 2),sep = "~`=`~"),
                    paste("p-value",format(pv_4,digits=2),sep = "~`=`~"),
                    sep = "~~~~")
label.text_5 <- paste(paste("SR_5.3","italic(R)^2", format(summary(m_5)$r.squared, digits = 2),
                            sep = "~`=`~"),
                      paste("b",format(x_5, digits = 2),sep = "~`=`~"),
                      paste("p-value",format(pv_5,digits=2),sep = "~`=`~"),
                      sep = "~~~~")
label.text_6 <- paste(paste("SR_6.6","italic(R)^2", format(summary(m_6)$r.squared, digits = 2),
                            sep = "~`=`~"),
                      paste("b",format(x_6, digits = 2),sep = "~`=`~"),
                      paste("p-value",format(pv_6,digits=2),sep = "~`=`~"),
                      sep = "~~~~")
label.text_7 <- paste(paste("SR_7.1","italic(R)^2", format(summary(m_7)$r.squared, digits = 2),
                            sep = "~`=`~"),
                      paste("b",format(x_7, digits = 2),sep = "~`=`~"),
                      paste("p-value",format(pv_7,digits=2),sep = "~`=`~"),
                      sep = "~~~~")
g<- g+annotate("text", x = 0.5, y = 2700, label = label.text_4,  family = "calibri", hjust = 0, parse = TRUE, size = 3)
g<- g+annotate("text", x = 0.5, y = 2900, label = label.text_5,  family = "calibri", hjust = 0, parse = TRUE, size = 3)
g<- g+annotate("text", x = 0.5, y = 3100, label = label.text_6,  family = "calibri", hjust = 0, parse = TRUE, size = 3)
g<- g+annotate("text", x = 0.5, y = 3300, label = label.text_7,  family = "calibri", hjust = 0, parse = TRUE, size = 3)
g<-g+xlab(bquote('Environmental Index ('*t~ha^-1*')'))+ylab(bquote('Profit ('*USD~ha^-1*')'))+ scale_y_continuous(breaks=seq(0,3500,200))+scale_x_continuous(breaks=seq(0,10,1))
plot(g) 
ggsave(g, file=paste(od,"NN.png", sep="/"), width = 14, height = 10, units = "cm")

-----------------------
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


