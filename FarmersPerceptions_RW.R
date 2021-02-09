libs <- c("ggplot2", "plyr", "Rmisc", "multcomp", "reshape2", "stringr","foreign",
          "dplyr","readxl","car","tree","tidyr")
lapply(libs, require, character.only = T)

library(emmeans)
library(ISLR)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)


# ****************************

wd <- "C:/Users/Diego/Documents/R analysis/Travertine/TreeRegression"
dd <- paste(wd, "Data", sep="/")
od <- paste(wd, "Output", sep="/")

# ***********************************************************
# 1. Importing Data in R
# ***********************************************************

d <- read.csv("nn.csv", na.strings = c("NA", "#N/A","N/D"," ", "  ","#VALUE!","#REF!","#DIV/0!"))

#Define as acidic soils those with pH< 5.2
d$acidic = ifelse(d$ph<5.2,1,0)

#variables transformations for all AEZs

d$fertlity_compared_owned_fields<-ifelse(d$fertlity_compared_owned_fields=="1","High",ifelse(d$fertlity_compared_owned_fields=="2","Med",ifelse(d$fertlity_compared_owned_fields=="3","Low","NA")))
d$fertility_compared_local_fields<-ifelse(d$fertility_compared_local_fields=="1","High",ifelse(d$fertility_compared_local_fields=="2","Med",ifelse(dBU$fertility_compared_local_fields=="3","Low","NA")))

#Narrowing N of classes within variable; e.g.,  Less_Fertile (less_than + Very_less), Medium,  More_fertile (More_than + Very_fert)
d$Fert_class = ifelse(d$Fert_class=="Less_than_medium"|d$Fert_class=="Very_less_fertile",
                   "Low",d$Fert_class)
d$Fert_class = ifelse(d$Fert_class=="More_than_medium"|d$Fert_class=="Very_fertile",
                     "High",d$Fert_class)

d$Land_pos = ifelse(is.na(d$Land_pos),d$Slope_pos,d$Land_pos)
d$Soil_color[d$Soil_color=="ivu_busa_nibara_ryivu"] = "grey"


d$Field_state = ifelse(d$Field_state=="None", "Notdefined", d$Field_state)

d$Soil_color2 = d$Soil_color
d$Soil_color2 =ifelse(d$Soil_color2=="black_less"|d$Soil_color2=="black_more","black",d$Soil_color2)
d$Soil_color2 =ifelse(d$Soil_color2=="red_less"|d$Soil_color2=="red_more","red",d$Soil_color2)

d$maize_vigor2 = d$maize_vigor
d$maize_vigor2 = ifelse(d$maize_vigor2=="similar","normal",d$maize_vigor2)
d$maize_vigor2 = ifelse(d$maize_vigor2=="big"|d$maize_vigor2=="very_big","bigger",
                        d$maize_vigor2)
d$maize_vigor2 = ifelse(d$maize_vigor2=="small"|d$maize_vigor2=="very_small","smaller",
                        d$maize_vigor2)

d$potato_vigor2 = d$potato_vigor
d$potato_vigor2 = ifelse(d$potato_vigor2=="similar","normal",d$potato_vigor2)
d$potato_vigor2 = ifelse(d$potato_vigor2=="big"|d$potato_vigor2=="very_big","bigger",
                        d$potato_vigor2)
d$potato_vigor2 = ifelse(d$potato_vigor2=="small"|d$potato_vigor2=="very_small","smaller",
                       d$potato_vigor2)

d$Slope_class2[d$Slope<11]="Low"
d$Slope_class2[d$Slope>10&d$Slope<21]="Medium"
d$Slope_class2[d$Slope>20]="High"


d$Rockness2 = d$Rockness
d$Rockness2 = ifelse(d$Rockness2=="much_small_sized_rocks"|d$Rockness2=="much_high_sized_rocks","much_rocks",d$Rockness2)
d$Rockness2 = ifelse(d$Rockness2=="less_rocky","medium_rocks",d$Rockness2)


#Multivariate analysis was performed using:

modelXAEZ = glm(acidic ~ ., data=XAEZ, family="binomial")
multi.XAEZ<-summary(modelER)coef.multi<-multi.BU[["coefficients"]]
write.csv(coef.multi,file=paste(,"Coeff.csv",sep="/"))


#Classification tree where performed using rpart package:
tree.BU2 <- rpart(acidic ~ ., dBU, method = "class")
rpart.plot(tree.BU2, main="NN", 
           box.palette = "GnRd", tweak = 0.8,cex = 0.9,  compress =TRUE, type =4, under=FALSE, extra=107, uniform = T)


#Trees were trimmed to remove branches which would make no agronomic sense or where none of the legs have > 15% of the total samples or where there is < 15% chance on acidic between the branches.
tree.new<-prp(tree.XAEZ, snip = TRUE)$obj
rpart.plot(tree.new, main="NN", 
           box.palette = "GnRd", tweak = 0.8, type = 4, under=FALSE, extra=107, uniform = T)

# Variables included on each AEZ
# BUBERUKA HIGHLANDS
# ******************
dBU = d[d$AEZ=="Buberuka highlands",c("Land_pos", "Fert_class", 'Slope_class2', 'fertlity_compared_owned_fields',"Soil_color2",
                                      "potato_vigor2","maize_vigor2","Soil_texture_farmer","field_erosion","stickness","acidic", "weed_Nyiranzara","weed_Urwiri","weed_Ikimari","weed_Uruteja",
                                      "weed_Inyabarasanya","weed_Igifuraninda","weed_Igihehe","weed_Amabyiyindege","weed_Indagarago")]
#Transformations for BU
#In "Land_Pos" there is only 3 radical teraces,  2 shoulders, 3 summit, 1 valley. Convert radical terraces and valley to "flat" and "shoulder" to "summit"
dBU$Land_pos<- ifelse(dBU$Land_pos=="Radical_terraces"|dBU$Land_pos=="Valley"|dBU$Land_pos=="Flat","Rad.Terr_Valley",as.character(dBU$Land_pos))
dBU$Land_pos<-ifelse(dBU$Land_pos=="shoulder","summit",as.character(dBU$Land_pos))
#In "Fert_class" there is only six "High"; so combiner "med" and "high" into "mid.high"
dBU$Fert_class <- ifelse(dBU$Fert_class=="Medium"|dBU$Fert_class=="High","med_high",as.character(dBU$Fert_class))
#Join Low + Medium slope
dBU$Slope_class2<-ifelse(dBU$Slope_class2=="Low"|dBU$Slope_class2=="Medium","low_medium",as.character(dBU$Slope_class2))
#Join 1 y 2 (high and med) in one and change name; low(3) y mid-high (2 and 1)
dBU$fertlity_compared_owned_fields<-ifelse(dBU$fertlity_compared_owned_fields=="Med"|dBU$fertlity_compared_owned_fields=="High","Med_high",as.character(dBU$fertlity_compared_owned_fields))
#Eliminate grey from colors; there is only one
dBU$Soil_color2<-ifelse(dBU$Soil_color2=="grey","black",as.character(dBU$Soil_color2))
#Join  "clay" (only 3) and clay_loamy"; and eliminate snady (only  1)
dBU$Soil_texture_farmer<-ifelse(dBU$Soil_texture_farmer=="clay","clay_loamy",ifelse(dBU$Soil_texture_farmer=="sandy","loamy",as.character(dBU$Soil_texture_farmer)))
#Join mid and high erosion; only 9
dBU$field_erosion<-ifelse(dBU$field_erosion=="medium"|dBU$field_erosion=="Highly","Med_high",ifelse(dBU$field_erosion=="law","Low",as.character(dBU$Soil_texture_farmer)))
#Join highly_stick + moderate_sticky
dBU$stickness<-ifelse(dBU$stickness=="highly_stick"|dBU$stickness=="moderate_sticky","Med_high",ifelse(dBU$stickness=="less_stick","Low",as.character(dBU$stickness)))



# CONGO NILE
# **********
dCN = d[d$AEZ=="Congo Nile",c("Land_pos", "Fert_class", 'Slope_class2',"fertility_compared_local_fields","Field_state","fertlity_compared_owned_fields","Soil_color2",
                              "potato_vigor2","maize_vigor2","Soil_texture_farmer","field_erosion","stickness","acidic","Soil_depth","weed_Nyiramunukanabi","weed_Urukuta","weed_Urukoko","weed_Ikimari","weed_Uruteja",
                              "weed_Inyabarasanya","weed_Igifuraninda","weed_Igihehe")]

#Transformations for CN
dCN$Land_pos<- ifelse(dCN$Land_pos=="Radical_terraces"|dCN$Land_pos=="Valley"|dCN$Land_pos=="Flat","Rad.Terr_Valley",as.character(dCN$Land_pos))
dCN$Soil_color2<-ifelse(dCN$Soil_color2=="grey","black",as.character(dCN$Soil_color2))
dCN$Soil_texture_farmer<-ifelse(dCN$Soil_texture_farmer=="clay","clay_loamy",ifelse(dCN$Soil_texture_farmer=="sandy","loamy",as.character(dCN$Soil_texture_farmer)))
dCN$field_erosion<-ifelse(dCN$field_erosion=="medium"|dCN$field_erosion=="Highly","Med_high",ifelse(dCN$field_erosion=="law","Low",as.character(dCN$Soil_texture_farmer)))


# EASTERNE RIDGES 
# ***************
dER = d[d$AEZ=="Eastern Ridges",c("Slope_pos","fertility_compared_local_fields","Field_state","Rockness","Soil_depth","Land_pos", "Fert_class", 'Slope_class2', 'fertlity_compared_owned_fields',"Soil_color2",
                                  "potato_vigor2","maize_vigor2","Soil_texture_farmer","field_erosion","stickness","acidic","weed_Urwiri","weed_Ikimari","weed_Uruteja",
                                  "weed_Inyabarasanya","weed_Kanyoro")]
dER$Land_pos<- ifelse(dER$Land_pos=="Radical_terraces"|dER$Land_pos=="Valley"|dER$Land_pos=="Flat","Rad.Terr_Valley",as.character(dER$Land_pos))
dER$Slope_class2<-ifelse(dER$Slope_class2=="High"|dER$Slope_class2=="Medium","medium_high",as.character(dER$Slope_class2))
dER$Soil_texture_farmer<-ifelse(dER$Soil_texture_farmer=="clay","clay_loamy",as.character(dER$Soil_texture_farmer))
dER$Rockness<-ifelse(dER$Rockness=="much_small_sized_rocks"|dER$Rockness=="much_high_sized_rocks","much_rock",as.character(dER$Rockness))
dER$field_erosion<-ifelse(dER$field_erosion=="medium"|dER$field_erosion=="Highly","Med_high",ifelse(dER$field_erosion=="law","Low",as.character(dER$field_erosion)))

# LAKE KIVU
# ***************

dLK = d[d$AEZ=="Lake Kivu",c("Slope_pos","fertility_compared_local_fields","Field_state","Rockness","Soil_depth","Land_pos", "Fert_class", 'Slope_class2', 'fertlity_compared_owned_fields',"Soil_color2",
                             "potato_vigor2","maize_vigor2","Soil_texture_farmer","field_erosion","stickness","acidic","weed_Urwiri","weed_Ikimari","weed_Uruteja",
                             "weed_Inyabarasanya")]
dLK$Land_pos<- ifelse(dLK$Land_pos=="Radical_terraces"|dLK$Land_pos=="Valley"|dLK$Land_pos=="Flat","Rad.Terr_Valley",as.character(dLK$Land_pos))
dLK$Soil_texture_farmer<-ifelse(dLK$Soil_texture_farmer=="clay","clay_loamy",as.character(dLK$Soil_texture_farmer))
dLK$Rockness<-ifelse(dLK$Rockness=="much_small_sized_rocks"|dLK$Rockness=="much_high_sized_rocks","much_rock",as.character(dLK$Rockness))
dLK$Soil_depth<-ifelse(dLK$Soil_depth=="NSP","Deep",as.character(dLK$Soil_depth))
dLK$field_erosion<-ifelse(dLK$field_erosion=="medium"|dLK$field_erosion=="Highly"|dLK$field_erosion=="N/A","Med_high",as.character(dLK$field_erosion))


# VOLCANIC CONES
# ***************
d5 = d[d$AEZ=="Volcanic cones",c("Slope_pos","Slope_class","Land_pos","Fert_class","Soil_color2",
                                 "Soil_acidity","Soil_depth",
                                 "Rockness","Field_state","maize_vigor2","potato_vigor2","field_erosion",
                                 "stickness","hard_tillage","Soil_color2","Soil_texture_farmer",
                                 "weed_Kurisuka","weed_Urwiri","weed_Nyiragashihe","weed_Isovu",
                                 "weed_Nyiramunukanabi","weed_Urukuta","weed_Umutsina",
                                 "weed_Umusaza","weed_Urucyembagufa","weed_Kanyoro",
                                 "weed_Ikimari","weed_Uruteja","weed_Inyabarasanya","weed_Igifuraninda",
                                 "weed_Igihehe","weed_Nyiragaheha","weed_Urukoko","weed_Indagarago",
                                 "weed_Uruteja","weed_Isununu","weed_Nyiranzara","acidic")]






