library(tidyverse)
library(RcppRoll)
library(tidymodels)
#calculate epidemics----
epidemic <- function(inputdata,cutoff){
  RSVn <- inputdata$RSVn
  RSVp <- RSVn / sum(RSVn)
  RSVcum_p <- cumsum(RSVn)/sum(RSVn) 
  epidemics <- ifelse(RSVcum_p - cutoff < 0, 1,
                      with(inputdata,
                           ifelse(RSVcum_p - RSVp - cutoff < 0, 
                                  (cutoff - (RSVcum_p - RSVp))/RSVp,
                                  0)))
}
#example for calculating epidemics
inputdata <- data.frame(location="Jiangsu",RSVmonth=seq(1,12,1),RSVn=c(161,145,96,59,13,0,10,49,63,54,87,144))
inputdata <- inputdata %>% arrange(desc(RSVn))
inputdata$epidemics_75<- epidemic(inputdata,0.75)
#k-means----
load("KmeansData.RData")
kclust6 <- kmeans(KmeansData, centers = 6,nstart = 10)
Assignment6 <- augment(kclust6,KmeansData) %>% dplyr::rename(name=.rownames)
kclust4 <- kmeans(KmeansData, centers = 4,nstart = 10)
Assignment4 <- augment(kclust4,KmeansData) %>% dplyr::rename(name=.rownames)
kclust2 <- kmeans(KmeansData, centers = 2,nstart = 10)
Assignment2 <- augment(kclust2,KmeansData) %>% dplyr::rename(name=.rownames)
#moving interval approach----
add_rollsum <- function(inputdata){
  inputdata$m13 <- inputdata$m1
  inputdata$m14 <- inputdata$m2
  inputdata$m15 <- inputdata$m3
  inputdata$m16 <- inputdata$m4
  inputdata$m17 <- inputdata$m5
  inputdata$sum_RSVn=rowSums(inputdata[,c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12")])
  inputdata <- pivot_longer(inputdata,
                       cols = starts_with("m"),
                       names_to = "month",
                       names_prefix = "m",
                       values_to = "count")
  inputdata$RollSum6 <- roll_suml(inputdata$count,n=6,by=1,fill = NA)
  inputdata$RollProp6 <- round(inputdata$RollSum6/inputdata$sum_RSVn,2)
  inputdata <- inputdata[1:12,]
}
#example
inputdata1 <- data.frame(Province="Anhui", m1=89, m2=24, m3=11, m4=4, m5=8, m6=12, m7=7, m8=10, m9=4, m10=5, m11=23, m12=85)
ProvinceRoll <- add_rollsum(inputdata1)
Province_MIA <-  ProvinceRoll %>% filter(RollSum6 == max(RollSum6))
Province_MIA <- Province_MIA %>% mutate(cluster_80=as.factor(if_else(RollProp6 >= 0.80,month,"0")),
                                cluster_70=as.factor(if_else(RollProp6 >= 0.70,month,"0")),
                                cluster_90=as.factor(if_else(RollProp6 >= 0.90,month,"0")))
#The value in cluster_xx indicates the optimal beginning of seasonal immunization,
# 0 indicates that seasonal immunization is not suitable




