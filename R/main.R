
# packages and settings

library(tidyverse)
library(readr)

# import and clean data --------------------------------------------------

d <- read_csv("Data/benalmadena_data.csv")

d$Tipos_pais <- as.factor(d$Tipos_pais)
d$Tam_CVisus <- as.factor(d$Tam_CVisus)                           # correct ?
d$Tpaisaje_sin_Proximidad <- as.factor(d$Tpaisaje_sin_Proximidad) # correct ?

# remove outlier

d <- d[d$M2_HABITA!=0,]

# vizualize data ----------------------------------------------------------

# response variable
hist(log(d$Prec_pond),breaks=1000)
d$log_Prec_pond <- log(d$Prec_pond)

# CVisual_m2 ( ) - explanatory variable of interest
hist(d$CVisual_m2,breaks = 1000)
plot(d$CVisual_m2,log(d$Prec_pond))


# M2_HABITA variable (***) 
hist(log(d$M2_HABITA),breaks=1000) 
plot(log(d$M2_HABITA),d$log_Prec_pond)
d$log_M2_HABITA <- log(d$M2_HABITA)

# M2_PARCEL variable (***) 

hist(d$M2_PARCEL,breaks=1000) 
plot(d$M2_PARCEL,d$log_Prec_pond)

# Precio_Cat (***) - factor?

plot(d$Precio_Cat,d$log_Prec_pond) 

# Dist_Playa variable ( )

hist(d$Dist_Playa,breaks=1000)
plot(d$Dist_Playa,d$log_Prec_pond)

# V_Bosque ( . )

hist(d$V_Bosque,breaks = 1000)
plot(d$V_Bosque,d$log_Prec_pond)
   
# V_Golf ( )

hist(d$V_Golf,breaks = 1000)
plot(d$V_Golf,d$log_Prec_pond)

# V_Equipami ( )

hist(d$V_Equipami,breaks = 1000)
plot(d$V_Equipami,d$log_Prec_pond)

# V_MAR ( )

hist(d$V_Mar,breaks = 1000)
plot(d$V_Mar,d$log_Prec_pond)

# V_Marrotal ( )

hist(d$V_Matorral,breaks = 1000)
plot(d$V_Matorral,d$log_Prec_pond)

# V_RedVia ( )

hist(d$V_RedVia,breaks = 1000)
plot(d$V_RedVia,d$log_Prec_pond)

# V_UTrad ( )

hist(d$V_UTrad,breaks = 1000)
plot(d$V_UTrad,d$log_Prec_pond)

# V_Cultivo ( )

hist(d$V_Cultivo,breaks = 1000)
plot(d$V_Cultivo,d$log_Prec_pond)

# V_UPlurif ( )

hist(d$V_UPlurif,breaks = 1000)
plot(d$V_UPlurif,d$log_Prec_pond)

# V_UUnif ( )

hist(d$V_UUnif,breaks = 1000)
plot(d$V_UUnif,d$log_Prec_pond)

# V_Mina ( )

hist(d$V_Mina,breaks = 1000)
plot(d$V_Mina,d$log_Prec_pond)

# Tipos_pais ( )

plot(d$Tipos_pais,d$log_Prec_pond)

# Tam_CVisus ( )

plot(d$Tam_CVisus,d$log_Prec_pond)

# V_TPaiDef ( ) - factor?

plot(d$V_TPaiDef,d$log_Prec_pond)

# Tpaisaje_sin_Proximidad ( )

plot(d$Tpaisaje_sin_Proximidad,d$log_Prec_pond)

# Med_Vist_valor_final ( ) - factor?

plot(d$Med_Vist_valor_final,d$log_Prec_pond)  

# SpParc_Cat ( ) - factor?

plot(d$SpParc_Cat,d$log_Prec_pond)  

# AreaMedZon ( ) - factor?

plot(d$AreaMedZon,d$log_Prec_pond) 

# Eur_m2_Cat ( ) - factor?

plot(d$Eur_m2_Cat,d$log_Prec_pond) 


# check for multicolinearity in signinficant variables and fit a linear model  ------------------

temp <- d %>% 
  select(log_Prec_pond,log_M2_HABITA,CVisual_m2,Precio_Cat,M2_PARCEL) 

temp %>% pairs()

lm_manual <- lm(log_Prec_pond~.,data=temp)
summary(lm_manual)

# fit a linear model with the stepAIC algorithm, including all the variables 

temp <- d %>% select(-c(REFCAT,COORD_X,COORD_Y,V_Matorral,Prec_pond,M2_HABITA))
step(lm(log_Prec_pond~.,data=temp))
lm_AIC <- lm(log_Prec_pond ~ M2_PARCEL + V_Golf + V_RedVia + Eur_m2_Cat + Precio_Cat + log_M2_HABITA,
             data=temp)
summary(lm_AIC)

# Analysis of variance 

anova(lm_AIC,lm_manual)


# removing unnecessary variables

d <- d %>% select(-c(REFCAT,COORD_X,COORD_Y,V_Matorral)) # - also remove Precio_Cat?

###############
# Demonstration
###############

#### create data and fit model

set.seed(1)
n <- 300
visual_m2 <- rnorm(300,1000,200)
precios <- rnorm(300,1000,200)
data <- data.frame(visual_m2,precios)

# figure
plot(visual_m2,precios)

# correlation test and linear model
cor(visual_m2, precios, method = c("pearson"))^2
lm(precios~visual_m2,data=data) %>% summary()

#### transform data and fit model

# order data
data_order <- arrange(data,by_group=precios)

# calculate cumsum and mean_cumsum
cumsum_visual_m2 <- cumsum(data_order$visual_m2) 
mean_cumsum_visual_m2 <- sapply(1:n,function(x) mean(cumsum_visual_m2[1:x]))

cumsum_precios <- cumsum(data_order$precios) 
mean_cumsum_precios <- sapply(1:n,function(x) mean(cumsum_precios[1:x]))

# figures
plot(cumsum_visual_m2,cumsum_precios)
plot(mean_cumsum_visual_m2,mean_cumsum_precios)

# correlation test and linear model
cor(cumsum_visual_m2, cumsum_precios, method = c("pearson"))^2
cor(mean_cumsum_visual_m2, mean_cumsum_precios, method = c("pearson"))^2
lm(mean_cumsum_precios~mean_cumsum_visual_m2,
   data=data.frame(mean_cumsum_visual_m2,mean_cumsum_precios)) %>% summary()

