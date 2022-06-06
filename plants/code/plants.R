#Import data
d <- read.csv("D:/搜狗高速下载/project/plants/combined.csv")

#select variables
d <- select(d,OriginalID,ConSpecies,ConTemp,
            StandardisedTraitName,StandardisedTraitValue,StandardisedTraitUnit,
            Habitat,Location, ConKingdom, Citation)

#dot plots #####################################################################
library(ggplot2)

pdf(file = paste("all", "_","dots", ".pdf"),width=7,height=3.5)
for (i in unique(d$OriginalID)){
  temp <- subset(d,d$OriginalID==i)
  print(ggplot(temp, aes(y=StandardisedTraitValue, x=ConTemp))+geom_point()+
          labs(x="Temperature(oC)", y= temp$StandardisedTraitUnit, title= paste(i, "_",temp$ConSpecies))
        +theme_bw())
}
dev.off()

################################################################################
#Fitting########################################################################
################################################################################
remotes::install_github("padpadpadpad/rTPC")
install.packages("nls.multstart")
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(ggrepel)
library(MuMIn)

get_model_names()

################################################################################
##sharpschoolhigh_1981
results_high <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                           e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                           thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric())

pdf(file = "sharpeschoolhigh.pdf",width=7,height=3.5)

for (i in unique(d$OriginalID)){
  # choose model
  mod = 'sharpschoolhigh_1981'
  temp <- subset(d,d$OriginalID==i)
  # get start values
  start_vals <- get_start_vals(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
  # get limits
  low_lims <- get_lower_lims(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
  upper_lims <- get_upper_lims(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
  # fit model
  fit <- nls_multstart(StandardisedTraitValue~sharpeschoolhigh_1981(temp = ConTemp, r_tref,e,eh,th, tref = 0),
                       data = temp,
                       iter = 500,
                       start_lower = start_vals - 10,
                       start_upper = start_vals + 10,
                       lower = low_lims,
                       upper = upper_lims,
                       supp_errors = 'Y')
  # calculate additional traits
  calc_params(fit) %>%
    # round for easy viewing
    mutate_all(round, 2)
  para <- calc_params(fit)
  para$orginalID <- i
  if(class(fit)=="NULL"){
    para$AIC <- "NULL"
  }else{para$AIC <- AIC(fit)}
  results_high <- rbind(results_high,para)
  
  #plotting
  if(length(fit)==5){
    preds <- data.frame(T = seq(min(temp$ConTemp), max(temp$ConTemp), length.out=100))
    coef <- coef(fit)
    k <- 8.617e-05
    r_tref <- coef(fit)[1]
    e <- coef(fit)[2]
    eh <- coef(fit)[3]
    th <- coef(fit)[4]
    tref = 0
    preds$rate <- r_tref*(exp(e/k*(1/(tref+273.15) - 1/(preds$T + 273.15))) /
                            (1 + exp(eh/k*(1/(th + 273.15) - 1/(preds$T + 273.15)))))
    print(ggplot(temp, aes(y=StandardisedTraitValue, x=ConTemp)) +
            geom_point() +
            geom_line(aes(T, rate), preds, col = 'blue') +
            theme_bw(base_size = 12) +
            labs(x = 'Temperature (oC)',
                 y = temp$StandardisedTraitUnit, title= paste(i, "_",temp$ConSpecies)))
  } 
}
dev.off()
write.csv(results_high,"results_high_Tref=0.csv")


################################################################################
###sharpschoollow_1981
results_low <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                          e=numeric(),el=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                          thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric())
pdf(file = "sharpeschoollow.pdf",width=7,height=3.5)

for (i in unique(d$OriginalID)){
  temp <- subset(d,d$OriginalID==i)
  #fit sharpeschoollow_1981
  start_vals <- get_start_vals(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoollow_1981')
  low_lims <- get_lower_lims(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoollow_1981')
  upper_lims <- get_upper_lims(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoollow_1981')
  fit <- nls_multstart(StandardisedTraitValue~sharpeschoollow_1981(temp = ConTemp, r_tref,e,el,tl, tref = 0),
                       data = temp,
                       iter = 500,
                       start_lower = start_vals - 10,
                       start_upper = start_vals + 10,
                       lower = low_lims,
                       upper = upper_lims,
                       supp_errors = 'Y')
  
  para <- calc_params(fit)
  para$orginalID <- i
  if(class(fit)=="NULL"){
    para$AIC <- "NULL"
  }else{para$AIC <- AIC(fit)}
  results_low <- rbind(results_low,para)
  
  #plotting
  if(length(fit)==5){
    preds <- data.frame(T = seq(min(temp$ConTemp), max(temp$ConTemp), length.out=100))
    coef <- coef(fit)
    k <- 8.617e-05
    r_tref <- coef(fit)[1]
    e <- coef(fit)[2]
    el <- coef(fit)[3]
    tl <- coef(fit)[4]
    tref = 0
    preds$rate <- r_tref*(exp(e/k*(1/(tref+273.15) - 1/(preds$T + 273.15))) /
                            (1 + exp(-(el/k)*(1/(tl + 273.15) - 1/(preds$T + 273.15)))))
    print(ggplot(temp, aes(y=StandardisedTraitValue, x=ConTemp)) +
            geom_point() +
            geom_line(aes(T, rate), preds, col = 'blue') +
            theme_bw(base_size = 12) +
            labs(x = 'Temperature (oC)',
                 y = sub$StandardisedTraitUnit, title= paste(i, "_",temp$ConSpecies)))
    
  }
}
dev.off()
write.csv(results_low,"results_low_Tref=0.csv")

################################################################################
###sharpschoolfull_1981
results_full <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                           e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                           thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric(),AIC=numeric())

for (i in unique(d$OriginalID)){
  mod = 'sharpschoolfull_1981'
  temp <- subset(d,d$OriginalID==i)
  start_vals <- get_start_vals(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolfull_1981')
  low_lims <- get_lower_lims(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolfull_1981')
  upper_lims <- get_upper_lims(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolfull_1981')
  fit <- nls_multstart(StandardisedTraitValue~sharpeschoolfull_1981(temp = ConTemp, r_tref,e,el,tl,eh,th, tref = 0),
                       data = temp,
                       iter = 500,
                       start_lower = start_vals - 10,
                       start_upper = start_vals + 10,
                       lower = low_lims,
                       upper = upper_lims,
                       supp_errors = 'Y')
  para <- calc_params(fit)
  para$orginalID <- i
  if(class(fit)=="NULL"){
    para$AIC <- "NULL"
  }else{para$AIC <- AIC(fit)}
  results_full <- rbind(results_full,para)
  
  #plotting
  if(length(fit)==5){
    preds <- data.frame(T = seq(min(temp$ConTemp), max(temp$ConTemp), length.out=100))
    coef <- coef(fit)
    k <- 8.617e-05
    r_tref <- coef(fit)[1]
    e <- coef(fit)[2]
    el <- coef(fit)[3]
    tl <- coef(fit)[4]
    eh <- coef(fit)[5]
    th <- coef(fit)[6]
    tref = 0
    preds$rate <- r_tref*(exp(e/k*(1/(tref+273.15) - 1/(preds$T + 273.15))) /
                            (1 + exp(-(el/k)*(1/(tl + 273.15) - 1/(preds$T + 273.15)))+
                               exp(eh/k*(1/(th+273.15)-1/(preds$T+273.15)))))
    print(ggplot(temp, aes(y=StandardisedTraitValue, x=ConTemp)) +
            geom_point() +
            geom_line(aes(T, rate), preds, col = 'blue') +
            theme_bw(base_size = 12) +
            labs(x = 'Temperature (oC)',
                 y = sub$StandardisedTraitUnit, title= paste(i, "_",temp$ConSpecies)))
  }
}

write.csv(results_full,"results_full_Tref=0.csv")

################################################################################
##compare between different models
results_high <- read.csv("D:/搜狗高速下载/project/plants/fitting/AIC/results_high_Tref=0.csv")
results_low <- read.csv("D:/搜狗高速下载/project/plants/fitting/AIC/results_low_Tref=0.csv")
results_full <- read.csv("D:/搜狗高速下载/project/plants/fitting/AIC/results_full_Tref=0.csv")
AIC <- data.frame(ID = results_high$orginalID,
                  sharpschoolhigh_1981 = results_high$AIC, 
                  sharpschoollow_1981 = results_low$AIC,
                  sharpschoolfull_1981 = results_full$AIC)
models<- pivot_longer(AIC,names_to = "model_name",values_to = "AIC",
                      cols=c(sharpschoolhigh_1981,sharpschoollow_1981,sharpschoolfull_1981))
models$AIC_num <- as.numeric(models$AIC)

best_model <- data.frame(ID <- character(),temp_best_model=character())
for (i in unique(models$ID)){
  temp_AIC <- subset(models,ID==i)
  if (temp_AIC$AIC[1] == "NULL" & temp_AIC$AIC[2] == "NULL" & temp_AIC$AIC[3]== "NULL"){
    temp_model <- c(i,"NA")
    best_model <- rbind(best_model,temp_model)
  }else{
    temp_model <- filter(temp_AIC, AIC_num == min(AIC_num,na.rm = TRUE)) %>% pull(model_name)
    temp_model <- c(i,temp_model)
    best_model <- rbind(best_model,temp_model)
  }
}


AIC_results <- data.frame(ID = results_high$orginalID,
                          sharpschoolhigh_1981 = results_high$AIC, 
                          sharpschoollow_1981 = results_low$AIC,
                          sharpschoolfull_1981 = results_full$AIC,
                          best_model = best_model$X.sharpschoolhigh_1981.
                          )
write.csv(AIC_results,"AIC_results.csv")

##check the percentage of different models 
table(AIC_results$best_model)
72/2005  ###percentage of TPCs cannot be fitted

#there are 72 TPCs(3.6%) cannot be fitted by SS,so omit them
AIC_results <- filter(AIC_results,best_model!="NA")
AIC_results$ID_new  <-  seq(from=1,to=1933)

#filter the TPCs with the best model is SS_full
SS_full <- filter(AIC_results,best_model=="sharpschoolfull_1981")

554/1933  #percentage of SS_full

SS_full_stack<- pivot_longer(SS_full,names_to = "model_name",values_to = "AIC",
                         cols=c(sharpschoolhigh_1981,sharpschoollow_1981,sharpschoolfull_1981))
SS_full_stack$AIC_num <- as.numeric(SS_full_stack$AIC)
SS_best_model <- data.frame(ID <- character(),temp_best_model=character())
for (i in unique(SS_full_stack$ID)){
  temp <- subset(SS_full_stack,ID==i)
  temp1 <- arrange(temp,AIC_num)
  dif_AIC <- temp1$AIC_num[1]-temp1$AIC_num[2]
  
  #If the difference of AIC is less than 2, reject SS_full
  if(abs(dif_AIC)<2){
    temp_model <- c(i,temp1$model_name[2])
    SS_best_model <- rbind(SS_best_model,temp_model)
  }else{
    temp_model <- c(i,temp1$model_name[1])
    SS_best_model <- rbind(SS_best_model,temp_model)
  }
  
}

SS_full_new <- cbind(SS_full,SS_best_model$X.sharpschoolhigh_1981.)
table(SS_full_new$`SS_best_model$X.sharpschoolhigh_1981.`)

#reject SS_full 

AIC_results$ID_new_num  <-  as.numeric(AIC_results$ID_new)
SS_full_new$ID_new_num <- as.numeric(SS_full_new$ID_new)

for (i in unique(SS_full_new$ID_new_num)){
  j <- which(SS_full_new$ID_new_num==i)
  AIC_results$best_model[i] <-  SS_full_new$`SS_best_model$X.sharpschoolhigh_1981.`[j]
}

485/1933  #percentage of SS_full

#################################################################################
#parameters from the best fitted models

results_parameter <- data.frame(orginalID=character(), ID=numeric(),AIC=numeric(),species=character(),AIC=numeric(),rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                                e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                                thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric())
for (i in unique(AIC_results$ID)){
  temp <- subset(AIC_results,ID==i)
  if(temp$best_model=="sharpschoolfull_1981"){
    results_parameter <- rbind(results_parameter,results_full[i,])
  }
  if(temp$best_model=="sharpschoolhigh_1981"){
    results_parameter <- rbind(results_parameter,results_high[i,])
  }
  if(temp$best_model=="sharpschoollow_1981"){
    results_parameter <- rbind(results_parameter,results_low[i,])
  }
}

write.csv(results_parameter,"plants_parameter.csv")
