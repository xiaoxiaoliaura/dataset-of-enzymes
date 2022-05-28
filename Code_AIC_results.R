library(rTPC)
library(broom)
library(tidyverse)
library(ggrepel)
library(MuMIn)

#import data
d <- read.csv("combined.csv")
#select variables
d <- select(d,OriginalID,ConSpecies,ConTemp,
            StandardisedTraitName,StandardisedTraitValue,StandardisedTraitUnit,
            Habitat,Location, ConKingdom, Citation)

################################################################################
#sharpschoolhigh_1981
results_high <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                           e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                           thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric(),AIC=numeric())

for (i in unique(d$OriginalID)){
  mod = 'sharpschoolhigh_1981'
  temp <- subset(d,d$OriginalID==i)
  start_vals <- get_start_vals(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
  low_lims <- get_lower_lims(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
  upper_lims <- get_upper_lims(temp$ConTemp, temp$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
  fit <- nls_multstart(StandardisedTraitValue~sharpeschoolhigh_1981(temp = ConTemp, r_tref,e,eh,th, tref = 0),
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
  
  results_high <- rbind(results_high,para)
}
write.csv(results_high,"results_high_Tref=0.csv")

#sharpschoollow_1981

results_low <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                          e=numeric(),el=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                          thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric(),AIC=numeric())

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
}

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
}

write.csv(results_full,"results_full_Tref=0.csv")


################################################################################
##compare between different models
results_high <- read.csv("results_high_Tref=0.csv")
results_low <- read.csv("results_low_Tref=0.csv")
results_full <- read.csv("results_full_Tref=0.csv")
AIC <- data.frame(ID = results_high$orginalID,
                  sharpschoolhigh_1981 = results_high$AIC, 
                  sharpschoollow_1981 = results_low$AIC,
                  sharpschoolfull_1981 = results_full$AIC)
models<- pivot_longer(AIC,names_to = "model_name",values_to = "AIC",
                      cols=c(sharpschoolhigh_1981,sharpschoollow_1981,sharpschoolfull_1981))
best_model <- data.frame(ID <- character(),temp_best_model=character())
for (i in unique(models$ID)){
  temp_AIC <- subset(models,ID==i)
  if (temp_AIC$AIC[1] == "NULL" & temp_AIC$AIC[2] == "NULL" & temp_AIC$AIC[3]== "NULL"){
    temp_model <- c(i,"NULL")
    best_model <- rbind(best_model,temp_model)
  }else{
    temp_model <- filter(temp_AIC, AIC == min(AIC)) %>% pull(model_name)
    temp_model <- c(i,temp_model)
    best_model <- rbind(best_model,temp_model)
  }
}
AIC_results <- data.frame(ID = results_high$orginalID,
                          sharpschoolhigh_1981 = results_high$AIC, 
                          sharpschoollow_1981 = results_low$AIC,
                          sharpschoolfull_1981 = results_full$AIC,
                          best_model = best_model$X.sharpschoolfull_1981.)
write.csv(AIC_results,"AIC_results.csv")










