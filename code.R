#Import data
d <- read.csv("combined.csv")
as.factor(d$OriginalID)

#dot plots #####################################################################
library(ggplot2)
for (i in unique(d$OriginalID)){
  temp <- subset(d,d$OriginalID==i)
  pdf(file = paste0(i, "_", ".pdf"),width=7,height=3.5)
  print(ggplot(temp, aes(y=StandardisedTraitValue, x=ConTemp))+geom_point()+
          labs(x="Temperature(oC)", y= temp$StandardisedTraitUnit, title= temp$ConSpecies)+theme_bw())
  dev.off()
}

################################################################################
#Fitting########################################################################
################################################################################
remotes::install_github("padpadpadpad/rTPC")
install.packages("nls.multstart")
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)

get_model_names()

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

##single TPCs 12
sub <- subset(d,d$OriginalID==12)

mod = 'sharpschoolhigh_1981'
ggplot(sub, aes(y=StandardisedTraitValue, x=ConTemp))+geom_point()+
  labs(x="Temperature(oC)", y= sub$StandardisedTraitUnit, title= sub$ConSpecies)+theme_bw()

start_vals <- get_start_vals(sub$ConTemp, sub$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
low_lims <- get_lower_lims(sub$ConTemp, sub$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(sub$ConTemp, sub$StandardisedTraitValue, model_name = 'sharpeschoolhigh_1981')
fit <- nls_multstart(StandardisedTraitValue~sharpeschoolhigh_1981(temp = ConTemp, r_tref,e,eh,th, tref = 0),
                       data = sub,
                       iter = 500,
                       start_lower = start_vals - 10,
                       start_upper = start_vals + 10,
                       lower = low_lims,
                       upper = upper_lims,
                       supp_errors = 'Y')
calc_params(fit)  #failure to fit the model

#1. change start value
value <- c(0.00000827,0.25476795,0.78943420,32.50000000)
start_vals <-value  
fit <- nls_multstart(StandardisedTraitValue~sharpeschoolhigh_1981(temp = ConTemp, r_tref,e,eh,th, tref = 0),
                     data = sub,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')
calc_params(fit)


preds <- data.frame(T = seq(min(sub$ConTemp), max(sub$ConTemp), length.out=100))
coef <- coef(fit)
k <- 8.617e-05
r_tref <- coef(fit)[1]
e <- coef(fit)[2]
eh <- coef(fit)[3]
th <- coef(fit)[4]
tref = 0
preds$rate <- r_tref*(exp(e/k*(1/(tref+273.15) - 1/(preds$T + 273.15))) /
                            (1 + exp(eh/k*(1/(th + 273.15) - 1/(preds$T + 273.15)))))
ggplot(sub, aes(y=StandardisedTraitValue, x=ConTemp)) +    
            geom_point() +
            geom_line(aes(T, rate), preds, col = 'blue') +
            theme_bw(base_size = 12) +
            labs(x = 'Temperature (oC)',
                 y = sub$StandardisedTraitUnit, title= sub$ConSpecies) #plotting


#2. "mod=sharpeschoollow_1981"
start_vals_low <- get_start_vals(sub$ConTemp, sub$StandardisedTraitValue, model_name = 'sharpeschoollow_1981')
low_lims_low <- get_lower_lims(sub$ConTemp, sub$StandardisedTraitValue, model_name = 'sharpeschoollow_1981')
upper_lims_low <- get_upper_lims(sub$ConTemp, sub$StandardisedTraitValue, model_name = 'sharpeschoollow_1981')
fit <- nls_multstart(StandardisedTraitValue~sharpeschoollow_1981(temp = ConTemp, r_tref,e,el,tl, tref = 0),
                     data = sub,
                     iter = 500,
                     start_lower = start_vals_low - 10,
                     start_upper = start_vals_low + 10,
                     lower = low_lims_low,
                     upper = upper_lims_low,
                     supp_errors = 'Y')
coef(fit)
calc_params(fit)


preds <- data.frame(T = seq(min(sub$ConTemp), max(sub$ConTemp), length.out=100))
coef <- coef(fit)
k <- 8.617e-05
r_tref <- coef(fit)[1]
e <- coef(fit)[2]
el <- coef(fit)[3]
tl <- coef(fit)[4]
tref = 0
preds$rate <- r_tref*(exp(e/k*(1/(tref+273.15) - 1/(preds$T + 273.15))) /
                        (1 + exp(-el/k*(1/(tl + 273.15) - 1/(preds$T + 273.15)))))
ggplot(sub, aes(y=StandardisedTraitValue, x=ConTemp)) +
  geom_point() +
  geom_line(aes(T, rate), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (oC)',
       y = sub$StandardisedTraitUnit, title= sub$ConSpecies) #plotting

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
