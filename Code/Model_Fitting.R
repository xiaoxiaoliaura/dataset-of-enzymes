#Import data
d <- read.csv("combined.csv")

#select variables
library(dplyr)
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

remotes::install_github("padpadpadpad/rTPC")
install.packages("nls.multstart")
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(ggrepel)
library(MuMIn)


# 1.Model_Fitting ---------------------------------------------------------
################################################################################
###1. sharpschoolhigh_1981
results_high <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                           e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                           thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric(),AIC=numeric(),
                           coef_r_tref=numeric(), coef_e = numeric(),coef_eh =numeric(),coef_th =numeric())


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
  para$species <- unique(temp$Species)
  if(class(fit)=="NULL"){
    para$AIC <- "NA"
    para$coef_r_tref <- "NA"
    para$coef_e <- "NA"
    para$coef_eh <- "NA"
    para$coef_th <- "NA"
  }else{para$AIC <- AIC(fit)
  para$coef_r_tref <- coef(fit)[1]
  para$coef_e <- coef(fit)[2]
  para$coef_eh <- coef(fit)[3]
  para$coef_th <- coef(fit)[4]
  } 
  results_high <- rbind(results_high,para)
}


write.csv(results_high,"results_high_new.csv")


################################################################################
###2. sharpschoollow_1981
results_low <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                          e=numeric(),el=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                          thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric(),AIC=numeric(),
                          coef_r_tref=numeric(),coef_e =numeric(),coef_el =numeric(),coef_tl =numeric())


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
  para$species <- unique(temp$Species)
  if(class(fit)=="NULL"){
    para$AIC <- "NA"
    para$coef_r_tref <- "NA"
    para$coef_e <- "NA"
    para$coef_el <- "NA"
    para$coef_tl <- "NA"
  }else{para$AIC <- AIC(fit)
  para$coef_r_tref <- coef(fit)[1]
  para$coef_e <- coef(fit)[2]
  para$coef_el <- coef(fit)[3]
  para$coef_tl <- coef(fit)[4]
  }
  results_low <- rbind(results_low,para)
}


write.csv(results_low,"results_low_new.csv")

################################################################################
###3. sharpschoolfull_1981
results_full <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                           e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                           thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric(),AIC=numeric(),
                           coef_r_tref=numeric(),
                           coef_e =numeric(),coef_el =numeric(),coef_tl =numeric(),
                           coef_eh =numeric(),coef_th =numeric())
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
  para$species <- unique(temp$Species)
  if(class(fit)=="NULL"){
    para$AIC <- "NA"
    para$coef_r_tref <- "NA"
    para$coef_e <- "NA"
    para$coef_el <- "NA"
    para$coef_tl <- "NA"
    para$coef_eh <- "NA"
    para$coef_th <- "NA"
  }else{para$AIC <- AIC(fit)
  para$coef_r_tref <- coef(fit)[1]
  para$coef_e <- coef(fit)[2]
  para$coef_el <- coef(fit)[3]
  para$coef_tl <- coef(fit)[4]
  para$coef_eh <- coef(fit)[5]
  para$coef_th <- coef(fit)[6]
  }
  
  results_full <- rbind(results_full,para)
}

write.csv(results_full,"results_full_new.csv")

################################################################################
###4. Arrhenius model

library(installr)
library(usethis)
library(devtools)
library(remotes)
remotes::install_github("https://github.com/mberk/sme.git")

###functions
BOLTZ <- 8.617e-5  # Boltzmann constant, in eV/K.
Tref_K <- 273.15

GetE <- function(tmp_K, rate, Tp_K, k=BOLTZ)
{
  # Estimate starting value for E, taking linear regression using the rise part
  # of the curve only.
  # ~~~ Parameters ~~~
  # tmp_K :numeric: temperature data (in K).
  # rate  :numeric: rate data corresponding to temperature above.
  # Tp_K  :numeric: temperature at which rate peaks, used as a cutoff point.
  # k     :numeric: Boltzmann constant.
  
  tmp_K <- as.numeric(tmp_K)
  rate <- as.numeric(rate)
  
  tmp_w <- which(tmp_K <= Tp_K)
  if (length(tmp_w) > 1)
  {
    m <- lm(log(rate[tmp_w]) ~ I(1 / (k * (tmp_K[tmp_w]))))
    return(abs(summary(m)$coefficients[2, c('Estimate', 'Std. Error')]))
  } else
  {
    return(c(0.7, 2))  # Arbitrary estimate if we can't do regression.
  }
}


GetLnB0 <- function(tmp_K, rate, Tref_K)
{
  # Estimate starting value for the normalisation constant B0.
  # ~~~ Parameters ~~~
  # tmp_K  :numeric: temperature data (in K).
  # rate   :numeric: rate data corresponding to temperature above.
  # Tref_K :numeric: estimate normalising constant at this temperature (in K).
  
  tmp_K <- as.numeric(tmp_K)
  rate  <- as.numeric(rate)
  
  if (min(tmp_K, na.rm=TRUE) > Tref_K)
  {
    return(log(rate[1]))
  } else
  {
    return(log(max(rate[which(tmp_K <= Tref_K)], na.rm=TRUE)))
  }
}



GetTpk <- function(tmp_K, rate)
{
  # Temperature at which the rate is maximised (estimate of T.peak).
  # ~~~ Parameters ~~~
  # tmp_K :numeric: Temperature data (in K).
  # rate  :numeric: Rate data corresponding to temperature above.
  
  tmp_K = as.numeric(tmp_K)
  rate  = as.numeric(rate)
  
  return(max(tmp_K[which.max(rate)]))
}



Arr <- function(lnB0, E, Tref_K, tmp_K, k=BOLTZ)
{
  # (Boltzmann)-Arrhenius response function, in ln scale. 
  # ~~~ Parameters ~~~
  # lnB0   :numeric: Ln rate at T_ref.
  # E      :numeric: Activation energy (eV).
  # Tref_K :numeric: Reference temperature at which the rate is 'lnB0' (K).
  
  return(lnB0 - E/k * (1/tmp_K - 1/Tref_K))
}


ArrFit <- function(dat, trait_name, temp_name, unc_name=NULL, Tref_K, n_rand=1,
                   k=BOLTZ){
  
  trait <- dat[["Rate"]]
  temp_K  <-  dat[["Temp_K"]]
  
  if (min(temp_K, na.rm=TRUE) < 200){
    
    stop('Error in Arrfit : make sure temperatures are in K')
  }
  
  if (length(unc_name) > 0){
    
    wgts = 1 / dat[[unc_name]]
  } else{
    
    l    = length(trait)
    wgts = rep(1 / l, l)
  }
  
  # start value estimate
  E_st <- as.numeric(GetE(tmp_K=temp_K, rate=trait, Tp_K=temp_K[which.max(trait)]))
  lnB_st<- GetLnB0(tmp_K=temp_K, rate=trait, Tref_K=Tref_K)
  B_st <- exp(lnB_st)
  
  # Create randomised starting points.
  E_st_pe<- E_st[1]  # Slope value point estimate.
  
  if (n_rand > 1){
    # We need truncated normal to ensure we don't get negative values of E.
    E_st <- c(E_st_pe, rtruncnorm(n_rand-1, a=0, b=Inf, mean=E_st[1], sd=2 * E_st[2]))
    # Randomise on linear scale. Again, we don't want negative rates.
    lnB_st <- c(lnB_st, log(rtruncnorm(n_rand-1, a=0, b=Inf, mean=B_st, sd=B_st / 2)))
  } else{
    E_st<- E_st_pe
  }
  
  # select the best model using AICc. Many of these are similar.
  aics_out <- rep(NA, n_rand)
  
  for (i in 1:n_rand){
    
    arr_fit <- try(nlsLM(log(trait) ~ Arr(lnB0, E, Tref_K=Tref_K, tmp_K=temp_K),
                         start=c(lnB0=lnB_st[i], E=E_st[i]),
                         lower=c(lnB0=-Inf, E=0),
                         upper=c(lnB0=Inf,  E=30),
                         weights=wgts,
                         control=list(minFactor=1 / 2^16, maxiter=1024)),
                   silent=TRUE)
    
    if (class(arr_fit) != 'try-error'){
      
      aics_out[i] <- AICc(arr_fit)
    }
  }
  
  w <- which.min(aics_out)
  
  if (length(w) > 0 & n_rand == 1){
    
    out <- cbind(summary(arr_fit)$coefficients[, c('Estimate', 'Std. Error')],
                 start=c(lnB_st, E_st), vcov(arr_fit), AIC_c=aics_out,
                 AIC=AIC(arr_fit), BIC=BIC(arr_fit), log_Lik=logLik(arr_fit))
  } else if (length(w) > 0 & n_rand > 1){
    
    arr_fit <- try(nlsLM(log(trait) ~ Arr(lnB0, E, Tref_K=Tref_K, tmp_K=temp_K),
                         start=c(lnB0=lnB_st[w], E=E_st[w]),
                         lower=c(lnB0=-Inf, E=0),
                         upper=c(lnB0=Inf,  E=30),
                         weights=wgts,
                         control=list(minFactor=1 / 2^16, maxiter=1024)),
                   silent=TRUE)
    
    out <- cbind(summary(arr_fit)$coefficients[, c('Estimate', 'Std. Error')],
                 start=c(lnB_st[w], E_st[w]), vcov(arr_fit), AIC_c=aics_out[w],
                 AIC=AIC(arr_fit), BIC=BIC(arr_fit), log_Lik=logLik(arr_fit))
  } else {
    
    out <- matrix(as.numeric(NA), ncol=2 + 1 + 2 + 1, nrow=2)
  }
  
  par_names <- c('lnB0', 'E')
  out <- data.frame(par_names, out, stringsAsFactors=FALSE, row.names=NULL)
  colnames(out) <- c('pars', 'est', 'se', 'start', 'lnB0', 'E', 'AIC_c', 'AIC',
                     'BIC', 'log_Lik')
  return(out)
}


d <-  mutate(d,Temp_K=ConTemp+273.15)

d_new <- data.frame(OriginalID=character(),ConSpecies=character(),ConTemp=numeric(),
                    StandardisedTraitName=character(),StandardisedTraitValue=numeric(),
                    StandardisedTraitUnit=character(),Temp_K=numeric())

for (i in unique(d$OriginalID)){
  temp <- subset(d,OriginalID==i)
  l <- length(temp$StandardisedTraitValue)
  max <- as.numeric(which.max(temp$StandardisedTraitValue))
  h <- temp$StandardisedTraitValue[1]
  t <- temp$StandardisedTraitValue[l]
  if(l-max<max-1&h<t){
    d_new <- rbind(d_new,temp)
  }
}


Arr_resullt <- data.frame(pars=numeric(),est=numeric(),se=numeric(),start=numeric(),lnB0=numeric(),
                          E=numeric(),AIC_c=numeric(),AIC=numeric(),BIC=numeric(),log_Lik=numeric())

#plotting
pdf(file = "Arr_plants.pdf",width=7,height=3.5)


for (ID in unique(d_new$OriginalID)){
  
  temp <- subset(d_new,OriginalID==ID)
  trait <- temp[["StandardisedTraitValue"]]
  temp_K  <-  temp[["Temp_K"]]
  unc_name <- NULL
  Tref_K <- 273.15
  n_rand <- 500
  k <- BOLTZ
  
  # start value estimate
  E_st <- as.numeric(GetE(tmp_K=temp_K, rate=trait, Tp_K=temp_K[which.max(trait)]))
  lnB_st<- GetLnB0(tmp_K=temp_K, rate=trait, Tref_K=Tref_K)
  B_st <- exp(lnB_st)
  
  # Create randomised starting points.
  E_st_pe<- E_st[1]  # Slope value point estimate.
  
  if (n_rand > 1){
    # We need truncated normal to ensure we don't get negative values of E.
    E_st <- c(E_st_pe, rtruncnorm(n_rand-1, a=0, b=Inf, mean=E_st[1], sd=2 * E_st[2]))
    # Randomise on linear scale. Again, we don't want negative rates.
    lnB_st <- c(lnB_st, log(rtruncnorm(n_rand-1, a=0, b=Inf, mean=B_st, sd=B_st / 2)))
  } else{
    E_st<- E_st_pe
  }
  
  # select the best model using AICc. Many of these are similar.
  aics_out <- rep(NA, n_rand)
  
  for (i in 1:n_rand){
    
    arr_fit <- try(nlsLM(log(trait) ~ Arr(lnB0, E, Tref_K=Tref_K, tmp_K=temp_K),
                         start=c(lnB0=lnB_st[i], E=E_st[i]),
                         lower=c(lnB0=-Inf, E=0),
                         upper=c(lnB0=Inf,  E=30),
                         control=list(minFactor=1 / 2^16, maxiter=1024)),
                   silent=TRUE)
    
    if (class(arr_fit) != 'try-error'){
      
      aics_out[i] <- AICc(arr_fit)
    }
  }
  
  w <- which.min(aics_out)
  
  if (length(w) > 0 & n_rand > 1){
    
    arr_fit <- try(nlsLM(log(trait) ~ Arr(lnB0, E, Tref_K=Tref_K, tmp_K=temp_K),
                         start=c(lnB0=lnB_st[w], E=E_st[w]),
                         lower=c(lnB0=-Inf, E=0),
                         upper=c(lnB0=Inf,  E=30),
                         control=list(minFactor=1 / 2^16, maxiter=1024)),
                   silent=TRUE)
    
    out <- cbind(summary(arr_fit)$coefficients[, c('Estimate', 'Std. Error')],
                 start=c(lnB_st[w], E_st[w]), vcov(arr_fit), AIC_c=aics_out[w],
                 AIC=AIC(arr_fit), BIC=BIC(arr_fit), log_Lik=logLik(arr_fit),
                 AIC_correct=AIC(arr_fit)+ sum(2*log(trait)))
  } else {
    
    out <- matrix(as.numeric(NA), ncol=10, nrow=2)
  }
  
  par_names <- c('lnB0', 'E')
  ID <- c(ID,ID)
  out <- data.frame(ID,par_names, out, stringsAsFactors=FALSE, row.names=NULL)
  colnames(out) <- c('ID','pars', 'est', 'se', 'start', 'lnB0', 'E', 'AIC_c', 'AIC',
                     'BIC', 'log_Lik','AIC_correct')
  Arr_resullt <-rbind(Arr_resullt,out)
  
  
  #plotting
  if(class(arr_fit)!="try-error"){preds <- data.frame(t = seq(min(temp$Temp_K), max(temp$Temp_K), length.out=100))
  preds$lnB <- Arr(lnB0=summary(arr_fit)$coefficients[ 'lnB0','Estimate'], 
                   E=summary(arr_fit)$coefficients[ 'E','Estimate'], Tref_K, preds$t,BOLTZ) 
  preds <- mutate(preds,kT_r=1/(BOLTZ*t))
  print(ggplot(temp, aes(1/(BOLTZ*Temp_K),log(StandardisedTraitValue))) +
          geom_point() +
          geom_line(aes(kT_r, lnB), preds, col = 'blue') +
          theme_bw(base_size = 12) +
          labs(x = '1/kT',
               y = "ln(metabolic rate)",
               title= paste(ID, "_",temp$ConSpecies))+theme_bw())}
  
}

dev.off()

##save results
write.csv(Arr_resullt,"Arr.csv")
Arr_plants <- select(Arr_resullt,ID,pars,est,AIC,AIC_correct)
Arr_plants <- pivot_wider(Arr_plants,names_from="pars",values_from="est")
Arr_plants <- mutate(Arr_plants,B0=exp(lnB0))
write.csv(Arr_plants,"Arr_plants.csv")


# 2. Model Selection -------------------------------------------------
results_high <- read.csv("results_high_new.csv")
results_low <- read.csv("results_low_new.csv")
results_full <- read.csv("results_full_new.csv")
results_Arr <- read.csv("Arr_plants.csv")

results_high$ID <- seq(from=1,to=2005)
results_low$ID <- seq(from=1,to=2005)
results_full$ID <- seq(from=1,to=2005)
results_Arr$orginalID <- results_Arr$ID

results_Arr_all<- merge(results_Arr,results_high,by="orginalID")
results_Arr_all <- select(results_Arr_all,orginalID, AIC_correct,lnB0,E,B0,topt,ID.y)

AIC <- data.frame(ID = results_high$ID,
                  sharpschoolhigh_1981 = results_high$AIC, 
                  sharpschoollow_1981 = results_low$AIC,
                  sharpschoolfull_1981 = results_full$AIC
)
models<- pivot_longer(AIC,names_to = "model_name",values_to = "AIC",
                      cols=c(sharpschoolhigh_1981,sharpschoollow_1981,sharpschoolfull_1981))
models$AIC_num <- as.numeric(models$AIC)

best_model <- data.frame(ID= numeric(),temp_best_model=character())


for (i in unique(models$ID)){
  temp_AIC <- subset(models,ID==i)
  if (temp_AIC$AIC[1] == "NA" & temp_AIC$AIC[2] == "NA" & temp_AIC$AIC[3]== "NA"){
    temp_model <- c(i,"NULL")
    best_model <- rbind(best_model,temp_model)
  }else{
    temp_model <- filter(temp_AIC, AIC_num == min(AIC_num,na.rm = TRUE)) %>% pull(model_name)
    temp_model <- c(i,temp_model)
    best_model <- rbind(best_model,temp_model)
  }
}

AIC_results <- data.frame(ID = results_high$ID,
                          OriginalID = results_high$orginalID,
                          sharpschoolhigh_1981 = results_high$AIC, 
                          sharpschoollow_1981 = results_low$AIC,
                          sharpschoolfull_1981 = results_full$AIC,
                          best_model= best_model$X.sharpschoolhigh_1981.
)

write.csv(AIC_results,"AIC_results.csv")

##check the percentage of different models 
table(AIC_results$best_model)
72/2005  ###percentage of TPCs cannot be fitted

#there are 72 TPCs(3.6%) cannot be fitted by SS,so omit them
AIC_results_1 <- filter(AIC_results,best_model!="NA")
str(AIC_results_1)

#filter the TPCs with the best model is SS_full
SS_full <- filter(AIC_results_1,best_model=="sharpschoolfull_1981")
length(SS_full$ID)
554/1933  #percentage of SS_full

SS_full_stack<- pivot_longer(SS_full,names_to = "model_name",values_to = "AIC",
                             cols=c(sharpschoolhigh_1981,sharpschoollow_1981,sharpschoolfull_1981))
SS_full_stack$AIC_num <- as.numeric(SS_full_stack$AIC)
SS_best_model <- data.frame(ID <- numeric(),temp_best_model=character())
for (i in unique(SS_full_stack$ID)){
  temp <- subset(SS_full_stack,ID==i)
  temp1 <- arrange(temp,AIC_num)
  dif_AIC <- temp1$AIC_num[1]-temp1$AIC_num[2]
  
  #If the difference of AIC is less than 2, reject SS_full
  if(abs(dif_AIC)<2){
    temp_model <- c(i,temp1$model_name[2])
    SS_best_model <- rbind(SS_best_model,temp_model)
  }else{
    temp_model <- c(i,"sharpschoolfull_1981")
    SS_best_model <- rbind(SS_best_model,temp_model)
  }
  
}

SS_full_new <- cbind(SS_full[1:5],SS_best_model$X.sharpschoolhigh_1981.)
table(SS_full_new$`SS_best_model$X.sharpschoolhigh_1981.`)
485/1933 #percentage of SS_full

#reject SS_full 
for (i in unique(SS_full_new$ID)){
  j <- which(SS_full_new$ID==i)
  k <- which(AIC_results_1$ID==i)
  AIC_results_1$best_model[k] <-  SS_full_new$`SS_best_model$X.sharpschoolhigh_1981.`[j]
}
table(AIC_results_1$best_model)

485/1933  #percentage of SS_full
960/1933  #percentage of SS_high
488/1933  #percentage of SS_low
write.csv(AIC_results_1,"model_select_result.csv")

#compare SS model and Arr model
results_Arr <- filter(results_Arr,AIC_correct!="NA")
Arr_best <- c()

for (i in unique(results_Arr$ID)){
  
  w <- which(results_Arr$ID==i)
  p <- which(AIC_results$OriginalID==i)
  
  if (results_Arr$AIC_correct[w]<AIC_results$sharpschoolhigh_1981[p]
      &results_Arr$AIC_correct[w]<AIC_results$sharpschoollow_1981[p]
      &results_Arr$AIC_correct[w]<AIC_results$sharpschoolfull_1981[p]){
    Arr_best <- c(Arr_best, i)
  }
  
}

for (i in Arr_best){
  w <- which(AIC_results$OriginalID==i)
  AIC_results$best_model[w] <- "Arrhenius"
}


###parameters from the best fitted model####

results_parameter <- data.frame(orginalID=character(), species=character(),topt=numeric(),
                                e=numeric(),q10=numeric(),coef_e=numeric())
results_Arr_all$coef_e <- "NA"
results_Arr_all$q10 <- "NA"
results_Arr_all$e <- results_Arr_all$E

AIC_results <- filter(AIC_results, best_model!="NA")
table(AIC_results$best_model)

for (i in unique(AIC_results$ID)){
  temp <- subset(AIC_results,ID==i)
  if(temp$best_model=="sharpschoolfull_1981"){
    para <- select(results_full[i,],topt,e,q10,orginalID,coef_e)
    results_parameter <- rbind(results_parameter,para)
  }
  if(temp$best_model=="sharpschoolhigh_1981"){
    para <- select(results_high[i,],topt,e,q10,orginalID,coef_e)
    results_parameter <- rbind(results_parameter,para)
  }
  if(temp$best_model=="sharpschoollow_1981"){
    para <- select(results_low[i,],topt,e,q10,orginalID,coef_e)
    results_parameter <- rbind(results_parameter,para)
  }
  if(temp$best_model=="Arrhenius"){
    w <- which(results_Arr_all$ID.y==i)
    para <- select(results_Arr_all[w,],topt,e,q10,orginalID,coef_e)
    results_parameter <- rbind(results_parameter,para)
  }
}

write.csv(results_parameter,"plants_parameter.csv")


# 3. Rubisco --------------------------------------------------------------

#import data
rubisco <- read.csv("Rubisco.csv")

pdf(file = "rubisco_fitted_results.pdf",width=7,height=3.5)
rubisco_results <- data.frame(ID=character(), Species=character(),units=character(),
                              model_name=character(),fit=list(),topt=numeric(),e=numeric(),
                              q10=numeric(),AIC=numeric(),coef=list())
# fit three chosen model formulation in rTPC

for (i in unique(rubisco$ID)){
  sub <- subset(rubisco,rubisco$ID==i)
  ## fit 
  d_fits <- nest(sub, data = c(Temp, Rate)) %>%
    mutate(sharpeschoolhigh = map(data, ~nls_multstart(Rate~sharpeschoolhigh_1981(temp = Temp, r_tref,e,eh,th, tref = 0),
                                                       data = .x,
                                                       iter = 500,
                                                       start_lower = get_start_vals(.x$Temp, .x$Rate, model_name = 'sharpeschoolhigh_1981') - 10,
                                                       start_upper = get_start_vals(.x$Temp, .x$Rate, model_name = 'sharpeschoolhigh_1981') + 10,
                                                       lower = get_lower_lims(.x$Temp, .x$Rate, model_name = 'sharpeschoolhigh_1981'),
                                                       upper = get_upper_lims(.x$Temp, .x$Rate, model_name = 'sharpeschoolhigh_1981'),
                                                       supp_errors = 'Y')),
           sharpeschoollow = map(data, ~nls_multstart(Rate~sharpeschoollow_1981(temp = Temp, r_tref,e,el,tl, tref = 0),
                                                      data = .x,
                                                      iter = 500,
                                                      start_lower = get_start_vals(.x$Temp, .x$Rate, model_name = 'sharpeschoollow_1981') - 10,
                                                      start_upper = get_start_vals(.x$Temp, .x$Rate, model_name = 'sharpeschoollow_1981') + 10,
                                                      lower = get_lower_lims(.x$Temp, .x$Rate, model_name = 'sharpeschoollow_1981'),
                                                      upper = get_upper_lims(.x$Temp, .x$Rate, model_name = 'sharpeschoollow_1981'),
                                                      supp_errors = 'Y')),
           sharpeschoolfull = map(data, ~nls_multstart(Rate~sharpeschoolfull_1981(temp = Temp, r_tref,e,el,tl,eh,th, tref = 0),
                                                       data = .x,
                                                       iter = 500,
                                                       start_lower = get_start_vals(.x$Temp, .x$Rate, model_name = 'sharpeschoolfull_1981') - 10,
                                                       start_upper = get_start_vals(.x$Temp, .x$Rate, model_name = 'sharpeschoolfull_1981') + 10,
                                                       lower = get_lower_lims(.x$Temp, .x$Rate, model_name = 'sharpeschoolfull_1981'),
                                                       upper = get_upper_lims(.x$Temp, .x$Rate, model_name = 'sharpeschoolfull_1981'),
                                                       supp_errors = 'Y')))
  
  test <- d_fits$sharpeschoolhigh != "NULL"|d_fits$sharpeschoollow != "NULL"|d_fits$sharpeschoolfull!= "NULL"
  if (test==TRUE){ 
    # stack models and calculate extra params
    d_params <- pivot_longer(d_fits, names_to = 'model_name', values_to = 'fit', c(sharpeschoolhigh,sharpeschoollow,sharpeschoolfull)) %>%
      mutate(params = map(fit, calc_params))  %>%
      mutate(coef=map(fit,coef)) %>% 
      unnest(params) %>% select(ID,Species,units,model_name,fit,topt,e,q10,coef) 
    
    rubisco_results <- rbind(rubisco_results,d_params)
  }
  
  ## get predictions using augment
  newdata <- tibble(Temp = seq(min(sub$Temp), max(sub$Temp), length.out = 100))
  d_preds <- d_stack %>%
    mutate(., preds = map(fit, augment, newdata = newdata)) %>%
    select(-fit) %>%
    unnest(preds)
  
  # plot
  print(ggplot(d_preds, aes(Temp, .fitted)) +
          geom_line(aes(col = model_name)) +
          geom_point(aes(Temp, Rate), sub) +
          theme_bw(base_size = 12) +
          theme(legend.position = "right") +
          labs(x = 'Temperature (oC)',
               y = "metabolic rate",
               title = paste(i, "_",sub$Species)) +
          geom_hline(aes(yintercept = 0), linetype = 2) +
          scale_color_brewer(type = 'qual', palette = 2))
}

save(rubisco_results,file = "rubisco_results.Rdata" )
dev.off()

# AIC
#compare AIC between different models
rubisco_high <- read.csv("rubisco_high.csv")
rubisco_low <- read.csv("rubisco_low.csv")
rubisco_full <- read.csv("rubisco_full.csv")
rubisco_AIC <- data.frame(ID = rubisco_high$ID,
                          sharpschoolhigh_1981 = rubisco_high$AIC, 
                          sharpschoollow_1981 = rubisco_low$AIC,
                          sharpschoolfull_1981 = rubisco_full$AIC)

rubisco_models<- pivot_longer(rubisco_AIC,names_to = "model_name",values_to = "AIC",
                              cols=c(sharpschoolhigh_1981,sharpschoollow_1981,sharpschoolfull_1981))
rubisco_models$AIC_num <- as.numeric(rubisco_models$AIC)

rubisco_best_model <- data.frame(ID <- character(),temp_best_model=character())
for (i in unique(rubisco_models$ID)){
  temp <- subset(rubisco_models,ID==i)
  if (temp$AIC[1] == "NULL" & temp$AIC[2] == "NULL" & temp$AIC[3]== "NULL"){
    temp_model <- c(i,"NA")
    rubisco_best_model <- rbind(rubisco_best_model,temp_model)
  }else{
    temp_model <- filter(temp, AIC_num == min(AIC_num,na.rm = TRUE)) %>% pull(model_name)
    temp_model <- c(i,temp_model)
    rubisco_best_model <- rbind(rubisco_best_model,temp_model)
  }
}

rubisco_AIC_results <- data.frame(ID = rubisco_high$orginalID,
                                  species = rubisco_high$species,
                                  sharpschoolhigh_1981 = rubisco_high$AIC, 
                                  sharpschoollow_1981 = rubisco_low$AIC,
                                  sharpschoolfull_1981 = rubisco_full$AIC,
                                  rubisco_best_model = rubisco_best_model$X.sharpschoolfull_1981.)

write.csv(rubisco_AIC_results,"rubisco_AIC_results.csv")

##check the percentage of different models 
rubisco_AIC_results <- read.csv("D:/ËÑ¹·¸ßËÙÏÂÔØ/project/rubisco/SS_fitting_results/rubisco_AIC_results.csv")
table(rubisco_AIC_results$rubisco_best_model)
10/71 ###percentage of TPCs cannot be fitted

##there are 10 TPCs(14%) cannot be fitted by SS,omit them
rubisco_AIC_results <- filter(rubisco_AIC_results,rubisco_best_model!="NA")

##filter the TPCs with the best model is SS_full
rubisco_SS_full <- filter(rubisco_AIC_results,rubisco_best_model=="sharpschoolfull_1981")

7/61   ###percentage of SS_full

##try to use the second best model to instead the SS_full
rubisco_SS_full_1<- pivot_longer(rubisco_SS_full,names_to = "model_name",values_to = "AIC",
                                 cols=c(sharpschoolhigh_1981,sharpschoollow_1981,sharpschoolfull_1981))
rubisco_SS_full_1$AIC_num <- as.numeric(rubisco_SS_full_1$AIC)
rubisco_SS_best_model <- data.frame(ID <- character(),temp_best_model=character())
for (i in unique(rubisco_SS_full_1$ID)){
  temp <- subset(rubisco_SS_full_1,ID==i)
  temp1 <- arrange(temp,AIC_num)
  dif_AIC <- temp1$AIC_num[1]-temp1$AIC_num[2]
  
  #If the difference of AIC is less than 2, reject SS_full
  if(abs(dif_AIC)<2){
    temp_model <- c(i,temp1$model_name[2])
    rubisco_SS_best_model <- rbind(rubisco_SS_best_model,temp_model)
  }else{
    temp_model <- c(i,temp1$model_name[1])
    rubisco_SS_best_model <- rbind(rubisco_SS_best_model,temp_model)
  }
  
}

rubisco_SS_full_new <- cbind(rubisco_SS_full,rubisco_SS_best_model$X.sharpschoolfull_1981.)
table(rubisco_SS_full_new$`rubisco_SS_best_model$X.sharpschoolfull_1981.`)  ### SS_full couldn't be rejected for these 7 TPCs

################################################################################
#parameters from the best fitted models
rubisco_parameter <- data.frame(orginalID=character(), species=character(),AIC=numeric(),rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                                e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                                thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric())

for (i in unique(rubisco_AIC_results$ID)){
  temp <- subset(rubisco_AIC_results,ID==i)
  if(temp$rubisco_best_model=="sharpschoolfull_1981"){
    rubisco_parameter <- rbind(rubisco_parameter,rubisco_full[i,])
  }
  if(temp$rubisco_best_model=="sharpschoolhigh_1981"){
    rubisco_parameter <- rbind(rubisco_parameter,rubisco_high[i,])
  }
  if(temp$rubisco_best_model=="sharpschoollow_1981"){
    rubisco_parameter <- rbind(rubisco_parameter,rubisco_low[i,])
  }
}
View(rubisco_parameter)
write.csv(rubisco_parameter,"rubisco_parameter.csv")

rubisco_parameter <- read.csv("rubisco_parameter.csv")




