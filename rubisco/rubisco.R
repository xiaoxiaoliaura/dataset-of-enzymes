#import data
rubisco <- read.csv("Rubisco.csv")



#Plots of the fitting results of three SS models

pdf(file = "rubisco_fitted_results.pdf",width=7,height=3.5)

for (i in unique(rubisco$ID)){
  temp <- subset(rubisco,rubisco$ID==i)
  ## fit three chosen model formulations in rTPC
  d_fits <- nest(temp, data = c(Temp, Rate)) %>%
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
  if (d_fits$sharpeschoolhigh == "NULL" & d_fits$sharpeschoollow == "NULL" & d_fits$sharpeschoolfull== "NULL"){
    
  }else{ ## stack models
    d_stack <- select(d_fits, -data) %>%
      pivot_longer(., names_to = 'model_name', values_to = 'fit', sharpeschoolhigh:sharpeschoolfull)
    
    ## get predictions using augment
    newdata <- tibble(Temp = seq(min(temp$Temp), max(temp$Temp), length.out = 100))
    d_preds <- d_stack %>%
      mutate(., preds = map(fit, augment, newdata = newdata)) %>%
      select(-fit) %>%
      unnest(preds)
    ## take a random point from each model for labelling
    d_labs <- filter(d_preds, Temp < 30) %>%
      group_by(., model_name) %>%
      sample_n(., 1) %>%
      ungroup()
    
    print(ggplot(d_preds, aes(Temp, .fitted)) +
            geom_line(aes(col = model_name)) +
            geom_label_repel(aes(Temp, .fitted, label = model_name, col = model_name), fill = 'white', nudge_y = 0.8, segment.size = 0.2, segment.colour = 'grey50', d_labs) +
            geom_point(aes(Temp, Rate), temp) +
            theme_bw(base_size = 12) +
            theme(legend.position = 'none') +
            labs(x = 'Temperature (oC)',
                 y = "metabolic rate",
                 title = paste(i, "_",temp$Species)) +
            geom_hline(aes(yintercept = 0), linetype = 2) +
            scale_color_brewer(type = 'qual', palette = 2))}
}

dev.off()

################################################################################
#Parameters and AIC
##sharpschoolhigh_1981
rubisco_high <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                           e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                           thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric())

for (i in unique(rubisco$ID)){
  mod = 'sharpschoolhigh_1981'
  temp <- subset(rubisco,rubisco$ID==i)
  start_vals <- get_start_vals(temp$Temp, temp$Rate, model_name = 'sharpeschoolhigh_1981')
  low_lims <- get_lower_lims(temp$Temp, temp$Rate, model_name = 'sharpeschoolhigh_1981')
  upper_lims <- get_upper_lims(temp$Temp, temp$Rate, model_name = 'sharpeschoolhigh_1981')
  fit <- nls_multstart(Rate~sharpeschoolhigh_1981(temp = Temp, r_tref,e,eh,th, tref = 0),
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
    para$AIC <- "NULL"
  }else{para$AIC <- AIC(fit)}
  
  rubisco_high <- rbind(rubisco_high,para)
}

write.csv(rubisco_high,"rubisco_high.csv")


##sharpschoollow_1981
rubisco_low <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                          e=numeric(),el=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                          thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric(),AIC=numeric())

for (i in unique(rubisco$ID)){
  temp <- subset(rubisco,rubisco$ID==i)
  start_vals <- get_start_vals(temp$Temp, temp$Rate, model_name = 'sharpeschoollow_1981')
  low_lims <- get_lower_lims(temp$Temp, temp$Rate, model_name = 'sharpeschoollow_1981')
  upper_lims <- get_upper_lims(temp$Temp, temp$Rate, model_name = 'sharpeschoollow_1981')
  fit <- nls_multstart(Rate~sharpeschoollow_1981(temp = Temp, r_tref,e,el,tl, tref = 0),
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
    para$AIC <- "NULL"
  }else{para$AIC <- AIC(fit)}
  
  rubisco_low <- rbind(rubisco_low,para)
}

write.csv(rubisco_low,"rubisco_low.csv")


##sharpschoolfull_1981
rubisco_full <- data.frame(orginalID=character(), rmax=numeric(),topt=numeric(),ctmin=numeric(),ctmax=numeric(),
                           e=numeric(),eh=numeric(),q10=numeric(),thermal_safety_margin=numeric(),
                           thermal_tolerance=numeric(),breadth=numeric(),skewness=numeric(),AIC=numeric())

for (i in unique(rubisco$ID)){
  mod = 'sharpschoolfull_1981'
  temp <- subset(rubisco,rubisco$ID==i)
  start_vals <- get_start_vals(temp$Temp, temp$Rate, model_name = 'sharpeschoolfull_1981')
  low_lims <- get_lower_lims(temp$Temp, temp$Rate, model_name = 'sharpeschoolfull_1981')
  upper_lims <- get_upper_lims(temp$Temp, temp$Rate, model_name = 'sharpeschoolfull_1981')
  fit <- nls_multstart(Rate~sharpeschoolfull_1981(temp = Temp, r_tref,e,el,tl,eh,th, tref = 0),
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
    para$AIC <- "NULL"
  }else{para$AIC <- AIC(fit)}
  
  rubisco_full <- rbind(rubisco_full,para)
}

write.csv(rubisco_full,"rubisco_full.csv")


################################################################################
#compare AIC between different models

rubisco_AIC <- data.frame(ID = rubisco_high$orginalID,
                          species = rubisco_high$species,
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

write.csv(rubisco_parameter,"rubisco_parameter.csv")
