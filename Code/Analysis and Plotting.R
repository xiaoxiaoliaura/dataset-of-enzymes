
# 1.Filter data -----------------------------------------------------------

#filter data which has the reliable T_opt and E
data <- read.csv("combined.csv")
data <- dplyr::select(data,OriginalID,ConSpecies,ConTemp,
                      StandardisedTraitName,StandardisedTraitValue,StandardisedTraitUnit,
                      Habitat,Location, ConKingdom, Citation)

##used activation energies for curves with at least three data points below both the observed and estimated T_pk
d2 <- data.frame(ID=numeric(),orginalID=character(),topt=numeric(),e=numeric(),q10=numeric(),
                 OriginalID=character(),ConSpecies=character(), StandardisedTraitName=character(),StandardisedTraitUnit=character(),
                 Habitat=character(),Location=character(),ConKingdom=character(),Citation=character())
for (i in unique(n$OriginalID)){
  temp <- subset(data,OriginalID==i)
  l <- length(temp$StandardisedTraitName)
  temp_p <- subset(n, OriginalID==i)
  t_opt <- temp_p$topt
  data_points <- dplyr::filter(temp,ConTemp<=t_opt)
  if(length(data_points$ConTemp)>=3){
    if(t_opt<temp$ConTemp[l]){d2 <- rbind(d2,temp_p)}
    
  }
}

##removed curves with E > 3 eV or E < 0 eV
d3 <- dplyr::filter(d2,e<=3)
d3 <- dplyr::filter(d3,e>0)
write.csv(d3,"new_data.csv")

length(unique(d3$ID))

###
#Add  habitat to species
##filter data without information of habitat
unique(d3$Habitat)
n1 <- subset(d3,d3$Habitat=="")
which(d3$Habitat=="")
unique(n1$ConSpecies)
for (i in 975:1024){
  if(d3$ConSpecies[i]=="Gossypium"|d3$ConSpecies[i]=="Madia.sativa"|
     d3$ConSpecies[i]=="Ipomoea batatas"|d3$ConSpecies[i]=="Spinacia oleracea"|
     d3$ConSpecies[i]=="populus trichocarpa"|d3$ConSpecies[i]=="Arabidopsis thaliana"|
     d3$ConSpecies[i]=="Gossypium hirsutum"|d3$ConSpecies[i]=="Nicotiana tabacum"|
     d3$ConSpecies[i]=="Eucalyptus pauciflor"|d3$ConSpecies[i]=="Triticum aestivum"|
     d3$ConSpecies[i]=="Quercus engelmannii"|d3$ConSpecies[i]=="Lophostemon confertus"|
     d3$ConSpecies[i]=="Ipomoea batata"|d3$ConSpecies[i]=="Dacryodes excelsa"|
     d3$ConSpecies[i]=="Nothofagus cunning"|d3$ConSpecies[i]=="Nothofagus moorei"|
     d3$ConSpecies[i]=="Eucalyptus globulus"|d3$ConSpecies[i]=="Larrea divaricata"|
     d3$ConSpecies[i]=="Plantago asiatica"|d3$ConSpecies[i]=="Rumex cyprius"|
     d3$ConSpecies[i]=="Diplotaxis harra"|d3$ConSpecies[i]=="cleome droserifolia"|
     d3$ConSpecies[i]=="neurada procumbens"|d3$ConSpecies[i]=="zygophyllum decumbens"|
     d3$ConSpecies[i]=="zila spinosa"|d3$ConSpecies[i]=="Acer saccharum"|
     d3$ConSpecies[i]=="Eucalyptus pauciflora"){
    d3$Habitat[i] <- "terrestrial"
  }
}


for (i in 975:1024){
  if(d3$ConSpecies[i]=="Glycine max"|d3$ConSpecies[i]=="Oryza sativa"){
    d3$Habitat[i] <- "freshwater"
  }
}

which(d3$Habitat=="")
unique(d3$Habitat)

d3_terra <- subset(d3, Habitat=="terrestrial")

d3_aqua <- subset(d3, Habitat=="marine"|Habitat=="freshwater"|Habitat=="aquatic")

d3_habit_both <- subset(d3, Habitat=="freshwater / terrestrial")

length(unique(d3_terra$ConSpecies))
length(unique(d3_aqua$ConSpecies))
length(unique(d3_habit_both$ConSpecies))

#separate different metabolic rate###
str(d3)
unique(d3$StandardisedTraitName)

for (i in 1:1084){
  if(d3$StandardisedTraitName[i]=="net photosynthesis"|
     d3$StandardisedTraitName[i]=="Net Light saturated photosynthesis"|
     d3$StandardisedTraitName[i]=="Surface Area-Specific Maximum Photosynthesis Rate"|
     d3$StandardisedTraitName[i]=="Mass-Specific Photosynthetic Oxygen Production Rate"|
     d3$StandardisedTraitName[i]=="Surface Area-Specific Photosynthetic Oxygen Production Rate"
  ){
    d3$Metabolic_rate[i] <- "NP"
  }
  if(d3$StandardisedTraitName[i]=="respiration rate"|
     d3$StandardisedTraitName[i]=="Surface Area-Specific Dark Respiration Rate"|
     d3$StandardisedTraitName[i]=="Mass-Specific Respiration Rate"){
    d3$Metabolic_rate[i] <- "R"
  }
  if(d3$StandardisedTraitName[i]=="gross photosynthesis"
  ){
    d3$Metabolic_rate[i] <- "P"
  }
}


#remove gross phtosynthesis
d4 <- subset(d3,d3$Metabolic_rate!="P")
table(d4$Metabolic_rate)

#Habitat and photosynthetic type

for (i in 1:1045){
  if (d4$Habitat[i]=="terrestrial"){
    d4$Habitat2[i] <- "Terrestrial"
  }
}
for (i in 1:1045){
  if (d4$Habitat[i]=="marine"|d4$Habitat[i]=="freshwater"|d4$Habitat[i]=="aquatic"){
    d4$Habitat2[i] <- "Aquatic"
  }
}

for (i in 1:1045){
  if (d4$Habitat[i]=="freshwater / terrestrial"){
    d4$Habitat2[i] <- "Both"
  }
}

write.csv(d4,"plot_data.csv")


# 2. Plotting -------------------------------------------------------------
windowsFonts(Times=windowsFont("Times New Roman"))
library("ggrepel")
library(plyr)
library(patchwork)

## Figure 1 ------------------------------------------------------------

d4 <- read.csv("plot_data.csv")
table(d3$Metabolic_rate)
d4_NP <- subset(d4,d4$Metabolic_rate=="NP")
d4_R <- subset(d4,d4$Metabolic_rate=="R")
#E####
cbPalette <- c("#009E73", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999", "#E69F00")
mu1 <- ddply(d4,"Metabolic_rate",summarise,grp.mean=mean(e),grp.median=median(e))
p1 <- ggplot(d4, aes(x=e, fill=Metabolic_rate)) +
  geom_histogram(aes(y=..density..), colour="#666666", fill="white")+
  geom_density(alpha=0.4,aes(color=Metabolic_rate))+ 
  geom_vline(data=mu1, aes(xintercept=grp.median, color=Metabolic_rate),
             linetype="dashed",size=.8)+ # Add mean lines
  labs(x="E (eV)", y = "Density") +theme_bw()+
  scale_fill_manual(values=cbPalette)+scale_colour_manual(values=cbPalette)+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        text = element_text(family = "Times",size=14))

p1 <- p1+facet_wrap(.~Metabolic_rate,scales="free")+
  theme(strip.background = element_rect(colour = "white",fill = "white"),
        axis.line = element_line(colour = "black"))
library(DescTools)
mean(d4_NP$e)
median(d4_NP$e)
MedianCI(d4_NP$e)
mean(d4_R$e)
median(d4_R$e)
MedianCI(d4_R$e)

#Topt####
mu2 <- ddply(d4,"Metabolic_rate",summarise,grp.mean=mean(topt),grp.median=median(e))
p2 <- ggplot(d4, aes(x=topt, fill=Metabolic_rate)) +
  geom_histogram(aes(y=..density..), colour="#666666", fill="white")+
  geom_density(alpha=0.4,aes(color=Metabolic_rate))+ 
  geom_vline(data=mu2, aes(xintercept=grp.mean, color=Metabolic_rate),
             linetype="dashed",size=.8)+ # Add mean lines
  labs(x="Temperature (oC)", y = "Density") +theme_bw()+
  scale_fill_manual(values=cbPalette)+scale_colour_manual(values=cbPalette)+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        text = element_text(family = "Times",size=14))


p2 <- p2+facet_wrap(.~Metabolic_rate,scales="free")+
  theme(strip.background = element_rect(colour = "white",fill = "white"),
        axis.line = element_line(colour = "black"))
mean(d4_NP$topt)
median(d4_NP$topt)
MedianCI(d4_NP$topt)
mean(d4_R$topt)
median(d4_R$topt)
MedianCI(d4_R$topt)

#Figure 1
(p1 ) /  p2+plot_annotation(tag_levels = 'A')

#analysis
summary(d4_NP$e)
length(d4_NP$e)
length(unique(d4_NP$ConSpecies))
length(unique(d4_R$ConSpecies))
summary(d4_R$e)
summary(d4_NP$topt)
summary(d4_R$topt)
sd(d4_NP$e)
sd(d4_R$e)
sd(d4_NP$topt)
sd(d4_R$topt)

#get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(d4_R$e)
getmode(d4_NP$e)
getmode(d4_NP$topt)
getmode(d4_R$topt)

mean <- mean(d4_NP$topt)
se <- sd(d4_NP$topt,na.rm = T)/sqrt(length(d4_R$topt))
mean-1.96*se
upper <- mean+1.96*se

## Figure 2 ------------------------------------------------------------
## NP####
###Kingdom
####Topt
length(unique(d4_NP$ConKingdom))
table(d4_NP$ConKingdom)

d4_NP_kindom <- subset(d4_NP,ConKingdom!="Chlorobiota")
d4_NP_kindom <- subset(d4_NP_kindom,ConKingdom!="Fungi")

table(d4_NP_kindom$ConKingdom)
d4_NP_kindom %>% group_by(ConKingdom) %>% 
  summarise(mean=mean(topt))
NP_kindom <- data.frame(kindom=character())
for (i in unique(d4_NP_kindom$ConKingdom)){
  temp <- subset(d4_NP_kindom,ConKingdom ==i)
  mean <- mean(temp$topt)
  se <- sd(temp$topt,na.rm = T)/sqrt(length(temp$topt))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$topt)
  df <- c(i,n,mean,lower,upper)
  
  NP_kindom <- rbind(NP_kindom,df)
}

col <- c("Kingdom","number","mean","lower","upper")
colnames(NP_kindom) <- col
NP_kindom[,-1] <- apply(NP_kindom[,-1], 2, as.numeric)
NP_kindom$index <- 1:4
NP_kindom$lable <- paste(NP_kindom$Kingdom," (",NP_kindom$number,")",sep="") 
F1 <- ggplot(data=NP_kindom, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(10,50)+
  scale_y_continuous(name = "", breaks=1:nrow(NP_kindom), 
                     labels= NP_kindom$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=26.4, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        text = element_text(family = "Times",size=11))

####E
NP_kindom_E <- data.frame(kindom=character())
for (i in unique(d4_NP_kindom$ConKingdom)){
  temp <- subset(d4_NP_kindom,ConKingdom ==i)
  mean <- mean(temp$e)
  median <- median(temp$e)
  se <- sd(temp$e,na.rm = T)/sqrt(length(temp$e))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$e)
  df <- c(i,n,median,mean,lower,upper)
  
  NP_kindom_E <- rbind(NP_kindom_E,df)
}
label <- c("","","","")
col <- c("Kingdom","number","median","mean","lower","upper")
colnames(NP_kindom_E) <- col
NP_kindom_E[,-1] <- apply(NP_kindom_E[,-1], 2, as.numeric)
NP_kindom_E$index <- 1:4
NP_kindom_E$lable <- paste(NP_kindom_E$Kingdom," (",NP_kindom_E$number,")",sep="") 
F1_E <- ggplot(data=NP_kindom_E, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.2)+
  xlim(0,1.5)+
  scale_y_continuous(name = "", breaks=1:nrow(NP_kindom_E), 
                     labels= NP_kindom_E$lable)+
  labs(x=element_blank(),y=element_blank()) +
  geom_vline(xintercept=0.32, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(family = "Times",size=11))
F1_E 

###Phylum
####Topt
length(unique(d4_NP$ConPhylum))
table(d4_NP$ConPhylum)

d4_NP_phylum <- subset(d4_NP,ConPhylum!="Ascomycota")
d4_NP_phylum <- subset(d4_NP_phylum,ConPhylum!="Bryophyta")
d4_NP_phylum <- subset(d4_NP_phylum,ConPhylum!="Dinoflagellata")
d4_NP_phylum <- subset(d4_NP_phylum,ConPhylum!="Heterokontophyta")
d4_NP_phylum <- subset(d4_NP_phylum,ConPhylum!="Magnoliophyta")
d4_NP_phylum <- subset(d4_NP_phylum,ConPhylum!="Proteobacteria")
d4_NP_phylum <- subset(d4_NP_phylum,ConPhylum!="Zygnemophyta")

length(unique(d4_NP_phylum$ConPhylum))
table(d4_NP_phylum$ConPhylum)

d4_NP_phylum %>% group_by(ConPhylum) %>% 
  summarise(mean=mean(topt))
NP_phylum <- data.frame(phylum=character())
for (i in unique(d4_NP_phylum$ConPhylum)){
  temp <- subset(d4_NP_phylum,ConPhylum ==i)
  mean <- mean(temp$topt)
  se <- sd(temp$topt,na.rm = T)/sqrt(length(temp$topt))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$topt)
  df <- c(i,n,mean,lower,upper)
  
  NP_phylum <- rbind(NP_phylum,df)
}

col <- c("Phylum","number","mean","lower","upper")
colnames(NP_phylum) <- col
NP_phylum[,-1] <- apply(NP_phylum[,-1], 2, as.numeric)
NP_phylum$index <- 1:11
NP_phylum$lable <- paste(NP_phylum$Phylum," (",NP_phylum$number,")",sep="")

F2 <- ggplot(data=NP_phylum, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(10,50)+
  scale_y_continuous(name = "", breaks=1:nrow(NP_phylum), 
                     labels= NP_phylum$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=26.4, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

####E
NP_phylum_E <- data.frame(phylum=character())
for (i in unique(d4_NP_phylum$ConPhylum)){
  temp <- subset(d4_NP_phylum,ConPhylum ==i)
  mean <- mean(temp$e)
  se <- sd(temp$e,na.rm = T)/sqrt(length(temp$e))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$e)
  df <- c(i,n,mean,lower,upper)
  
  NP_phylum_E <- rbind(NP_phylum_E,df)
}

col <- c("Phylum","number","mean","lower","upper")
colnames(NP_phylum_E) <- col
NP_phylum_E[,-1] <- apply(NP_phylum_E[,-1], 2, as.numeric)
NP_phylum_E$index <- 1:11
NP_phylum_E$lable <- paste(NP_phylum_E$Phylum," (",NP_phylum_E$number,")",sep="")

F2_E <- ggplot(data=NP_phylum_E, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(0,1.5)+
  scale_y_continuous(name = "", breaks=1:nrow(NP_phylum_E), 
                     labels= NP_phylum_E$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=0.32, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank()
        )

###Class
####Topt

length(unique(d4_NP$ConClass))
table(d4_NP$ConClass)

d4_NP_Class <- subset(d4_NP,ConClass!="Bangiophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Chlorophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Coscinodiscophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Dinophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Fragilariophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Gammaproteobacteria")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Lecanoromycetes")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Pavlovophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Phaeophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Prymnesiophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Sphagnopsida")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Trebouxiophyceae")
d4_NP_Class <- subset(d4_NP_Class,ConClass!="Zygnemophyceae")


table(d4_NP_Class$ConClass)
d4_NP_Class %>% group_by(ConClass) %>% 
  summarise(mean=mean(topt))
NP_Class <- data.frame(Class=character())
for (i in unique(d4_NP_Class$ConClass)){
  temp <- subset(d4_NP_Class,ConClass ==i)
  mean <- mean(temp$topt)
  se <- sd(temp$topt,na.rm = T)/sqrt(length(temp$topt))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$topt)
  df <- c(i,n,mean,lower,upper)
  
  NP_Class <- rbind(NP_Class,df)
}

col <- c("Class","number","mean","lower","upper")
colnames(NP_Class) <- col
NP_Class[,-1] <- apply(NP_Class[,-1], 2, as.numeric)
NP_Class$index <- 1:6
NP_Class$lable <- paste(NP_Class$Class," (",NP_Class$number,")",sep="") 
F3 <- ggplot(data=NP_Class, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(10,50)+
  scale_y_continuous(name = "", breaks=1:nrow(NP_Class), 
                     labels= NP_Class$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=26.4, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

####E
NP_Class_E <- data.frame(Class=character())
for (i in unique(d4_NP_Class$ConClass)){
  temp <- subset(d4_NP_Class,ConClass ==i)
  mean <- mean(temp$e)
  se <- sd(temp$e,na.rm = T)/sqrt(length(temp$e))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$e)
  df <- c(i,n,mean,lower,upper)
  
  NP_Class_E <- rbind(NP_Class_E,df)
}

col <- c("Class","number","mean","lower","upper")
colnames(NP_Class_E) <- col
NP_Class_E[,-1] <- apply(NP_Class_E[,-1], 2, as.numeric)
NP_Class_E$index <- 1:6
NP_Class_E$lable <- paste(NP_Class_E$Class," (",NP_Class_E$number,")",sep="") 
F3_E <- ggplot(data=NP_Class_E, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(0,1.5)+
  scale_y_continuous(name = "", breaks=1:nrow(NP_Class), 
                     labels= NP_Class$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=0.32, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

###Order
####Topt

length(unique(d4_NP$ConOrder))
table(d4_NP$ConOrder)

d4_NP_Order <- subset(d4_NP,ConOrder=="Alismatales"|ConOrder=="Asparagales"|
                        ConOrder=="Asterales"|ConOrder=="Alismatales"|
                        ConOrder=="Capparales"|ConOrder=="Caryophyllales"|
                        ConOrder=="Chroococcales"|ConOrder=="Cladophorales"|
                        ConOrder=="Ericales"|ConOrder=="Fabales"|
                        ConOrder=="Fagales"|ConOrder=="Gentianales"|
                        ConOrder=="Gigartinales"|ConOrder=="Lamiales"|
                        ConOrder=="Laminariales"|ConOrder=="Malvales"|
                        ConOrder=="Myrtales"|ConOrder=="Naviculales"|
                        ConOrder=="Pinales"|ConOrder=="Poales"|
                        ConOrder=="Rosales"|ConOrder=="Sapindales"|
                        ConOrder=="Solanales"|ConOrder=="Zygophyllales")


table(d4_NP_Order$ConOrder)
d4_NP_Order %>% group_by(ConOrder) %>% 
  summarise(mean=mean(topt))
NP_Order <- data.frame(Order=character())
for (i in unique(d4_NP_Order$ConOrder)){
  temp <- subset(d4_NP_Order,ConOrder ==i)
  mean <- mean(temp$topt)
  se <- sd(temp$topt,na.rm = T)/sqrt(length(temp$topt))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$topt)
  df <- c(i,n,mean,lower,upper)
  
  NP_Order <- rbind(NP_Order,df)
}

col <- c("Order","number","mean","lower","upper")
colnames(NP_Order) <- col
NP_Order[,-1] <- apply(NP_Order[,-1], 2, as.numeric)
NP_Order$index <- 1:23
NP_Order$lable <- paste(NP_Order$Order," (",NP_Order$number,")",sep="") 
F4 <- ggplot(data=NP_Order, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(10,50)+
  scale_y_continuous(name = "", breaks=1:nrow(NP_Order), 
                     labels= NP_Order$lable)+
  labs(x='Temperature (oC)')+
  geom_vline(xintercept=26.4, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())

####E

NP_Order_E <- data.frame(Order=character())
for (i in unique(d4_NP_Order$ConOrder)){
  temp <- subset(d4_NP_Order,ConOrder ==i)
  mean <- mean(temp$e)
  se <- sd(temp$e,na.rm = T)/sqrt(length(temp$e))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$e)
  df <- c(i,n,mean,lower,upper)
  
  NP_Order_E <- rbind(NP_Order_E,df)
}

col <- c("Order","number","mean","lower","upper")
colnames(NP_Order_E) <- col
NP_Order_E[,-1] <- apply(NP_Order_E[,-1], 2, as.numeric)
NP_Order_E$index <- 1:23
NP_Order_E$lable <- paste(NP_Order_E$Order," (",NP_Order_E$number,")",sep="") 
F4_E <- ggplot(data=NP_Order_E, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(0,1.5)+
  scale_y_continuous(name = "", breaks=1:nrow(NP_Order_E), 
                     labels= NP_Order_E$lable)+
  labs(x='E (eV)')+
  geom_vline(xintercept=0.32, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank())


(F1_E+F1)/
  (F2_E+F2)/
  (F3_E+F3)/
  (F4_E+F4)+plot_layout(widths = c(5,4),heights=c(4,11,6,23))


##R

###Kingdom
####Topt
length(unique(d4_R$ConKingdom))
table(d4_R$ConKingdom)

d4_R_kindom <- subset(d4_R,ConKingdom=="Fungi"|ConKingdom=="Plantae"|ConKingdom=="Protista")

table(d4_R_kindom$ConKingdom)

d4_R_kindom %>% group_by(ConKingdom) %>% 
  summarise(mean=mean(topt))
R_kindom <- data.frame(kindom=character())
for (i in unique(d4_R_kindom$ConKingdom)){
  temp <- subset(d4_R_kindom,ConKingdom ==i)
  mean <- mean(temp$topt)
  se <- sd(temp$topt,na.rm = T)/sqrt(length(temp$topt))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$topt)
  df <- c(i,n,mean,lower,upper)
  
  R_kindom <- rbind(R_kindom,df)
}

col <- c("Kingdom","number","mean","lower","upper")
colnames(R_kindom) <- col
R_kindom[,-1] <- apply(R_kindom[,-1], 2, as.numeric)
R_kindom$index <- 1:3
R_kindom$lable <- paste(R_kindom$Kingdom," (",R_kindom$number,")",sep="") 
R1 <- ggplot(data=R_kindom, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(10,70)+
  scale_y_continuous(name = "", breaks=1:nrow(R_kindom), 
                     labels= R_kindom$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=48.53, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(family = "Times",size=11),
        axis.text.y=element_blank())

####E
R_kindom_E <- data.frame(kindom=character())
for (i in unique(d4_R_kindom$ConKingdom)){
  temp <- subset(d4_R_kindom,ConKingdom ==i)
  mean <- mean(temp$e)
  se <- sd(temp$e,na.rm = T)/sqrt(length(temp$e))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$e)
  df <- c(i,n,mean,lower,upper)
  
  R_kindom_E <- rbind(R_kindom_E,df)
}

col <- c("Kingdom","number","mean","lower","upper")
colnames(R_kindom_E) <- col
R_kindom_E[,-1] <- apply(R_kindom_E[,-1], 2, as.numeric)
R_kindom_E$index <- 1:3
R_kindom_E$lable <- paste(R_kindom_E$Kingdom," (",R_kindom_E$number,")",sep="") 
R1_E <- ggplot(data=R_kindom_E, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(0,1.5)+
  scale_y_continuous(name = "", breaks=1:nrow(R_kindom_E), 
                     labels= R_kindom_E$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=0.65, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(family = "Times",size=11))
R1_E 


###Phylum
####Topt
length(unique(d4_R$ConPhylum))
table(d4_R$ConPhylum)

d4_R_phylum <- subset(d4_R,ConPhylum!="Arthropoda")
d4_R_phylum <- subset(d4_R_phylum,ConPhylum!="Cnidaria")
d4_R_phylum <- subset(d4_R_phylum,ConPhylum!="Cyanobacteria")
d4_R_phylum <- subset(d4_R_phylum,ConPhylum!="Dinoflagellata")
d4_R_phylum <- subset(d4_R_phylum,ConPhylum!="Heterokontophyta")
d4_R_phylum <- subset(d4_R_phylum,ConPhylum!="Zygnemophyta")
d4_R_phylum <- subset(d4_R_phylum,ConPhylum!="Zygnemophyta")
length(unique(d4_NP_phylum$ConPhylum))
table(d4_NP_phylum$ConPhylum)

d4_R_phylum %>% group_by(ConPhylum) %>% 
  summarise(mean=mean(topt))

R_phylum <- data.frame(phylum=character())
for (i in unique(d4_R_phylum$ConPhylum)){
  temp <- subset(d4_R_phylum,ConPhylum ==i)
  mean <- mean(temp$topt)
  se <- sd(temp$topt,na.rm = T)/sqrt(length(temp$topt))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$topt)
  df <- c(i,n,mean,lower,upper)
  
  R_phylum <- rbind(R_phylum,df)
}

col <- c("Phylum","number","mean","lower","upper")
colnames(R_phylum) <- col
R_phylum[,-1] <- apply(R_phylum[,-1], 2, as.numeric)
R_phylum$index <- 1:10
R_phylum$lable <- paste(R_phylum$Phylum," (",R_phylum$number,")",sep="")

R2 <- ggplot(data=R_phylum, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(10,70)+
  scale_y_continuous(name = "", breaks=1:nrow(R_phylum), 
                     labels= R_phylum$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=48.53, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

####E
R_phylum_E <- data.frame(phylum=character())
for (i in unique(d4_R_phylum$ConPhylum)){
  temp <- subset(d4_R_phylum,ConPhylum ==i)
  mean <- mean(temp$e)
  se <- sd(temp$e,na.rm = T)/sqrt(length(temp$e))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$e)
  df <- c(i,n,mean,lower,upper)
  
  R_phylum_E <- rbind(R_phylum_E,df)
}

col <- c("Phylum","number","mean","lower","upper")
colnames(R_phylum_E) <- col
R_phylum_E[,-1] <- apply(R_phylum_E[,-1], 2, as.numeric)
R_phylum_E$index <- 1:10
R_phylum_E$lable <- paste(R_phylum_E$Phylum," (",R_phylum_E$number,")",sep="")

R2_E <- ggplot(data=R_phylum_E, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(0,1.5)+
  scale_y_continuous(name = "", breaks=1:nrow(R_phylum_E), 
                     labels= R_phylum_E$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=0.65, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank())


###Class
####Topt
length(unique(d4_R$ConClass))
table(d4_R$ConClass)

d4_R_Class <- subset(d4_R,ConClass=="Florideophyceae"|
                       ConClass=="Lecanoromycetes"|
                       ConClass=="Liliopsida"|
                       ConClass=="Magnoliopsida"|
                       ConClass=="Pinopsida"|
                       ConClass=="Ulvophyceae")


table(d4_R_Class$ConClass)
d4_R_Class %>% group_by(ConClass) %>% 
  summarise(mean=mean(topt))
R_Class <- data.frame(Class=character())
for (i in unique(d4_R_Class$ConClass)){
  temp <- subset(d4_R_Class,ConClass ==i)
  mean <- mean(temp$topt)
  se <- sd(temp$topt,na.rm = T)/sqrt(length(temp$topt))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$topt)
  df <- c(i,n,mean,lower,upper)
  
  R_Class <- rbind(R_Class,df)
}

col <- c("Class","number","mean","lower","upper")
colnames(R_Class) <- col
R_Class[,-1] <- apply(R_Class[,-1], 2, as.numeric)
R_Class$index <- 1:6
R_Class$lable <- paste(R_Class$Class," (",R_Class$number,")",sep="") 
R3 <- ggplot(data=R_Class, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(10,70)+
  scale_y_continuous(name = "", breaks=1:nrow(R_Class), 
                     labels= R_Class$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=48.53, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

####E
R_Class_E <- data.frame(Class=character())
for (i in unique(d4_R_Class$ConClass)){
  temp <- subset(d4_R_Class,ConClass ==i)
  mean <- mean(temp$e)
  se <- sd(temp$e,na.rm = T)/sqrt(length(temp$e))
  lower <- mean-1.96*se
  upper <- mean+1.96*se
  n <- length(temp$e)
  df <- c(i,n,mean,lower,upper)
  
  R_Class_E <- rbind(R_Class_E,df)
}

col <- c("Class","number","mean","lower","upper")
colnames(R_Class_E) <- col
R_Class_E[,-1] <- apply(R_Class_E[,-1], 2, as.numeric)
R_Class_E$index <- 1:6
R_Class_E$lable <- paste(R_Class_E$Class," (",R_Class_E$number,")",sep="") 
R3_E <- ggplot(data=R_Class_E, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(0,1.5)+
  scale_y_continuous(name = "", breaks=1:nrow(R_Class), 
                     labels= R_Class$lable)+
  labs(x=element_blank()) +
  geom_vline(xintercept=0.65, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank())


###Order
####Topt
which(d4_R$ConOrder=="\tFabales")
d4_R$ConOrder[157] <- "Fabales"
d4_R$ConOrder[158] <- "Fabales"
d4_R$ConOrder[159] <- "Fabales"
d4_R$ConOrder[160] <- "Fabales"
which(d4_R$ConOrder=="Sapindales?")
d4_R$ConOrder[96] <- "Sapindales"
d4_R$ConOrder[97] <- "Sapindales"
d4_R$ConOrder[98] <- "Sapindales"
d4_R$ConOrder[99] <- "Sapindales"

length(unique(d4_R$ConOrder))
table(d4_R$ConOrder)

d4_R_Order <- d4_R
R_Order <- data.frame(Order=character())
for (i in unique(d4_R_Order$ConOrder)){
  temp <- subset(d4_R_Order,ConOrder ==i)
  n <- length(temp$topt)
  if(n>=5){
    mean <- mean(temp$topt)
    se <- sd(temp$topt,na.rm = T)/sqrt(length(temp$topt))
    lower <- mean-1.96*se
    upper <- mean+1.96*se
    df <- c(i,n,mean,lower,upper)
    
    R_Order <- rbind(R_Order,df)}
  
}

table(R_Order$X.Pinales.)

col <- c("Order","number","mean","lower","upper")
colnames(R_Order) <- col
R_Order[,-1] <- apply(R_Order[,-1], 2, as.numeric)
R_Order$index <- 1:22
R_Order$lable <- paste(R_Order$Order," (",R_Order$number,")",sep="") 
R4 <- ggplot(data=R_Order, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(10,70)+
  scale_y_continuous(name = "", breaks=1:nrow(R_Order), 
                     labels= R_Order$lable)+
  labs(x='Temperature (oC)')+
  geom_vline(xintercept=48.53, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())

####E

R_Order_E <- data.frame(Order=character())
for (i in unique(d4_R_Order$ConOrder)){
  temp <- subset(d4_R_Order,ConOrder ==i)
  n <- length(temp$e)
  if(n>=5){
    mean <- mean(temp$e)
    se <- sd(temp$e,na.rm = T)/sqrt(length(temp$e))
    lower <- mean-1.96*se
    upper <- mean+1.96*se
    df <- c(i,n,mean,lower,upper)
    
    R_Order_E <- rbind(R_Order_E,df)}
  
}

col <- c("Order","number","mean","lower","upper")
colnames(R_Order_E) <- col
R_Order_E[,-1] <- apply(R_Order_E[,-1], 2, as.numeric)
R_Order_E$index <- 1:22
R_Order_E$lable <- paste(R_Order_E$Order," (",R_Order_E$number,")",sep="") 
R4_E <- ggplot(data=R_Order_E, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  xlim(0,1.5)+
  scale_y_continuous(name = "", breaks=1:nrow(R_Order_E), 
                     labels= R_Order_E$lable)+
  labs(x='E (eV)')+
  geom_vline(xintercept=0.65, color='black', linetype='dashed', alpha=.5) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        axis.ticks.y=element_blank())


###Plot###
(R1_E+R1)/
  (R2_E+R2)/
  (R3_E+R3)/
  (R4_E+R4)+plot_layout(widths = c(5,4),heights=c(3,10,6,22))

#analysis####
one.way <- aov(e~ConKingdom, data=d4_NP)
summary(one.way )
one.way <- aov(e~ConPhylum, data=d4_NP)
summary(one.way )
one.way <- aov(e~ConOrder, data=d4_NP)
summary(one.way )
one.way <- aov(e~ConClass, data=d4_NP)
summary(one.way )

one.way <- aov(e~ConKingdom, data=d4_R)
summary(one.way )
one.way <- aov(e~ConPhylum, data=d4_R)
summary(one.way )
one.way <- aov(e~ConOrder, data=d4_R)
summary(one.way )
one.way <- aov(e~ConClass, data=d4_R)
summary(one.way )

one.way <- aov(topt~ConKingdom, data=d4_NP)
summary(one.way )
one.way <- aov(topt~ConPhylum, data=d4_NP)
summary(one.way )
one.way <- aov(topt~ConOrder, data=d4_NP)
summary(one.way )
one.way <- aov(topt~ConClass, data=d4_NP)
summary(one.way )

one.way <- aov(topt~ConKingdom, data=d4_R)
summary(one.way )
one.way <- aov(topt~ConPhylum, data=d4_R)
summary(one.way )
one.way <- aov(topt~ConOrder, data=d4_R)
summary(one.way )
one.way <- aov(topt~ConClass, data=d4_R)
summary(one.way )
## Figure 3 ----------------------------------------------------------------

#Habitat

Palette <- c("#56B4E9","#F0E442")

d4_ha <- subset(d4,Habitat!="Both")
p7 <- ggplot(d4_ha,aes(x=Metabolic_rate, y=topt,color=Habitat,fill=Habitat))+
  geom_boxplot(alpha=0.4)+
  theme_classic()+
  labs(x=element_blank(), y = expression("T"["opt"]*" (oC)"))+
  theme(text = element_text(family = "Times",size=11),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=11))+
  scale_fill_manual(values=Palette)+scale_colour_manual(values=Palette)
p7
table(d4$Habitat)

p8 <- ggplot(d4_ha,aes(x=Metabolic_rate, y=e,color=Habitat,fill=Habitat))+
  geom_boxplot(alpha=0.4)+
  theme_classic()+
  labs(x=element_blank(), y = "E(eV)")+
  theme(text = element_text(family = "Times",size=11),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=11))+
  scale_fill_manual(values=Palette)+scale_colour_manual(values=Palette)
p8

p7+p8

#Photosynthetic pathway
d4_PT <- subset(d4,Habitat=="Terrestrial")
d4_PT <- subset(d4_PT,PT!="")

cbPalette <- c("#009E73", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", 
               "#CC79A7","#999999", "#E69F00")
Pal <- c("#009E73", "#D55E00", 
         "#CC79A7")
p9 <- ggplot(d4_PT,aes(x=Metabolic_rate, y=topt,color=PT,fill=PT))+
  geom_boxplot(alpha=0.2)+
  theme_classic()+
  labs(x=element_blank(), y = expression("T"["opt"]*" (oC)"))+
  theme(text = element_text(family = "Times",size=11),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=11))+
  scale_fill_manual(values=Pal)+scale_colour_manual(values=Pal)
p9
table(d4_PT$PT)
p10 <- ggplot(d4_PT,aes(x=Metabolic_rate, y=e,color=PT,fill=PT))+
  geom_boxplot(alpha=0.2)+
  theme_classic()+
  labs(x=element_blank(), y = "E(eV)")+
  theme(text = element_text(family = "Times",size=11),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=11))+
  scale_fill_manual(values=Pal)+scale_colour_manual(values=Pal)
p10


(p8 | p7 ) /
  ( p10| p9)+plot_annotation(tag_levels = 'A')


# Figure 4 ----------------------------------------------------------------

#T_opt distrbution
col <- c("Species","number_R","R","e_R","number_NP","NP","e_NP","Habitat","PT")
d5_1 <- d5
colnames(d5_1) <- col
d5_longer <- pivot_longer(d5_1,names_to = "Metabolism",values_to = "T_opt",
                          cols = c(NP,R))

mu6_1 <- ddply(d5_longer,"Metabolism",summarise,grp.mean=mean(T_opt))
p6 <- ggplot(d5_longer, aes(x=T_opt, fill=Metabolism)) +
  geom_density(alpha=0.4,aes(color=Metabolism))+  
  geom_vline(data=mu6_1, aes(xintercept=grp.mean, color=Metabolism),
             linetype="dashed",size=.8)+ # Add mean lines
  labs(x="Temperature (oC)", y = "Density") +theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = c(0.9,0.9),
        text = element_text(family = "Times"))+
  theme(strip.background = element_rect(colour = "white",fill = "white"),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=cbPalette)+scale_colour_manual(values=cbPalette)

#E distribution
col <- c("Species","number_R","T_R","R","number_NP","T_NP","NP","Habitat","PT")
d5_1 <- d5
colnames(d5_1) <- col
d5_longer <- pivot_longer(d5_1,names_to = "Metabolism",values_to = "E",
                          cols = c(NP,R))


mu6_2 <- ddply(d5_longer,"Metabolism",summarise,grp.mean=mean(E),grp.median=median(E))
p6_2 <- ggplot(d5_longer, aes(x=E, fill=Metabolism)) +
  geom_density(alpha=0.4,aes(color=Metabolism))+  
  geom_vline(data=mu6_2, aes(xintercept=grp.median, color=Metabolism),
             linetype="dashed",size=.8)+ # Add mean lines
  labs(x="E (eV)", y = "Density") +theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        text = element_text(family = "Times"))+
  scale_fill_manual(values=cbPalette)+scale_colour_manual(values=cbPalette)+
  theme(strip.background = element_rect(colour = "white",fill = "white"),
        axis.line = element_line(colour = "black"))

#claculate average in species

d5_NP <- data.frame(ID=numeric())
for (i in unique(d4_NP$ConSpecies)){
  s <- subset(d4_NP,ConSpecies== i)
  n <- length(s$OriginalID)
  topt_NP <- mean(s$topt)
  e_NP <- mean(s$e)
  r <- c(i,n,topt_NP,e_NP)
  d5_NP <- rbind(d5_NP,r)
  
}

n <- c("Species","number","topt","e")
colnames(d5_NP) <- n

d5_R <- data.frame(ID=numeric())
for (i in unique(d4_R$ConSpecies)){
  s <- subset(d4_R,ConSpecies== i)
  n <- length(s$OriginalID)
  topt_R <- mean(s$topt)
  e_R <- mean(s$e)
  r <- c(i,n,topt_R,e_R)
  d5_R <- rbind(d5_R,r)
  
}

n <- c("Species","number","topt","e")
colnames(d5_R) <- n


d5 <- merge(d5_R,d5_NP,by="Species") #x is R, y is NP
d5 <- d5[-45,]

n <- c("Species","number_R","topt_R","e_R","number_NP","topt_NP","e_NP")
colnames(d5) <- n

##Topt
str(d5)
d5[,-1] <- apply(d5[,-1], 2, as.numeric)

habitat <- select(d4,ConSpecies,Habitat)
habitat <- unique(habitat)
cols <- c("Species","Habitat")
colnames(habitat) <- cols
d5 <- merge(d5,habitat,by="Species")

PT <- select(d4,ConSpecies,PT)
PT <- unique(PT)
cols <- c("Species","PT")
colnames(PT) <- cols
d5 <- merge(d5,PT,by="Species")

pal3 <- c("#56B4E9","#F0E442")
p3 <- ggplot(d5, aes(x=topt_R, y=topt_NP,color=Habitat)) + 
  geom_point(size=2,alpha=0.6)+
  geom_abline(intercept = 0,slope = 1,linetype="dashed",size=0.2)+
  xlim(10,60)+ylim(10,60)+
  labs(x=expression("T"["opt,R"]*" (oC)"), y = expression("T"["opt,NP"]*" (oC)"))+
  theme_classic()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        legend.title = element_blank(),
        legend.position = c(0.2,0.9))+
  scale_fill_manual(values=pal3)+scale_colour_manual(values=pal3)

p3

p3E <- ggplot(d5, aes(x=e_R, y=e_NP,color=Habitat)) + 
  geom_point(size=2,alpha=0.6)+
  geom_abline(intercept = 0,slope = 1,linetype="dashed",size=0.2)+
  xlim(0,2)+ylim(0,2)+
  labs(x=expression("E"["R"]*" (eV)"), y = expression("E"["NP"]*" (eV)"))+
  theme_classic()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11),
        legend.title = element_blank(),
        legend.position = "none")+
  scale_fill_manual(values=pal3)+scale_colour_manual(values=pal3)

p3E


(p6_2|p6)/(p3E|p3)+plot_annotation(tag_levels = 'A')






# Figure 5 ----------------------------------------------------------------

rubisco <- read.csv("rubisco_parameter_new.csv")


#Distribution

pr1 <- ggplot(rubisco,aes(topt))+
  geom_histogram(aes(y=..density..),bins=30, colour="#666666", fill="white")+
  geom_density(alpha=0.4,color="#56B4E9",fill="#56B4E9")+ 
  geom_vline(aes(xintercept=mean(topt)),linetype="dashed",size=.6)+
  labs(x="Temperature (oC)", y = "Density")+
  theme_classic()+
  theme(text = element_text(family = "Times",size=11))


pr2 <- ggplot(rubisco,aes(e))+
  geom_histogram(aes(y=..density..),bins=30, colour="#666666", fill="white")+
  geom_density(alpha=0.4,color="#CC79A7",fill="#CC79A7")+ 
  geom_vline(aes(xintercept=0.49),linetype="dashed",size=.6)+
  labs(x="E (eV)", y = "Density")+
  theme_classic()+
  theme(text = element_text(family = "Times",size=11))


mean(rubisco$e,na.rm=T)
mean(rubisco$topt,na.rm=T)

#Paired

d5_NP <- data.frame(ID=numeric())
for (i in unique(d4_NP$ConSpecies)){
  s <- subset(d4_NP,ConSpecies== i)
  n <- length(s$OriginalID)
  topt_NP <- mean(s$topt)
  e_NP <- mean(s$e)
  r <- c(i,n,topt_NP,e_NP)
  d5_NP <- rbind(d5_NP,r)
  
}

n <- c("Species","number","topt","e")
colnames(d5_NP) <- n

ru <- data.frame(ID=numeric())
for (i in unique(rubisco$Species)){
  s <- subset(rubisco,Species== i)
  n <- length(s$ID)
  topt_NP <- mean(s$topt)
  e_NP <- mean(s$e)
  r <- c(i,n,topt_NP,e_NP)
  ru <- rbind(ru,r)
  
}
n <- c("Species","number","topt","e")
colnames(ru) <- n

ru.p <- merge(d5_NP,ru,by="Species") 

n <- c("Species","number_NP","topt_NP","e_NP","number_Ru","topt_Ru","e_Ru")
colnames(ru.p) <- n

habitat <- select(d4,ConSpecies,Habitat)
habitat <- unique(habitat)
cols <- c("Species","Habitat")
colnames(habitat) <- cols
ru.p <- merge(ru.p,habitat,by="Species")

PT <- select(d4,ConSpecies,PT)
PT <- unique(PT)
cols <- c("Species","PT")
colnames(PT) <- cols
ru.p <- merge(ru.p,PT,by="Species")

#plot
str(ru.p)
ru.p[,c(-1,-8,-9)] <- apply(ru.p[,c(-1,-8,-9)], 2, as.numeric)

pr3 <- ggplot(ru.p, aes(x=topt_Ru, y=topt_NP)) + 
  geom_point(size=2.5,shape=21,color="darkblue",fill="lightblue")+
  geom_smooth(method=lm,linetype="dashed",
              color="red")+
  labs(x=expression("T"["opt,Rubisco"]*" (oC)"), y = expression("T"["opt,NP"]*" (oC)"))+
  theme_classic()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11))

pr3 <- ggplot(ru.p, aes(x=topt_Ru, y=topt_NP)) + 
  geom_point()+
  geom_smooth(method=lm,color="black")+
  labs(x=expression("T"["opt,Rubisco"]*" (oC)"), y = expression("T"["opt,NP"]*" (oC)"))+
  theme_classic()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11))

ru.E <- read.csv("ru.p.1csv.csv")
pr4 <- ggplot(ru.E, aes(x=e_Ru, y=e_NP)) + 
  geom_point()+
  geom_smooth(method=lm,color="black")+
  labs(x=expression("E"["Rubisco"]*" (eV)"), y = expression("E"["NP"]*" (eV)"))+
  theme_classic()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        text = element_text(family = "Times",size=11))

regre <- lm(topt_NP~topt_Ru,data = ru.p)
summary(regre)
regre1 <- lm(e_NP~e_Ru,data = ru.E)
summary(regre1)


(pr2+pr1)/
  (pr4+pr3)+plot_annotation(tag_levels = 'A')

