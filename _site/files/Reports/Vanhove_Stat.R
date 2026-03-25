install.packages("grid.arrange")
install.packages("gridExtra")
install.packages()

library(gridExtra)
library(grid.arrange)
library(readxl)
library(vegan)
library(reshape2)
library(ggplot2)
library(ade4)
library(gclus)        
library(cluster)
library(RColorBrewer)
library(lubridate)
library(gdata)
library(ggplot2)
require(reshape2)
require(plyr)
library(lubridate)
library(gdata)
library(ggplot2)
require(reshape2)
require(plyr)

source("../Functions/panelutils.R")
source("../Functions/coldiss.R")
source("../Functions/evplot.R")
source("../Functions/hcoplot.R")
source("../Functions/Answers.R")

source("../Functions/panelutils.R")
source("../Functions/coldiss.R")
source("../Functions/evplot.R")
source("../Functions/hcoplot.R")

Especes <- read_excel("~/Desktop/master oceano/M2/Maths/Marilaure/Exam janvier/Especes.xlsx")
View(Especes)
Especes <- Especes[-c(2, 3, 25, 26),]
FullData <- read_excel("~/Desktop/master oceano/M2/Maths/Marilaure/Exam janvier/FullData.xlsx")
View(FullData)
FullData <- FullData[-1, ]

data_spc <- read.table("~/Desktop/master oceano/M2/Maths/Marilaure/Exam janvier/BiomassDominantSpecies.txt", header=TRUE, sep="\t")
data_env <- read.table("~/Desktop/master oceano/M2/Maths/Marilaure/Exam janvier/EnvData.txt", header=TRUE, sep="\t")
data_station <- read.table("~/Desktop/master oceano/M2/Maths/Marilaure/Exam janvier/Stations.txt", header=TRUE, sep="\t")
exclude_cols <- c("Depth", "latitude", "longitude", "Station")

# Convert all columns to numeric except depth, lat, long, and station
FullData[] <- lapply(FullData, function(x) {
  if (!any(names(x) %in% exclude_cols)) {
    as.numeric(x)
  } else {
    x
  }
})

exclude_cols <- c("Depth", "latitude", "longitude", "Station")

# Convert all columns to numeric except depth, lat, long, and station
Especes[] <- lapply(Especes, function(x) {
  if (!any(names(x) %in% exclude_cols)) {
    as.numeric(x)
  } else {
    x
  }
})

########
##1) Analyze the normality of the distribution of the descriptors and propose appropriate transformations if the variables are not normally distributed; /2
## Test of descriptor normality: the histogram obtained shows an approximately normal distribution
########

##Porosity##
hist(data_env$Porosity, main="Histogramme de la Porosité", xlab="Porosité", freq=FALSE, col="lightblue")
abline(v = c(mean(data_env$Porosity),median(data_env$Porosity)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

#sum((FullData$Porosity-mean(FullData$Porosity))^3)/((length(FullData$Porosity)-1)*sd(FullData$Porosity)^3)
#skewness(FullData$Porosity)
#Asymétrie  de -0.3 faible 
#sum((FullData$Porosity-mean(FullData$Porosity))^4)/((length(FullData$Porosity)-1)*sd(FullData$Porosity)^4)
#kurtosis(FullData$Porosity)
#Applatissement = 2.3 donc proche d'une distribution normale
shapiro.test(data_env$Porosity) #0.6005 --> Pas de transformation

##GRAIN##
hist(data_env$Grain, main = "Histogramme du Grain", xlab= "Grain", freq = FALSE, col = "lightblue")
abline(v=c(mean(data_env$Grain), median(data_env$Grain)), col= c("green", "blue"), lty=c(2,2), lwd=c(3,3))

#sum((data_env$Grain-mean(data_env$Grain))^3)/((length(data_env$Grain)-1)*sd(data_env$Grain)^3)
#skewness(data_env$Grain)
#Forte Asymétrie Grain de 1.84 à droite
#sum((data_env$Grain-mean(data_env$Grain))^4)/((length(data_env$Grain)-1)*sd(data_env$Grain)^4)
#kurtosis(data_env$Grain)
# Kurtosis= 4.8 : distribution leptokurtique avec des queues plus épaisses
#Transformation avec log
shapiro.test(data_env$Grain) #4.802e-06


# Test normality on the transformed data
resultats_normalite <- lapply("Grain", 
                              function(v) {
  st.raw <- shapiro.test(data_env[[v]])
  st.log <- shapiro.test(log1p(data_env[[v]]))
  st.sqrt <- shapiro.test(sqrt(data_env[[v]]))
  st.stand <- shapiro.test(scale(data_env[[v]]))
  st.stand.sqrt <- shapiro.test(scale(sqrt(data_env[[v]])))
  st.inverse <- shapiro.test(1 / data_env[[v]])
  st.inverse.sqrt <- shapiro.test(sqrt(1 / data_env[[v]]))
 
   return(data.frame(variable = v,
                    p.raw = st.raw$p.value,
                    p.log = st.log$p.value,
                    p.sqrt = st.sqrt$p.value,
                    p.stand = st.stand$p.value,
                    p.stand.sqrt = st.stand.sqrt$p.value,
                    p.inverse = st.inverse$p.value,
                    p.inverse.sqrt = st.inverse.sqrt$p.value))
})

# Create a dataframe
resultats_normalite <- do.call(rbind, resultats_normalite)
print(resultats_normalite)
# Transorm with inverse function



##Silt##
hist(data_env$Silt, main="Histogramme du limon", xlab="limon", freq=FALSE, col="lightblue")
abline(v = c(mean(data_env$Silt),median(data_env$Silt)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

#sum((data_env$Silt-mean(data_env$Silt))^3)/((length(data_env$Silt)-1)*sd(data_env$Silt)^3)
#skewness(data_env$Silt)
#Forte Asymétrie de -1.53 à gauche
#sum((data_env$Silt-mean(data_env$Silt))^4)/((length(data_env)-1)*sd(data_env$Silt)^4)
#kurtosis(data_env$Silt)
# Kurtosis= 3.6 : légèrement plus concentrées autour de la moyenne et que vous avez un peu plus de valeurs extrêmes (outliers) que dans une distribution normale.
# Transformation ne font pas mieux donc je garde comme ça 
#Je peux laisser comme ça
shapiro.test(data_env$Silt) #5.385e-06

resultats_normalite <- lapply("Silt", 
                              function(v) {
                                st.raw <- shapiro.test(data_env[[v]])
                                st.log <- shapiro.test(log1p(data_env[[v]]))
                                st.sqrt <- shapiro.test(sqrt(data_env[[v]]))
                                st.stand <- shapiro.test(scale(data_env[[v]]))
                                st.stand.sqrt <- shapiro.test(scale(sqrt(data_env[[v]])))
                                st.inverse <- shapiro.test(1 / data_env[[v]])
                                st.inverse.sqrt <- shapiro.test(sqrt(1 / data_env[[v]]))
                                
                                return(data.frame(variable = v,
                                                  p.raw = st.raw$p.value,
                                                  p.log = st.log$p.value,
                                                  p.sqrt = st.sqrt$p.value,
                                                  p.stand = st.stand$p.value,
                                                  p.stand.sqrt = st.stand.sqrt$p.value,
                                                  p.inverse = st.inverse$p.value,
                                                  p.inverse.sqrt = st.inverse.sqrt$p.value))
                              })

# Data frame
resultats_normalite <- do.call(rbind, resultats_normalite)
print(resultats_normalite)
#No transformation applied




##CaCO3##
hist(data_env$CaCO3, main="Histogramme du CaCO3", xlab="CaCO3", freq=FALSE, col="lightblue")
abline(v = c(mean(data_env$CaCO3),median(data_env$CaCO3)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

#sum((data_env$CaCO3-mean(data_env$CaCO3))^3)/((length(data_env$CaCO3)-1)*sd(data_env$CaCO3)^3)
#skewness(data_env$CaCO3)
# Légère asymétrie vers la droite : 0.8 
#sum((data_env$CaCO3-mean(data_env$CaCO3))^4)/((length(data_env$CaCO3)-1)*sd(data_env$CaCO3)^4)
#kurtosis(data_env$CaCO3)
# Kurtosis= 3.5 : légèrement leptokurtique avec des valeur un peu plus  concentrées autour de la moyenne
shapiro.test(data_env$CaCO3) #0.001633

resultats_normalite <- lapply("CaCO3", 
                              function(v) {
                                st.raw <- shapiro.test(data_env[[v]])
                                st.log <- shapiro.test(log1p(data_env[[v]]))
                                st.sqrt <- shapiro.test(sqrt(data_env[[v]]))
                                st.stand <- shapiro.test(scale(data_env[[v]]))
                                st.stand.sqrt <- shapiro.test(scale(sqrt(data_env[[v]])))
                                st.inverse <- shapiro.test(1 / data_env[[v]])
                                st.inverse.sqrt <- shapiro.test(sqrt(1 / data_env[[v]]))
                                
                                return(data.frame(variable = v,
                                                  p.raw = st.raw$p.value,
                                                  p.log = st.log$p.value,
                                                  p.sqrt = st.sqrt$p.value,
                                                  p.stand = st.stand$p.value,
                                                  p.stand.sqrt = st.stand.sqrt$p.value,
                                                  p.inverse = st.inverse$p.value,
                                                  p.inverse.sqrt = st.inverse.sqrt$p.value))
                              })

# Data.frame
resultats_normalite <- do.call(rbind, resultats_normalite)
print(resultats_normalite)
# No transformation applied


##Shells##
hist(FullData$Shells, main="Histogramme des coquilles", xlab="Shells", freq=FALSE, col="lightblue")
abline(v = c(mean(FullData$Shells),median(FullData$Shells)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

#sum((FullData$Shells-mean(FullData$Shells))^3)/((length(FullData$Shells)-1)*sd(FullData$Shells)^3)
#skewness(FullData$Shells)
# Légère asymétrie vers la droite : 0.5
#sum((FullData$Shells-mean(FullData$Shells))^4)/((length(FullData$Shells)-1)*sd(FullData$Shells)^4)
#kurtosis(FullData$Shells)
# Kurtosis= 2
shapiro.test(data_env$Shells) #0.001633

resultats_normalite <- lapply("Shells", 
                              function(v) {
                                st.raw <- shapiro.test(data_env[[v]])
                                st.log <- shapiro.test(log1p(data_env[[v]]))
                                st.sqrt <- shapiro.test(sqrt(data_env[[v]]))
                                st.stand <- shapiro.test(scale(data_env[[v]]))
                                st.stand.sqrt <- shapiro.test(scale(sqrt(data_env[[v]])))
                                st.inverse <- shapiro.test(1 / data_env[[v]])
                                st.inverse.sqrt <- shapiro.test(sqrt(1 / data_env[[v]]))
                                
                                return(data.frame(variable = v,
                                                  p.raw = st.raw$p.value,
                                                  p.log = st.log$p.value,
                                                  p.sqrt = st.sqrt$p.value,
                                                  p.stand = st.stand$p.value,
                                                  p.stand.sqrt = st.stand.sqrt$p.value,
                                                  p.inverse = st.inverse$p.value,
                                                  p.inverse.sqrt = st.inverse.sqrt$p.value))
                              })

# Data.frame
resultats_normalite <- do.call(rbind, resultats_normalite)
print(resultats_normalite)
# No transformation applied


##OrgC##
# 1) OrgC instead of Shells
x = FullData$OrgC
hist(x, main="Histogram of OrgC", freq=FALSE)
abline(v = c(mean(x), median(x)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

#sum((x-mean(x))^3)/((length(x)-1)*sd(x)^3)
#skewness(x) # 0.6
#sum((x-mean(x))^4)/((length(x)-1)*sd(x)^4)
#kurtosis(x)#2.99
shapiro.test(data_env$OrgC) #0.2781
#On ne change pas


##TotN##
x = data_env$TotN
hist(x, main="Histogram of TotN", freq=FALSE)
abline(v = c(mean(x), median(x)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))
#sum((x-mean(x))^3)/((length(x)-1)*sd(x)^3)
#skewness(x) # 0.15
#sum((x-mean(x))^4)/((length(x)-1)*sd(x)^4)
#kurtosis(x)#1.8
shapiro.test(data_env$TotN) 

##CN##
x = data_env$CN
hist(x, main="Histogram of CN", freq=FALSE)
abline(v = c(mean(x), median(x)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))
#sum((x-mean(x))^3)/((length(x)-1)*sd(x)^3)
#skewness(x)# 0.6
#sum((x-mean(x))^4)/((length(x)-1)*sd(x)^4)
#kurtosis(x)#7.58
shapiro.test(data_env$CN)

resultats_normalite <- lapply("CN", 
                              function(v) {
                                st.raw <- shapiro.test(data_env[[v]])
                                st.log <- shapiro.test(log1p(data_env[[v]]))
                                st.sqrt <- shapiro.test(sqrt(data_env[[v]]))
                                st.stand <- shapiro.test(scale(data_env[[v]]))
                                st.stand.sqrt <- shapiro.test(scale(sqrt(data_env[[v]])))
                                st.inverse <- shapiro.test(1 / data_env[[v]])
                                st.inverse.sqrt <- shapiro.test(sqrt(1 / data_env[[v]]))
                                
                                return(data.frame(variable = v,
                                                  p.raw = st.raw$p.value,
                                                  p.log = st.log$p.value,
                                                  p.sqrt = st.sqrt$p.value,
                                                  p.stand = st.stand$p.value,
                                                  p.stand.sqrt = st.stand.sqrt$p.value,
                                                  p.inverse = st.inverse$p.value,
                                                  p.inverse.sqrt = st.inverse.sqrt$p.value))
                              })

# Data.frame
resultats_normalite <- do.call(rbind, resultats_normalite)
print(resultats_normalite)
# Transformation using the inverse function


##TotFe## 
x = data_env$TotFe
hist(x, main="Histogram of TotFe", freq=FALSE)
abline(v = c(mean(x), median(x)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

#sum((x-mean(x))^3)/((length(x)-1)*sd(x)^3)
#skewness(x)
#sum((x-mean(x))^4)/((length(x)-1)*sd(x)^4)
#kurtosis(x)
shapiro.test(data_env$TotFe)

resultats_normalite <- lapply("Fe", 
                              function(v) {
                                st.raw <- shapiro.test(data_env[[v]])
                                st.log <- shapiro.test(log1p(data_env[[v]]))
                                st.sqrt <- shapiro.test(sqrt(data_env[[v]]))
                                st.stand <- shapiro.test(scale(data_env[[v]]))
                                st.stand.sqrt <- shapiro.test(scale(sqrt(data_env[[v]])))
                                st.inverse <- shapiro.test(1 / data_env[[v]])
                                st.inverse.sqrt <- shapiro.test(sqrt(1 / data_env[[v]]))
                                
                                return(data.frame(variable = v,
                                                  p.raw = st.raw$p.value,
                                                  p.log = st.log$p.value,
                                                  p.sqrt = st.sqrt$p.value,
                                                  p.stand = st.stand$p.value,
                                                  p.stand.sqrt = st.stand.sqrt$p.value,
                                                  p.inverse = st.inverse$p.value,
                                                  p.inverse.sqrt = st.inverse.sqrt$p.value))
                              })

# Data.frame
resultats_normalite <- do.call(rbind, resultats_normalite)
print(resultats_normalite)
# Transformation using the inverse function


##TotMn##
x = data_env$TotMn
hist(x, main="Histogram of TotMn", freq=FALSE)
abline(v = c(mean(x), median(x)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))
#sum((x-mean(x))^3)/((length(x)-1)*sd(x)^3)
#skewness(x)#1.84
#sum((x-mean(x))^4)/((length(x)-1)*sd(x)^4)
#kurtosis(x)#7.45
shapiro.test(data_env$TotMn)
resultats_normalite <- lapply("TotMn", 
                              function(v) {
                                st.raw <- shapiro.test(data_env[[v]])
                                st.log <- shapiro.test(log1p(data_env[[v]]))
                                st.sqrt <- shapiro.test(sqrt(data_env[[v]]))
                                st.stand <- shapiro.test(scale(data_env[[v]]))
                                st.stand.sqrt <- shapiro.test(scale(sqrt(data_env[[v]])))
                                st.inverse <- shapiro.test(1 / data_env[[v]])
                                st.inverse.sqrt <- shapiro.test(sqrt(1 / data_env[[v]]))
                                
                                return(data.frame(variable = v,
                                                  p.raw = st.raw$p.value,
                                                  p.log = st.log$p.value,
                                                  p.sqrt = st.sqrt$p.value,
                                                  p.stand = st.stand$p.value,
                                                  p.stand.sqrt = st.stand.sqrt$p.value,
                                                  p.inverse = st.inverse$p.value,
                                                  p.inverse.sqrt = st.inverse.sqrt$p.value))
                              })

# Data.frame
resultats_normalite <- do.call(rbind, resultats_normalite)
print(resultats_normalite)
# Transformation using the inverse function or inverse and square root

##TotP##
x = FullData$TotP
hist(x, main="Histogram of TotP", freq=FALSE)
abline(v = c(mean(x), median(x)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))
#sum((x-mean(x))^3)/((length(x)-1)*sd(x)^3)
#skewness(x)#0.24
#sum((x-mean(x))^4)/((length(x)-1)*sd(x)^4)
#kurtosis(x)#3.06
shapiro.test(data_env$TotP)


############
############
#2) Calculate biodiversity indices and briefly map their spatial distribution:
# use longitude and latitude as x and y, and vary point size and intensity according to the value.
############
############
data_spc <- read.table("~/Desktop/master oceano/M2/Maths/Marilaure/Exam janvier/BiomassDominantSpecies.txt", header=TRUE, sep="\t")

head(data_spc)

# Create a vector with stations numbers
stations <- c(1, 4, 5:20, 22:25, 29:33)

# Check stations length
length(stations)

# Make sure the length of the stations match the number of lines in the dataset data_spc
if(length(stations) == nrow(data_spc)) {
  # Add the column station to data_spc
  data_spc$Station <- rep(stations, length.out = nrow(data_spc))
}

# Check the new column Station
head(data_spc)

N0 <- rowSums(data_spc > 0)            # Species richness
H <- diversity(data_spc)               # Shannon entropy
N1 <- exp(H)                           # Shannon diversity number
N2 <- diversity(data_spc, "inv")       # Simpson diversity number
J <- H/log(N0)                         # Pielou evenness
E1 <- N1/N0                            # Shannon evenness (Hill's ratio)
E2 <- N2/N0                            # Simpson evenness (Hill's ratio)
div1 <- data.frame(N0, H, N1, N2, E1, E2, J)
div1

# Create a dataframe with the biodiversity indicators
biodiv_map <- data.frame(
  Station = rownames(data_spc),  # Name of the stations
  N0 = N0,
   H = H,                        # Shannon entropy
  N1 = N1,                      # Shannon diversity index
  N2 = N2,                      # Simpson diversirty index
  J = J,                        # Pielou equity
  E1 = E1,                      # Shannon equity
  E2 = E2                       # Simpson equity
)

stations <- c(1, 4, seq(5, 20), seq(22, 25), seq(29, 33))
biodiv_map$Station <- stations

library(dplyr)

# Rename the columns lat and long
colnames(data_env)[colnames(data_env) == "Lat"] <- "latitude"
colnames(data_env)[colnames(data_env) == "Long"] <- "longitude"

# Convert 'station' into character
biodiv_map$Station <- as.character(biodiv_map$Station)
data_station$Station <- as.character(data_station$Station)

biodiv_map <- biodiv_map %>%
  left_join(data_station, by = "Station")


# Plot with Simpson index (N2)
ggplot(biodiv_map, aes(x = longitude, y = latitude, size = N1, color = N1)) +
  geom_point(alpha = 0.7) +  
  geom_text(aes(label = Station), hjust = -0.3, vjust = -0.5, size = 4, color = "black") +
  scale_size_continuous(range = c(3, 10)) +  
  scale_color_gradient(low = "blue", high = "gold") +  
  labs(title = "Cartographie de l'Indicateur de Biodiversité : Shannon Index",
       x = "Longitude",
       y = "Latitude",
       size = "Valeur de Shannon (H)",
       color = "Valeur de Shannon (H)") +
  theme_minimal() +  
  theme(legend.position = "bottom")  


# Plot with Simpson index (N2) et add the stations numbers
ggplot(biodiv_map, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = N2, color = N2), alpha = 0.7) + 
  geom_text(aes(label = Station), hjust = -0.3, vjust = -0.5, size = 4, color = "black") +  
  scale_size_continuous(range = c(3, 10)) +  
  scale_color_gradient(low = "blue", high = "gold") + 
  labs(title = "Cartographie de l'Indicateur de Biodiversité : Simpson Index",
       x = "Longitude",
       y = "Latitude",
       size = "Valeur de Simpson (N2)",
       color = "Valeur de Simpson (N2)") +
  theme_minimal() + 
  theme(
    legend.position = "bottom",  
    legend.text = element_text(size = 11), 
    legend.title = element_text(size = 15)  
  )





############
############
#3) Calculate distance matrices (compare several appropriate metrics)
#between sites based on species distribution and environmental variables.
#Interpret.
############
############
library("plyr")
library("dplyr")

################################
############ SPECIES ###########
################################

FullData <- read_excel("~/Desktop/master oceano/M2/Maths/Marilaure/Exam janvier/FullData.xlsx")
FullData <- FullData[,-c(1, 15)]

# Convert into a classic dataframe
d.tot.allvalid <- as.data.frame(FullData)

#Creation of plots for environmental variables for each station.
#Exploring the distributions of environmental data.
ggplot(melt(d.tot.allvalid, id.vars = 'Station'), aes(Station))+
  geom_point(aes(y=variable,color=variable))+
  ylab('')+xlab('')+
  theme_bw()+theme(legend.position = 'none')

#Environmental variables and species are extracted
d.env.allvalid <-  d.tot.allvalid[ , c('Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP', 'Depth', 'longitude', 'latitude')]
d.spe.allvalid <-  d.tot.allvalid[ ,which( !(colnames(d.tot.allvalid) %in%  c('Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP', 'Depth', 'longitude', 'latitude', 'Station') ))]

d.env.all <-  FullData[ ,c('Station','Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP')]
d.spe.all <-  FullData[ ,which(  !(colnames(FullData) %in% c('Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP') ))]

ggplot(melt(d.spe.allvalid), aes(x=value))+
  geom_histogram(bins=50)+
  facet_wrap(~variable, scales = 'free')+
  theme_bw()

dsummary <- ddply(melt(data),"variable",summarize,
                  Obs=sum(value>0),
                  max.count=max(value),
                  mean.count=mean(value),
                  mean.spot.count=mean(value[value>0]))
dsummary <- dsummary[order(-dsummary$Obs),]
dsummary

#d.spe<-data_spc_filtre
d.spe <- d.spe.allvalid
#d.spe = data_env

#Testing different data transformation methods
display.methods <- function(mets){
  pl <-llply(mets,function(method){
    
    switch(method, 
           'raw'      ={ d.spe.norm <- d.spe},
           'sqrt'     ={ d.spe.norm <- sqrt(d.spe)},
           'log1p'    ={ d.spe.norm <- log1p(d.spe)},
           'totalsp'  ={ d.spe.norm <- decostand(d.spe,'total',MARGIN = 2)},
           'wisconsin'={ d.spe.norm <- wisconsin(d.spe)},
           {             d.spe.norm <- decostand(d.spe,method)}) # default case.
    
    d.spe.norm$date <- rownames(d.spe.norm)
    a<-ggplot(melt(d.spe.norm, id.vars = c('date')),aes(x=variable, y=value))+
      geom_boxplot(aes(color=variable))+
      ggtitle(method)+ylab("")+xlab("")+
      theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+theme(legend.position='none')
    return(a)})
  do.call("grid.arrange", c(pl, nrow=1))}

par(mfrow=c(2,2))

display.methods(c("raw","log1p", "sqrt"))
display.methods(c("max", "range","totalsp"))
display.methods(c("total", "normalize","hellinger"))
display.methods(c("chi.square","wisconsin"))

spe1<-wisconsin(d.spe.allvalid)
spe2<-decostand(decostand(d.spe.allvalid, method = 'max'),method='total',MARGIN = 1)

boxplot(spe1)
boxplot(spe2)

head(d.spe)

d.spe.total <- decostand(  d.spe, 'total') 
head(d.spe.total)

d.spe.max <- decostand(  d.spe, 'max') 
head(d.spe.max)

d.spe.max.total <-  decostand(  d.spe.max, 'total') 
head(d.spe.max.total)

d.spe.wis <- wisconsin(d.spe)
head(d.spe.wis)

boxplot(d.spe)
boxplot(d.spe.max)
boxplot(d.spe.max.total)
boxplot(d.spe.wis)

# Calculation of distance matrices.
spe.db <- vegdist(d.spe) # Bray-Curtis Brute
spe.log <- decostand(d.spe,'log')
spe.dbL <- vegdist(spe.log) # Bray-Curtis Log
spe.norm <- decostand(d.spe,'norm')
spe.dc <- dist(spe.norm) # Chord distance
spe.hel <- decostand(d.spe,'hel')
spe.dh <- dist(spe.hel) # Hellinger Distance

coldiss(spe.db,byrank = TRUE,diag=FALSE); title("Bray-Curtis Raw data")
coldiss(spe.dbL,byrank = TRUE,diag=FALSE); title("Bray-Curtis Log Transformed")
coldiss(spe.dc,byrank = FALSE,diag=FALSE); title("Chord")
coldiss(spe.dh,byrank = FALSE,diag=FALSE); title("Hellinger")



####################################
########### ENVIRONMENT ############
####################################

FullData <- read_excel("~/Desktop/master oceano/M2/Maths/Marilaure/Exam janvier/FullData.xlsx")
FullData <- FullData[,-c(1,15)]
d.tot.allvalid <- FullData
d.tot.allvalid <- as.data.frame(d.tot.allvalid)
d.env.allvalid <-  d.tot.allvalid[ ,c('Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP', 'Depth', 'longitude', 'latitude')]

same_value <- sapply(d.env.allvalid, function(x) length(unique(x))==1)
same_var <- names(d.env.allvalid)[same_value]
data_env_filtre <- d.env.allvalid[, !same_value]

d.env <- d.env.allvalid
stations <- c(1, 4, 5, 6,11, 12, 14, 16:20, 22:25, 29:31)
rownames(d.env) <- stations

#Testing different data transformation methods
display.methods <- function(mets){
  pl <-llply(mets,function(method){
    
    switch(method, 
           'raw'      ={ d.env.norm <- d.env},
           'sqrt'     ={ d.env.norm <- sqrt(d.env)},
           'log1p'    ={ d.env.norm <- log1p(d.env)},
           'totalsp'  ={ d.env.norm <- decostand(d.env,'total',MARGIN = 2)},
           'wisconsin'={ d.env.norm <- wisconsin(d.env)},
           {             d.env.norm <- decostand(d.env,method)}) # default case.
    
    d.env.norm$date <- rownames(d.env.norm)
    a<-ggplot(melt(d.env.norm, id.vars = c('date')),aes(x=variable, y=value))+
      geom_boxplot(aes(color=variable))+
      ggtitle(method)+ylab("")+xlab("")+
      theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+theme(legend.position='none')
    return(a)})
  do.call("grid.arrange", c(pl, nrow=1))}

par(mfrow=c(2,2))

display.methods(c("raw","log1p", "sqrt"))
display.methods(c("max", "range","totalsp"))
display.methods(c("total", "normalize","hellinger"))
display.methods(c("chi.square","wisconsin"))

env1<-wisconsin(d.env)
env2<-decostand(decostand(d.env, method = 'max'),method='total',MARGIN = 1)

boxplot(env1)
boxplot(env2)

head(d.env)

d.env.total <- decostand(  d.env, 'total') 
head(d.env.total)

d.env.max <- decostand(  d.env, 'max') 
head(d.env.max)

d.env.max.total <-  decostand(  d.env.max, 'total') 
head(d.env.max.total)

d.env.wis <- wisconsin(d.env)
head(d.env.wis)

boxplot(d.env)
boxplot(d.env.max)
boxplot(d.env.max.total)
boxplot(d.env.wis)

# Calculation of distance matrices.
env.db <- vegdist(d.env) # Bray-Curtis Brute
env.log <- decostand(d.env,'log')
env.dbL <- vegdist(env.log) # Bray-Curtis Log
env.norm <- decostand(d.env,'norm')
env.dc <- dist(env.norm) # Chord distance
env.hel <- decostand(d.env,'hel')
env.dh <- dist(env.hel) # Hellinger Distance

coldiss(env.db,byrank = TRUE,diag=FALSE); title("Bray-Curtis Raw data")
coldiss(env.dbL,byrank = TRUE,diag=FALSE); title("Bray-Curtis Log Transformed")
coldiss(env.dc,byrank = FALSE,diag=FALSE); title("Chord")
coldiss(env.dh,byrank = FALSE,diag=FALSE); title("Hellinger")


############
############
#4) Perform clustering of sites based on species and environmental variables.
# Choose the most appropriate method and justify it. Compare.
############
############

################################
############ SPECIES ###########
################################

#d.env.allvalid <-  d.tot.allvalid[ , c('Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP', 'Depth', 'longitude', 'latitude')]
d.spe.allvalid3 <-  d.tot.allvalid[ ,which( !(colnames(d.tot.allvalid) %in%  c('Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP', 'Depth', 'longitude', 'latitude', 'Station') ))]

d.spe2<-d.spe.allvalid3
#d.spe2 <- d.spe2[, -c(14)]
stations <- c(1, 4, 5, 6, 11, 12, 14, 16:20, 22:25, 29:31)
rownames(d.spe2) <- stations

spe.norm2 <- decostand(d.spe2,'norm')
spe.dist2 <- dist(spe.norm2) # Chord distance

spe.dist.single2   <- hclust(spe.dist2, method= "single")
plot(spe.dist.single2)

spe.dist.complete2 <- hclust(spe.dist2, method= "complete")
plot(spe.dist.complete2)

spe.dist.average2  <- hclust(spe.dist2, method= "average")
plot(spe.dist.average2)

spe.dist.centroid2 <- hclust(spe.dist2, method= "centroid")
plot(spe.dist.centroid2)

spe.dist.ward2 <- hclust(spe.dist2, method= "ward.D2")
spe.dist.ward2$height <- sqrt(spe.dist.ward2$height)
plot(spe.dist.ward2)

cophDF2 <- data.frame(method = c("Single","Complete","Average","Centroid","Ward"), CophCorr= NA)
rownames(cophDF2)<- cophDF2$method

spe.dist.single.coph2 <- cophenetic(spe.dist.single2)
cophDF2["Single","CophCorr"] <- cor(spe.dist2,spe.dist.single.coph2)

spe.dist.complete.coph2 <- cophenetic(spe.dist.complete2)
cophDF2["Complete","CophCorr"] <- cor(spe.dist2,spe.dist.complete.coph2)

spe.dist.average.coph2 <- cophenetic(spe.dist.average2)
cophDF2["Average","CophCorr"] <- cor(spe.dist2,spe.dist.average.coph2)

spe.dist.centroid.coph2 <- cophenetic(spe.dist.centroid2)
cophDF2["Centroid","CophCorr"] <- cor(spe.dist2,spe.dist.centroid.coph2)

spe.dist.ward.coph2 <- cophenetic(spe.dist.ward2)
cophDF2["Ward","CophCorr"] <- cor(spe.dist2,spe.dist.ward.coph2)

cophDF2

plot(spe.dist.average2$height, nrow(d.spe2):2, type="S",
     main = "Fusion levels - Chord - Average (UPGMA)", 
     ylab = "k (number of clusters)", 
     xlab = "h (node height)", 
     col="grey")
text (spe.dist.average2$height, nrow(d.spe2):2,nrow(d.spe2):2,col="red", cex=0.8)

kfix <-5

spebc.single.g2    <- cutree(spe.dist.single2,k=kfix)
spebc.complete.g2  <- cutree(spe.dist.complete2,k=kfix)
spebc.average.g2   <- cutree(spe.dist.average2,k=kfix)
spebc.centroid.g2  <- cutree(spe.dist.centroid2,k=kfix)
spebc.ward.g2      <- cutree(spe.dist.ward2,k=kfix)

table(spebc.average.g2, spebc.complete.g2)


finalClust2 <- spe.dist.average2

asw <- numeric(nrow(d.spe2))


for (k in 2:(nrow(d.spe2)-1)){
  sil <- silhouette(cutree(finalClust2, k=k), spe.dist2)
  asw[k]<-summary(sil)$avg.width
}

k.best <- which.max(asw)

plot( 1:nrow(d.spe2) , asw, type= 'h', 
      main = paste0("Silhouette optimal number of cluster for ", finalClust2$method,',',finalClust2$dist.method))
axis(1, k.best, col='red')
points(k.best, max(asw), col='red')


finalk     <- 5

cutg <- cutree(finalClust2,k=finalk)
sil  <- silhouette(cutg, spe.dist2) 
silo <- sortSilhouette(sil)
rownames(silo) <- row.names(d.spe2)[attr(silo,"iOrd")]

cutgo <- cutg[attr(silo,"iOrd")]
cluster_colors <- rainbow(finalk)  # Une palette de `k` couleurs

plot(silo, main= paste0( "silhouette Plot - ", finalClust2$method,',',finalClust2$dist.method," - k = ", finalk),
     cex.names=0.8, col= cutgo+1, nmax.lab = 100)

spe.chwo2<-reorder.hclust(finalClust2,spe.dist2)
hcoplot(finalClust2, spe.dist2, k=finalk)


FullData$Station<- rownames(FullData)
d.spe.clust <- decostand(d.spe,'total')
d.spe.clust$Station <- FullData[is.finite(FullData$`Cardium edule`),]$Station
d.spe.clust$g     <- cutg
d.spe.clust$sil   <- sil[,3]

ggplot(melt(d.spe.clust, id.vars = c("Station","g","sil")), aes(x=g, y=(value), group=factor(g))) +
  geom_boxplot()+
  geom_point(aes(color=sil))+scale_color_viridis_c()+
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~variable, scale='free_y')+theme_bw()


################################
########## ENVIRONMENT #########
################################

#Environmental variables are extracted
d.env.allvalid2 <-  d.tot.allvalid2[ , c('Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP', 'Depth', 'longitude', 'latitude')]

d.env.all2 <-  FullData[ ,c('Station','Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP' , 'Depth', 'longitude', 'latitude')]

d.env2<-d.env

env.norm2 <- decostand(d.env2,'norm')
env.dist2 <- dist(env.norm2) # Chord distance

env.dist.single2   <- hclust(env.dist2, method= "single")
plot(env.dist.single2)

env.dist.complete2 <- hclust(env.dist2, method= "complete")
plot(env.dist.complete2)

env.dist.average2  <- hclust(env.dist2, method= "average")
plot(env.dist.average2)

env.dist.centroid2 <- hclust(env.dist2, method= "centroid")
plot(env.dist.centroid2)

env.dist.ward2 <- hclust(env.dist2, method= "ward.D2")
env.dist.ward2$height <- sqrt(env.dist.ward2$height)
plot(env.dist.ward2)

cophDF2 <- data.frame(method = c("Single","Complete","Average","Centroid","Ward"), CophCorr= NA)
rownames(cophDF2)<- cophDF2$method

env.dist.single.coph2 <- cophenetic(env.dist.single2)
cophDF2["Single","CophCorr"] <- cor(env.dist2,env.dist.single.coph2)

env.dist.complete.coph2 <- cophenetic(env.dist.complete2)
cophDF2["Complete","CophCorr"] <- cor(env.dist2,env.dist.complete.coph2)

env.dist.average.coph2 <- cophenetic(env.dist.average2)
cophDF2["Average","CophCorr"] <- cor(env.dist2,env.dist.average.coph2)

env.dist.centroid.coph2 <- cophenetic(env.dist.centroid2)
cophDF2["Centroid","CophCorr"] <- cor(env.dist2,env.dist.centroid.coph2)

env.dist.ward.coph2 <- cophenetic(env.dist.ward2)
cophDF2["Ward","CophCorr"] <- cor(env.dist2,env.dist.ward.coph2)

cophDF2

plot(env.dist.average2$height, nrow(d.spe2):2, type="S",
     main = "Fusion levels - Chord - Average (UPGMA)", 
     ylab = "k (number of clusters)", 
     xlab = "h (node height)", 
     col="grey")
text (env.dist.average2$height, nrow(d.env2):2,nrow(d.env2):2,col="red", cex=0.8)

kfix <-4

envbc.single.g2    <- cutree(env.dist.single2,k=kfix)
envbc.complete.g2  <- cutree(env.dist.complete2,k=kfix)
envbc.average.g2   <- cutree(env.dist.average2,k=kfix)
envbc.centroid.g2  <- cutree(env.dist.centroid2,k=kfix)
envbc.ward.g2      <- cutree(env.dist.ward2,k=kfix)

table(envbc.average.g2, envbc.complete.g2)

finalClust2 <- env.dist.average2

asw <- numeric(nrow(d.env2))


for (k in 2:(nrow(d.env2)-1)){
  sil <- silhouette(cutree(finalClust2, k=k), env.dist2)
  asw[k]<-summary(sil)$avg.width
}

k.best <- which.max(asw)

plot( 1:nrow(d.env2) , asw, type= 'h', 
      main = paste0("Silhouette optimal number of cluster for ", finalClust2$method,',',finalClust2$dist.method))
axis(1, k.best, col='red')
points(k.best, max(asw), col='red')


finalk     <- 4

cutg <- cutree(finalClust2,k=finalk)
sil  <- silhouette(cutg, env.dist2) 
silo <- sortSilhouette(sil)
rownames(silo) <- row.names(d.env2)[attr(silo,"iOrd")]

cutgo <- cutg[attr(silo,"iOrd")]
cluster_colors <- rainbow(finalk)  # Une palette de `k` couleurs

plot(silo, main= paste0( "silhouette Plot - ", finalClust2$method,',',finalClust2$dist.method," - k = ", finalk),
     cex.names=0.8, col= cutgo+1, nmax.lab = 100)

env.chwo2<-reorder.hclust(finalClust2,env.dist2)
hcoplot(finalClust2, env.dist2, k=finalk)


FullData$Station<- rownames(FullData)
d.env.clust <- d.env
d.env.clust$Station <- FullData[is.finite(FullData$Porosity),]$Station
d.env.clust$g     <- cutg
d.env.clust$sil   <- sil[,3]

ggplot(melt(d.env.clust, id.vars = c("Station","g","sil")), aes(x=g, y=(value), group=factor(g))) +
  geom_boxplot()+
  geom_point(aes(color=sil))+scale_color_viridis_c()+
  facet_wrap(~variable, scale='free_y')+theme_bw()





############
############
#5) Perform a PCA analysis of both matrices. Compare the two ordinations and interpret the role 
#of environmental variables in species distribution. /6 (3 points per PCA)

################################
########### SPECIES ############
################################

stations <- c(1, 4, 5, 6, 11, 12, 14, 16:20, 22:25, 29:31)
rownames(spe.hel) <- stations
spe.pca <- rda(spe.hel)

spe.pca

ev <- spe.pca$CA$eig
evplot(ev)
summary(spe.pca)

par(mfrow=c(2,2))
biplot(spe.pca,ax1=1, ax2=2, ax3=3,ax4=4, scaling=1, main="PCA-biplot scaling 1")
biplot(spe.pca,ax1=1, ax2=2, ax3=3,ax4=4, scaling=2, main="PCA-biplot scaling 2")
biplot(spe.pca,ax1=1, ax2=2, ax3=3,ax4=4, scaling=3, main="PCA-biplot scaling 2")


################################
########## ENVIRONMENT #########
################################
d.env.sc<-decostand(sqrt(d.env[,c('Porosity','Grain','Silt','CaCO3','Shells','OrgC','TotN','CN','TotFe','TotMn','TotP','Depth')]), 'standardize')

env.pca <- rda(d.env.sc) # Argument scale=TRUE calls for a
# standardization of the variables
env.pca
summary(env.pca) # Default scaling 2
summary(env.pca, scaling=1)

ev <- env.pca$CA$eig

ev[ev > mean(ev)]

# Broken stick model
n <- length(ev)
bsm <- data.frame(j=seq(1:n), p=0)
bsm$p[1] <- 1/n
for (i in 2:n) {
  bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
}
bsm$p <- 100*bsm$p/n
bsm

# Plot eigenvalues and % of variance for the first 4 axes
par(mfrow=c(2,1))
barplot(ev, main="Eigenvalues", col="bisque", las=2)
abline(h=mean(ev), col="red")	# average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
barplot(t(cbind(100*ev/sum(ev),bsm$p[n:1])), beside=TRUE, 
        main="% variance", col=c("bisque",2), las=2)
legend("topright", c("% eigenvalue", "Broken stick model"), 
       pch=15, col=c("bisque",2), bty="n")
evplot(ev)

par(mfrow=c(1,2))
biplot(env.pca,scaling=1, main="PCA-biplot scaling 1")
biplot(env.pca,scaling=2, main="PCA-biplot scaling 2")


############
############
#6) Interpret the results based on the functioning of the system
############
############

############
############
#7) Perform a PCA analysis of the sites based on biodiversity indices
############
############

d.bio <- biodiv_map[ , c('N0', 'H', 'N1', 'N2', 'J', 'E1', 'E2' )]
d.bio.valid <- subset(biodiv_map, is.finite(N0+H+N1+N2+J+E1+E2))
stations <- c(1, 4, 5: 20, 22:25, 29:33)
rownames(d.bio.valid) <- stations
d.bio.valid <- d.bio.valid[,-c(1) ]

d.bio.sc<-decostand(sqrt(d.bio.valid[,c('N0','H', 'N1', 'N2', 'J', 'E1', 'E2')]), 'standardize')

env.pca <- rda(d.bio.valid) # Argument scale=TRUE calls for a
# standardization of the variables
env.pca
summary(env.pca) # Default scaling 2
summary(env.pca, scaling=1)

ev <- env.pca$CA$eig

ev[ev > mean(ev)]

# Broken stick model
n <- length(ev)
bsm <- data.frame(j=seq(1:n), p=0)
bsm$p[1] <- 1/n
for (i in 2:n) {
  bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
}
bsm$p <- 100*bsm$p/n
bsm

# Plot eigenvalues and % of variance for the first 4 axes
par(mfrow=c(2,1))
barplot(ev, main="Eigenvalues", col="bisque", las=2)
abline(h=mean(ev), col="red")	# average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
barplot(t(cbind(100*ev/sum(ev),bsm$p[n:1])), beside=TRUE, 
        main="% variance", col=c("bisque",2), las=2)
legend("topright", c("% eigenvalue", "Broken stick model"), 
       pch=15, col=c("bisque",2), bty="n")
evplot(ev)

par(mfrow=c(1,2))
biplot(env.pca,scaling=1, main="PCA-biplot scaling 1")
biplot(env.pca,scaling=2, main="PCA-biplot scaling 2")


