#Library utilisée
library(dplyr)
library(tidyr)
library(plyr)

#Systeme de location pour le format des dates
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

#choix du répertoire pour le changement des csv MFP
setwd("/JLA/Sauvegarde/Dropbox/Santé/Raw_Data/Export_MFP/")
file_list <- list.files()
MFP <- do.call("rbind",lapply(file_list, FUN=function(files){read.csv(files)}))

#Modification du format de date dans les fichiers MyFitnessPal
MFP  <- mutate(MFP, Date  = as.Date(Date, "%d-%b-%y"))

#choix du répertoire pour la suite
setwd("/JLA/Sauvegarde/Dropbox/Santé/Raw_Data/")

#Ouverture des fichiers sources Jawbone et Withings
Activity  <- read.csv("./Withings - Activity Julien.csv")
Poids  <- read.csv("./Withings - Weight Julien.csv")
sleep_w  <- read.csv("./Withings - Sleep Julien.csv")
Jawbone  <- read.csv("./Jawbone.csv")

#Modification du fichier d'historique Jawbone
Jawbone <- Jawbone  %>%
  select(DATE, Cal_Burned = m_calories, Steps = m_steps, m_distance, Weight = weight, 
         s_deep, s_light) %>%
  mutate(Date = as.Date(as.character(DATE),"%Y%m%d"), Distance = (m_distance)/1000,  
         deep = s_deep/3600, light = s_light/3600) %>%
  select(Date, Cal_Burned, Steps, Distance, Weight, deep, light) %>%
  distinct(Date) %>%
  filter(Date < "2015-01-01")

#Création d'une nouvelle table pour le poids, a cause du format de Date
Poids <- Poids %>%
  mutate(Date = substr(Date,1,10), Weight = Weight..kg.) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Weight)

#Renommage dans le fichier d'activité
Activity  <- Activity %>%
  mutate(Date = as.Date(Date), Steps, 
         Cal_Burned = Calories, Distance = Distance..km.)
  
sleep_w  <- sleep_w %>%
  distinct(to.) %>%
  mutate(Date  = as.Date(to., "%Y-%m-%d") - 1, 
         deep..s. = deep..s. / 3600, light..s. = light..s. / 3600) %>%
  select(Date, deep = deep..s., light = light..s.)

#Jointure des fichiers poids et activités
dfs  <- list(Activity, Poids, sleep_w)
Withings <- join_all(dfs)

# Finalisation des données Withings
Withings  <- Withings %>%
  select(Date, Steps, Distance, Cal_Burned, Weight, deep, light) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= "2015-01-01") %>%
  distinct(Date) 

#fusion des données de Jawbone et Withing
Withing_JB  <- rbind(Withings,Jawbone)
Withing_JB  <- distinct(Withing_JB, Date)

# jointure avec le fichier des calories
Global  <-  merge(Withing_JB, MFP, all = TRUE, sort = TRUE)
Global  <-  arrange(Global, desc(Date))
View(Global)

#Export au format CSV et visualisation des résultats

write.table(Global, "data_health.csv", row.names=FALSE, sep=",",dec=".", na=" ")