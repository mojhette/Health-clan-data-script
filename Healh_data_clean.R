#Library utilisées
library(dplyr)
library(tidyr)
library(plyr)
library(lubridate)

#Systeme de location pour le format des dates
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

#choix du répertoire pour le changement et la concatenation des csv MFP
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

#Ma date d'anniversaire
bd  <- as.Date("1981-12-05", "%Y-%m-%d")

#Modification du fichier d'historique Jawbone
#changement du format de date / passage des secondes en heures pour le sommeil
#creation d'une variable du sommeil total
#données filtrées jusqu'au 1 janvier 2015
Jawbone <- Jawbone  %>%
  select(DATE, Cal_Burned = m_calories, Steps = m_steps, m_distance, Weight = weight, 
         s_deep, s_light) %>%
  mutate(Date = as.Date(as.character(DATE),"%Y%m%d"), Distance = (m_distance)/1000,  
         deep = s_deep/3600, light = s_light/3600) %>%
  mutate(Sleep = deep + light) %>%
  select(Date, Cal_Burned, Steps, Sleep, Distance, Weight, deep, light) %>%
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

#Modification du fichier de sommeil Withings
#changement du format de date / passage des secondes en heures pour le sommeil
#creation d'une variable du sommeil total
sleep_w  <- sleep_w %>%
  distinct(to.) %>%
  mutate(Date  = as.Date(to., "%Y-%m-%d") - 1, 
         deep..s. = deep..s. / 3600, light..s. = light..s. / 3600) %>%
  mutate(Sleep = deep..s. + light..s.) %>%
  select(Date, Sleep, deep = deep..s., light = light..s.)

#Jointure des fichiers poids, activités et sommeil
dfs  <- list(Activity, Poids, sleep_w)
Withings <- join_all(dfs)

# Finalisation des données Withings
# utilisation des données à partir du 1er janvier 2015
Withings  <- Withings %>%
  select(Date, Steps, Distance, Cal_Burned, Weight, deep, light, Sleep) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= "2015-01-01") %>%
  distinct(Date) 

#fusion des données de Jawbone et Withings
Withing_JB  <- rbind(Withings,Jawbone)
Withing_JB  <- distinct(Withing_JB, Date)

# jointure avec le fichier des calories
# creation d'une variable pour la difference de poids J - J-1
# creation d'une variable pour calculer les calories consommées au repos
# creation d'une variable pour voir la diff entre cal brulees, repos et conso
Global  <-  merge(Withing_JB, MFP, all = TRUE, sort = TRUE)
Global  <-  Global %>%
  mutate(Diff_Weight = Weight - lag(Weight)) %>%
  mutate(age = year(Date) - year(bd) ) %>% 
  mutate(Cal_rest = (13.707 * Weight) + (492.3*1.74) - (6.673*age) + 77.607) %>%
  mutate(ratio_cal = Calories - (Cal_rest + Cal_Burned) ) %>%
  select(Date, Weight, Diff_Weight, Calories, Steps, Sleep, Distance, 
         Cal_Burned, deep, light, Cal_rest, ratio_cal) %>%
  arrange(desc(Date))

# Affichage du resultat final dans R
View(Global)

#Export au format CSV et visualisation des résultats

write.table(Global, "data_health.csv", row.names=FALSE, sep=",",dec=".", na=" ")