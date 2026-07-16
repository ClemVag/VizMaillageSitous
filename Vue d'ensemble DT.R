# 0. PACKAGES ----
library(tidyverse)
library(dplyr)
library(DiagrammeR)
library(readxl)
library(writexl)
library(rsvg)
library(xlsx)
library(openxlsx)
library(stringr)
library(rmarkdown)
library(quarto)
library(here)

# 1. PARAMETRAGE  ----

# Choix des sites : indiquer dans le code ci-dessous
# - 012 pour les sites industriels
# - 013 pour les sites agricoles
# - 029 pour les STEP

# Visualisation : permet de sortir les fichiers PDF de visualisation des sitous
# DT  : pour le rangement

type_site = "012"

visualisation = "N"

DT_maillage = "DVO"
#Changer le nom aussi sur le fichier de données le cas échéant.



  
  # 2. EXECUTION ----
  
  ## 2.1 IMPORT DES FICHIERS ET TABLES PREPARATOIRES ----
  
  ### 2.1.1. FICHIER DE DONNEES ----
  #### 2.1.1.1. LIAISONS ----
sitous<- read_excel("data/OUTIL-016-2025_Maillage_sitous.xlsx", sheet = "Sitous")
sitous_DVO<-sitous %>% 
  filter(DT=="DVO") %>% 
  select(No_Sitou,Nom_Sitou)

data <- read_excel("data/OUTIL-016-2025_Maillage_sitous.xlsx", sheet = "Liaisons")
data <- data %>%
  select(-`Liaison_Sitou-Amont`) %>% 
  mutate(liaison = paste0(str_sub(No_Sitou_Am, -3), "-", str_sub(No_Sitou, -3))) %>% 
  filter(No_Sitou %in% sitous_DVO$No_Sitou | No_Sitou_Am %in% sitous_DVO$No_Sitou)

nb_liaisons<-data %>% 
  group_by(liaison) %>% 
  summarise(nb=n())

data_select<-data %>% 
  left_join(nb_liaisons,by="liaison") %>%
  left_join(sitous_DVO,by=c("No_Sitou_Am"="No_Sitou")) %>% 
  rename("Nom_Sitou_Am" = Nom_Sitou) %>% 
  relocate(Nom_Sitou_Am,.after = No_Sitou_Am) %>% 
  left_join(sitous_DVO,by="No_Sitou") %>% 
  relocate(Nom_Sitou,.after = No_Sitou) 

write.xlsx(data_select,"resultat/data_select.xlsx")

