#0. PACKAGES ----
library(tidyverse)
library(dplyr)
library(DiagrammeR)
library(readxl)
library(writexl)

#1. IMPORT FICHIER DE DONNEES ----

## Graphe avec des liens sur tout le bassin ----
noeuds <- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                     sheet = "noeuds")


liens <- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                    sheet = "liens") 

#Les liens et les noeuds sont issus de tout le bassin. Les noeuds n'ayant aucune liaison ont été retirés
# à la main suite à une première édition du graphe. 

noeuds<-noeuds %>% 
  rename(identifiant="No_type_Sitou")
noeuds$Type_Sitou<-gsub("ô","o",noeuds$Type_Sitou)
noeuds$Type_Sitou<-gsub("é","e",noeuds$Type_Sitou)
noeuds$Type_Sitou<-gsub("è","e",noeuds$Type_Sitou)
noeuds$Type_Sitou<-gsub("'","-",noeuds$Type_Sitou) # Attention, il ne faut pas d'apostrophe dans les noms, sinon ça plante !
noeuds$Type_Sitou<-gsub(" ","\n",noeuds$Type_Sitou) # Passage à la ligne à chaque espace, meilleure lisibilité
noeuds_no<-noeuds %>% 
  mutate(Type_Sitou=paste0(noeuds$identifiant,"\n",Type_Sitou))

liens<-liens %>% 
  select(No_Type_Sitou,No_Type_Sitou_Am) %>% 
  distinct()

graphe<-create_graph() %>% 
  add_nodes_from_table(
    table = noeuds_no,
    label_col = "Type_Sitou")
#get_node_df(graphe)

graphe<-graphe %>% 
  add_edges_from_table(table = liens,
                       from_col = "No_Type_Sitou_Am",
                       to_col = "No_Type_Sitou",
                       from_to_map = "identifiant"
                       )

# get_edge_df(graphe)

render_graph(graphe, 
             layout = "nicely",
             output = "graph",
             title="Liaisons Sitous Bassin")
#Fichier de sortie MaillageSitousBassin.pdf

## Graphe spécifique aux liens entre sitous sur DVO ----
liens_DVO <- read_excel("CV-Généalogie_des_sitous_DVO.xlsx", 
                    sheet = "liens_DVO") 
# On garde la table noeuds de la partie précédente, cela permettra de visualiser plus facilement
# les noeuds sans liens le cas échéant.

graphe_DVO<-create_graph() %>% 
  add_nodes_from_table(
    table = noeuds,
    label_col = "Type_Sitou") %>% 
  add_edges_from_table(table = liens,
                       from_col = "No_Type_Sitou_Am",
                       to_col = "No_Type_Sitou",
                       from_to_map = "identifiant"
  )

render_graph(graphe_DVO, 
             layout = "nicely", #alternatives : nicely,fr
             output = "graph",
             title="Liaisons Sitous DVO")

## Graphe spécifique aux liens entre sitous sur DVO ----
liens_DVO <- read_excel("CV-Généalogie_des_sitous_DVO.xlsx", 
                    sheet = "liens_DVO") 
# On garde la table noeuds de la partie précédente, cela permettra de visualiser plus facilement
# les noeuds sans liens le cas échéant.

graphe_DVO<-create_graph() %>% 
  add_nodes_from_table(
    table = noeuds,
    label_col = "Type_Sitou") %>% 
  add_edges_from_table(table = liens,
                       from_col = "No_Type_Sitou_Am",
                       to_col = "No_Type_Sitou",
                       from_to_map = "identifiant"
  )

render_graph(graphe_DVO, 
             layout = "nicely", #alternatives : nicely,fr
             output = "graph",
             title="Liaisons Sitous DVO")

# 2. MAILLAGE INDUSTRIE ----
## 2.1. Maillage par type de sitou ----
noeuds <- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                     sheet = "noeuds")


liens <- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                    sheet = "liens") 

#Les liens et les noeuds sont issus de tout le bassin. Les noeuds n'ayant aucune liaison ont été retirés
# à la main suite à une première édition du graphe. 
noeuds <- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                     sheet = "noeuds")
noeuds<-noeuds %>% 
  rename(identifiant="No_type_Sitou")
noeuds$Type_Sitou<-gsub("ô","o",noeuds$Type_Sitou)
noeuds$Type_Sitou<-gsub("é","e",noeuds$Type_Sitou)
noeuds$Type_Sitou<-gsub("è","e",noeuds$Type_Sitou)
noeuds$Type_Sitou<-gsub("'","-",noeuds$Type_Sitou) # Attention, il ne faut pas d'apostrophe dans les noms, sinon ça plante !
noeuds$Type_Sitou<-gsub(" ","\n",noeuds$Type_Sitou)

noeuds_indus<-noeuds %>% 
  filter(Indus=="Oui") %>% 
  select(identifiant,Type_Sitou)



liens <- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                    sheet = "liens") 
liens_indus<-liens %>% 
  filter(No_Type_Sitou_Am %in% noeuds_indus$identifiant | No_Type_Sitou %in% noeuds_indus$identifiant ) %>% 
  filter(No_Type_Sitou_Am != No_Type_Sitou) %>% 
  distinct()



graphe_indus<-create_graph() %>% 
  add_nodes_from_table(
    table = noeuds_indus,
    label_col = "Type_Sitou") %>% 
  add_edges_from_table(table = liens_indus,
                       from_col = "No_Type_Sitou_Am",
                       to_col = "No_Type_Sitou",
                       from_to_map = "identifiant"
                      )
get_node_df(graphe_indus)

graphe_indus<- graphe_indus %>% 
  select_nodes_by_id(nodes = 12:15) %>% #Les numéros correspondent au numéros de lignes de la table noeuds_indus
  set_node_attrs_ws(node_attr = color, value = "yellow") %>%
  set_node_attrs_ws(node_attr = fontcolor, value = "black") %>%
  clear_selection()


render_graph(graphe_indus, 
             layout = "nicely", #alternatives : nicely,fr
             output = "graph",
             title="Liaisons Sitous industriels DVO")


## 2.2. Maillage pour un sitou ----
# AVANT D'OUVRIR LE FICHIER
# Supprimer tous les " et les . dans le fichier Excel par un rechercher-remplacer. 
# R ne sait pas le faire !


gene<- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                  sheet = "SitousAmontAval")
gene<-gene %>% 
  select(-Situation_Sitou,-Situation_Sitou_Am,-`Liaison_Sitou-Amont`) #%>% 
  #filter(No_Type_Sitou %in% noeuds_indus$identifiant & No_Type_Sitou_Am %in% noeuds_indus$identifiant )

# On crée la table des noeuds de sitous indus
# Etape 1 : créer la liste des noeuds dont le type de sitou nous intéresse
# ET qui sont en relation avec des types de sitous qui nous intéresse 
noeuds_sitous_indus1<-gene %>% 
  select(1:4)
noeuds_sitous_indus2<-gene %>% 
  select(5:8)  
colnames(noeuds_sitous_indus2)=colnames(noeuds_sitous_indus1)

noeuds_sitous_indus<-rbind(noeuds_sitous_indus1,noeuds_sitous_indus2) 
# Etape 2 : couper les ensembles isolés (abandonné car retire des sitous qu'on souhaite garder)
# On ne garde que ceux qui ont plus d'une relation (pas de "morceaux isolés")
#noeuds_sitous_indus_nb<-noeuds_sitous_indus%>% 
#  group_by(No_Sitou) %>% 
#  summarise(nb=n()) %>% 
#  filter(nb>1)

#noeuds_sitous_indus<-noeuds_sitous_indus %>% 
#  filter(No_Sitou %in% noeuds_sitous_indus_nb$No_Sitou) %>% 

noeuds_sitous_indus<-noeuds_sitous_indus %>% 
  distinct() %>% 
  select(-Type_Sitou)

noeuds_sitous_indus$Nom_Sitou<-gsub("ô","o",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("é","e",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("è","e",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("ê","e",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("&","et",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("'","-",noeuds_sitous_indus$Nom_Sitou) # Attention, il ne faut pas d'apostrophe dans les noms, sinon ça plante !
noeuds_sitous_indus$Nom_Sitou<-gsub(" ","\n",noeuds_sitous_indus$Nom_Sitou) # Pour passage à la ligne entre chaque mot
noeuds_sitous_indus<-noeuds_sitous_indus %>% 
  rename(identifiant="No_Sitou")

# Etape 2 : créer la liste des liens
liens_sitous_indus<-gene %>% 
  select(No_Sitou_Am,No_Sitou) %>% 
   distinct()

#  filter(No_Sitou_Am %in% noeuds_sitous_indus$identifiant & No_Sitou %in% noeuds_sitous_indus$identifiant) %>% 
#  distinct()

# Etape 3: réaliser le maillage pour 1 sitou
## 3.1 : on initialise avec le site
code_site <- "14032_012"
nom_site <-noeuds_sitous_indus$Nom_Sitou[which(noeuds_sitous_indus$identifiant==code_site)]
noeuds_site <- data.frame(identifiant = NA_character_, 
                          Nom_Sitou= NA_character_, 
                          No_Type_Sitou= NA_character_ )
noeuds_site_add <- noeuds_sitous_indus %>% 
  filter(identifiant==code_site)
noeuds_site<-rbind(noeuds_site,noeuds_site_add)
noeuds_site<-na.omit(noeuds_site)
noeuds_site2<-noeuds_site
n1<-1
n2<-0

## 3.2 Boucle à faire
### On cherche avec quels autres sitou notre liste de sitous a des liens
# D'abord les liens amont
while(n2!=n1)
{
n1<-nrow(noeuds_site)

lien_site<-liens_sitous_indus %>% 
  filter(No_Sitou %in% noeuds_site$identifiant) %>% 
  select(No_Sitou_Am) %>% 
  rename("No_Sitou"=No_Sitou_Am)


### On ajoute les sitous liés à la liste des sitous noeuds
noeuds_site_add <- noeuds_sitous_indus %>% 
  filter(identifiant %in% lien_site$No_Sitou)
noeuds_site<-rbind(noeuds_site,noeuds_site_add)
noeuds_site<-distinct(noeuds_site)
n2<-nrow(noeuds_site)
}

#Puis les liens aval
n1<-1
n2<-0


while(n2!=n1)
{
  n1<-nrow(noeuds_site2)
  
  lien_site<-liens_sitous_indus %>% 
    filter(No_Sitou_Am %in% noeuds_site2$identifiant) %>% 
    select(No_Sitou)
  
  
  ### On ajoute les sitous liés à la liste des sitous noeuds
  noeuds_site_add <- noeuds_sitous_indus %>% 
    filter(identifiant %in% lien_site$No_Sitou)
  noeuds_site2<-rbind(noeuds_site2,noeuds_site_add)
  noeuds_site2<-distinct(noeuds_site2)
  n2<-nrow(noeuds_site2)
}

noeuds_site_def<-rbind(noeuds_site,noeuds_site2) %>% 
  distinct()
### Fin de la boucle
### On crée le fichier de liens
lien_site_def<-liens_sitous_indus %>% 
  filter(No_Sitou %in% noeuds_site$identifiant | No_Sitou_Am %in% noeuds_site$identifiant) 



# Etape 4 : le graphenoeuds_site# Etape 4 : le graphe
graphe_sitous_indus<-create_graph() %>% 
  add_nodes_from_table(
    table = noeuds_site_def,
    label_col = "Nom_Sitou") %>% 
  add_edges_from_table(table = liens_sitous_indus,
                       from_col = "No_Sitou_Am",
                       to_col = "No_Sitou",
                       from_to_map = "identifiant"
  )
get_node_df(graphe_sitous_indus)




render_graph(graphe_sitous_indus, 
             layout = "fr", #alternatives : nicely,fr
             output = "graph",
             title= nom_site)


