#0. PACKAGES ----
library(tidyverse)
library(dplyr)
library(DiagrammeR)
library(readxl)
library(writexl)
library(rsvg)
library(xlsx)
library(openxlsx)
library(stringr)

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


# 3. EXPORT DU MAILLAGE EN MASSE ----


# AVANT D'OUVRIR LE FICHIER
# Supprimer tous les " et les . dans le fichier Excel par un rechercher-remplacer. 
# R ne sait pas le faire !

##3.1. Création des tables préparatoires ----
noeuds <- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                     sheet = "noeuds")
types_sitou<-noeuds %>% 
  select(identifiant,Type_Sitou) %>% 
  rename("No_Type_Sitou" = identifiant)
types_sitou$No_Type_Sitou <- as.character(types_sitou$No_Type_Sitou)

gene<- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                  sheet = "SitousAmontAval")
gene<-gene %>% 
  select(-Situation_Sitou,-Situation_Sitou_Am,-`Liaison_Sitou-Amont`) 

# On crée la table des noeuds de sitous indus
# Etape 1 : créer la liste des noeuds dont le type de sitou nous intéresse
# ET qui sont en relation avec des types de sitous qui nous intéresse 
noeuds_sitous_indus1<-gene %>% 
  select(1:4)
noeuds_sitous_indus2<-gene %>% 
  select(5:8)  
colnames(noeuds_sitous_indus2)=colnames(noeuds_sitous_indus1)

noeuds_sitous_indus<-rbind(noeuds_sitous_indus1,noeuds_sitous_indus2) 

noeuds_sitous_indus<-noeuds_sitous_indus %>% 
  distinct() %>% 
  select(-Type_Sitou)

noeuds_sitous_indus$Nom_Sitou<-gsub("ô","o",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("é","e",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("è","e",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("ê","e",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("&","et",noeuds_sitous_indus$Nom_Sitou)
noeuds_sitous_indus$Nom_Sitou<-gsub("'","-",noeuds_sitous_indus$Nom_Sitou) # Attention, il ne faut pas d'apostrophe dans les noms, sinon ça plante !
noeuds_sitous_indus<-noeuds_sitous_indus %>% 
  rename(identifiant="No_Sitou")

#Création de la liste des sites industriels
liste_sites_indus<-noeuds_sitous_indus %>% 
  filter(No_Type_Sitou==12) %>% 
  distinct() %>% 
  select(identifiant,Nom_Sitou)


noeuds_sitous_indus$Nom_Sitou<-gsub(" ","\n",noeuds_sitous_indus$Nom_Sitou) # Pour passage à la ligne entre chaque mot


# Etape 2 : créer la liste des liens
liens_sitous_indus<-gene %>% 
  select(No_Sitou_Am,No_Sitou) %>% 
   distinct()


## 3.2. LOOP SUR LA LISTE DES SITES ----

#for (k in 1:nrow(liste_sites_indus))
wb <- createWorkbook()
for (k in 1:10)
{
  code_site <- liste_sites_indus$identifiant[k]
  nom_site <- liste_sites_indus$Nom_Sitou[k]
  
  noeuds_site <- data.frame(
    identifiant = NA_character_,
    Nom_Sitou = NA_character_,
    No_Type_Sitou = NA_character_
  )
  noeuds_site_add <- noeuds_sitous_indus %>%
    filter(identifiant == code_site)
  noeuds_site <- rbind(noeuds_site, noeuds_site_add)
  noeuds_site <- na.omit(noeuds_site)
  noeuds_site2 <- noeuds_site
  n1 <- 1
  n2 <- 0
  
  
  ### On cherche avec quels autres sitou notre liste de sitous a des liens
  # D'abord les liens amont
  while (n2 != n1)
  {
    n1 <- nrow(noeuds_site)
    
    lien_site <- liens_sitous_indus %>%
      filter(No_Sitou %in% noeuds_site$identifiant) %>%
      select(No_Sitou_Am) %>%
      rename("No_Sitou" = No_Sitou_Am)
    

### On ajoute les sitous liés à la liste des sitous noeuds
    noeuds_site_add <- noeuds_sitous_indus %>%
      filter(identifiant %in% lien_site$No_Sitou)
    noeuds_site <- rbind(noeuds_site, noeuds_site_add)
    noeuds_site <- distinct(noeuds_site)
    n2 <- nrow(noeuds_site)
  }
  
  #Puis les liens aval
  n1 <- 1
  n2 <- 0
  

  while(n2 != n1)
  {
    n1 <- nrow(noeuds_site2)
    
    lien_site <- liens_sitous_indus %>%
      filter(No_Sitou_Am %in% noeuds_site2$identifiant) %>%
      select(No_Sitou)
    
    
    ### On ajoute les sitous liés à la liste des sitous noeuds
    noeuds_site_add <- noeuds_sitous_indus %>%
      filter(identifiant %in% lien_site$No_Sitou)
    noeuds_site2 <- rbind(noeuds_site2, noeuds_site_add)
    noeuds_site2 <- distinct(noeuds_site2)
    n2 <- nrow(noeuds_site2)
  }
  
  noeuds_site_def <- rbind(noeuds_site, noeuds_site2) %>%
    distinct()
  ### Fin de la boucle pour les noeuds
  
  ### On crée le fichier de liens
  lien_site_def <- liens_sitous_indus %>%
    filter(No_Sitou %in% noeuds_site_def$identifiant |
             No_Sitou_Am %in% noeuds_site_def$identifiant)
  
  
  ## 3.3. Extraction Excel des liaisons ----
 #  write.xlsx(noeuds_site_def, "liaisons.xlsx", sheetName = nom_site,
 #            append = TRUE)
  
  # Ajouter une feuille
  if(nchar(nom_site)<=20)
  {
  addWorksheet(wb, nom_site)
  }
  else
  {
   nom_feuille<-str_sub(nom_site,1,20) #récupère les caractères entre le 1er et le 20e
   addWorksheet(wb, nom_feuille)
  }  
  # Écrire les données dans la feuille
  noeuds_site_export<-noeuds_site_def
  noeuds_site_export<-left_join(noeuds_site_export,types_sitou, by = "No_Type_Sitou")
  
  noeuds_site_export$Nom_Sitou<-gsub("\n"," ",noeuds_site_export$Nom_Sitou)
  setColWidths(wb, sheet = k, cols = 1:3, width = "auto")
  writeData(wb, sheet = k, noeuds_site_export, startRow = 1, startCol = 1)
  
  
  
  
  ## 3.4. Paramétrage du graphe  ----
  
  graphe_sitous_indus <- create_graph() %>%
    add_nodes_from_table(table = noeuds_site_def, label_col = "Nom_Sitou") %>%
    add_edges_from_table(
      table = lien_site_def,
      from_col = "No_Sitou_Am",
      to_col = "No_Sitou",
      from_to_map = "identifiant"
    ) %>%
    set_node_attrs(node_attr = style, value = "filled") %>%
    #### Mise en forme par type de sitous ----
  ##### Type Sitou = site industriel (12) ----
  select_nodes(conditions = No_Type_Sitou == "12") %>%
    set_node_attrs_ws(node_attr = color, value = "coral3") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "coral3") %>%
    set_node_attrs_ws(node_attr = shape, value = "circle") %>%
    set_node_attrs_ws(node_attr = height, value = "0.35") %>%
    clear_selection()
  
  get_node_df(graphe_sitous_indus)
  
  ##### Type Sitou = atelier industriel (85) ----
  if (any(noeuds_site_def$No_Type_Sitou == 85))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "85") %>%
      set_node_attrs_ws(node_attr = color, value = "coral2") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "coral2") %>%
      set_node_attrs_ws(node_attr = shape, value = "circle") %>%
      set_node_attrs_ws(node_attr = height, value = "0.15") %>%
      clear_selection()
  }
  
  
  ##### Type Sitou = STEP indus (25) ----
  if (any(noeuds_site_def$No_Type_Sitou == 25))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "25") %>%
      set_node_attrs_ws(node_attr = color, value = "coral") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "coral") %>%
      set_node_attrs_ws(node_attr = shape, value = "square") %>%
      clear_selection()
  }
  
  ##### Type Sitou = ouvrage de rejet souterrain (102)----
  if (any(noeuds_site_def$No_Type_Sitou == 102))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "102") %>%
      set_node_attrs_ws(node_attr = color, value = "steelblue4") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "steelblue4") %>%
      set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
      set_node_attrs_ws(node_attr = orientation, value = 180) %>%
      clear_selection()
  }
  
  ##### Type Sitou = ouvrage de rejet superficiel (26)----
  if (any(noeuds_site_def$No_Type_Sitou == 26))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "26") %>%
      set_node_attrs_ws(node_attr = color, value = "steelblue1") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "steelblue1") %>%
      set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
      set_node_attrs_ws(node_attr = orientation, value = 180) %>%
      clear_selection()
  }
  
  ##### Type Sitou = point de prélèvement eau souterraine (18)----
  if (any(noeuds_site_def$No_Type_Sitou == 18))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "18") %>%
      set_node_attrs_ws(node_attr = color, value = "steelblue4") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "steelblue4") %>%
      set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
      clear_selection()
  }
  
  ##### Type Sitou = point de prélèvement eau superficielle (17)----
  if (any(noeuds_site_def$No_Type_Sitou == 17))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "17") %>%
      set_node_attrs_ws(node_attr = color, value = "steelblue1") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "steelblue1") %>%
      set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
      clear_selection()
  }
  
  ##### Type Sitou = point d'autosurveillance (224)----
  if (any(noeuds_site_def$No_Type_Sitou == 224))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "224") %>%
      set_node_attrs_ws(node_attr = color, value = "yellowgreen") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "yellowgreen") %>%
      set_node_attrs_ws(node_attr = shape, value = "polygon") %>%
      set_node_attrs_ws(node_attr = sides, value = "8") %>%
      set_node_attrs_ws(node_attr = height, value = "0.2") %>%
      set_node_attrs_ws(node_attr = width, value = "0.2") %>%
      clear_selection()
  }
  
  ##### Type Sitou = point d'autosurveillance SCL (242)----
  if (any(noeuds_site_def$No_Type_Sitou == 242))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "242") %>%
      set_node_attrs_ws(node_attr = color, value = "gray77") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "gray77") %>%
      set_node_attrs_ws(node_attr = shape, value = "polygon") %>%
      set_node_attrs_ws(node_attr = sides, value = "8") %>%
      set_node_attrs_ws(node_attr = height, value = "0.2") %>%
      set_node_attrs_ws(node_attr = width, value = "0.2") %>%
      clear_selection()
  }
  
  ##### Type Sitou = point de comptage (20)----
  if (any(noeuds_site_def$No_Type_Sitou == 20))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "20") %>%
      set_node_attrs_ws(node_attr = color, value = "goldenrod1") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "goldenrod1") %>%
      set_node_attrs_ws(node_attr = shape, value = "polygon") %>%
      set_node_attrs_ws(node_attr = sides, value = "8") %>%
      set_node_attrs_ws(node_attr = height, value = "0.2") %>%
      set_node_attrs_ws(node_attr = width, value = "0.2") %>%
      clear_selection()
  }
  
  ##### Type Sitou = STEP collectivité (29) ----
  if (any(noeuds_site_def$No_Type_Sitou == 29))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "29") %>%
      set_node_attrs_ws(node_attr = color, value = "mediumpurple1") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "mediumpurple1") %>%
      set_node_attrs_ws(node_attr = shape, value = "square") %>%
      clear_selection()
  }
  
  
  ##### Type Sitou = sous-système de collecte (24) ----
  if (any(noeuds_site_def$No_Type_Sitou == 24))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "24") %>%
      set_node_attrs_ws(node_attr = color, value = "mediumpurple1") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "mediumpurple1") %>%
      set_node_attrs_ws(node_attr = shape, value = "circle") %>%
      clear_selection()
  }
  
  ##### Type Sitou = système de collecte (243) ----
  if (any(noeuds_site_def$No_Type_Sitou == 243))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "243") %>%
      set_node_attrs_ws(node_attr = color, value = "mediumpurple4") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "mediumpurple4") %>%
      set_node_attrs_ws(node_attr = shape, value = "circle") %>%
      clear_selection()
  }
  
  ##### Type Sitou = périmètre de gestion d'autosurveillance (11) ----
  if (any(noeuds_site_def$No_Type_Sitou == 11))
  {
    graphe_sitous_indus <- graphe_sitous_indus  %>%
      select_nodes(conditions = No_Type_Sitou == "11") %>%
      set_node_attrs_ws(node_attr = color, value = "mintcream") %>%
      set_node_attrs_ws(node_attr = fillcolor, value = "mintcream") %>%
      set_node_attrs_ws(node_attr = shape, value = "circle") %>%
      clear_selection()
  }
  
  #####Couleur et taille du texte général, taille des flèches ----
  
  graphe_sitous_indus <- graphe_sitous_indus  %>%
    set_node_attrs(node_attr = fontcolor, value = "black") %>%
    set_node_attrs(node_attr = fontsize, value = "7") %>%
    set_edge_attrs(edge_attr = color, value = "gray33") %>%
    set_edge_attrs(edge_attr = penwidth, value = "0.5")
  
  get_node_df(graphe_sitous_indus)
  
  render_graph(
    graphe_sitous_indus,
    layout = "fr",  #alternatives : nicely,fr
    output = "graph",
    title = nom_site
  )
 
  export_graph(
    graphe_sitous_indus,
    file_name = paste0("diagrammes/", nom_site, ".png"),
    file_type = "PNG",
    title = nom_site,
    width = 1600,
    height = 1100
  )
  
  
}
# Enregistrer le fichier Excel avec le nom désiré
saveWorkbook(wb, file.path("noeuds_sitous.xlsx"), overwrite = TRUE) # fichier excel pour visualisation


## 3.5. Fichier légende ----
{
  graphe_légende <- create_graph() %>%
  add_nodes_from_table(table = noeuds_indus, label_col = "Type_Sitou") %>%
  set_node_attrs(node_attr = style, value = "filled") %>%
  #### Mise en forme par type de sitous ----
##### Type Sitou = site industriel (12) ----
select_nodes(conditions = identifiant == "12") %>%
  set_node_attrs_ws(node_attr = color, value = "coral3") %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "coral3") %>%
  set_node_attrs_ws(node_attr = shape, value = "circle") %>%
  set_node_attrs_ws(node_attr = height, value = "0.35") %>%
  clear_selection()

get_node_df(graphe_légende)

##### Type Sitou = atelier industriel (85) ----
if (any(noeuds_indus$identifiant == "85"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "85") %>%
    set_node_attrs_ws(node_attr = color, value = "coral2") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "coral2") %>%
    set_node_attrs_ws(node_attr = shape, value = "circle") %>%
    set_node_attrs_ws(node_attr = height, value = "0.15") %>%
    clear_selection()
}


##### Type Sitou = STEP indus (25) ----
if (any(noeuds_indus$identifiant == "25"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "25") %>%
    set_node_attrs_ws(node_attr = color, value = "coral") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "coral") %>%
    set_node_attrs_ws(node_attr = shape, value = "square") %>%
    clear_selection()
}

##### Type Sitou = ouvrage de rejet souterrain (102)----
if (any(noeuds_indus$identifiant == "102"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "102") %>%
    set_node_attrs_ws(node_attr = color, value = "steelblue4") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "steelblue4") %>%
    set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
    set_node_attrs_ws(node_attr = orientation, value = 180) %>%
    clear_selection()
}

##### Type Sitou = ouvrage de rejet superficiel (26)----
if (any(noeuds_indus$identifiant == "26"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "26") %>%
    set_node_attrs_ws(node_attr = color, value = "steelblue1") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "steelblue1") %>%
    set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
    set_node_attrs_ws(node_attr = orientation, value = 180) %>%
    clear_selection()
}

##### Type Sitou = point de prélèvement eau souterraine (18)----
if (any(noeuds_indus$identifiant == "18"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "18") %>%
    set_node_attrs_ws(node_attr = color, value = "steelblue4") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "steelblue4") %>%
    set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
    clear_selection()
}

##### Type Sitou = point de prélèvement eau superficielle (17)----
if (any(noeuds_indus$identifiant == "17"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "17") %>%
    set_node_attrs_ws(node_attr = color, value = "steelblue1") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "steelblue1") %>%
    set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
    clear_selection()
}

##### Type Sitou = point d'autosurveillance (224)----
if (any(noeuds_indus$identifiant == "224"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "224") %>%
    set_node_attrs_ws(node_attr = color, value = "yellowgreen") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "yellowgreen") %>%
    set_node_attrs_ws(node_attr = shape, value = "polygon") %>%
    set_node_attrs_ws(node_attr = sides, value = "8") %>%
    set_node_attrs_ws(node_attr = height, value = "0.2") %>%
    set_node_attrs_ws(node_attr = width, value = "0.2") %>%
    clear_selection()
}

##### Type Sitou = point d'autosurveillance SCL (242)----
if (any(noeuds_indus$identifiant == "242"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "242") %>%
    set_node_attrs_ws(node_attr = color, value = "gray77") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "gray77") %>%
    set_node_attrs_ws(node_attr = shape, value = "polygon") %>%
    set_node_attrs_ws(node_attr = sides, value = "8") %>%
    set_node_attrs_ws(node_attr = height, value = "0.2") %>%
    set_node_attrs_ws(node_attr = width, value = "0.2") %>%
    clear_selection()
}

##### Type Sitou = point de comptage (20)----
if (any(noeuds_indus$identifiant == "20"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "20") %>%
    set_node_attrs_ws(node_attr = color, value = "goldenrod1") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "goldenrod1") %>%
    set_node_attrs_ws(node_attr = shape, value = "polygon") %>%
    set_node_attrs_ws(node_attr = sides, value = "8") %>%
    set_node_attrs_ws(node_attr = height, value = "0.2") %>%
    set_node_attrs_ws(node_attr = width, value = "0.2") %>%
    clear_selection()
}

##### Type Sitou = STEP collectivité (29) ----
if (any(noeuds_indus$identifiant == "29"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "29") %>%
    set_node_attrs_ws(node_attr = color, value = "mediumpurple1") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "mediumpurple1") %>%
    set_node_attrs_ws(node_attr = shape, value = "square") %>%
    clear_selection()
}


##### Type Sitou = sous-système de collecte (24) ----
if (any(noeuds_indus$identifiant == "24"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "24") %>%
    set_node_attrs_ws(node_attr = color, value = "mediumpurple1") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "mediumpurple1") %>%
    set_node_attrs_ws(node_attr = shape, value = "circle") %>%
    clear_selection()
}

##### Type Sitou = système de collecte (243) ----
if (any(noeuds_indus$identifiant == "243"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "243") %>%
    set_node_attrs_ws(node_attr = color, value = "mediumpurple4") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "mediumpurple4") %>%
    set_node_attrs_ws(node_attr = shape, value = "circle") %>%
    clear_selection()
}

##### Type Sitou = périmètre de gestion d'autosurveillance (11) ----
if (any(noeuds_indus$identifiant == "11"))
{
  graphe_légende <- graphe_légende  %>%
    select_nodes(conditions = identifiant == "11") %>%
    set_node_attrs_ws(node_attr = color, value = "mintcream") %>%
    set_node_attrs_ws(node_attr = fillcolor, value = "mintcream") %>%
    set_node_attrs_ws(node_attr = shape, value = "circle") %>%
    clear_selection()
}

#####Couleur et taille du texte général, taille des flèches ----

graphe_légende <- graphe_légende  %>%
  set_node_attrs(node_attr = fontcolor, value = "black") %>% 
  set_node_attrs(node_attr = fontsize, value = "6")

render_graph(
  graphe_légende,
  layout = "nicely",  #alternatives : nicely,fr
  output = "graph",
  title = "Codes de représentation des sitous maillés"
)

export_graph(
  graphe_légende,
  file_name = paste0("diagrammes/Legende.png"),
  file_type = "PNG",
  title = "Codes de représentation des sitous maillés",
  width = 1600,
  height = 1100
)
}

