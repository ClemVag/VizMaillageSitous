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
library(rmarkdown)
library(quarto)
library(here)

# 3. EXPORT DU MAILLAGE EN MASSE ----


# AVANT D'OUVRIR LE FICHIER
# Supprimer tous les " et les . dans le fichier Excel par un rechercher-remplacer. 
# R ne sait pas le faire !

##3.1. Création des tables préparatoires ----
noeuds <- read_excel("CV-Généalogie_des_sitous_test.xlsx", 
                     sheet = "noeuds")
types_sitou<-noeuds %>% 
  select(No_type_Sitou,Type_Sitou) %>% 
  rename("No_Type_Sitou" = No_type_Sitou)
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

for (k in 1:nrow(liste_sites_indus))
#for (k in 100:115)

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
    distinct() %>% 
    filter(No_Type_Sitou != c(242))  # on supprime les sitous qui ne nous intéressent pas
  # 242 = Points autosurveillance SCL
  
  
  
  
  ### Fin de la boucle pour les noeuds
  
  ### On crée le fichier de liens
  lien_site_def <- liens_sitous_indus %>%
    filter(No_Sitou %in% noeuds_site_def$identifiant |
             No_Sitou_Am %in% noeuds_site_def$identifiant) %>% 
    #On crée une colonne pour supprimer les liens qui ne sont pas à représenter
    mutate(liaison=paste0(str_sub(No_Sitou_Am,-3),"-",str_sub(No_Sitou,-3)))
   
    #Suppression de la liaison 029-224 liaison STEP coll - Point AS   
    if (any(lien_site_def$liaison == "029-224"))
    {
      lien_site_def <- lien_site_def %>%
        filter(liaison != "029-224")
    }
    #Suppression de la liaison 242-026 Point AS SCL - Rejet sup  
    if (any(lien_site_def$liaison == "242-026"))
    {
      lien_site_def <- lien_site_def %>%
        filter(liaison != "242-026")
    }

  #Suppression de la colonne temporaire liaison
  lien_site_def <- lien_site_def %>%
    select(-liaison)
  
  ### On réajuste le fichier de noeud "épuré" des liens indésirables
  noeuds_site_def<-noeuds_site_def %>% 
    filter(identifiant %in% lien_site_def$No_Sitou_Am | identifiant %in% lien_site_def$No_Sitou)
  
  
  ## 3.3. Extraction Excel des liaisons ----
  
  # Écrire les données dans la feuille
  noeuds_site_export<-noeuds_site_def
  noeuds_site_export<-left_join(noeuds_site_export,types_sitou, by = "No_Type_Sitou")
  
  noeuds_site_export$Nom_Sitou<-gsub("\n"," ",noeuds_site_export$Nom_Sitou)
  write.xlsx(noeuds_site_export,"quarto_files/noeuds.xlsx")
  
  
  
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
  
  render_graph(
    graphe_sitous_indus,
    layout = "fr",  #alternatives : nicely,fr
    output = "graph",
    title = nom_site
  )
  
  export_graph(
    graphe_sitous_indus,
    file_name = "quarto_files/diagramme.png",
    file_type = "PNG",
    title = nom_site,
    width = 1100,
    height = 800
  )
  
  
  
## 3.5. Export dans le dossier de l'UH ----
export_chemin<- read_excel("Sites_indus_par_UH.xlsx")
UH<-export_chemin$UH[which(export_chemin$`No interne Sitou`==code_site)]
  
  
  chemin<-file.path(paste0("resultat/",UH))
  if (!dir.exists(chemin)) {
    dir.create(chemin, recursive = TRUE)
  }
  
  names_file<-paste0(nom_site,".pdf")
  
  quarto_render(here("Maillage_Quarto.qmd"),output_format = "pdf", 
                output_file = names_file)
  
  file.rename(names_file, file.path(chemin, names_file))
  
  
}

