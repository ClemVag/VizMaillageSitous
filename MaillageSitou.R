# VISUALISATION DES LIAISONS ENTRE LES SITOUS
# Ce script permet de représenter les liaisons entre les sitous telles qu'existantes
# dans Sitouref.
# Septembre 2025
# Développement : Clémence VAGNEUR (DTVO)

# 0. PARAMETRAGE  ----

# / ! \ AVANT DE DEMARRER
#
# Supprimer tous les " dans le fichier Excel CV-Généalogie_des_sitous_[R].xlsx
# par un rechercher-remplacer.
# R ne sait pas le faire !

# Choix des sites : indiquer dans le code ci-dessous
# - 012 pour les sites industriels
# - 013 pour les sites agricoles

# Visualisation : permet de sortir les fichiers PDF de visualisation des sitous
# DT  : pour le rangement

type_site = "012"

visualisation = "O"

DT_maillage = "DVM"
#Changer le nom aussi sur le fichier de données le cas échéant.


{
  #1. PACKAGES ----
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

# 2. EXECUTION ----

  ## 2.1 IMPORT DES FICHIERS ET TABLES PREPARATOIRES ----
  
  ### 2.1.1. FICHIER DE DONNEES ----
  #### 2.1.1.1. LIAISONS ----
  data <- read_excel("data/CV-Généalogie_des_sitous_[R].xlsx", sheet = "Liaisons")
  data <- data %>%
    select(-`Liaison_Sitou-Amont`)
  
  #### 2.1.1.1. NOEUDS ----
  noeuds_sitous<- read_excel("data/CV-Généalogie_des_sitous_[R].xlsx", sheet = "Sitous")
  noeuds_sitous<-noeuds_sitous %>% 
    select(-`Libellé UG`) %>% 
    rename(identifiant = "No_Sitou")
  
  #Recherche et remplacement des caractères posant problème
  noeuds_sitous$Nom_Sitou<- gsub("ô", "o", noeuds_sitous$Nom_Sitou)
  noeuds_sitous$Nom_Sitou<- gsub("é", "e", noeuds_sitous$Nom_Sitou)
  noeuds_sitous$Nom_Sitou<- gsub("è", "e", noeuds_sitous$Nom_Sitou)
  noeuds_sitous$Nom_Sitou <- gsub("ê", "e", noeuds_sitous$Nom_Sitou)
  noeuds_sitous$Nom_Sitou <- gsub("&", " et ",noeuds_sitous$Nom_Sitou)
  noeuds_sitous$Nom_Sitou <- gsub("'", "-", noeuds_sitous$Nom_Sitou)# Attention, il ne faut pas d'apostrophe dans les noms, sinon ça plante !
  noeuds_sitous$Nom_Sitou <- gsub(":", " ", noeuds_sitous$Nom_Sitou)
  noeuds_sitous$Nom_Sitou <- gsub("/", "-", noeuds_sitous$Nom_Sitou)
 
  ### 2.1.2. LISTE DE TYPES DE SITOUS ----
  
  types_sitou <- read_excel("data/Type_sitous.xlsx")
  # Recodage du type Sitou sur 3 caractères
  types_sitou$No_Type_Sitou <- case_when(
    nchar(types_sitou$No_Type_Sitou) == 1 ~ paste0("00", types_sitou$No_Type_Sitou),
    nchar(types_sitou$No_Type_Sitou) == 2 ~ paste0("0", types_sitou$No_Type_Sitou),
    TRUE ~ types_sitou$No_Type_Sitou
  )
  # Création d'un vecteur contenant uniquement les n° de sitous pour la table récapitulative
  num_type <- data.frame(types_sitou$No_Type_Sitou)
  colnames(num_type) = "base"
  
  ## 2.2. CREATION DES TABLES OBJETS ET LIAISONS ----
  
  ### 2.2.1. CREATION DE LA TABLE DES SITES SELON LE TYPE SOUHAITE ----
   liste_sites <- noeuds_sitous %>% 
    filter(No_Type_Sitou == type_site & DT== DT_maillage)  %>%
    distinct() %>%
    select(identifiant, Nom_Sitou)
  
  noeuds_sitous<-noeuds_sitous %>% 
    select(-DT)
  ### 2.2.2. CREATION DE LA TABLE DES LIENS ----
  liens_sitous <- data %>%
    distinct()
  
  
  ### 2.2.3. CREATION DE LA STRUCTURE DE LA TABLE RECAP ----
  nb_liaison_sites <- data.frame(c("code_site", "nom_site", "nb_liaisons"))
  colnames(nb_liaison_sites) = "base"
  nb_liaison_sites <- rbind(nb_liaison_sites, num_type)
  
  
  # 3. LOOP SUR LA LISTE DES SITES ----
  ### 3.1. CREATION DES TABLES SPECIFIQUES PAR SITE ----
  
  for (k in 1:nrow(liste_sites))
  {
    code_site <- liste_sites$identifiant[k]
    nom_site <- liste_sites$Nom_Sitou[k]
    
    noeuds_site <- data.frame(
      identifiant = NA_character_,
      Nom_Sitou = NA_character_,
      No_Type_Sitou = NA_character_
    )
    noeuds_site_add <- noeuds_sitous %>%
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
      
      lien_site <- liens_sitous %>%
        filter(No_Sitou %in% noeuds_site$identifiant) %>%
        select(No_Sitou_Am) %>%
        rename("No_Sitou" = No_Sitou_Am)
      
      
      ### On ajoute les sitous liés à la liste des sitous noeuds
      noeuds_site_add <- noeuds_sitous %>%
        filter(identifiant %in% lien_site$No_Sitou)
      noeuds_site <- rbind(noeuds_site, noeuds_site_add)
      noeuds_site <- distinct(noeuds_site)
      n2 <- nrow(noeuds_site)
    }
    
    #Puis les liens aval
    n1 <- 1
    n2 <- 0
    
    
    while (n2 != n1)
    {
      n1 <- nrow(noeuds_site2)
      
      lien_site <- liens_sitous %>%
        filter(No_Sitou_Am %in% noeuds_site2$identifiant) %>%
        select(No_Sitou)
      
      
      ### On ajoute les sitous liés à la liste des sitous noeuds
      noeuds_site_add <- noeuds_sitous %>%
        filter(identifiant %in% lien_site$No_Sitou)
      noeuds_site2 <- rbind(noeuds_site2, noeuds_site_add)
      noeuds_site2 <- distinct(noeuds_site2)
      n2 <- nrow(noeuds_site2)
    }
    
    noeuds_site_def <- rbind(noeuds_site, noeuds_site2) %>%
      distinct() %>%
      filter(No_Type_Sitou != "242" &
               No_Type_Sitou != "011")  # on supprime les sitous qui ne nous intéressent pas
    # 242 = Points autosurveillance SCL
    # 011 = Périmètre d'autosurveillance
    
    #S'il existe un sitou "sous système de collecte", on cherche le sitou "syst de collecte associé"
    if (any(noeuds_site_def$No_Type_Sitou == "024"))
    {
      SCL <- data %>%
        filter(No_Sitou == noeuds_site_def$identifiant[which(noeuds_site_def$No_Type_Sitou == "024")]) %>%
        filter(str_sub(No_Sitou_Am,-3) == "243") %>%
        distinct()
      SCL2<-noeuds_sitous %>% 
        filter(identifiant %in% SCL$No_Sitou_Am)
      colnames(SCL2) = colnames(noeuds_site_def)
      noeuds_site_def <- rbind(noeuds_site_def, SCL2)
    }
    
    
    ### 3.1.1. SUPPRESSION DES LIENS SUPERFLUS ----
    
    ### On crée le fichier de liens
    lien_site_def <- liens_sitous %>%
      filter(
        No_Sitou %in% noeuds_site_def$identifiant |
          No_Sitou_Am %in% noeuds_site_def$identifiant
      ) %>%
      #On crée une colonne pour supprimer les liens qui ne sont pas à représenter
      mutate(liaison = paste0(str_sub(No_Sitou_Am, -3), "-", str_sub(No_Sitou, -3))) %>% 
      distinct()
    
    # Il sera nécessaire de modifier cette liste si on veut faire une représentation
    # centrée sur les systèmes d'assainissement des collectivités
    
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
    #Suppression de la liaison 243-011 Syst de collecte - PG autosurveillance
    if (any(lien_site_def$liaison == "243-011"))
    {
      lien_site_def <- lien_site_def %>%
        filter(liaison != "243-011")
    }
    #Suppression de la liaison 012-011 Site indus - PG autosurveillance
    if (any(lien_site_def$liaison == "012-011"))
    {
      lien_site_def <- lien_site_def %>%
        filter(liaison != "012-011")
    }
    
    #Suppression de la liaison 211-212 ME cours d'eau - BV dans les deux sens
    if (any(lien_site_def$liaison == "211-212"))
    {
      lien_site_def <- lien_site_def %>%
        filter(liaison != "211-212")
    }
    if (any(lien_site_def$liaison == "212-211"))
    {
      lien_site_def <- lien_site_def %>%
        filter(liaison != "212-211")
    }
    
    #Suppression de la liaison 211-212 ME cours d'eau - ME cours d'eau
    if (any(lien_site_def$liaison == "211-211"))
    {
      lien_site_def <- lien_site_def %>%
        filter(liaison != "211-211")
    }

    
    ### 3.1.2. INVERSION DE LIAISONS MAL MONTEES DANS L'ENTREPOT ----
    
    #Inversion de la liaison 012-020 Site indus - Point de comptage
    if (any(lien_site_def$liaison == "012-020"))
    {
      lien_site_def_temp <- lien_site_def %>%
        filter(liaison == "012-020") %>%
        relocate(No_Sitou, .before = No_Sitou_Am)
      colnames(lien_site_def_temp) = colnames(lien_site_def)
      
      lien_site_def <- lien_site_def %>%
        filter(liaison != "012-020") %>%
        rbind(lien_site_def_temp)
    }
    
    #Inversion de la liaison 020-018 Point de comptage-Point d'eau
    if (any(lien_site_def$liaison == "020-018"))
    {
      lien_site_def_temp <- lien_site_def %>%
        filter(liaison == "020-018") %>%
        relocate(No_Sitou, .before = No_Sitou_Am)
      colnames(lien_site_def_temp) = colnames(lien_site_def)
      
      lien_site_def <- lien_site_def %>%
        filter(liaison != "020-018") %>%
        rbind(lien_site_def_temp)
    }
    #Inversion de la liaison 020-017 Point de comptage-Point de prélèvement eau sup
    if (any(lien_site_def$liaison == "020-017"))
    {
      lien_site_def_temp <- lien_site_def %>%
        filter(liaison == "020-017") %>%
        relocate(No_Sitou, .before = No_Sitou_Am)
      colnames(lien_site_def_temp) = colnames(lien_site_def)
      
      lien_site_def <- lien_site_def %>%
        filter(liaison != "020-017") %>%
        rbind(lien_site_def_temp)
    }
    
    
    
    #Suppression de la colonne temporaire liaison
    lien_site_def <- lien_site_def %>%
      select(-liaison)
    
    ### On réajuste le fichier de noeud "épuré" des liens indésirables
    noeuds_site_def <- noeuds_site_def %>%
      filter(
        identifiant %in% lien_site_def$No_Sitou_Am |
          identifiant %in% lien_site_def$No_Sitou
      )
    
    ## 3.2. VISUALISATION ----
    if (visualisation == "O")
      #On reprend ce qui a été défini au moment du paramétrage
    {
      if(nrow(noeuds_site_def)>2) 
        #On ne représente que les sitous qui ont au moins 2 liaisons
        #Ceux qui en ont moins seront quand même pris en compte dans le tableau
        #récap, mais le graphe n'est vraiment pas intéressant !
        
        {
      ### 3.2.1. EXPORT VERS EXCEL POUR QUARTO  ----
      noeuds_site_export <- noeuds_site_def
      noeuds_site_export <- left_join(noeuds_site_export, types_sitou, by = "No_Type_Sitou")
      write.xlsx(noeuds_site_export, "quarto_files/noeuds.xlsx")
      
      ### 3.2.2. GRAPHE ET PARAMETRAGE  ----
      noeuds_site_def$Nom_Sitou <- gsub(" ", "\n", noeuds_site_def$Nom_Sitou) # Pour passage à la ligne entre chaque mot dans le graphique
      
      graphe_sitous <- create_graph() %>%
        add_nodes_from_table(table = noeuds_site_def, label_col = "Nom_Sitou") %>%
        add_edges_from_table(
          table = lien_site_def,
          from_col = "No_Sitou_Am",
          to_col = "No_Sitou",
          from_to_map = "identifiant"
        ) %>%
        set_node_attrs(node_attr = style, value = "filled")
        
      #### Mise en forme par type de sitous ----
      ##### Type Sitou = site industriel (12) ----
      if (any(noeuds_site_def$No_Type_Sitou == "012"))
      {
        graphe_sitous <- graphe_sitous  %>%
        select_nodes(conditions = No_Type_Sitou == "012") %>%
        set_node_attrs_ws(node_attr = color, value = "coral3") %>%
        set_node_attrs_ws(node_attr = fillcolor, value = "coral3") %>%
        set_node_attrs_ws(node_attr = shape, value = "circle") %>%
        set_node_attrs_ws(node_attr = height, value = "0.35") %>%
        clear_selection()
      
      }
      
      ##### Type Sitou = atelier industriel (85) ----
      if (any(noeuds_site_def$No_Type_Sitou == "085"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "085") %>%
          set_node_attrs_ws(node_attr = color, value = "coral") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "coral") %>%
          set_node_attrs_ws(node_attr = shape, value = "circle") %>%
          set_node_attrs_ws(node_attr = height, value = "0.15") %>%
          clear_selection()
      }
      
      
      ##### Type Sitou = STEP indus (25) ----
      if (any(noeuds_site_def$No_Type_Sitou == "025"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "025") %>%
          set_node_attrs_ws(node_attr = color, value = "coral") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "coral") %>%
          set_node_attrs_ws(node_attr = shape, value = "square") %>%
          clear_selection()
      }
      
      ##### Type Sitou = ouvrage de rejet souterrain (102)----
      if (any(noeuds_site_def$No_Type_Sitou == "102"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "102") %>%
          set_node_attrs_ws(node_attr = color, value = "steelblue4") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "steelblue4") %>%
          set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
          set_node_attrs_ws(node_attr = orientation, value = 180) %>%
          clear_selection()
      }
      
      ##### Type Sitou = ouvrage de rejet superficiel (26)----
      if (any(noeuds_site_def$No_Type_Sitou == "026"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "026") %>%
          set_node_attrs_ws(node_attr = color, value = "steelblue1") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "steelblue1") %>%
          set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
          set_node_attrs_ws(node_attr = orientation, value = 180) %>%
          clear_selection()
      }
      
      ##### Type Sitou = point de prélèvement eau souterraine (18)----
      if (any(noeuds_site_def$No_Type_Sitou == "018"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "018") %>%
          set_node_attrs_ws(node_attr = color, value = "steelblue4") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "steelblue4") %>%
          set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
          clear_selection()
      }
      
      ##### Type Sitou = point de prélèvement eau superficielle (17)----
      if (any(noeuds_site_def$No_Type_Sitou == "017"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "017") %>%
          set_node_attrs_ws(node_attr = color, value = "steelblue1") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "steelblue1") %>%
          set_node_attrs_ws(node_attr = shape, value = "triangle") %>%
          clear_selection()
      }
      
      ##### Type Sitou = point d'autosurveillance (224)----
      if (any(noeuds_site_def$No_Type_Sitou == "224"))
      {
        graphe_sitous <- graphe_sitous  %>%
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
      if (any(noeuds_site_def$No_Type_Sitou == "242"))
      {
        graphe_sitous <- graphe_sitous  %>%
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
      if (any(noeuds_site_def$No_Type_Sitou == "020"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "020") %>%
          set_node_attrs_ws(node_attr = color, value = "goldenrod1") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "goldenrod1") %>%
          set_node_attrs_ws(node_attr = shape, value = "polygon") %>%
          set_node_attrs_ws(node_attr = sides, value = "8") %>%
          set_node_attrs_ws(node_attr = height, value = "0.2") %>%
          set_node_attrs_ws(node_attr = width, value = "0.2") %>%
          clear_selection()
      }
      
      ##### Type Sitou = STEP collectivité (29) ----
      if (any(noeuds_site_def$No_Type_Sitou == "029"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "029") %>%
          set_node_attrs_ws(node_attr = color, value = "orchid1") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "orchid1") %>%
          set_node_attrs_ws(node_attr = shape, value = "square") %>%
          clear_selection()
      }
      
      
      ##### Type Sitou = sous-système de collecte (24) ----
      if (any(noeuds_site_def$No_Type_Sitou == "024"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "024") %>%
          set_node_attrs_ws(node_attr = color, value = "orchid1") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "orchid1") %>%
          set_node_attrs_ws(node_attr = shape, value = "circle") %>%
          clear_selection()
      }
      
      ##### Type Sitou = système de collecte (243) ----
      if (any(noeuds_site_def$No_Type_Sitou == "243"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "243") %>%
          set_node_attrs_ws(node_attr = color, value = "darkorchid1") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "darkorchid1") %>%
          set_node_attrs_ws(node_attr = shape, value = "circle") %>%
          clear_selection()
      }
      
      ##### Type Sitou = Exploitation agricole (13) ----
      if (any(noeuds_site_def$No_Type_Sitou == "013"))
      {
        graphe_sitous <- graphe_sitous  %>%
        select_nodes(conditions = No_Type_Sitou == "013") %>%
        set_node_attrs_ws(node_attr = color, value = "coral3") %>%
        set_node_attrs_ws(node_attr = fillcolor, value = "coral3") %>%
        set_node_attrs_ws(node_attr = shape, value = "circle") %>%
        set_node_attrs_ws(node_attr = height, value = "0.35") %>%
        clear_selection()
      
      }
      
      ##### Type Sitou = atelier agricole (86) ----
      if (any(noeuds_site_def$No_Type_Sitou == "086"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "086") %>%
          set_node_attrs_ws(node_attr = color, value = "coral") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "coral") %>%
          set_node_attrs_ws(node_attr = shape, value = "circle") %>%
          set_node_attrs_ws(node_attr = height, value = "0.15") %>%
          clear_selection()
      }
      
      ##### Type Sitou = Masse d'eau bassin versant (212) ou Cours d'eau (211) ----
      if (any(noeuds_site_def$No_Type_Sitou %in% c("212","211")))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou  %in% c("212","211")) %>%
          set_node_attrs_ws(node_attr = color, value = "steelblue1") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "steelblue1") %>%
          set_node_attrs_ws(node_attr = shape, value = "circle") %>%
          clear_selection()
      }
      
      ##### Type Sitou = Masse d'eau souterraine (215 / 216) ----
      if (any(noeuds_site_def$No_Type_Sitou %in% c("215","216")))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou  %in% c("215","216")) %>%
          set_node_attrs_ws(node_attr = color, value = "steelblue4") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "steelblue4") %>%
          set_node_attrs_ws(node_attr = shape, value = "circle") %>%
          clear_selection()
      }
      
      ##### Type Sitou = Périmètre d'épandage (014) ----
      if (any(noeuds_site_def$No_Type_Sitou == "014"))
      {
        graphe_sitous <- graphe_sitous  %>%
          select_nodes(conditions = No_Type_Sitou == "014") %>%
          set_node_attrs_ws(node_attr = color, value = "antiquewhite3") %>%
          set_node_attrs_ws(node_attr = fillcolor, value = "antiquewhite3") %>%
          set_node_attrs_ws(node_attr = shape, value = "circle") %>%
          clear_selection()
      }
      
      ##### Couleurs de police et de bords ----
      graphe_sitous <- graphe_sitous  %>%
        set_node_attrs(node_attr = fontcolor, value = "black") %>%
        set_node_attrs(node_attr = fontsize, value = "7") %>%
        set_edge_attrs(edge_attr = color, value = "gray33") %>%
        set_edge_attrs(edge_attr = penwidth, value = "0.5")
      
      render_graph(
        graphe_sitous,
        layout = "fr",  #alternatives : nicely,fr
        output = "graph",
        title = nom_site
      )
      
      export_graph(
        graphe_sitous,
        file_name = "quarto_files/diagramme.png",
        file_type = "PNG",
        title = nom_site,
        width = 1100,
        height = 800
      )
      
      
      
      ## 2.4. Export dans le dossier de l'UH ----
      export_chemin<- read_excel("data/Sites_par_UH.xlsx")
      UH<-export_chemin$UH[which(export_chemin$`No interne Sitou`==code_site)]
      
      if(type_site == "012")
      {
        chemin<-file.path(paste0("resultat/",DT,"/indus/",UH))
      }
      if(type_site == "013")
      {
        chemin<-file.path(paste0("resultat/",DT,"/agri/",UH))
      }
      
      if (!dir.exists(chemin)) {
        dir.create(chemin, recursive = TRUE)
      }
      
      ifelse(nchar(nom_site)>20,substring(nom_site,1,20),nom_site)
      names_file<-paste0(nom_site,".pdf")
      
      
      quarto_render(here("Maillage_Quarto.qmd"),output_format = "pdf", 
                    output_file = names_file)
      
      file.rename(names_file, file.path(chemin, names_file))  
      
   } }
    
    ## 3.3. TABLE RECAPITULATIVE ----
    # Comptage du nombre de liaisons
    nb_noeuds_site_def <- noeuds_site_def %>%
      group_by(No_Type_Sitou) %>%
      summarize(nb = n())
    colnames(nb_noeuds_site_def) = c("base", "nb")
    
    
    info_site <- tibble(c("code_site", "nom_site", "nb_liaisons"),
                        c(code_site, nom_site, sum(nb_noeuds_site_def$nb)))
    colnames(info_site) = c("base", "nb")
    
    info_site <- rbind(info_site, nb_noeuds_site_def)
    colnames(info_site) = c("base", k)
    
    nb_liaison_sites <- left_join(nb_liaison_sites, info_site, by = "base")
    
  }
  
  nb_liaison_sites <- left_join(nb_liaison_sites, types_sitou, by = c("base" =
                                                                        "No_Type_Sitou"))
  nb_liaison_sites <- nb_liaison_sites %>%
    relocate(Type_Sitou, .after = "base")
  nb_liaison_sites[1, 2] <- nb_liaison_sites[1, 1]
  nb_liaison_sites[2, 2] <- nb_liaison_sites[2, 1]
  nb_liaison_sites[3, 2] <- nb_liaison_sites[3, 1]
  nb_liaison_sites <- nb_liaison_sites[, -1]
  
  nb_liaison_sites_export <- as.data.frame(t(nb_liaison_sites))
  colnames(nb_liaison_sites_export) = nb_liaison_sites_export[1, ]
  nb_liaison_sites_export <- nb_liaison_sites_export %>%
    filter(code_site != "code_site")
  for (i in ncol(nb_liaison_sites_export):1)
  {
    if (all(is.na(nb_liaison_sites_export[i])))
    {
      nb_liaison_sites_export <- nb_liaison_sites_export[, -i]
    }
    
  }
  if (type_site == "012")
  {
    write.xlsx(nb_liaison_sites_export,
               paste0("resultat/",DT,"/indus/TableauRecap.xlsx"))
  }
  if (type_site == "013")
  {
    write.xlsx(nb_liaison_sites_export,
               paste0("resultat/",DT,"/agri/TableauRecap.xlsx"))
  }
  
}

