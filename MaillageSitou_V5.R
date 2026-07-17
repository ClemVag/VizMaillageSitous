# VISUALISATION DES LIAISONS ENTRE LES SITOUS
# Ce script permet de représenter les liaisons entre les sitous telles qu'existantes
# dans Sitouref.
# JUILLET 2026 (V5)
# Développement : Clémence VAGNEUR (DTVO)

#  PACKAGES ET FONCTIONS ----
{
  library(tidyverse)
  library(dplyr)
  library(DiagrammeR)
  library(readxl)
  library(writexl)
  library(rsvg)
  
  library(openxlsx)
  library(stringr)
  library(rmarkdown)
  library(quarto)
  library(here)
  library(conflicted)
  conflict_prefer("filter", "dplyr")
  conflict_prefer("write.xlsx", "openxlsx")
}

# FONCTIONS
`%nin%` <- negate(`%in%`)  


# PARAMETRAGE  ----

# Choix des sites : indiquer dans le code ci-dessous
# - 01    pour les sites industriels
# - 02    pour les sites agricoles
# - 03    pour le maillage assainissement incluant les entreprises déversant dans les STEU
# - 04    pour le maillage assainissement incluant les points de rejet issus du système de collecte
# - 05    pour le maillage des points de rejets se jetant dans une masse d'eau cours d'eau



# Visualisation : permet de sortir les fichiers PDF de visualisation des sitous
# DT  : pour le rangement

Représentation = "05" # Attention, mettre les chiffres entre guillemets !

visualisation = "O"

DT_maillage = "DVO"

#Changer le nom aussi sur le fichier de données le cas échéant.




# - 012 pour les sites industriels
# - 013 pour les sites agricoles
# - 029 pour le maillage assainissement
# - 211 pour le maillage masse d'eau cours d'eau
# - 026 pour le maillage point de rejet


# 2. EXECUTION ----

## 2.1 IMPORT DES FICHIERS DE BASE ----
{
  ### 2.1.1. FICHIER DE DONNEES ----
  # Le premier fichier de données est une extraction de Sitouref, qui contient sur un onglet l'ensemble des liaisons, 
  # sur un autre onglet l'ensemble des Sitous avec leur caractéristique du type de site. 
  # L'extraction provient de l'entrepôt de données. 
  #### 2.1.1.1. LIENS ----
  l_base <- read_excel("data/OUTIL-016-2025_Maillage_sitous.xlsx", sheet = "Liaisons")
  l_base <- l_base |> 
    select(-`Liaison_Sitou-Amont`)
  
  #### 2.1.1.1. NOEUDS ----
  n_base <- read_excel("data/OUTIL-016-2025_Maillage_sitous.xlsx", sheet = "Sitous")
  n_base<-n_base |>  
    select(-`Libellé UG`) |>  
    rename(identifiant = "No_Sitou")
  
  
  #Recherche et remplacement des caractères posant problème
  n_base$Nom_Sitou<- gsub("ô", "o", n_base$Nom_Sitou)
  n_base$Nom_Sitou<- gsub("é", "e", n_base$Nom_Sitou)
  n_base$Nom_Sitou<- gsub("è", "e", n_base$Nom_Sitou)
  n_base$Nom_Sitou <- gsub("ê", "e", n_base$Nom_Sitou)
  n_base$Nom_Sitou <- gsub("&", " et ",n_base$Nom_Sitou)
  n_base$Nom_Sitou <- gsub("'", "-", n_base$Nom_Sitou)# Attention, il ne faut pas d'apostrophe dans les noms, sinon ça plante !
  n_base$Nom_Sitou <- gsub(":", " ", n_base$Nom_Sitou)
  n_base$Nom_Sitou <- gsub("/", "-", n_base$Nom_Sitou)
  n_base$Nom_Sitou <- gsub('"', "", n_base$Nom_Sitou)
  n_base$Nom_Sitou <- gsub("_", "-", n_base$Nom_Sitou)
  
  ### 2.1.2. LISTE DE TYPES DE SITOUS ----
  
  types_sitou <- read_excel("data/Type_sitous.xlsx")
  # Recodage du type Sitou sur 3 caractères
  types_sitou$No_Type_Sitou <- case_when(
    nchar(types_sitou$No_Type_Sitou) == 1 ~ paste0("00", types_sitou$No_Type_Sitou),
    nchar(types_sitou$No_Type_Sitou) == 2 ~ paste0("0", types_sitou$No_Type_Sitou),
    TRUE ~ types_sitou$No_Type_Sitou
  )
  # write.xlsx(types_sitou,"data/types_sitou_3_char.xlsx")
  
  # Création d'un vecteur contenant uniquement les n° de sitous pour la table récapitulative
  num_type <- data.frame(types_sitou$No_Type_Sitou)
  colnames(num_type) = "base"
  
  ### 2.1.3. UH ----
  export_chemin<- read_excel("data/Sites_par_UH.xlsx")
  
  ### 2.1.4. MISE EN FORME FICHIER FINAL ----
  entete <- createStyle(
    fontSize = 10,           # Taille de la police
    textDecoration = "bold",     # Texte en gras
    halign = "center",       # Alignement horizontal centré
    valign = "center",       # Alignement vertical centré
    fgFill = "steelblue1",      
    wrapText = TRUE )
  
  autres_lignes<- createStyle(
    fontSize = 10,           # Taille de la police
    halign = "left",       # Alignement horizontal à gauche
    valign = "center",       # Alignement vertical centré
    wrapText = TRUE )
  
  
  ### 2.1.5. LISTE DE STYLES ----
  # Ces styles sont définis pour les graphes.
  
  styles <- list(
    # Type Sitou = site industriel (012) ou exploitation agricole (013)
    list(type = "012", color = "coral3", shape ="circle", extras = list(height = "0.35")),
    list(type = "013", color = "coral3", shape ="circle", extras = list(height = "0.35")),
    
    # Type Sitou = atelier industriel (085) ou atelier agricole (086)
    list(type ="085", color = "coral", shape ="circle", extras = list(height = "0.15")),
    list(type ="086", color = "coral", shape ="circle", extras = list(height = "0.15")),
    
    # Type Sitou = STEP indus (025)
    list(type = "025", color = "coral", shape ="square", extras = list()),
    
    # Type Sitou = ouvrage de rejet souterrain (102)
    list(type = "102", color = "steelblue4", shape ="triangle", extras = list(orientation = 180)),
    
    # Type Sitou = ouvrage de rejet superficiel (026)
    list(type = "026", color = "steelblue1", shape ="triangle", extras = list(orientation = 180)),
    
    # Type Sitou = point de prélèvement eau souterraine (018)
    list(type = "102", color = "steelblue4", shape ="triangle", extras = list()),
    
    # Type Sitou = point de prélèvement eau superficielle (017)
    list(type = "017", color = "steelblue1", shape = "triangle",  extras = list()),
    
    # Type Sitou = point d'autosurveillance (224)
    list(type = "224", color = "yellowgreen", shape = "polygon",   extras = list(sides = "8", height = "0.2", width = "0.2")),
    
    # Type Sitou = point d'autosurveillance SCL (242)
    list(type = "224", color = "gray77", shape ="polygon", extras = list(sides = "8", height = "0.2", width = "0.2")),
    
    # Type Sitou = point de comptage (020)
    list(type = "020", color = "goldenrod1", shape ="polygon", extras = list(sides = "8", height = "0.2", width = "0.2")),
    
    # Type Sitou = STEP collectivité (029)
    list(type = "029", color = "orchid1", shape ="square", extras = list()),
    
    # Type Sitou = sous-système de collecte (029)
    list(type = "209", color = "orchid1", shape ="circle", extras = list()),
    
    # Type Sitou = système de collecte (243)
    list(type = "243", color = "darkorchid1", shape ="circle", extras = list()),
    
    # Type Sitou = Masse d'eau bassin versant (212) ou Cours d'eau (211)
    list(type = "212", color = "steelblue1", shape ="circle", extras = list()),
    list(type = "211", color = "steelblue1", shape ="circle", extras = list()),
    
    # Type Sitou = Masse d'eau souterraine (215 / 216)
    list(type = "215", color = "steelblue4", shape ="circle", extras = list()),
    list(type = "216", color = "steelblue4", shape ="circle", extras = list()),
    
    # Type Sitou = Périmètre d'épandage (014)
    list(type = "014", color = "antiquewhite3", shape ="circle", extras = list())
  )
  
}

## 2.2. CREATION DES TABLES OBJETS ET LIAISONS ----

### 2.2.1. CREATION DE LA TABLE DES SITES SELON LE TYPE SOUHAITE ----
type_site = case_when(Représentation == "01" ~ "012",
                      Représentation == "02" ~ "013",
                      Représentation == "03" ~ "029",
                      Représentation == "04" ~ "029",
                      Représentation == "05" ~ "211",
                      TRUE ~ "Représentation non reconnue")

# Pour constituer la liste des sites à étudier, on récupère tous les sites de la DT
# correspondant au type de site attendu. 
liste_sites <- n_base |>  
  filter(No_Type_Sitou == type_site & DT %in% c(DT_maillage, "Autres"))  |>  
  distinct() |> 
  select(identifiant, Nom_Sitou)
# filter(identifiant %in% c("HR202A_211"))


# TODO : il sera nécessaire d'ajouter un moyen de sélectionner les cours d'eau pour le paramétrage 
# REJ_COURS_EAU, tous les cours d'eau étant enregistrés dans les "ressources bassin".

# On retire la colonne DT de la table des noeuds pour l'alléger. 
n_base<-n_base |>  
  select(-DT)

### 2.2.2. CREATION DE LA TABLE DES LIENS ----
l_base <- l_base  |> 
  mutate(liaison = paste0(str_sub(No_Sitou_Am, -3), "-", str_sub(No_Sitou, -3))) |>  
  distinct()

# Rétablissement du bon sens de certaines liaisons mal montées dans l'entrepôt.
# Liaisons à inverser 
{liaisons_à_inverser <-c("012-020",  # 012-020 Site indus - Point de comptage
                         "020-018",  # 020-018 Point de comptage-Point d'eau
                         "020-017",  # 020-017 Point de comptage-Point de prélèvement eau sup  
                         "018-216",  # 018-216 Point d'eau - masse d'eau sout   
                         "017-211",  # 017-211 Point de prélèvement eau sup - Masse d'eau sup  
                         "006-020",  # 006-020 EPCI - Point de comptage
                         "137-020"   # 137-020 Commune - Point de comptage
)
  
  
  l_base_temp <- l_base |> 
    filter(liaison %in% liaisons_à_inverser) |> 
    relocate(No_Sitou, .before = No_Sitou_Am) |>  
    mutate(liaison = paste0(str_sub(No_Sitou, -3), "-", str_sub(No_Sitou_Am, -3)))
  colnames(l_base_temp) = colnames(l_base)
  
  l_base <- l_base |> 
    filter(liaison %nin% liaisons_à_inverser) |> 
    rbind(l_base_temp)
  }

### 2.2.3. CREATION DE LA STRUCTURE DE LA TABLE RECAP POUR LA VUE D'ENSEMBLE FINALE----
nb_l_sites <- data.frame(c("code_site", "nom_site", "nb_liaisons"))
colnames(nb_l_sites) = "base"
nb_l_sites <- rbind(nb_l_sites, num_type)

### 2.2.3. CREATION DE LA STRUCTURE DE LA TABLE LISTE DES SITOUS LIES ----
liste_sitous_lies <- data.frame(
  UH = character(),
  code = character(),
  site = character(),
  identifiant = character(),
  Nom_Sitou = character(),
  No_Type_Sitou = character()
)

### 2.2.4. CHEMIN DE SAUVEGARDE DES FICHIERS ----
chemin<-file.path("resultat",DT_maillage,Représentation)
if (!dir.exists(chemin)) {
  dir.create(chemin, recursive = TRUE)
}


##2.3. LISTE DES OBJETS "DEBUT","FIN" DE MAILLAGE (BREAK) ET EXCLUS ----
# Deux listes sont utilisées : une en amont, et une en aval,
# pour inquer les sitous avant / après lesquels on va "couper" la recherche 
# de généalogie.
# Une liste complémentaire est utilisée pour effacer du diagramme final tous
# les objets qui ne sont pas nécessaires (exclude_list). 

### 2.3.1. ASSAINISSEMENT ----

if(type_site=="029")
{
  break_list_amont<-c("137", # Commune
                      "012", # Site indus
                      "006") # EPCI
  
  break_list_aval<-c("129", # Zone red poll
                     "211", # ME Cours d'eau
                     "212", # Bassin versant
                     "213", # ME Côtière
                     "214", # ME de transition
                     "216") # ME Souterraine
  
  exclude_list<-c()
}


### 2.3.2. INDUSTRIE ----
if(type_site=="012")
{
  break_list_amont<-c("129", # Zone red poll
                      "211", # ME Cours d'eau
                      "212", # Bassin versant
                      "213", # ME Côtière
                      "214", # ME de transition
                      "216") # ME Souterraine
  
  
  break_list_aval<-c("129", # Zone red poll
                     "211", # ME Cours d'eau
                     "212", # Bassin versant
                     "213", # ME Côtière
                     "214", # ME de transition
                     "216", # ME Souterraine
                     "242") # Point autosurveillance SSCL
  
  exclude_list<-c("242", # Point autosurveillance SSCL
                  "")
}

### 2.3.3. AGRICULTURE ----
if(type_site=="013")
{
  break_list_amont<-c("129", # Zone red poll
                      "211", # ME Cours d'eau
                      "212", # Bassin versant
                      "213", # ME Côtière
                      "214", # ME de transition
                      "216") # ME Souterraine
  
  
  break_list_aval<-c("129", # Zone red poll
                     "211", # ME Cours d'eau
                     "212", # Bassin versant
                     "213", # ME Côtière
                     "214", # ME de transition
                     "216") # ME Souterraine
  
  exclude_list<-c("242", # Point autosurveillance SSCL
                  "")
}

### 2.3.3. COURS D'EAU (REJET) ----
if(type_site=="211")
{
  break_list_amont<-c("012", # Site industriel
                      #"029", # STEU collectivité
                      "243", # SCL
                      "137", # Commune
                      "006", # EPCI
                      "134", # Délégataire
                      "")
  
  break_list_aval<-c( #"017", # Point prélèvement eau sup
    #"211", # ME Cours d'eau
    "212", # Bassin versant
    "213", # ME Côtière
    "214", # ME de transition
    "216", # ME Souterraine
    "017") 
  
  exclude_list<-c("017", # Point prélèvement eau sup
                  "137", # Commune
                  "006", # EPCI
                  "011") # Périmètre de gestion autosurveillance
  
  
  
  keep_l_list<-c("026-211",
                 "026-212",
                 "242-026",
                 "029-026",
                 "025-026",
                 "085-026",
                 
                 "137-029",
                 "006-029",
                 "024-029",
                 
                 "243-024",
                 "024-024",
                 "025-024",
                 "085-024",
                 
                 "025-014",
                 "085-014",
                 
                 "085-224",
                 "025-224",
                 "026-224",
                 "029-224",
                 
                 "012-085",
                 "012-025",
                 "025-025",
                 "085-025",
                 
                 "024-242",
                 
                 #INFILTRATION
                 "102-216",
                 "029-102",
                 "025-102",
                 "085-102"
  )
}


# 3. LOOP SUR LA LISTE DES SITES ----

for (k in 1:10)
  #for (k in 1:50)
  #k=60
  
{
  ## 3.1. INFO SITE ----
  
  # Informations relatives au site et à son futur chemin d'export
  code_site <- liste_sites$identifiant[k]
  nom_site <- liste_sites$Nom_Sitou[k]
  UH<-"test2"
  # UH <- ifelse(length(export_chemin$UH[which(export_chemin$`No interne Sitou` ==
  #                                              code_site)]) == 0,
  #              "Pas d'UH",
  #              export_chemin$UH[which(export_chemin$`No interne Sitou` == code_site)])
  # 
  
  ## 3.2. TABLE DES NOEUDS ----
  n_site <- data.frame(
    identifiant = character(),
    Nom_Sitou = character(),
    No_Type_Sitou = character()
  )
  n_breaklist_amont<-n_site
  n_breaklist_aval<-n_site
  
  n_site_add <- n_base |> 
    filter(identifiant == code_site)
  n_site <- n_site_add
  n_site_aval <- n_site
  n_site_amont<-n_site
  
  n1 <- 1
  n2 <- 0
  
  ### On cherche avec quels autres sitou notre liste de sitous a des liens
  # D'abord les liens amont
  while (n2 != n1)
  {
    n1 <- nrow(n_site_amont)
    
    if(n1>1) #On n'exécute pas cette étape sur la première itération pour éviter d'écarter le sitou central.
    {
      # On crée une liste de noeuds avec les codes issus de la "breaklist" 
      # issus de l'itération précédente 
      n_breaklist_temp<-n_site_amont |>  
        filter(No_Type_Sitou %in% break_list_amont)
      
      # On la compile avec la breaklist de l'itération précédente
      n_breaklist_amont<-distinct(rbind(n_breaklist_amont,n_breaklist_temp))
      
      
      # On retire les sitous issus de la breaklist de la liste de noeuds à considérer.
      n_site_amont<-n_site_amont |>  
        filter(No_Type_Sitou %nin% break_list_amont)
    }
    
    lien_site <- l_base |> 
      filter(No_Sitou %in% n_site_amont$identifiant) |> 
      select(No_Sitou_Am) |> 
      rename("No_Sitou" = No_Sitou_Am)
    
    
    ### On ajoute les sitous liés à la liste des sitous noeuds
    n_site_add <- n_base |> 
      filter(identifiant %in% lien_site$No_Sitou)  
    
    n_site_amont <- distinct(rbind(n_site_amont, n_site_add))
    n2 <- nrow(n_site_amont)
  }
  # On ajoute (en retirant les doublons) les noeuds dans la breaklist pour avoir
  # une liste finale de sitous amont.
  n_site_amont<-distinct(rbind(n_site_amont,n_breaklist_amont))
  
  # Idem les liens aval
  n1 <- 1
  n2 <- 0
  
  while (n2 != n1)
  {
    n1 <- nrow(n_site_aval)
    
    if(n1>1) #On n'exécute pas cette étape sur la première itération pour éviter d'écarter le sitou central.
    {
      # On crée une liste de noeuds avec les codes issus de la "breaklist" 
      # issus de l'itération précédente 
      n_breaklist_temp<-n_site_aval |>  
        filter(No_Type_Sitou %in% break_list_aval)
      
      # On la compile avec la breaklist de l'itération précédente
      n_breaklist_aval<-distinct(rbind(n_breaklist_aval,n_breaklist_temp))
      
      
      # On retire les sitous issus de la breaklist de la liste de noeuds à considérer.
      n_site_aval<-n_site_aval |>  
        filter(No_Type_Sitou %nin% break_list_aval)
    }
    
    lien_site <- l_base |> 
      filter(No_Sitou_Am %in% n_site_aval$identifiant) |> 
      select(No_Sitou)
    
    
    ### On ajoute les sitous liés à la liste des sitous noeuds
    n_site_add <- n_base |> 
      filter(identifiant %in% lien_site$No_Sitou)
    n_site_aval <- distinct(rbind(n_site_aval, n_site_add))
    n2 <- nrow(n_site_aval)
  } 
  
  # On ajoute (en retirant les doublons) les noeuds dans la breaklist pour avoir
  # une liste finale de sitous amont.
  n_site_aval<-distinct(rbind(n_site_aval,n_breaklist_aval))
  
  # La préparation dela liste des noeuds est terminée
  n_site_def<-distinct(rbind(n_site,n_site_amont,n_site_aval)) |> 
    filter(No_Type_Sitou %nin% exclude_list)
  
  #Partie industrie uniquement : on rajoute le SCL
  if (type_site %in% c("012", "013"))
  {
    #S'il existe un sitou "sous système de collecte", on cherche le sitou "syst de collecte associé"
    # Edit 21/05/2026 : bug : abandonné temporairement
    # if (nrow(n_site_def) > 1 &
    #     any(n_site_def$No_Type_Sitou == "024"))
    # {
    #   SCL <- data |> 
    #     filter(No_Sitou == n_site_def$identifiant[which(n_site_def$No_Type_Sitou == "024")]) |> 
    #     filter(str_sub(No_Sitou_Am, -3) == "243") |> 
    #     distinct()
    #   SCL2 <- n_base |> 
    #     filter(identifiant %in% SCL$No_Sitou_Am) |> 
    #     relocate(Nom_Sitou, .before = No_Type_Sitou)
    #   n_site_def <- rbind(n_site_def, SCL2)
    # }
  }  
  
  ## 3.3. TABLE DES LIENS ----
  l_site <- l_base |> 
    filter(
      No_Sitou %in% n_site_def$identifiant |
        No_Sitou_Am %in% n_site_def$identifiant
    ) |>  
    mutate(type_Am=str_sub(No_Sitou_Am, -3),
           type_Av=str_sub(No_Sitou, -3))
  
  # Suppression des liaisons avec les types de sitous exlus 
  # et élagage des branches connectées aux types de sitous des breaks lists.
  l_site <-l_site |>  
    filter(type_Am %nin% exclude_list & type_Av %nin% exclude_list) |>  
    filter(type_Am %nin% break_list_aval) |>  
    filter(type_Av %nin% break_list_amont)
  
  #Suppression de la colonne temporaire liaison
  l_site_def <- l_site |> 
    select(-liaison,type_Am,type_Av)
  
  # ### On réajuste le fichier de noeud "épuré" des liens indésirables
  # n_site_def <- n_site_def |> 
  #   filter(
  #     identifiant %in% l_site_def$No_Sitou_Am |
  #       identifiant %in% l_site_def$No_Sitou
  #   )
  
  ## 3.4. PREPARATION DU FICHIER DES SITOUS LIES----
  col_nom_site = rep.int(nom_site,nrow(n_site_def))
  col_code_site = rep.int(code_site,nrow(n_site_def))
  col_UH = rep.int(UH,nrow(n_site_def))
  liste_sitous_lies_temp <- cbind(col_UH,col_code_site,col_nom_site,n_site_def)
  colnames(liste_sitous_lies_temp) = colnames(liste_sitous_lies)
  liste_sitous_lies <- distinct(rbind(liste_sitous_lies,liste_sitous_lies_temp))
  
  # 4. VISUALISATION GRAPHE COMPLET ----
  if (visualisation == "O")
    #On reprend ce qui a été défini au moment du paramétrage
  {
    if(nrow(n_site_def)>2) 
      #On ne représente que les sitous qui ont au moins 2 liaisons
      #Ceux qui en ont moins seront quand même pris en compte dans le tableau
      #récap, mais le graphe n'est vraiment pas intéressant !
      # Idem, s'il y a plus de 50 liaisons, il y a vraisemblablement un problème !
      
    {
      ## 4.1. EXPORT VERS EXCEL POUR QUARTO  ----
      n_site_export <- n_site_def
      n_site_export <- left_join(n_site_export, types_sitou, by = "No_Type_Sitou")
      write.xlsx(n_site_export, "quarto_files/noeuds.xlsx")
      
      ## 4.2. GRAPHE COMPLET ET PARAMETRAGE  ----
      # On remplace les espaces par le caractère \n pour passer à la ligne dans le rendu graphique
      n_site_def$Nom_Sitou <- gsub(" ", "\n", n_site_def$Nom_Sitou) 
      
      # Initialisation du graphique
      graphe_sitous <- create_graph() |> 
        add_nodes_from_table(table = n_site_def, label_col = "Nom_Sitou") |> 
        add_edges_from_table(
          table = l_site_def,
          from_col = "No_Sitou_Am",
          to_col = "No_Sitou",
          from_to_map = "identifiant"
        ) |> 
        set_node_attrs(node_attr = style, value = "filled")
      
      # Application des styles de la liste définie au 2.1.5
      
      for (s in styles) {
        if (any(n_site_def$No_Type_Sitou == s$type)) {
          graphe_sitous <- graphe_sitous |>
            select_nodes(conditions = No_Type_Sitou == s$type) |>
            set_node_attrs_ws(node_attr = color,     value = s$color) |>
            set_node_attrs_ws(node_attr = fillcolor, value = s$color) |>
            set_node_attrs_ws(node_attr = shape,     value = s$shape)
          
          for (attr in names(s$extras)) {
            graphe_sitous <- set_node_attrs_ws(graphe_sitous, node_attr = !!sym(attr), value = s$extras[[attr]])
            
            # Explication de cette syntaxe : 
            # for (attr in names(s$extras))	    Boucle sur les noms des attributs
            # s$extras[[attr]]	                Récupère la valeur associée
            # sym(attr)	                        Convertit le nom (texte) en symbole R
            # !!	                              Injecte le symbole comme argument de la fonction - "évalue ce symbole maintenant et injecte-le tel quel"
          }
          
          graphe_sitous <- graphe_sitous |> clear_selection()
        }
      }
      
      # Application de la couleurs de police et de flèches
      graphe_sitous <- graphe_sitous  |> 
        set_node_attrs(node_attr = fontcolor, value = "black") |> 
        set_node_attrs(node_attr = fontsize, value = "7") |> 
        set_edge_attrs(edge_attr = color, value = "gray33") |> 
        set_edge_attrs(edge_attr = penwidth, value = "0.5")
      
      # Rendu du graphique 
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
        # title = nom_site,
        width = 1100,
        height = 800
      )
      
      ## 4.3. GRAPHE SIMPLIFIE ----
      # Ce graphe ne s'applique que pour la vision cours d'eau. 
      if(Représentation == "05")
      {
        ### 4.3.1. TABLE DE DONNEES SIMPLIFIEE ----
        
        # Dans le graphe simplifié, on veut pouvoir afficher uniquement l'origine
        # et la destination du rejet.
        # On va donc chercher à éliminer tous les types d'objets intermédiaires, 
        # à l'exception du point d'autosurveillance SSCL (224), à qui on peut rattacher
        # les informations d'autosurveillance.
        # Les objets "origine" sont :
        # - STEU collectivité locale (029)
        # - Système de collecte (243)
        # - Site industriel (012)
        
        # On commence par prendre la liste de toutes les liaisons du site.
        # Ce sera notre liste de référence pour la suite. 
        l_site_reference<-l_site_def |>
          select(No_Sitou_Am, No_Sitou)
        
        
        # On crée une "keep list" (liste à conserver) des liaisons existantes pour :
        # - les rejets reliés directement à une station, un SCL ou une entreprise en amont
        # - les rejets reliés à une masse d'eau ou à un bassin versant en aval
        # Cette liste sera incrémentée par la suite par les éléments à conserver sur le 
        # graphique simplifié. 
        
        keep_list <- l_site_def |> 
          filter((type_Av=="026" & type_Am %in% c("029","243","012")) | (type_Am=="026" & type_Av %in% c("211","212")))  |> 
          select(No_Sitou_Am,No_Sitou)
        
        # On crée une liste pour amorcer l'algorithme avec des sitous aval de type
        # point de rejet et des sitous amont qui ne soient pas des sitous "origine"
        # (STEU, système de collecte ou site industriel)
        l_int_simpl_01 <- l_site_def |>  
          filter(type_Av=="026" & type_Am %nin% c("029","243","012")) |> 
          select(No_Sitou_Am, No_Sitou)
        
        # On fait une première jointure entre la liste de toutes les liaisons du site et
        # la liste précédemment définie des liaisons entre points de rejet et sitous amont.
        # Cela permet de récupérer un niveau de liaison amont supplémentaire.
        # Le renommage de la colonne Rejet permettra de se simplifier l'extraction des données
        # qui nous intéressent par la suite. 
        l_int_simpl_02 <- l_site_reference |> 
          left_join(l_int_simpl_01,by=c("No_Sitou"="No_Sitou_Am")) |> 
          rename("Rejet"="No_Sitou.y") |> 
          filter(!is.na(Rejet))
        
        # On crée une table intermérdiaire l_int_simpl_03 qui servira à l'algorithme.
        # Son nombre de lignes sera utilisé pour savoir quand arrêter la boucle while.
        l_int_simpl_03<-l_int_simpl_02
        
        while(nrow(l_int_simpl_03)>0)
        {
          
          # Cas des systèmes de collecte 
          # Ce cas est particulier car on souhaite garder la liaison avec le point 
          # d'autosurveillance SCL 
          if(any(str_sub(l_int_simpl_03$No_Sitou_Am,-3) == "243"))
          {
            # On crée un tableau intermédiaire pour conserver les rejets reliés à un système de collecte.
            l_int_simpl_04 <- l_int_simpl_03 |> 
              filter(str_sub(No_Sitou_Am,-3) == "243")
            
            # On a sur le dataframe l_int_simpl_05 la liste desliaisons entre les SCL et les points AS SCL
            l_int_simpl_05<-l_int_simpl_04[c(1,ncol(l_int_simpl_04)-1)]
            colnames(l_int_simpl_05)=colnames(keep_list)
            
            # Et sur le dataframe l_int_simpl_06 la liste desliaisons entre les points AS SCL et les points de rejets
            l_int_simpl_06<-l_int_simpl_04[c(ncol(l_int_simpl_04)-1,ncol(l_int_simpl_04))]
            colnames(l_int_simpl_06)=colnames(keep_list)
            
            # On ajoute ces deux tableaux à la liste de liaisons à conserver 
            keep_list<-bind_rows(keep_list,l_int_simpl_05,l_int_simpl_06)
            
            # Et enfin, on retire ces lignes du tableau l_int_simpl_03 afin de ne plus les traiter.
            l_int_simpl_03<-l_int_simpl_03 |> 
              filter(No_Sitou_Am %nin% l_int_simpl_04$No_Sitou_Am)
          }
          
          # Cas des sites industriels
          # On va aller chercher tous les ascendants en ligne directe des points de rejet
          # tout en supprimant ceux qui transitent par un système de collecte (déjà pris
          # en compte avec les STEU).
          if(any(str_sub(l_int_simpl_03$No_Sitou_Am,-3) %in% c("012")))
          {
            l_int_simpl_07 <- l_int_simpl_03 |> 
              filter(str_sub(No_Sitou_Am,-3) == "012") |> 
              filter(if_all(everything(), ~ str_sub(.x, -3) != "024")) # Retire toutes les branches dont les rejets passent par des SCL
            
            # K2<- l_int_simpl_03 |> 
            #   filter(str_sub(No_Sitou_Am,-3) == "012") |> 
            #   filter(if_all(everything(), ~ str_sub(.x, -3) == "024"))
            
            l_int_simpl_08<- l_int_simpl_07[c(1,ncol(l_int_simpl_07))]
            colnames(l_int_simpl_08) = colnames(keep_list)
            
            keep_list<-bind_rows(keep_list,l_int_simpl_08)
            
            l_int_simpl_03<-l_int_simpl_03 |> 
              filter(No_Sitou_Am %nin% l_int_simpl_07$No_Sitou_Am)
          }
          
          l_int_simpl_03<-left_join(l_site_reference,l_int_simpl_03,by=c("No_Sitou"="No_Sitou_Am"), relationship = "many-to-many") |> 
            filter(!is.na(Rejet))
          
        }
        
        keep_list<-distinct(keep_list)
        
        # On "empile" les deux colonne de la liste de liaisons à conserver (keep_list)
        # pour obtenir une liste de noeuds à conserver. 
        noeuds_keep_list<-c(keep_list$No_Sitou_Am,keep_list$No_Sitou) |> 
          unique()
        
        # On récupère tous les sites qui ont au moins une liaison dans la Keep List
        # pour en faire la base du graphique simplifié. 
        n_site_simpl<-n_site_def |> 
          filter(identifiant %in% noeuds_keep_list)
        
        
        ### 4.3.2. RENDU GRAPHIQUE ----
        
        # Initialisation du graphique
        graphe_simpl <- create_graph() |> 
          add_nodes_from_table(table = n_site_simpl, label_col = "Nom_Sitou") |> 
          add_edges_from_table(
            table = keep_list,
            from_col = "No_Sitou_Am",
            to_col = "No_Sitou",
            from_to_map = "identifiant"
          ) |> 
          set_node_attrs(node_attr = style, value = "filled")
        
        # Application des styles de la liste définie au 2.1.5
        
        for (s in styles) {
          if (any(n_site_simpl$No_Type_Sitou == s$type)) {
            graphe_simpl <- graphe_simpl |>
              select_nodes(conditions = No_Type_Sitou == s$type) |>
              set_node_attrs_ws(node_attr = color,     value = s$color) |>
              set_node_attrs_ws(node_attr = fillcolor, value = s$color) |>
              set_node_attrs_ws(node_attr = shape,     value = s$shape)
            
            for (attr in names(s$extras)) {
              graphe_simpl <- set_node_attrs_ws(graphe_simpl, node_attr = !!sym(attr), value = s$extras[[attr]])
            }
            
            graphe_simpl <- graphe_simpl |> clear_selection()
          }
        }
        
        # Application de la couleurs de police et de flèches
        graphe_simpl <- graphe_simpl  |> 
          set_node_attrs(node_attr = fontcolor, value = "black") |> 
          set_node_attrs(node_attr = fontsize, value = "7") |> 
          set_edge_attrs(edge_attr = color, value = "gray33") |> 
          set_edge_attrs(edge_attr = penwidth, value = "0.5")
        
        # Rendu du graphique 
        render_graph(
          graphe_simpl,
          layout = "fr",  #alternatives : nicely,fr
          output = "graph",
          title = nom_site
        )
        
        # Export du graphique pour intégration dans Quarto
        export_graph(
          graphe_simpl,
          file_name = "quarto_files/diagramme_simpl.png",
          file_type = "PNG",
          # title = nom_site,
          width = 1100,
          height = 800
        )
        
        
      }
      
    }
    ## 4.4. EXPORT DANS LE DOSSIER DE L'UH ----
    
    chemin_UH<-file.path(chemin,UH)
    
    
    if (!dir.exists(chemin_UH)) {
      dir.create(chemin_UH, recursive = TRUE)
    }
    
    ifelse(nchar(nom_site)>20,substring(nom_site,1,20),nom_site)
    names_file<-paste0(nom_site,".pdf")
    
    
    quarto_render(here("Maillage_Quarto.qmd"),output_format = "pdf", 
                  output_file = names_file)
    
    file.rename(names_file, file.path(chemin_UH, names_file))  
    
  } 
  
  # 5. TABLES RECAPITULATIVES ----
  ## 5.1. COMPTAGE DU NOMBRE DE LIAISONS ----
  nb_n_site_def <- n_site_def |> 
    group_by(No_Type_Sitou) |> 
    summarize(nb = n())
  colnames(nb_n_site_def) = c("base", "nb")
  
  
  info_site <- tibble(c("code_site", "nom_site", "nb_liaisons"),
                      c(code_site, nom_site, sum(nb_n_site_def$nb)))
  colnames(info_site) = c("base", "nb")
  
  info_site <- rbind(info_site, nb_n_site_def)
  colnames(info_site) = c("base", k)
  
  nb_l_sites <- left_join(nb_l_sites, info_site, by = "base")
}

nb_l_sites <- left_join(nb_l_sites, types_sitou, by = c("base" ="No_Type_Sitou"))
nb_l_sites <- nb_l_sites |> 
  relocate(Type_Sitou, .after = "base")
nb_l_sites[1, 2] <- nb_l_sites[1, 1]
nb_l_sites[2, 2] <- nb_l_sites[2, 1]
nb_l_sites[3, 2] <- nb_l_sites[3, 1]
nb_l_sites <- nb_l_sites[, -1]

nb_l_sites_export <- as.data.frame(t(nb_l_sites))
colnames(nb_l_sites_export) = nb_l_sites_export[1, ]
nb_l_sites_export <- nb_l_sites_export |> 
  filter(code_site != "code_site")
for (i in ncol(nb_l_sites_export):1)
{
  if (all(is.na(nb_l_sites_export[i])))
  {
    nb_l_sites_export <- nb_l_sites_export[, -i]
  }
  
}

## Export avec mise en forme 
# Créer un fichier Excel avec les données et appliquer des styles
wb <- openxlsx::createWorkbook()

# Ajouter une feuille
addWorksheet(wb, "Recap")

# Écrire les données dans la feuille
writeData(wb, sheet = 1, nb_l_sites_export, startRow = 1, startCol = 1)

# Appliquer le style à la première ligne (en-tête)
addStyle(wb, sheet = 1, style = entete, rows = 1, cols = 1:ncol(nb_l_sites_export), gridExpand = TRUE)

# Ajuster automatiquement la largeur des colonnes en fonction du contenu
setColWidths(wb, sheet = 1, cols = 1:ncol(nb_l_sites_export), width = "auto")
# Créer un style avec retour à la ligne
wrapStyle <- createStyle(wrapText = TRUE)
# Ajouter un filtre sur toutes les colonnes
addFilter(wb, sheet = "Recap", row = 1, col = 1:ncol(nb_l_sites_export))
# Enregistrer le fichier dans le chemin défini au début
openxlsx::saveWorkbook(wb, file.path(chemin,"TableauRecap.xlsx"), overwrite = TRUE)

## 5.3. LISTE DES SITOUS LIES ----
liste_sitous_lies<- left_join(liste_sitous_lies,types_sitou,by="No_Type_Sitou") 
liste_sitous_lies<- liste_sitous_lies |>  
  select(-No_Type_Sitou)
liste_sitous_lies<-  liste_sitous_lies[-1,]
## Export avec mise en forme 
# Créer un fichier Excel avec les données et appliquer des styles
wb2 <- openxlsx::createWorkbook()

# Ajouter une feuille
addWorksheet(wb2, "liaisons")

# Écrire les données dans la feuille
writeData(wb2, sheet = 1, liste_sitous_lies, startRow = 1, startCol = 1)

# Appliquer le style à la première ligne (en-tête)
addStyle(wb2, sheet = 1, style = entete, rows = 1, cols = 1:ncol(liste_sitous_lies), gridExpand = TRUE)

# Ajuster automatiquement la largeur des colonnes en fonction du contenu
setColWidths(wb2, sheet = 1, cols = 1:ncol(liste_sitous_lies), width = "auto")
# Créer un style avec retour à la ligne
wrapStyle <- createStyle(wrapText = TRUE)
# Ajouter un filtre sur toutes les colonnes
addFilter(wb2, sheet = "liaisons", row = 1, col = 1:ncol(liste_sitous_lies))
# Enregistrer le fichier dans le chemin défini au début
openxlsx::saveWorkbook(wb2, file.path(chemin,"ListeSitousLiés.xlsx"), overwrite = TRUE)


# 6. VISUALISATION GRAPHE SCHEMATIQUE





