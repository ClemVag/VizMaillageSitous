# VISUALISATION DES LIAISONS ENTRE LES SITOUS
# JUILLET 2026 
# Développement : Clémence VAGNEUR (DTVO)
# Ce script permet de représenter les liaisons entre les ** types ** de sitous telles qu'existantes
# dans Sitouref.Ceci permet d'avoir une vue d'ensemble des liaisons existantes.
# Il n'est pas à utiliser en le lançant en une fois : l'idéal est de partir d'une liste
# de sitous à retirer et de liaisons à retirer vide (ou au moins réduite), puis de l'incrémenter
# au fil de l'analyse, en pointant les liaisons à conserver sur le graphe final, ou celles à
# expurger. 
# Le diagramme en cercle (layout = "circle") est bien adapté pour faire l'inventaire des liaisons et 
# de leur intérêt, à condition de les retirer au fur et à mesure qu'on les traite.




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
  `%nin%` <- negate(`%in%`)  
  
}
# IMPORT FICHIERS BASE ----
{l_base <- read_excel("data/OUTIL-016-2025_Maillage_types_sitous.xlsx", sheet = "Liaisons")
n_base <- read_excel("data/OUTIL-016-2025_Maillage_types_sitous.xlsx", sheet = "Etiquettes")

#Recherche et remplacement des caractères posant problème
n_base$Libellé<- gsub("ô", "o", n_base$Libellé)
n_base$Libellé<- gsub("é", "e", n_base$Libellé)
n_base$Libellé<- gsub("è", "e", n_base$Libellé)
n_base$Libellé <- gsub("ê", "e", n_base$Libellé)
n_base$Libellé <- gsub("&", " et ",n_base$Libellé)
n_base$Libellé <- gsub("'", "-", n_base$Libellé)# Attention, il ne faut pas d'apostrophe dans les noms, sinon ça plante !
n_base$Libellé <- gsub(":", " ", n_base$Libellé)
n_base$Libellé <- gsub("/", "-", n_base$Libellé)
n_base$Libellé <- gsub('"', "", n_base$Libellé)
n_base$Libellé <- gsub("_", "-", n_base$Libellé)
n_base$Libellé <- gsub(" ", "\n", n_base$Libellé) # Pour passage à la ligne entre chaque mot dans le graphique

# NETTOYAGE DU GRAPHE ----
noeuds_a_supprimer<-c(
                      "108", # Zone redevance prel eaux sup
                      "109", # Zone redevance prel eaux sout
                      "128", # Zone redevance bassin
                      "129", # Zone redevance pollution
                      "231", # Zone vulnérable
                      "232", # Zone redevabilité élevage
                      "235", # Zonage redevabilité chaleur
                      "238", # Zone SGL sout
                      "239", # Zone SGL sup
                      
                      "229", # Natura 2000 Habitats
                      "230", # Natura 2000 Oiseaux
                      
                      "218", # Contrat avec programme d'action
                      "219", # Contrat anim-assist technique
                      "220", # Contrat institutionnel
                      "221", # Enjeu de bassin
                      "222", # Enjeu thématique
                      "225", # SAGE

                      "227", # Eaux conchylocoles
                      "228", # Eaux de baignade
                      
                      "217", # Distributeur agréé de phytos
                      "236", # Action groupée d'activité éco
                      
                      # Suppressions spécifiques à DVO suite à étude des maillages
                      # Ne pas forcément conserver sur d'autres DT !
                      
                      "040", # Obstacle à l'écoulement (pas présent sur DVO)
                      "105", # Unité de compétence (ne semble plus utilisé)
                      "015", # Installation hydroélectrique
                      "104", # Plan d'eau
                      "035", # Lieu plan d'eau
                      "214", # ME Transition
                      "213", # ME Cotiere
                      
                      # Autres types de sitou à retirer dans le cadre de l'étude des pressions qualitatives
                      "134", # Délégataire
                      "013", # Exploitation agricole
                      "086", # Atelier agricole
                      "027", # Centre de traitement déchets
                      "011", # Périmètre gestion autosurveillance
                      "096", # Site non économique
                      
                      # Sitous à réactiver dans le cas d'une étude en lien avec le prélèvement
                      "140", # Bief de canal (utilisé uniquement pour la redevance canaux)
                      "233", # AAC,
                      "139", # Qualitomètre
                      "010", # Dispo collect milieu nat
                      "036", # Station littorale
                      "138", # Lieu prel littoral
                      "032", # Station ESU
                      "033", # Lieu qualité rivière
                      "020", # Point de comptage
                      "215", # ME Souterraine affleurante
                      "017", # Pt prel eaux sup
                      "018", # Pt d'eau
                      "226", # UH
                      
                      
                      
                      "")

liaisons_a_supprimer<-c(
                      # Liaisons non autorisées
                      "018-226", # Point d'eau --> UH
                      "017-226", # Point de prél eaux sup --> UH
                      "012-029", # Site indus --> STEP
                      "024-026", # SSCL --> Ouvrage rejet
                      
                      # Liaison retirées pour alléger
                      "006-137", # EPCI --> Commune
                      "137-024", # Commune --> SSCL 
                      
                      
                      #Liaisons inexistantes à DVO
                      "029-024", # STEU Collectivité --> SSCL
                      "024-025", # SSCL --> STEU indus
                      
                      # Liaisons à conserver
                      "026-211",
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


n_base2<-n_base |> 
  filter(Identifiant %nin% noeuds_a_supprimer)

l_base2<-l_base |> 
  mutate(liaison = paste0(Num_Type_Am,"-",Num_Type_Av)) |> 
  filter(Num_Type_Av %nin% noeuds_a_supprimer & Num_Type_Am %nin% noeuds_a_supprimer) |>
  filter(liaison %nin% liaisons_a_supprimer) |> 
  select(-liaison)



# CREATION DU GRAPHIQUE ----
graphe_sitous <- create_graph() |> 
  add_nodes_from_table(table = n_base2, label_col = "Libellé") |> 
  add_edges_from_table(
    table = l_base2,
    from_col = "Num_Type_Am",
    to_col = "Num_Type_Av",
    from_to_map = "Identifiant"
  ) |> 
  set_node_attrs(node_attr = fontsize, value = 3) |> 
  set_node_attrs(node_attr = width, value = 0.02) |> 
  set_edge_attrs(edge_attr = penwidth, value = 0.5) 
render_graph(
       graphe_sitous ,
       layout = "circle",
       output = NULL,
       as_svg = FALSE,
       title = NULL,
       width = NULL,
       height = NULL
   )
}
