# =============================================================================
# DÉFI 1 – Où le risque de décrochage scolaire est-il le plus fort, par région,
#          et comment l'anticiper ?      (Burkina Faso, 13 régions x 2016-2025)
#
# SCRIPT v3 – INDICE DE RISQUE PAR RÉGION + MODÈLE PRÉDICTIF + CARTE D'ALERTE ESU
# Même méthodologie que 01_analyse_reference_probit.R :
#   label en 3 classes (déperdition + indice composite, terciles) ; robustesse du
#   label ; probit ordinal (référence) / logit (robustesse) / effets aléatoires /
#   forêt aléatoire ; anti-circularité ; validation temporelle ET par région ;
#   comparaison à la persistance ; alerte probabiliste 2026.
#
# CE QUI CHANGE PAR RAPPORT À LA v2
#   1) Base d'entrée : Panel_DOC_EHCVM2021.xlsx (panel + 7 variables EHCVM 2021).
#   2) Découpage : apprentissage 2017-2023 (année t-1 -> t), test 2024-2025.
#   3) Intégration de sources complémentaires (fichier optionnel, voir section 1B) :
#      fermetures ESU, PDI, EDS 2021 (mariage précoce), tableaux de bord primaire
#      (abandon, redoublement), annuaires (abandons par province), post-primaire
#      (transition). Une source absente est signalée, jamais inventée.
#   4) Nouveau livrable 1 : INDICE DE RISQUE COMPOSITE PAR RÉGION (section 8).
#   5) Nouveau livrable 2 : CARTE D'ALERTE croisant la probabilité 2026 et les
#      fermetures d'écoles ESU (section 8).
#
# MODE D'EMPLOI
#   1. Placer dans data/ : Panel_DOC_EHCVM2021.xlsx (obligatoire) et, si on les a,
#      donnees_complementaires.xlsx (modèle fourni) et un fond de carte
#      bfa_adm1.shp / bfa_adm1.geojson (facultatif).
#   2. Installer les packages une fois (ligne ci-dessous).
#   3. Exécuter tout le script : le dossier « sorties_indice_risque » contient
#      graphiques, classeur Excel, base labellisée, JSON du tableau de bord et
#      journal de session.
#
# Convention de lecture : QUOI = ce que fait le bloc ; POURQUOI = la raison.
#
# install.packages(c("readxl","dplyr","tidyr","ggplot2","MASS","ordinal",
#                    "randomForest","writexl","jsonlite","ggrepel","sf"))
# (sf est facultatif : seulement si on fournit un fond de carte)
# =============================================================================


# =============================================================================
# 0. CHARGEMENT DES OUTILS ET PARAMÈTRES
# =============================================================================
suppressPackageStartupMessages({
  library(readxl)     # lecture des classeurs Excel
  library(dplyr)      # manipulation de tableaux
  library(tidyr)      # remise en forme
  library(ggplot2)    # graphiques
})
# MASS, ordinal, randomForest, writexl, ggrepel, sf : appelés avec "::" pour que
# MASS::select() ne masque pas dplyr::select().

# QUOI : graine fixée. POURQUOI : k-means et forêt aléatoire utilisent du hasard.
set.seed(2026)

# QUOI : chemins des fichiers d'entrée.
# POURQUOI : centraliser les noms ici ; data/ du dépôt, sinon dossier courant.
chemin <- function(nom) if (file.exists(file.path("data", nom))) file.path("data", nom) else nom
FICHIER       <- chemin("Panel_DOC_EHCVM2021.xlsx")        # panel + EHCVM (obligatoire)
FICHIER_COMPL <- chemin("donnees_complementaires.xlsx")    # ESU, PDI, EDS... (facultatif)
FOND_CARTE    <- c(chemin("bfa_adm1.shp"), chemin("bfa_adm1.geojson"))  # facultatif

# QUOI : lien du modèle de référence et de robustesse. POURQUOI : comme en v2.
REF_LINK <- "probit"
ALT_LINK <- "logit"
SORTIE   <- "sorties_indice_risque"

# QUOI : dernière année d'APPRENTISSAGE (cible t entre 2017 et 2023).
# POURQUOI : consigne du défi : entraînement 2017-2023, test 2024-2025.
ANNEE_FIN_APPRENTISSAGE <- 2023

# QUOI : seuils d'alerte sur la probabilité de risque élevé en 2026.
SEUIL_ALERTE    <- 0.50
SEUIL_VIGILANCE <- 1/3

# QUOI : seuil de fermetures d'écoles (part des écoles fermées ou perdues) qui
# fait passer une région « Normal » en « Vigilance » dans la carte d'alerte.
# POURQUOI : la probabilité du modèle repose sur la persistance du score ; une
# région peut avoir une offre scolaire fortement dégradée sans que le score le
# traduise encore. Convention modifiable.
SEUIL_FERMETURE <- 0.10

# QUOI : durée de la cohorte (écart CP1 -> CM2). POURQUOI : comme en v2.
COHORTE_ANNEES <- 5

# QUOI : faut-il mettre les variables EHCVM (pauvreté, Gini) dans le MODÈLE PRÉDICTIF ?
# POURQUOI : elles sont toujours utilisées dans l'indice composite (section 8) et
# dans les profils (section 4). Dans le modèle, elles ne sont gardées que si elles
# améliorent la validation ; le script compare les deux versions (apport_ehcvm) et
# la valeur par défaut reflète ce constat (voir le rapport). Passer à TRUE pour
# forcer l'inclusion.
INCLURE_EHCVM_MODELE <- FALSE

# QUOI : poids des dimensions de l'indice composite (section 8).
# POURQUOI : poids égaux par défaut = hypothèse la plus neutre ; une dimension
# absente (donnée non fournie) est écartée et les poids sont renormalisés.
POIDS_INDICE <- c(edu_actuel = 1, anticipe = 1, pauvrete = 1,
                  fermetures = 1, pdi = 1, mariage = 1)

dir.create(SORTIE, showWarnings = FALSE)

polr_method <- function(link) if (link == "logit") "logistic" else "probit"
lien_nom    <- function(link) if (link == "logit") "logit" else "probit"
z           <- function(x) as.numeric(scale(x))
accord      <- function(a, b) mean(as.character(a) == as.character(b), na.rm = TRUE)

# QUOI : met un nom de région sous forme de clé (majuscules, sans accent, sans
# espace ni tiret). POURQUOI : jointure fiable entre fichiers d'origines différentes.
norm_reg <- function(x) gsub("[^A-Z]", "", toupper(iconv(x, to = "ASCII//TRANSLIT")))

# QUOI : nom de région lisible (Centre-Nord, Boucle du Mouhoun). POURQUOI : graphiques.
joli <- function(x) {
  y <- gsub("(^|[- ])([a-z])", "\\1\\U\\2", tolower(x), perl = TRUE)
  gsub(" Du ", " du ", y)
}


# =============================================================================
# 1. IMPORT ET CONTRÔLE DU PANEL
# =============================================================================
# OBJECTIF : base fiable, contrôlée, sans donnée inventée.

if (!file.exists(FICHIER)) stop("Fichier introuvable : ", FICHIER)

d <- read_excel(FICHIER, sheet = "Feuil1") |> arrange(id_region, annee)
d$cle <- norm_reg(d$region)

stopifnot(all(table(d$region) == length(unique(d$annee))))
cat("Panel :", n_distinct(d$region), "régions x", n_distinct(d$annee),
    "années =", nrow(d), "observations,", ncol(d), "colonnes\n")

na_avant <- colSums(is.na(d)); na_avant <- na_avant[na_avant > 0]
cat("Valeurs manquantes avant traitement :\n"); print(na_avant)

# QUOI : interpolation linéaire des éventuels manquants, région par région.
# POURQUOI : (le fichier corrigé n'en contient plus) garder le garde-fou de la v2.
interp <- function(x) {
  if (!anyNA(x) || sum(!is.na(x)) < 2) return(x)
  i <- seq_along(x); ok <- !is.na(x)
  approx(i[ok], x[ok], xout = i, rule = 2)$y
}
d <- d |> group_by(region) |> arrange(annee, .by_group = TRUE) |>
  mutate(across(where(is.numeric), interp)) |> ungroup()
stopifnot(!anyNA(d))

# QUOI : contrôles d'intégrité (les six de la v2 + deux sur l'EHCVM).
# POURQUOI : une base incohérente ne doit jamais être analysée.
controles <- data.frame(
  controle = c("Panel équilibré : chaque région a toutes les années",
               "Aucun doublon région-année",
               "Nouveaux entrants au CP1 <= effectifs du primaire",
               "Salles préscolaires >= structures préscolaires",
               "Achèvement et CEP <= 100 %",
               "Effectifs et nombre d'écoles strictement positifs",
               "EHCVM 2021 : une seule valeur par région (donnée transversale)",
               "EHCVM 2021 : incidences et Gini dans leurs bornes"),
  reussi = c(all(table(d$region) == length(unique(d$annee))),
             !anyDuplicated(d[c("id_region", "annee")]),
             all(d$nouv_cp1_t <= d$eff_prim_t),
             all(d$salles_presco_tot >= d$struct_presco_tot),
             all(d$tap_t <= 100 & d$cep_t <= 100),
             all(d$eff_prim_t > 0 & d$ecoles_tot > 0),
             all(sapply(grep("^ehcvm21_", names(d), value = TRUE),
                        function(v) all(tapply(d[[v]], d$region, function(s) length(unique(s))) == 1))),
             all(d$ehcvm21_incidence >= 0 & d$ehcvm21_incidence <= 100 &
                 d$ehcvm21_extreme   >= 0 & d$ehcvm21_extreme   <= 100 &
                 d$ehcvm21_gini > 0 & d$ehcvm21_gini < 1)))
cat("\nContrôles d'intégrité :\n"); print(controles)
stopifnot(all(controles$reussi))

totaux_nationaux <- d |> group_by(annee) |>
  summarise(eff_prim_total = sum(eff_prim_t), nouv_cp1_total = sum(nouv_cp1_t),
            ecoles_total = sum(ecoles_tot), eff_presco_total = sum(eff_presco_t),
            .groups = "drop")
cat("\nTotaux nationaux (somme des 13 régions) :\n"); print(as.data.frame(totaux_nationaux))


# =============================================================================
# 1B. INTÉGRATION DES SOURCES COMPLÉMENTAIRES
# =============================================================================
# OBJECTIF : brancher les sources du cahier des charges sans jamais en inventer.
# PRINCIPE : le classeur donnees_complementaires.xlsx contient une feuille par
#   source (voir le modèle fourni). Feuille absente ou vide = source « non
#   disponible » : elle est notée dans l'inventaire et la partie du calcul qui en
#   dépend est soit écartée, soit remplacée par un indicateur de repli étiqueté.
#   Feuilles attendues et colonnes :
#     esu_fermetures    : region, annee, ecoles_fermees, [ecoles_total_esu], [source]
#     pdi               : region, annee, pdi
#     eds2021           : region, mariage_precoce_pct
#     primaire_tdb      : region, annee, taux_abandon, taux_redoublement
#     annuaire_province : region, province, annee, abandons
#     post_primaire     : region, annee, taux_transition

# QUOI : lit une feuille facultative. POURQUOI : renvoie NULL si fichier, feuille
# ou données manquent, sans arrêter le script.
lire_compl <- function(feuille) {
  if (!file.exists(FICHIER_COMPL)) return(NULL)
  if (!feuille %in% readxl::excel_sheets(FICHIER_COMPL)) return(NULL)
  x <- as.data.frame(readxl::read_excel(FICHIER_COMPL, sheet = feuille))
  x <- x[rowSums(!is.na(x)) > 0, , drop = FALSE]
  if (nrow(x) == 0 || !"region" %in% names(x)) return(NULL)
  x$cle <- norm_reg(x$region)
  inconnues <- setdiff(x$cle, d$cle)
  if (length(inconnues)) warning(feuille, " : régions non reconnues : ",
                                 paste(unique(x$region[x$cle %in% inconnues]), collapse = ", "))
  x[x$cle %in% d$cle, , drop = FALSE]
}

# QUOI : jointure annuelle région-année, avec contrôle de doublons.
fusion_annuelle <- function(base, x, cols) {
  stopifnot(!anyDuplicated(x[c("cle", "annee")]))
  base |> left_join(x[c("cle", "annee", cols)], by = c("cle", "annee"))
}

inventaire <- data.frame(source = c("Panel primaire + préscolaire (tableaux de bord, annuaires)",
                                    "EHCVM 2021 (profils de pauvreté)",
                                    "Rapports ST-ESU (écoles fermées)",
                                    "PDI par région",
                                    "EDS 2021 (mariage précoce des filles)",
                                    "Tableaux de bord primaire (abandon, redoublement)",
                                    "Annuaires primaire (abandons par province)",
                                    "Tableaux de bord post-primaire (transition)"),
                         statut = "absente", utilisation = "", stringsAsFactors = FALSE)
inventaire$statut[1:2] <- "intégrée"
inventaire$utilisation[1] <- "Label de risque, prédicteurs, validation"
inventaire$utilisation[2] <- "Indice de pauvreté (ACP) : dimension de l'indice composite, profils, variante testée du modèle"

# --- Fermetures d'écoles ESU
esu <- lire_compl("esu_fermetures")
ESU_DISPO <- !is.null(esu)
if (ESU_DISPO) {
  if (!"ecoles_total_esu" %in% names(esu)) esu$ecoles_total_esu <- NA_real_
  esu <- esu |> left_join(d[c("cle", "annee", "ecoles_tot")], by = c("cle", "annee")) |>
    mutate(taux_fermeture_esu = ecoles_fermees / coalesce(ecoles_total_esu, ecoles_tot))
  d <- fusion_annuelle(d, esu, c("ecoles_fermees", "taux_fermeture_esu"))
  inventaire$statut[3] <- "intégrée"
  inventaire$utilisation[3] <- "Carte d'alerte, test d'extension, dimension de l'indice"
} else {
  inventaire$utilisation[3] <- "REPLI : déficit d'offre scolaire dérivé du panel (voir ci-dessous)"
}

# QUOI : indicateur de repli = part des écoles perdues par rapport au maximum
# observé jusque-là dans la région (1 - écoles / maximum cumulé).
# POURQUOI : sans série ESU, c'est la seule mesure de fermeture que le panel
# permette. Elle mêle fermetures et fusions/regroupements : à lire comme un
# déficit d'offre et non comme le décompte officiel des fermetures.
d <- d |> group_by(region) |> arrange(annee, .by_group = TRUE) |>
  mutate(deficit_offre = 1 - ecoles_tot / cummax(ecoles_tot)) |> ungroup()

# --- PDI (rapporté au nombre d'élèves du primaire)
pdi <- lire_compl("pdi")
if (!is.null(pdi)) {
  d <- fusion_annuelle(d, pdi, "pdi")
  d$pdi_par_1000 <- d$pdi / d$eff_prim_t * 1000
  inventaire$statut[4] <- "intégrée"
  inventaire$utilisation[4] <- "Dimension de l'indice composite (PDI pour 1 000 élèves), test d'extension"
}

# --- EDS 2021 (donnée transversale, une valeur par région)
eds <- lire_compl("eds2021")
if (!is.null(eds)) {
  stopifnot(!anyDuplicated(eds$cle))
  d <- d |> left_join(eds[c("cle", "mariage_precoce_pct")], by = "cle")
  inventaire$statut[5] <- "intégrée"
  inventaire$utilisation[5] <- "Dimension de l'indice composite"
}

# --- Tableaux de bord primaire : abandon, redoublement
tdb <- lire_compl("primaire_tdb")
if (!is.null(tdb)) {
  cols <- intersect(c("taux_abandon", "taux_redoublement"), names(tdb))
  d <- fusion_annuelle(d, tdb, cols)
  inventaire$statut[6] <- "intégrée"
  inventaire$utilisation[6] <- "Validation externe du label (corrélation) et test d'extension"
}

# --- Annuaires : abandons par province, agrégés à la région
prov <- lire_compl("annuaire_province")
if (!is.null(prov)) {
  prov_reg <- prov |> group_by(cle, annee) |>
    summarise(abandons_prov = sum(abandons, na.rm = TRUE), .groups = "drop") |> as.data.frame()
  d <- fusion_annuelle(d, prov_reg, "abandons_prov")
  d$abandons_prov <- d$abandons_prov / d$eff_prim_t * 100   # en % des effectifs
  inventaire$statut[7] <- "intégrée"
  inventaire$utilisation[7] <- "Abandons pour 100 élèves : validation du label et test d'extension"
}

# --- Post-primaire : transition
pp <- lire_compl("post_primaire")
if (!is.null(pp)) {
  d <- fusion_annuelle(d, pp, "taux_transition")
  inventaire$statut[8] <- "intégrée"
  inventaire$utilisation[8] <- "Test d'extension"
}

cat("\nInventaire des sources :\n"); print(inventaire)


# =============================================================================
# 2. CONSTRUCTION DU LABEL DE RISQUE  (inchangé par rapport à la v2)
# =============================================================================
d <- d |> group_by(region) |> arrange(annee, .by_group = TRUE) |>
  mutate(
    deperdition      = 1 - tap_t / tba_t,
    deperdition_coh5 = 1 - tap_t / lag(tba_t, COHORTE_ANNEES),
    var_eff          = (eff_prim_t / lag(eff_prim_t) - 1) * 100,
    ecart_parite     = abs(ips_tap - 1),
    var_ecoles       = (ecoles_tot / lag(ecoles_tot) - 1) * 100) |>
  ungroup()

d <- d |> mutate(r_tbs = -z(tbs_t), r_tap = -z(tap_t), r_cep = -z(cep_t),
                 r_eff = -z(var_eff), r_par = z(ecart_parite))
COMP <- c("r_tbs", "r_tap", "r_cep", "r_eff", "r_par")
d$indice_composite <- rowMeans(as.matrix(d[COMP]), na.rm = TRUE)
d$score_brut  <- (z(d$deperdition) + z(d$indice_composite)) / 2
d$score_risque <- z(d$score_brut)

seuils <- quantile(d$score_risque, probs = c(1/3, 2/3))
d$classe <- cut(d$score_risque, breaks = c(-Inf, seuils, Inf),
                labels = c("Faible", "Moyen", "Eleve"), ordered_result = TRUE)
cat("\nSeuils des terciles du score :", round(seuils, 3), "\n"); print(table(d$classe))

# 2.5 Indice de pauvreté EHCVM (nouveau)
# QUOI : première composante principale de six mesures de pauvreté 2021 (incidence,
# profondeur, sévérité, pauvreté alimentaire, extrême pauvreté, - log consommation).
# POURQUOI : ces variables sont quasi colinéaires (13 régions seulement) ; un
# indice unique évite d'estimer six coefficients sur 13 valeurs distinctes.
# Le Gini (inégalité) reste séparé : il mesure autre chose que le niveau de pauvreté.
ehc <- d |> filter(annee == min(annee)) |> as.data.frame()
M_pauv <- cbind(ehc[c("ehcvm21_incidence", "ehcvm21_profondeur", "ehcvm21_severite",
                      "ehcvm21_pauv_alim", "ehcvm21_extreme")],
                neg_log_conso = -log(ehc$ehcvm21_conso_pers))
acp_pauv <- prcomp(M_pauv, scale. = TRUE)
pc_pauv  <- acp_pauv$x[, 1]
if (cor(pc_pauv, ehc$ehcvm21_incidence) < 0) pc_pauv <- -pc_pauv   # plus de pauvreté = plus haut
pauv_tab <- data.frame(cle = ehc$cle, pauvrete_idx = as.numeric(scale(pc_pauv)))
d <- d |> left_join(pauv_tab, by = "cle")
cat("\nIndice de pauvreté EHCVM : part de variance du 1er axe =",
    round(summary(acp_pauv)$importance[2, 1], 3), "\n")


# =============================================================================
# 3. ROBUSTESSE DU LABEL  (inchangé + validation par l'abandon si disponible)
# =============================================================================
terciles <- function(x) cut(x, c(-Inf, quantile(x, c(1/3, 2/3), na.rm = TRUE), Inf),
                            labels = c("Faible", "Moyen", "Eleve"), ordered_result = TRUE)

cc <- complete.cases(d[COMP])
acp_poids <- prcomp(d[cc, COMP], scale. = TRUE)
pc1 <- acp_poids$x[, 1]
if (cor(pc1, d$indice_composite[cc]) < 0) pc1 <- -pc1
d$indice_acp <- NA_real_; d$indice_acp[cc] <- pc1
d$classe_acp <- terciles((z(d$deperdition) + z(d$indice_acp)) / 2)
d$classe_coh5 <- terciles((z(d$deperdition_coh5) + z(d$indice_composite)) / 2)

km <- kmeans(d$score_risque, centers = 3, nstart = 50)
ordre <- order(km$centers[, 1])
d$classe_km <- factor(c("Faible", "Moyen", "Eleve")[match(km$cluster, ordre)],
                      levels = c("Faible", "Moyen", "Eleve"), ordered = TRUE)

COMP_SANS_EFF <- setdiff(COMP, "r_eff")
d$indice_sans_eff <- rowMeans(as.matrix(d[COMP_SANS_EFF]))
d$classe_sanseff <- terciles((z(d$deperdition) + z(d$indice_sans_eff)) / 2)

sensibilite_cohorte <- do.call(rbind, lapply(5:7, function(k) {
  dep_k <- 1 - d$tap_t / ave(d$tba_t, d$region, FUN = function(v) dplyr::lag(v, k))
  sc_k <- (z(dep_k) + z(d$indice_composite)) / 2
  cl_k <- terciles(sc_k); ok <- !is.na(cl_k)
  m_k <- tapply(sc_k, d$region, mean, na.rm = TRUE)
  m_0 <- tapply(d$score_risque, d$region, mean)
  data.frame(decalage_ans = k, annees_utilisables = sum(ok),
             accord_avec_label = round(accord(d$classe[ok], cl_k[ok]), 3),
             correlation_rangs_regions = round(cor(m_k, m_0[names(m_k)], method = "spearman"), 3))
}))

robustesse_label <- data.frame(
  variante = c("Poids ACP", "Cohorte t-5 (2021-2025)", "k-means (3 groupes)",
               "Sans croissance des effectifs (choc 2023)"),
  accord_avec_label = c(accord(d$classe, d$classe_acp),
                        accord(d$classe[!is.na(d$classe_coh5)], d$classe_coh5[!is.na(d$classe_coh5)]),
                        accord(d$classe, d$classe_km),
                        accord(d$classe, d$classe_sanseff)))
robustesse_label$accord_avec_label <- round(robustesse_label$accord_avec_label, 3)
cat("\nAccord entre le label retenu et ses variantes :\n"); print(robustesse_label)
cat("\nSensibilité à la durée de cohorte :\n"); print(sensibilite_cohorte)

# 3.1 Validation externe du label par les abandons observés (si fournis)
# QUOI : corrélation de rang entre le score de risque et les taux d'abandon /
# redoublement des tableaux de bord et annuaires. POURQUOI : ces taux n'entrent
# pas dans le label ; s'ils lui sont corrélés, c'est une validation indépendante.
validation_externe <- NULL
for (v in intersect(c("taux_abandon", "taux_redoublement", "abandons_prov"), names(d))) {
  ok <- !is.na(d[[v]])
  if (sum(ok) >= 20) {
    validation_externe <- rbind(validation_externe, data.frame(
      indicateur = v, n = sum(ok),
      rho_spearman = round(cor(d$score_risque[ok], d[[v]][ok], method = "spearman"), 3)))
  }
}
if (!is.null(validation_externe)) { cat("\nValidation externe du label :\n"); print(validation_externe) }


# =============================================================================
# 4. EXPLORATION ET CARTOGRAPHIE SANS FOND DE CARTE  (inchangé + EHCVM)
# =============================================================================
classement <- d |> group_by(region) |>
  summarise(score_moyen = mean(score_risque),
            score_2025 = score_risque[annee == max(annee)],
            tendance_annuelle = coef(lm(score_risque ~ annee))[2],
            annees_risque_eleve = sum(classe == "Eleve"),
            annees_risque_faible = sum(classe == "Faible"),
            classe_2025 = as.character(classe[annee == max(annee)]),
            .groups = "drop") |>
  arrange(desc(score_moyen)) |>
  mutate(rang = row_number(), across(where(is.numeric), ~ round(.x, 3)))
cat("\nClassement des régions (score moyen décroissant) :\n"); print(as.data.frame(classement))

ordre_reg <- rev(classement$region)
d$region_f <- factor(d$region, levels = ordre_reg)
pal <- c(Faible = "#2a9d8f", Moyen = "#e9c46a", Eleve = "#c0392b")

g1 <- ggplot(d, aes(factor(annee), region_f, fill = score_risque)) +
  geom_tile(colour = "white") + geom_text(aes(label = substr(classe, 1, 1)), size = 3) +
  scale_fill_gradient2(low = "#2a9d8f", mid = "#ffffbf", high = "#c0392b", midpoint = 0, name = "Score") +
  labs(title = "Risque de décrochage : région x année",
       subtitle = "F = faible, M = moyen, E = élevé", x = NULL, y = NULL) + theme_minimal()
ggsave(file.path(SORTIE, "01_heatmap_risque.png"), g1, width = 9, height = 6, dpi = 200)

g2 <- ggplot(classement, aes(reorder(region, score_moyen), score_moyen, fill = score_moyen)) +
  geom_col() + coord_flip() +
  scale_fill_gradient2(low = "#2a9d8f", mid = "#ffffbf", high = "#c0392b", midpoint = 0, guide = "none") +
  labs(title = "Score moyen de risque, 2016-2025", x = NULL, y = "Score (z)") + theme_minimal()
ggsave(file.path(SORTIE, "02_classement_regions.png"), g2, width = 8, height = 5.5, dpi = 200)

g3 <- ggplot(d, aes(annee, score_risque)) +
  geom_hline(yintercept = c(seuils), linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = ANNEE_FIN_APPRENTISSAGE + 0.5, linetype = "dotted", colour = "grey40") +
  geom_line(colour = "#34495e") + geom_point(aes(colour = classe), size = 1.6) +
  scale_colour_manual(values = pal, name = "Classe") + facet_wrap(~ region, ncol = 4) +
  scale_x_continuous(breaks = c(2016, 2020, 2025)) +
  labs(title = "Trajectoires du score de risque",
       subtitle = "Pointillé vertical : début de la période de test (2024)", x = NULL, y = "Score (z)") +
  theme_minimal() + theme(strip.text = element_text(size = 7))
ggsave(file.path(SORTIE, "03_trajectoires.png"), g3, width = 11, height = 7, dpi = 200)

g4 <- d |> count(region_f, classe) |> ggplot(aes(region_f, n, fill = classe)) +
  geom_col() + coord_flip() + scale_fill_manual(values = pal, name = "Classe") +
  labs(title = "Nombre d'années par classe de risque", x = NULL, y = "Années") + theme_minimal()
ggsave(file.path(SORTIE, "04_repartition_classes.png"), g4, width = 8, height = 5.5, dpi = 200)

d <- d |> mutate(taille_ecole = eff_prim_t / ecoles_tot)

prof <- d |> group_by(region) |>
  summarise(tbs = mean(tbs_t), tba = mean(tba_t), tap = mean(tap_t), cep = mean(cep_t),
            deperdition = mean(deperdition), var_eff = mean(var_eff, na.rm = TRUE),
            ecart_parite = mean(ecart_parite), taille_ecole = mean(taille_ecole),
            tbs_presco = mean(tbs_presco_t),
            pauvrete = first(ehcvm21_incidence), gini = first(ehcvm21_gini),   # nouveau : EHCVM
            .groups = "drop")
M <- as.data.frame(prof[, -1]); rownames(M) <- prof$region
acp <- prcomp(M, scale. = TRUE)
cat("\nVariance expliquée par l'ACP :\n"); print(round(summary(acp)$importance[2:3, 1:4], 3))

png(file.path(SORTIE, "05_acp_biplot.png"), 1800, 1400, res = 200)
biplot(acp, cex = 0.7, main = "ACP des profils régionaux (moyennes 2016-2025, EHCVM incluse)")
dev.off()

cah <- hclust(dist(scale(M)), method = "ward.D2")
prof$profil <- cutree(cah, k = 3)
png(file.path(SORTIE, "06_cah_dendrogramme.png"), 1800, 1200, res = 200)
plot(cah, main = "Classification hiérarchique des régions (Ward)", xlab = "", sub = "")
rect.hclust(cah, k = 3, border = c("#2a9d8f", "#e9c46a", "#c0392b"))
dev.off()

profils_types <- prof |> group_by(profil) |>
  summarise(n_regions = n(), regions = paste(region, collapse = " ; "),
            across(c(tbs, tap, cep, deperdition, taille_ecole, tbs_presco, pauvrete),
                   ~ round(mean(.x), 2)), .groups = "drop")
cat("\nProfils types :\n"); print(as.data.frame(profils_types[, 1:3]))


# =============================================================================
# 5. MODÉLISATION : ANTICIPER LA CLASSE DE L'ANNÉE t À PARTIR DE L'ANNÉE t-1
# =============================================================================
# SPÉCIFICATION : par défaut, 9 prédicteurs de la v2 (INCLURE_EHCVM_MODELE = FALSE).
# La version avec EHCVM est toujours estimée pour comparaison (apport_ehcvm).
# ANTI-CIRCULARITÉ : TBS, TAP, CEP et l'écart admission-achèvement, qui fabriquent
# le label, ne sont PAS prédicteurs. Les variables EHCVM (pauvreté, Gini) ne
# servent pas non plus à fabriquer le label : elles sont admises.
# ATTENTION : l'EHCVM est mesurée en 2021 et constante dans le temps. Elle se
# comporte comme une caractéristique structurelle de la région, pas comme un
# signal annuel ; le modèle à effet aléatoire régional en tient compte.

d <- d |> mutate(log_eff_prim = log(eff_prim_t),
                 eff_par_struct_presco = eff_presco_t / struct_presco_tot,
                 salles_par_struct = salles_presco_tot / struct_presco_tot,
                 log_tbs_presco = log(tbs_presco_t))

X_BASE <- c("log_eff_prim", "taille_ecole", "log_tbs_presco", "eff_par_struct_presco",
            "salles_par_struct", "ips_tbs_presco", "ips_tba", "ips_tbs", "score_risque")
X_EHCVM <- c("pauvrete_idx", "ehcvm21_gini")
X0 <- c(X_BASE, X_EHCVM)                 # toutes les variables retardées et standardisées
X  <- paste0(X0, "_l1")
X_B <- paste0(X_BASE, "_l1")             # spécification « sans EHCVM »
XF <- if (INCLURE_EHCVM_MODELE) X else X_B   # spécification RETENUE du modèle

# Variables candidates pour le test d'extension (présentes selon les sources)
EXTRA <- intersect(c("var_ecoles", "deficit_offre", "taux_fermeture_esu", "taux_abandon",
                     "taux_redoublement", "abandons_prov", "taux_transition", "pdi_par_1000"),
                   names(d))

base <- d |> group_by(region) |> arrange(annee, .by_group = TRUE) |>
  mutate(across(all_of(X0), ~ lag(.x), .names = "{.col}_l1"),
         across(all_of(EXTRA), ~ lag(.x), .names = "{.col}_l1"),
         classe_l1 = lag(classe)) |>
  ungroup()

md <- base |> filter(!is.na(score_risque_l1)) |> as.data.frame()
md$region <- factor(md$region)
NIV <- levels(md$classe)

# QUOI : découpage temporel. Cible t = 2017..2023 -> apprentissage ; 2024-2025 -> test.
train <- md[md$annee <= ANNEE_FIN_APPRENTISSAGE, ]
test  <- md[md$annee >  ANNEE_FIN_APPRENTISSAGE, ]
cat("\nApprentissage :", nrow(train), "obs (", min(train$annee), "-", max(train$annee),
    ") | Test :", nrow(test), "obs (", min(test$annee), "-", max(test$annee), ")\n")
cat("Test : répartition des classes observées\n"); print(table(test$classe))

fabrique_std <- function(df) {
  mu <- colMeans(df[X]); sg <- apply(df[X], 2, sd)
  function(newdf) { newdf[X] <- sweep(sweep(as.matrix(newdf[X]), 2, mu, "-"), 2, sg, "/"); newdf }
}
std <- fabrique_std(train)
trS <- std(train); teS <- std(test)

form_b  <- as.formula(paste("classe ~", paste(X_B, collapse = " + ")))      # sans EHCVM
form_e  <- as.formula(paste("classe ~", paste(X,   collapse = " + ")))      # avec EHCVM
form    <- if (INCLURE_EHCVM_MODELE) form_e else form_b                     # retenue
form_cmp <- if (INCLURE_EHCVM_MODELE) form_b else form_e                    # comparaison
LIB_CMP  <- if (INCLURE_EHCVM_MODELE) "SANS EHCVM" else "AVEC EHCVM"
form_re <- as.formula(paste("classe ~", paste(XF, collapse = " + "), "+ (1 | region)"))

ajuste_polr <- function(df, link, f = form)
  suppressWarnings(MASS::polr(f, data = df, Hess = TRUE, method = polr_method(link)))

ajuste_clmm <- function(df, link) {
  tryCatch(ordinal::clmm(form_re, data = df, link = lien_nom(link), Hess = TRUE),
           error = function(e) { message("clmm (", link, ") : ", conditionMessage(e)); NULL })
}

pred_clmm <- function(fit, nd, link) {
  Fcdf <- if (link == "logit") plogis else pnorm
  eta <- as.matrix(nd[names(fit$beta)]) %*% fit$beta
  cum <- sapply(fit$Theta, function(t) Fcdf(t - as.numeric(eta)))
  cbind(Faible = cum[, 1], Moyen = cum[, 2] - cum[, 1], Eleve = 1 - cum[, 2])
}

m_ref  <- ajuste_polr(trS, REF_LINK)
m_alt  <- ajuste_polr(trS, ALT_LINK)
m_cmp  <- ajuste_polr(trS, REF_LINK, form_cmp)    # même modèle, avec/sans EHCVM
re_ref <- ajuste_clmm(trS, REF_LINK)
re_alt <- ajuste_clmm(trS, ALT_LINK)
rf <- randomForest::randomForest(form, data = trS, ntree = 500, importance = TRUE)

cat("\n--- Modèle de RÉFÉRENCE :", REF_LINK, "ordinal ---\n"); print(summary(m_ref))

# 5.1 Coefficients et effets marginaux
coef_tab <- function(m) {
  s <- summary(m)$coefficients[XF, , drop = FALSE]
  data.frame(coef = s[, "Value"], se = s[, "Std. Error"],
             p = 2 * pnorm(-abs(s[, "Value"] / s[, "Std. Error"])))
}
ame_eleve <- function(m, df) {
  sapply(XF, function(v) {
    plus <- df; plus[[v]] <- plus[[v]] + 1
    mean(predict(m, plus, type = "probs")[, "Eleve"] - predict(m, df, type = "probs")[, "Eleve"])
  })
}
cr <- coef_tab(m_ref); ca <- coef_tab(m_alt)
tab_coef <- data.frame(variable = sub("_l1$", "", XF),
                       coef_ref = cr$coef, p_ref = cr$p, ame_eleve_ref = ame_eleve(m_ref, trS),
                       coef_alt = ca$coef, p_alt = ca$p, ame_eleve_alt = ame_eleve(m_alt, trS),
                       meme_signe = sign(cr$coef) == sign(ca$coef), row.names = NULL)
if (REF_LINK == "logit") tab_coef$odds_ratio_ref <- exp(cr$coef)
if (ALT_LINK == "logit") tab_coef$odds_ratio_alt <- exp(ca$coef)
tab_coef <- tab_coef |> mutate(across(where(is.numeric), ~ round(.x, 4)))
names(tab_coef) <- sub("_ref$", paste0("_", REF_LINK, "_REF"), names(tab_coef))
names(tab_coef) <- sub("_alt$", paste0("_", ALT_LINK, "_robustesse"), names(tab_coef))
cat("\nCoefficients (variables standardisées) :\n"); print(tab_coef)

# 5.2 Effets aléatoires régionaux
tab_re <- NULL
if (!is.null(re_ref) && !is.null(re_alt)) {
  tab_re <- data.frame(variable = sub("_l1$", "", names(re_ref$beta)),
                       coef_re_ref = round(re_ref$beta, 4),
                       coef_re_alt = round(re_alt$beta[names(re_ref$beta)], 4), row.names = NULL)
  names(tab_re)[2:3] <- c(paste0("coef_clmm_", REF_LINK), paste0("coef_clmm_", ALT_LINK))
  cat("\nVariance de l'effet aléatoire régional :", REF_LINK,
      round(as.numeric(re_ref$ST[[1]])^2, 4), "|", ALT_LINK,
      round(as.numeric(re_alt$ST[[1]])^2, 4), "\n")
}

# 5.3 Tests d'extension : chaque variable candidate améliore-t-elle le modèle ?
# QUOI : pour chaque variable supplémentaire disponible (variation des écoles,
# déficit d'offre, fermetures ESU, abandon, PDI...), compare AIC et p-value avec
# et sans elle, sur exactement les mêmes lignes (apprentissage seul).
# POURQUOI : elles ne sont pas dans le modèle principal ; on ne les y met que
# si elles apportent quelque chose.
test_ext <- NULL
for (v in EXTRA) {
  vl <- paste0(v, "_l1")
  sous <- train[!is.na(train[[vl]]), ]
  if (nrow(sous) < 40) next
  sous[[vl]] <- as.numeric(scale(sous[[vl]]))
  sousS <- fabrique_std(sous)(sous)
  r <- tryCatch({
    f0 <- ajuste_polr(sousS, REF_LINK)
    f1 <- suppressWarnings(MASS::polr(update(form, as.formula(paste(". ~ . +", vl))),
                                      data = sousS, Hess = TRUE, method = polr_method(REF_LINK)))
    data.frame(variable_ajoutee = v, n_obs = nrow(sous),
               AIC_sans = round(AIC(f0), 2), AIC_avec = round(AIC(f1), 2),
               coef = round(unname(coef(f1)[vl]), 3),
               p = round(2 * pnorm(-abs(summary(f1)$coefficients[vl, "t value"])), 3))
  }, error = function(e) NULL)
  test_ext <- rbind(test_ext, r)
}
cat("\nTests d'extension :\n"); print(test_ext)


# =============================================================================
# 6. VALIDATION TEMPORELLE (TEST 2024-2025) ET PAR RÉGION
# =============================================================================
auc <- function(score, pos) {
  n1 <- sum(pos); n0 <- sum(!pos)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(rank(score)[pos]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
perf <- function(prob, y, nom) {
  pred <- factor(NIV[max.col(prob, ties.method = "first")], levels = NIV, ordered = TRUE)
  data.frame(modele = nom, exactitude = mean(pred == y),
             exactitude_a_une_classe_pres = mean(abs(as.integer(pred) - as.integer(y)) <= 1),
             erreur_absolue_moyenne = mean(abs(as.integer(pred) - as.integer(y))),
             auc_risque_eleve = auc(prob[, "Eleve"], y == "Eleve"))
}

p_ref  <- predict(m_ref,  teS, type = "probs")
p_alt  <- predict(m_alt,  teS, type = "probs")
p_rf   <- predict(rf,     teS, type = "prob")
p_cmp  <- predict(m_cmp,  teS, type = "probs")
y <- test$classe
p_naif <- sapply(NIV, function(k) as.numeric(test$classe_l1 == k)); colnames(p_naif) <- NIV

res <- list(perf(p_ref, y, paste0(REF_LINK, " ordinal (REFERENCE)")),
            perf(p_alt, y, paste0(ALT_LINK, " ordinal (robustesse)")),
            perf(p_rf,  y, "Foret aleatoire"),
            perf(p_naif, y, "Persistance (classe t-1)"),
            perf(p_cmp,  y, paste0(REF_LINK, " ordinal ", LIB_CMP)))
if (!is.null(re_ref)) res <- c(res, list(perf(pred_clmm(re_ref, teS, REF_LINK), y,
                                              paste0(REF_LINK, " ordinal a effets aleatoires"))))
if (!is.null(re_alt)) res <- c(res, list(perf(pred_clmm(re_alt, teS, ALT_LINK), y,
                                              paste0(ALT_LINK, " ordinal a effets aleatoires"))))
tab_perf <- do.call(rbind, res); tab_perf[-1] <- round(tab_perf[-1], 3)
cat("\nPerformance sur la période de test (", min(test$annee), "-", max(test$annee), ") :\n", sep = "")
print(tab_perf)

# QUOI : comparaison avec / sans EHCVM (AIC d'apprentissage et AUC de test).
# (apprentissage, test 2024-2025 et validation par région : voir plus bas pour cette dernière)
comparaison_ehcvm <- data.frame(
  modele = if (INCLURE_EHCVM_MODELE) c("Avec EHCVM (retenu)", "Sans EHCVM") else c("Sans EHCVM (retenu)", "Avec EHCVM"),
  AIC_apprentissage = round(c(AIC(m_ref), AIC(m_cmp)), 2),
  exactitude_test = round(c(perf(p_ref, y, "")$exactitude, perf(p_cmp, y, "")$exactitude), 3),
  auc_test = round(c(perf(p_ref, y, "")$auc_risque_eleve, perf(p_cmp, y, "")$auc_risque_eleve), 3))
cat("\nApport de l'EHCVM :\n"); print(comparaison_ehcvm)

pred_ref <- factor(NIV[max.col(p_ref, ties.method = "first")], levels = NIV)
cat("\nMatrice de confusion (", REF_LINK, ", référence) : lignes = observé\n")
print(table(Observe = y, Predit = pred_ref))

concordance <- data.frame(
  indicateur = c("Correlation des P(eleve) ref / robustesse", "Accord des classes predites ref / robustesse"),
  valeur = round(c(cor(p_ref[, "Eleve"], p_alt[, "Eleve"]),
                   accord(pred_ref, factor(NIV[max.col(p_alt, ties.method = "first")]))), 3))
cat("\nConcordance des deux modèles de référence :\n"); print(concordance)

imp <- data.frame(variable = sub("_l1$", "", rownames(rf$importance)),
                  importance_gini = round(rf$importance[, "MeanDecreaseGini"], 3), row.names = NULL) |>
  arrange(desc(importance_gini))
png(file.path(SORTIE, "07_importance_foret.png"), 1600, 1100, res = 200)
par(mar = c(5, 12, 3, 1))
barplot(rev(imp$importance_gini), names.arg = rev(imp$variable), horiz = TRUE, las = 1,
        col = "#34495e", main = "Forêt aléatoire : importance des variables")
dev.off()

# 6.1 Détail région-année du test : probabilité prédite vs classe observée
detail_test <- data.frame(region = test$region, annee = test$annee,
                          classe_observee = as.character(y), classe_predite = as.character(pred_ref),
                          P_eleve = round(p_ref[, "Eleve"], 3))
g_test <- ggplot(detail_test, aes(factor(annee), reorder(region, P_eleve), fill = P_eleve)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = paste0(substr(classe_observee, 1, 1), "/", substr(classe_predite, 1, 1))), size = 3) +
  scale_fill_gradient(low = "#ffffbf", high = "#c0392b", limits = c(0, 1), name = "P(élevé)") +
  labs(title = "Période de test : observé / prédit et P(risque élevé)",
       subtitle = "Modèle entraîné sur 2017-2023 ; E = élevé, M = moyen, F = faible", x = NULL, y = NULL) +
  theme_minimal()
ggsave(file.path(SORTIE, "09_validation_test_2024_2025.png"), g_test, width = 7.5, height = 5.5, dpi = 200)

# 6.2 Validation en laissant une région de côté
regions_liste <- levels(md$region)
loro_une_region <- function(r) {
  tr <- md[md$region != r, ]; te <- md[md$region == r, ]
  sfun <- fabrique_std(tr); trs <- sfun(tr); tes <- sfun(te)
  list(y = te$classe, l1 = te$classe_l1,
       p1 = predict(ajuste_polr(trs, REF_LINK), tes, type = "probs"),
       p2 = predict(ajuste_polr(trs, ALT_LINK), tes, type = "probs"),
       p3 = predict(randomForest::randomForest(form, data = trs, ntree = 300), tes, type = "prob"),
       p4 = predict(ajuste_polr(trs, REF_LINK, form_cmp), tes, type = "probs"))
}
res_loro <- lapply(regions_liste, loro_une_region)
y_l <- factor(unlist(lapply(res_loro, function(x) as.character(x$y))), levels = NIV, ordered = TRUE)
P1 <- do.call(rbind, lapply(res_loro, `[[`, "p1")); P2 <- do.call(rbind, lapply(res_loro, `[[`, "p2"))
P3 <- do.call(rbind, lapply(res_loro, `[[`, "p3")); P4 <- do.call(rbind, lapply(res_loro, `[[`, "p4"))
c_l1 <- unlist(lapply(res_loro, function(x) as.character(x$l1)))
PN <- sapply(NIV, function(k) as.numeric(c_l1 == k))
tab_loro <- rbind(perf(P1, y_l, paste0(REF_LINK, " ordinal (REFERENCE)")),
                  perf(P2, y_l, paste0(ALT_LINK, " ordinal (robustesse)")),
                  perf(P3, y_l, "Foret aleatoire"),
                  perf(PN, y_l, "Persistance (classe t-1)"),
                  perf(P4, y_l, paste0(REF_LINK, " ordinal ", LIB_CMP)))
tab_loro[-1] <- round(tab_loro[-1], 3)
cat("\nValidation en laissant une région de côté :\n"); print(tab_loro)


# =============================================================================
# 7. PRÉVISION 2026 (ALERTE PRÉCOCE)
# =============================================================================
std_all <- fabrique_std(md); allS <- std_all(md)
f_ref <- ajuste_polr(allS, REF_LINK)
f_alt <- ajuste_polr(allS, ALT_LINK)
f_rf  <- randomForest::randomForest(form, data = allS, ntree = 500)

nd <- d |> filter(annee == max(annee)) |> as.data.frame()
for (v in X0) nd[[paste0(v, "_l1")]] <- nd[[v]]
nd <- std_all(nd)

pr <- predict(f_ref, nd, type = "probs")
pa <- predict(f_alt, nd, type = "probs")
pf <- predict(f_rf,  nd, type = "prob")

# QUOI : effort de sortie de l'alerte. P(élevé) >= 0,50 équivaut à eta >= seuil 2 ;
# la baisse de score_risque (unités du score, 0 = moyenne des 13 régions) qui
# ramène eta sous ce seuil vaut (eta - zeta2) / coefficient x écart-type du score.
eta_ref <- as.numeric(as.matrix(nd[XF]) %*% coef(f_ref))
eta_alt <- as.numeric(as.matrix(nd[XF]) %*% coef(f_alt))
sd_score <- sd(md$score_risque_l1)
effort <- pmax(0, (eta_ref - f_ref$zeta[2]) / coef(f_ref)["score_risque_l1"]) * sd_score

# QUOI : indicateur de fermetures utilisé pour la carte et l'indice.
# Source ESU si fournie (dernière année disponible par région), sinon repli panel.
if (ESU_DISPO && any(!is.na(d$taux_fermeture_esu))) {
  ferm <- d |> filter(!is.na(taux_fermeture_esu)) |> group_by(cle) |> slice_max(annee, n = 1) |>
    ungroup() |> transmute(cle, ferm_ind = taux_fermeture_esu, ferm_annee = annee)
  FERM_SOURCE <- "Rapports ST-ESU (écoles fermées / écoles)"
} else {
  ferm <- d |> filter(annee == max(annee)) |>
    transmute(cle, ferm_ind = deficit_offre, ferm_annee = annee)
  FERM_SOURCE <- "REPLI panel : déficit d'offre scolaire (1 - écoles / maximum antérieur), pas un décompte ESU"
}
cat("\nIndicateur de fermetures :", FERM_SOURCE, "\n")

prevision <- data.frame(
  region = nd$region, cle = nd$cle, classe_2025 = as.character(nd$classe),
  P_eleve_ref = round(pr[, "Eleve"], 3), P_eleve_robustesse = round(pa[, "Eleve"], 3),
  P_eleve_foret = round(pf[, "Eleve"], 3),
  classe_predite_2026 = NIV[max.col(pr, ties.method = "first")],
  effort_sortie_alerte = round(effort, 2)) |>
  left_join(ferm, by = "cle") |>
  mutate(niveau_alerte = case_when(P_eleve_ref >= SEUIL_ALERTE ~ "ALERTE",
                                   P_eleve_ref >= SEUIL_VIGILANCE ~ "Vigilance",
                                   TRUE ~ "Normal"),
         # alerte « renforcée » : le modèle est calme mais l'offre scolaire est dégradée
         niveau_alerte_ferm = case_when(niveau_alerte == "Normal" & !is.na(ferm_ind) &
                                        ferm_ind >= SEUIL_FERMETURE ~ "Vigilance",
                                        TRUE ~ niveau_alerte)) |>
  arrange(desc(P_eleve_ref))

prevision_graph <- prevision
prevision_graph$p_plot <- prevision_graph$P_eleve_ref
names(prevision)[4:5] <- c(paste0("P_eleve_", REF_LINK, "_REF"), paste0("P_eleve_", ALT_LINK, "_robustesse"))
cat("\nPrévision 2026 (probabilité de risque élevé) :\n"); print(prevision |> select(-cle))

pal_alerte <- c(ALERTE = "#c0392b", Vigilance = "#e9c46a", Normal = "#2a9d8f")
g5 <- ggplot(prevision_graph, aes(reorder(region, p_plot), p_plot, fill = niveau_alerte_ferm)) +
  geom_col() + coord_flip() +
  geom_hline(yintercept = c(SEUIL_VIGILANCE, SEUIL_ALERTE), linetype = "dashed") +
  scale_fill_manual(values = pal_alerte, name = NULL) +
  labs(title = paste0("Probabilité de risque élevé en 2026 (", REF_LINK, " ordinal)"),
       x = NULL, y = "P(risque élevé)") + theme_minimal()
ggsave(file.path(SORTIE, "08_prevision_2026.png"), g5, width = 8, height = 5.5, dpi = 200)


# =============================================================================
# 8. INDICE DE RISQUE PAR RÉGION ET CARTE D'ALERTE
# =============================================================================
# OBJECTIF : livrables finaux du défi.
# 8.1 INDICE COMPOSITE : moyenne pondérée de dimensions standardisées entre les
#     13 régions (0 = région moyenne ; plus haut = plus de risque) :
#       edu_actuel : score de risque éducatif 2025 (label de la section 2)
#       anticipe   : moyenne des P(risque élevé) 2026 des trois modèles
#       pauvrete   : indice de pauvreté EHCVM 2021
#       fermetures : fermetures d'écoles (ESU, ou repli panel)
#       pdi        : PDI pour 1 000 élèves du primaire (si fourni)
#       mariage    : mariage précoce des filles, EDS 2021 (si fourni)
#     Une dimension sans donnée est écartée et les poids sont renormalisés.
# Lecture : l'indice classe des régions entre elles à une date ; il ne mesure pas
# un niveau absolu de décrochage.

dim_tab <- prevision_graph |>
  transmute(region, cle,
            edu_actuel = nd$score_risque[match(region, nd$region)],
            anticipe   = (P_eleve_ref + P_eleve_robustesse + P_eleve_foret) / 3,
            pauvrete   = nd$pauvrete_idx[match(region, nd$region)],
            fermetures = ferm_ind)
if ("pdi_par_1000" %in% names(d)) {
  pdi_last <- d |> filter(!is.na(pdi_par_1000)) |> group_by(cle) |> slice_max(annee, n = 1) |>
    ungroup() |> select(cle, pdi = pdi_par_1000)
  dim_tab <- left_join(dim_tab, pdi_last, by = "cle")
}
if ("mariage_precoce_pct" %in% names(d)) {
  dim_tab <- left_join(dim_tab, d |> group_by(cle) |> summarise(mariage = first(mariage_precoce_pct)), by = "cle")
}
DIMS <- intersect(names(POIDS_INDICE), names(dim_tab))
DIMS <- DIMS[sapply(DIMS, function(v) sum(!is.na(dim_tab[[v]])) >= 10)]   # au moins 10 régions renseignées
cat("\nDimensions de l'indice composite :", paste(DIMS, collapse = ", "), "\n")

zdim <- dim_tab; for (v in DIMS) zdim[[v]] <- z(dim_tab[[v]])

indice_pondere <- function(zt, dims, w = POIDS_INDICE) {
  W <- matrix(w[dims], nrow(zt), length(dims), byrow = TRUE)
  W[is.na(as.matrix(zt[dims]))] <- 0
  rowSums(as.matrix(zt[dims]) * W, na.rm = TRUE) / rowSums(W)
}
zdim$indice_z <- indice_pondere(zdim, DIMS)
rg <- range(zdim$indice_z)
zdim$indice_0_100 <- round(100 * (zdim$indice_z - rg[1]) / (rg[2] - rg[1]), 1)

# QUOI : niveaux de l'indice. POURQUOI : convention lisible, modifiable.
zdim$niveau_risque <- cut(zdim$indice_z, c(-Inf, -0.5, 0, 0.75, Inf),
                          labels = c("Faible", "Modéré", "Élevé", "Critique"), right = FALSE)
zdim$rang <- rank(-zdim$indice_z, ties.method = "min")

# 8.2 Sensibilité de l'indice (robustesse, comme pour le label)
sens_indice <- do.call(rbind, c(
  lapply(DIMS, function(v) {
    dd <- setdiff(DIMS, v)
    data.frame(variante = paste("Sans la dimension", v),
               correlation_rangs = round(cor(zdim$indice_z, indice_pondere(zdim, dd), method = "spearman"), 3))
  }),
  list(data.frame(variante = "Dimensions éducatives seules (edu_actuel + anticipe)",
                  correlation_rangs = round(cor(zdim$indice_z, indice_pondere(zdim, intersect(c("edu_actuel", "anticipe"), DIMS)),
                                                method = "spearman"), 3))),
  list({
    zc <- zdim[DIMS]; zc <- zc[complete.cases(zc), ]
    if (nrow(zc) == nrow(zdim) && length(DIMS) >= 3) {
      pc <- prcomp(zdim[DIMS], scale. = TRUE)$x[, 1]
      if (cor(pc, zdim$indice_z) < 0) pc <- -pc
      data.frame(variante = "Poids issus de l'ACP (axe 1)",
                 correlation_rangs = round(cor(zdim$indice_z, pc, method = "spearman"), 3))
    } else NULL
  })))
cat("\nSensibilité de l'indice (corrélation de rangs avec l'indice retenu) :\n"); print(sens_indice)

indice_regions <- zdim |>
  left_join(prevision_graph |> select(cle, classe_2025, P_eleve_ref, niveau_alerte, niveau_alerte_ferm, effort_sortie_alerte),
            by = "cle") |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  arrange(rang) |> select(-cle)
cat("\nINDICE DE RISQUE PAR RÉGION :\n"); print(as.data.frame(indice_regions))

# Figure 10 : barres de l'indice ; Figure 11 : profil des dimensions
pal_niv <- c(Faible = "#2a9d8f", Modéré = "#e9c46a", Élevé = "#e67e22", Critique = "#c0392b")
g10 <- ggplot(indice_regions, aes(reorder(joli(region), indice_0_100), indice_0_100, fill = niveau_risque)) +
  geom_col() + coord_flip() + geom_text(aes(label = indice_0_100), hjust = -0.15, size = 3) +
  scale_fill_manual(values = pal_niv, name = "Niveau", drop = FALSE) +
  scale_y_continuous(limits = c(0, 108)) +
  labs(title = "Indice de risque composite par région (0-100)",
       subtitle = paste("Dimensions :", paste(DIMS, collapse = ", ")), x = NULL, y = "Indice") +
  theme_minimal()
ggsave(file.path(SORTIE, "10_indice_risque_regions.png"), g10, width = 8.5, height = 5.8, dpi = 200)

lib_dim <- c(edu_actuel = "Risque éducatif 2025", anticipe = "Risque anticipé 2026",
             pauvrete = "Pauvreté (EHCVM)", fermetures = "Fermetures d'écoles",
             pdi = "PDI", mariage = "Mariage précoce")
g11 <- zdim |> select(region, rang, all_of(DIMS)) |>
  pivot_longer(all_of(DIMS), names_to = "dimension", values_to = "zv") |>
  mutate(dimension = factor(lib_dim[dimension], levels = lib_dim[DIMS]),
         region = factor(joli(region), levels = rev(joli(indice_regions$region)))) |>
  ggplot(aes(dimension, region, fill = zv)) + geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(is.na(zv), "n.d.", sprintf("%.1f", zv))), size = 3) +
  scale_fill_gradient2(low = "#2a9d8f", mid = "#ffffbf", high = "#c0392b", midpoint = 0,
                       na.value = "grey90", name = "z") +
  labs(title = "Profil du risque par région et par dimension", x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(SORTIE, "11_profil_dimensions.png"), g11, width = 8.5, height = 6, dpi = 200)

# 8.3 CARTE D'ALERTE : alerte 2026 x fermetures d'écoles
# QUOI : positions approximatives (longitude, latitude) des 13 régions.
# POURQUOI : pas de fond de carte exigé (faisabilité). Positions schématiques,
# légèrement écartées pour les petites régions du Centre. Si un fichier
# bfa_adm1.shp / .geojson est fourni, il est tracé en fond.
coord_reg <- data.frame(
  cle = c("BOUCLEDUMOUHOUN", "CASCADES", "CENTRE", "CENTREEST", "CENTRENORD", "CENTREOUEST",
          "CENTRESUD", "EST", "HAUTSBASSINS", "NORD", "PLATEAUCENTRAL", "SAHEL", "SUDOUEST"),
  lon = c(-3.4, -4.6, -1.75, -0.45, -1.0, -2.4, -1.2, 0.8, -4.3, -2.4, -1.05, -0.2, -3.1),
  lat = c(12.4, 10.2, 12.15, 11.8, 13.2, 12.1, 11.5, 12.0, 11.3, 13.4, 12.7, 14.2, 10.5))
carte <- prevision_graph |> left_join(coord_reg, by = "cle") |>
  left_join(zdim |> select(cle, indice_0_100, niveau_risque), by = "cle") |>
  mutate(etiquette = paste0(joli(region), "\nP=", sprintf("%.2f", P_eleve_ref)),
         ferm_pct = 100 * ferm_ind,
         ferm_pt = pmax(100 * ferm_ind, 2))   # taille minimale : une région sans fermeture reste visible

fond <- NULL
if (requireNamespace("sf", quietly = TRUE)) {
  f_ok <- FOND_CARTE[file.exists(FOND_CARTE)]
  if (length(f_ok)) fond <- tryCatch(sf::st_read(f_ok[1], quiet = TRUE), error = function(e) NULL)
}

g12 <- ggplot()
if (!is.null(fond)) g12 <- g12 + geom_sf(data = fond, fill = "grey96", colour = "grey60")
g12 <- g12 +
  geom_point(data = carte, aes(lon, lat, size = ferm_pt, fill = niveau_alerte_ferm),
             shape = 21, colour = "grey20", alpha = 0.9) +
  ggrepel::geom_text_repel(data = carte, aes(lon, lat, label = etiquette), size = 2.8,
                           lineheight = 0.9, min.segment.length = 0.1, box.padding = 0.6, seed = 2026) +
  scale_fill_manual(values = pal_alerte, name = "Niveau d'alerte 2026") +
  scale_size_area(max_size = 16, name = "Écoles fermées (%)") +
  coord_sf(xlim = c(-5.8, 2.0), ylim = c(9.8, 15.0)) +
  labs(title = "Carte d'alerte 2026 : probabilité de risque élevé et fermetures d'écoles",
       subtitle = paste0("Couleur : alerte (P >= ", SEUIL_ALERTE, ") ou vigilance (P >= ", round(SEUIL_VIGILANCE, 2),
                         " ou fermetures >= ", 100 * SEUIL_FERMETURE, " %). Taille : fermetures."),
       caption = paste0("Fermetures : ", FERM_SOURCE, ". Positions régionales schématiques."),
       x = NULL, y = NULL) +
  theme_minimal() + theme(plot.caption = element_text(size = 7, hjust = 0),
                          axis.text = element_blank(), panel.grid = element_line(colour = "grey92"))
ggsave(file.path(SORTIE, "12_carte_alerte_ESU.png"), g12, width = 9, height = 6.5, dpi = 200)

# Carte jumelle : indice composite
g13 <- ggplot()
if (!is.null(fond)) g13 <- g13 + geom_sf(data = fond, fill = "grey96", colour = "grey60")
g13 <- g13 +
  geom_point(data = carte, aes(lon, lat, fill = niveau_risque), size = 11, shape = 21, colour = "grey20") +
  ggrepel::geom_text_repel(data = carte, aes(lon, lat, label = paste0(joli(region), "\n", indice_0_100)),
                           size = 2.8, lineheight = 0.9, box.padding = 0.6, seed = 2026) +
  scale_fill_manual(values = pal_niv, name = "Niveau de l'indice", drop = FALSE) +
  coord_sf(xlim = c(-5.8, 2.0), ylim = c(9.8, 15.0)) +
  labs(title = "Indice de risque composite par région (0-100)", caption = "Positions régionales schématiques.",
       x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text = element_blank(), panel.grid = element_line(colour = "grey92"))
ggsave(file.path(SORTIE, "13_carte_indice_risque.png"), g13, width = 9, height = 6.5, dpi = 200)


# =============================================================================
# 9. EXPORT DES RÉSULTATS
# =============================================================================
col_base <- intersect(c("id_region", "region", "annee", "annee_scol", "deperdition", "deperdition_coh5",
                        "var_eff", "ecart_parite", "indice_composite", "indice_acp", "score_risque",
                        "classe", "classe_acp", "classe_coh5", "classe_km", "pauvrete_idx",
                        "deficit_offre", "taux_fermeture_esu", "pdi_par_1000", "taux_abandon",
                        "taux_redoublement", "abandons_prov", "taux_transition"), names(d))
base_label <- d |> select(all_of(col_base)) |> mutate(across(where(is.numeric), ~ round(.x, 4)))

feuilles <- list(
  indice_risque_regions = as.data.frame(indice_regions),
  alerte_2026 = prevision |> select(-cle),
  sensibilite_indice = sens_indice,
  base_labellisee = base_label,
  classement_regions = as.data.frame(classement),
  robustesse_label = robustesse_label,
  profils_types = as.data.frame(profils_types),
  coefficients = tab_coef,
  performance_test_2024_25 = tab_perf,
  detail_test_2024_25 = detail_test,
  apport_ehcvm = comparaison_ehcvm,
  concordance_modeles = concordance,
  importance_foret = imp,
  validation_par_region = tab_loro,
  tests_extension = if (is.null(test_ext)) data.frame(note = "aucune variable supplémentaire disponible") else test_ext,
  inventaire_sources = inventaire,
  controles_integrite = controles,
  totaux_nationaux = as.data.frame(totaux_nationaux),
  sensibilite_cohorte = sensibilite_cohorte)
if (!is.null(tab_re)) feuilles$effets_aleatoires <- tab_re
if (!is.null(validation_externe)) feuilles$validation_externe_label <- validation_externe

writexl::write_xlsx(feuilles, file.path(SORTIE, "resultats_indice_risque.xlsx"))
write.csv(base_label, file.path(SORTIE, "base_labellisee.csv"), row.names = FALSE)

regions_json <- lapply(classement$region, function(r) {
  x <- d[d$region == r, ]; j <- match(r, nd$region); pg <- prevision_graph[prevision_graph$region == r, ]
  ir <- indice_regions[indice_regions$region == r, ]
  list(n = r, s = round(x$score_risque, 2), d = round(x$deperdition, 2), e = x$ecoles_tot,
       c = paste(substr(x$classe, 1, 1), collapse = ""), c25 = substr(nd$classe[j], 1, 1),
       pr = pg$P_eleve_ref, pa = pg$P_eleve_robustesse, pf = pg$P_eleve_foret,
       er = round(eta_ref[j], 4), ea = round(eta_alt[j], 4),
       ix = ir$indice_0_100, nv = as.character(ir$niveau_risque), fm = round(pg$ferm_ind, 3))
})
jsonlite::write_json(
  list(annees = sort(unique(d$annee)), seuils = round(as.numeric(seuils), 3),
       lien_ref = REF_LINK, lien_alt = ALT_LINK,
       b_ref = round(unname(coef(f_ref)["score_risque_l1"]), 4), z_ref = round(unname(f_ref$zeta[2]), 4),
       b_alt = round(unname(coef(f_alt)["score_risque_l1"]), 4), z_alt = round(unname(f_alt$zeta[2]), 4),
       sd_score = round(sd_score, 4), source_fermetures = FERM_SOURCE, regions = regions_json),
  file.path(SORTIE, "dashboard_data.json"), auto_unbox = TRUE, digits = 4)

writeLines(capture.output(sessionInfo()), file.path(SORTIE, "session_info.txt"))
cat("\nTerminé. Résultats dans le dossier :", normalizePath(SORTIE), "\n")
