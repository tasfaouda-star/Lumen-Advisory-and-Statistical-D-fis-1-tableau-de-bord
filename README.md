# Données d'entrée

Placer ici, avant d'exécuter `analysis/03_analyse_indice_risque_alerte.R` :

| Fichier | Statut | Contenu |
|---|---|---|
| `Panel_DOC_EHCVM2021.xlsx` | obligatoire | Panel 13 régions × 2016-2025 (feuille `Feuil1`) + 7 variables EHCVM 2021 |
| `donnees_complementaires.xlsx` | facultatif | Une feuille par source : `esu_fermetures`, `pdi`, `eds2021`, `primaire_tdb`, `annuaire_province`, `post_primaire` |
| `bfa_adm1.shp` ou `bfa_adm1.geojson` | facultatif | Fond de carte pour les graphiques R |

Ces fichiers ne sont pas fournis dans le dépôt (voir `.gitignore`) : vérifier les droits de diffusion avant de les publier.
