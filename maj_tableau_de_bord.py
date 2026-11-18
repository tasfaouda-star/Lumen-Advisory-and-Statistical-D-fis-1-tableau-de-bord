#!/usr/bin/env python3
"""Met à jour assets/data.js (tableau de bord) à partir du classeur produit par le script R.

Usage :
    python3 analysis/maj_tableau_de_bord.py sorties_indice_risque/resultats_indice_risque.xlsx

Lit les feuilles : indice_risque_regions, alerte_2026, classement_regions, base_labellisee,
totaux_nationaux, performance_test_2024_25, validation_par_region, coefficients, inventaire_sources.
Le fond de carte, les textes de méthode et les seuils sont conservés tels quels dans data.js.
Dépendances : pandas, openpyxl.
"""
import json, re, sys
from pathlib import Path
import pandas as pd

RACINE = Path(__file__).resolve().parent.parent
DATA_JS = RACINE / "assets" / "data.js"
LIBELLES = {"score_risque": "Score de risque (t−1)", "log_eff_prim": "Effectifs du primaire (log)", "taille_ecole": "Taille moyenne d'école",
            "log_tbs_presco": "TBS préscolaire (log)", "salles_par_struct": "Salles par structure préscolaire", "ips_tba": "IPS au TBA",
            "eff_par_struct_presco": "Effectif par structure préscolaire", "ips_tbs": "IPS au TBS", "ips_tbs_presco": "IPS au TBS préscolaire",
            "pauvrete_idx": "Indice de pauvreté EHCVM", "ehcvm21_gini": "Gini (EHCVM 2021)"}
CLASSE = {"Eleve": "E", "Moyen": "M", "Faible": "F", "Élevé": "E"}

def charger():
    txt = DATA_JS.read_text(encoding="utf8")
    return json.loads(re.sub(r"^window\.DATA=|;\s*$", "", txt.strip()))

def main(xlsx):
    D = charger()
    S = lambda nom: pd.read_excel(xlsx, sheet_name=nom)
    ind, ale, cla, bas = S("indice_risque_regions"), S("alerte_2026"), S("classement_regions"), S("base_labellisee")
    ale = ale.rename(columns={c: c for c in ale.columns})
    p_probit = next(c for c in ale.columns if c.startswith("P_eleve_probit") or c == "P_eleve_ref")
    p_logit = next(c for c in ale.columns if c.startswith("P_eleve_logit") or c == "P_eleve_robustesse")
    anciens = {r["id"]: r for r in D["regions"]}
    regions = []
    for _, r in cla.iterrows():
        rid = str(r["region"]).strip().upper()
        if rid not in D["carte"]["paths"]:
            sys.exit(f"Région absente du fond de carte : {rid}")
        i = ind[ind["region"].str.upper() == rid].iloc[0]
        a = ale[ale["region"].str.upper() == rid].iloc[0]
        b = bas[bas["region"].str.upper() == rid].sort_values("annee")
        grille = "".join(CLASSE[str(c)] for c in b["classe"])
        regions.append(dict(id=rid, nom=anciens.get(rid, {}).get("nom", rid.title()), classe2025=str(a["classe_2025"]),
            score_moy=float(r["score_moyen"]), score_2025=float(r["score_2025"]), tendance=float(r["tendance_annuelle"]),
            ans_eleve=int(r["annees_risque_eleve"]), ans_faible=int(r["annees_risque_faible"]), rang_score=int(r["rang"]),
            dim=dict(edu=float(i["edu_actuel"]), ant=float(i["anticipe"]), pauv=float(i["pauvrete"]), ferm=float(i["fermetures"])),
            indice_z=float(i["indice_z"]), indice100=float(i["indice_0_100"]), p_probit=float(a[p_probit]), p_logit=float(a[p_logit]),
            p_foret=float(a["P_eleve_foret"]), effort=float(a["effort_sortie_alerte"]), ferm=round(float(a["ferm_ind"]), 4), grille=grille))
    D["regions"] = sorted(regions, key=lambda x: x["indice_z"], reverse=True)
    nat = S("totaux_nationaux")
    D["nationaux"] = [dict(annee=int(x.annee), eff_prim=int(x.eff_prim_total), nouv_cp1=int(x.nouv_cp1_total), ecoles=int(x.ecoles_total), eff_presco=int(x.eff_presco_total)) for x in nat.itertuples()]
    perf, loro = S("performance_test_2024_25"), S("validation_par_region")
    mod = []
    for x in loro.itertuples():
        p = perf[perf["modele"] == x.modele]
        if p.empty: continue
        p = p.iloc[0]
        mod.append(dict(m=str(x.modele), exa=float(p["exactitude"]), exa1=float(p["exactitude_a_une_classe_pres"]), mae=float(p["erreur_absolue_moyenne"]),
                        auc=float(p["auc_risque_eleve"]), loro=float(x.exactitude), loro_auc=float(x.auc_risque_eleve)))
    D["modeles"] = mod
    co = S("coefficients")
    D["coefs"] = [[LIBELLES.get(x.variable, x.variable), float(x.coef_probit_REF), float(x.p_probit_REF), float(x.ame_eleve_probit_REF), float(x.odds_ratio_logit_robustesse)] for x in co.itertuples()]
    DATA_JS.write_text("window.DATA=" + json.dumps(D, ensure_ascii=False, separators=(",", ":")) + ";", encoding="utf8")
    print("assets/data.js mis à jour :", len(D["regions"]), "régions,", len(D["modeles"]), "modèles")

if __name__ == "__main__":
    if len(sys.argv) != 2: sys.exit(__doc__)
    main(sys.argv[1])
