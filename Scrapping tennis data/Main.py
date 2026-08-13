"""
============================================================
PIPELINE COMPLET ATP : TOURNOIS -> MATCHS -> STATS
============================================================
Enchaine automatiquement les 3 scripts pour une ou plusieurs annees :
    1. scrape_atp_tournaments_year.py   -> tournois_{year}.csv
    2. scrape_atp_tournament_matches_batch.py -> matches_{year}.csv
    3. scrape_atp_stats_batch.py        -> stats_{year}.csv

Chaque etape est lancee comme un VRAI PROCESS SEPARE (via subprocess),
pas importee directement -> evite le conflit "Playwright Sync API
inside the asyncio loop" si ce script est lui-meme lance depuis un
notebook Jupyter/Colab.

Reprise automatique : chaque etape sait deja sauter ce qui est deja
fait (cf. logique de reprise interne a chaque script). Si le pipeline
est interrompu (annee 3 sur 10, ou etape 2 sur 3), le relancer avec la
meme commande reprend exactement ou il s'etait arrete.

Utilisation :
    python run_atp_pipeline.py 2010
    python run_atp_pipeline.py 2010 2011 2012 2013
    python run_atp_pipeline.py 2010 --skip-step 1     # si tournois_2010.csv existe deja
============================================================
"""

import argparse
import subprocess
import sys
from pathlib import Path

import pandas as pd

# Dossier contenant ce script (et ses scripts "enfants") -> permet de les
# retrouver peu importe le dossier courant depuis lequel on lance Main.py
# (ex: python ATP_Tour/Main.py depuis /home/onyxia/work)
SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))

from scrape_atp_tournaments_year import TOURNAMENT_TYPES  # {"atpgs": "...", "ch": "..."}


def run_step(description, script_name, script_args):
    print(f"\n{'=' * 60}")
    print(f"  {description}")
    print(f"{'=' * 60}")

    script_path = str(SCRIPT_DIR / script_name)
    # "-u" = sortie non bufferisee : sans ca, les print() du sous-process
    # ne s'affichent qu'a la toute fin (voire jamais) quand la sortie est
    # redirigee via un pipe (cas de Jupyter), au lieu de s'afficher au fur
    # et a mesure comme dans un vrai terminal.
    cmd = [sys.executable, "-u", script_path] + script_args
    print(f"  Commande : {' '.join(cmd)}\n")

    result = subprocess.run(cmd)

    if result.returncode != 0:
        print(f"\n[!] La commande a echoue (code {result.returncode}).")
        return False
    return True


def ensure_tournois_csv_for_category(year, category, target_csv):
    """Si le CSV filtre par categorie (ex: tournois_2010_atpgs.csv) n'existe
    pas encore mais que le CSV combine (tournois_2010.csv, avec les 2
    categories) existe deja, on le derive par un simple filtrage pandas
    -> aucun re-scraping necessaire. Retourne True si le fichier est pret
    (deja present ou derive a l'instant), False s'il faut vraiment scraper."""
    if Path(target_csv).exists():
        return True
    if category == "both":
        return False  # rien a deriver, c'est deja le fichier de reference

    base_csv = f"tournois_{year}.csv"
    if not Path(base_csv).exists():
        return False  # pas de fichier combine dispo -> il faudra scraper

    df = pd.read_csv(base_csv)
    label = TOURNAMENT_TYPES.get(category, category)
    filtered = df[df["tournament_type"] == label]

    if filtered.empty:
        print(f"[Annee {year}] [!] {base_csv} existe mais 0 ligne '{label}' dedans -> re-scraping necessaire.")
        return False

    filtered.to_csv(target_csv, index=False)
    print(f"[Annee {year}] {target_csv} derive de {base_csv} par filtrage sur '{label}' ({len(filtered)} tournois, pas de re-scraping).")
    return True


def ensure_matches_csv_for_category(year, category, target_csv):
    """Meme principe que ensure_tournois_csv_for_category, mais pour le
    fichier de matchs (matches_{year}.csv -> matches_{year}_{category}.csv)."""
    if Path(target_csv).exists():
        return True
    if category == "both":
        return False

    base_csv = f"matches_{year}.csv"
    if not Path(base_csv).exists():
        return False

    df = pd.read_csv(base_csv)
    if "tournament_type" not in df.columns:
        return False

    label = TOURNAMENT_TYPES.get(category, category)
    filtered = df[df["tournament_type"] == label]

    if filtered.empty:
        print(f"[Annee {year}] [!] {base_csv} existe mais 0 ligne '{label}' dedans -> re-scraping necessaire.")
        return False

    filtered.to_csv(target_csv, index=False)
    print(f"[Annee {year}] {target_csv} derive de {base_csv} par filtrage sur '{label}' ({len(filtered)} matchs, pas de re-scraping).")
    return True


def run_pipeline_for_year(year, skip_steps, category):
    suffix = "" if category == "both" else f"_{category}"
    tournois_csv = f"tournois_{year}{suffix}.csv"
    matches_csv = f"matches_{year}{suffix}.csv"
    stats_csv = f"stats_{year}{suffix}.csv"

    ok = True

    # ---- Etape 1 : liste des tournois ----
    if 1 in skip_steps:
        print(f"\n[Annee {year}] Etape 1 sautee (--skip-step 1).")
    elif ensure_tournois_csv_for_category(year, category, tournois_csv):
        pass  # deja present, ou derive a l'instant par filtrage -> pas besoin de scraper
    else:
        ok = run_step(
            f"[Annee {year}] ETAPE 1/3 - Liste des tournois ({category})",
            "scrape_atp_tournaments_year.py",
            [str(year), "--category", category, "--out", tournois_csv],
        )
        if not ok:
            return False

    # ---- Etape 2 : liste des matchs de chaque tournoi ----
    if 2 in skip_steps:
        if not ensure_matches_csv_for_category(year, category, matches_csv):
            print(f"\n[Annee {year}] Etape 2 sautee (--skip-step 2), mais {matches_csv} introuvable et impossible a deriver -> l'etape 3 va probablement echouer.")
        else:
            print(f"\n[Annee {year}] Etape 2 sautee (--skip-step 2), {matches_csv} pret.")
    else:
        ok = run_step(
            f"[Annee {year}] ETAPE 2/3 - Liste des matchs par tournoi",
            "scrape_atp_tournament_matches_batch.py",
            [tournois_csv, "--out", matches_csv],
        )
        if not ok:
            return False

    # ---- Etape 3 : stats de chaque match ----
    if 3 in skip_steps:
        print(f"\n[Annee {year}] Etape 3 sautee (--skip-step 3).")
    else:
        ok = run_step(
            f"[Annee {year}] ETAPE 3/3 - Stats de chaque match",
            "scrape_atp_stats_batch.py",
            [matches_csv, "--out", stats_csv],
        )
        if not ok:
            return False

    print(f"\n[Annee {year}] Pipeline termine avec succes -> {stats_csv}")
    return True


def main():
    parser = argparse.ArgumentParser(description="Pipeline complet ATP : tournois -> matchs -> stats")
    parser.add_argument("years", nargs="+", type=int, help="Annee(s) a traiter, ex: 2010 2011 2012")
    parser.add_argument(
        "--skip-step", type=int, action="append", default=[], choices=[1, 2, 3],
        help="Sauter une etape specifique (utile pour relancer manuellement apres un CSV deja genere). "
             "Peut etre repete, ex: --skip-step 1 --skip-step 2"
    )
    parser.add_argument(
        "--category", choices=["atpgs", "ch", "both"], default="both",
        help="Categorie a traiter : atpgs (ATP + Grand Chelem), ch (Challenger), ou both (les deux, par defaut)"
    )
    parser.add_argument("--stop-on-error", action="store_true", help="Arreter tout le pipeline si une annee echoue (par defaut, on continue avec les annees suivantes)")
    args = parser.parse_args()

    python_exe = sys.executable  # conserve pour reference, utilise directement dans run_step
    skip_steps = set(args.skip_step)

    results = {}
    for year in args.years:
        success = run_pipeline_for_year(year, skip_steps, args.category)
        results[year] = success
        if not success and args.stop_on_error:
            print(f"\n[!] Arret du pipeline suite a l'echec sur l'annee {year} (--stop-on-error actif).")
            break

    print(f"\n{'=' * 60}")
    print("  RECAPITULATIF")
    print(f"{'=' * 60}")
    for year, success in results.items():
        status = "OK" if success else "ECHEC"
        print(f"  {year} : {status}")


if __name__ == "__main__":
    main()