"""
============================================================
ETAPE 1 -> ETAPE 2 : liste des matchs pour TOUS les tournois d'un CSV
============================================================
Prend en entree le CSV produit par scrape_atp_tournaments_year.py
(colonnes attendues : results_url, + toute colonne de metadonnees comme
year, tournament_type, tourney_id, tournament_name, slug, category)

Pour chaque tournoi, scrape la liste de ses matchs (Round, Duration,
Stats_URL) via get_tournament_match_urls() de
scrape_atp_matches_stealth.py, en reutilisant la MEME session de
navigateur (plus rapide que d'en relancer une par tournoi, vu le cout
du contournement Cloudflare a chaque lancement).

Sauvegarde progressive + reprise : si le script est interrompu, les
tournois deja traites (bases sur results_url) sont sautes au relancement.

Utilisation :
    python scrape_atp_tournament_matches_batch.py tournois_2010.csv --out matches_2010.csv
============================================================
"""

import argparse
import time
from pathlib import Path

import pandas as pd
from playwright.sync_api import sync_playwright
from playwright_stealth import Stealth

from scrape_atp_matches_stealth import get_tournament_match_urls


def load_already_done_tournaments(out_path):
    if not Path(out_path).exists():
        return set()
    try:
        done_df = pd.read_csv(out_path)
        if "tournament_results_url" in done_df.columns:
            return set(done_df["tournament_results_url"].dropna().tolist())
    except Exception:
        pass
    return set()


def append_rows_to_csv(rows, out_path, write_header):
    if not rows:
        return
    df_rows = pd.DataFrame(rows)
    df_rows.to_csv(out_path, mode="a", index=False, header=write_header, encoding="utf-8-sig")


def run_batch(tournaments_csv, out_path, headless=True, debug=False, pause_between=3):
    tournaments_df = pd.read_csv(tournaments_csv)
    if "results_url" not in tournaments_df.columns:
        raise ValueError(f"Le CSV d'entree doit contenir une colonne 'results_url'. Colonnes : {list(tournaments_df.columns)}")

    already_done = load_already_done_tournaments(out_path)
    write_header = not Path(out_path).exists() or len(already_done) == 0

    to_process = tournaments_df[~tournaments_df["results_url"].isin(already_done)].reset_index(drop=True)
    print(f"{len(tournaments_df)} tournois au total, {len(already_done)} deja traites, {len(to_process)} restants.")

    if len(to_process) == 0:
        print("Rien a faire, tout est deja scrape.")
        return

    meta_cols = [c for c in tournaments_df.columns if c != "results_url"]

    with Stealth().use_sync(sync_playwright()) as p:
        browser = p.chromium.launch(
            headless=headless,
            channel="chrome",
            args=["--disable-blink-features=AutomationControlled", "--no-sandbox"]
        )
        context = browser.new_context(
            viewport={"width": 1400, "height": 900},
            locale="en-US",
            user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
                       "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
        )
        page = context.new_page()

        for i, tourney_row in to_process.iterrows():
            results_url = tourney_row["results_url"]
            print(f"\n[{i + 1}/{len(to_process)}] Tournoi : {results_url}")

            try:
                match_records = get_tournament_match_urls(page, results_url, debug=debug)
            except Exception as e:
                print(f"  [ERREUR] {e}")
                match_records = []

            # Rattache les metadonnees du tournoi (year, tourney_id, etc.) a chaque match
            for rec in match_records:
                rec["tournament_results_url"] = results_url
                for col in meta_cols:
                    rec[col] = tourney_row[col]

            append_rows_to_csv(match_records, out_path, write_header)
            write_header = False

            # Meme si 0 match trouve, on marque le tournoi comme traite pour ne pas
            # le re-scraper en boucle -> on ecrit une ligne "vide" tracee
            if not match_records:
                placeholder = {"Round": None, "Duration": None, "Stats_URL": None, "tournament_results_url": results_url}
                for col in meta_cols:
                    placeholder[col] = tourney_row[col]
                append_rows_to_csv([placeholder], out_path, write_header)
                write_header = False

            if i < len(to_process) - 1:
                time.sleep(pause_between)

        browser.close()

    print(f"\nTermine. Resultats -> {out_path}")


def main():
    parser = argparse.ArgumentParser(description="Scrape la liste des matchs pour tous les tournois d'un CSV")
    parser.add_argument("tournaments_csv", help="CSV produit par scrape_atp_tournaments_year.py")
    parser.add_argument("--out", default="matches_all_tournaments.csv")
    parser.add_argument("--no-headless", action="store_true")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--pause", type=float, default=3.0, help="Pause en secondes entre deux tournois")
    args = parser.parse_args()

    run_batch(
        args.tournaments_csv,
        args.out,
        headless=not args.no_headless,
        debug=args.debug,
        pause_between=args.pause,
    )


if __name__ == "__main__":
    main()