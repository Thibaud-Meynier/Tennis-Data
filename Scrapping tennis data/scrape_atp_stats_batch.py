"""
============================================================
ETAPE 2 -> ETAPE 3 : scraping des stats pour TOUS les matchs listes
============================================================
Prend en entree le CSV produit par scrape_atp_tournament_matches_batch.py
(colonne obligatoire : "Stats_URL", + toutes les colonnes de contexte
comme Round, Duration, year, tournament_type, tourney_id, tournament_name).

Reutilise scrape_atp_match() de scrape_atp_match.py (meme dossier
requis, ou ajuste le nom de fichier importe ci-dessous si le tien
s'appelle differemment, ex: 2_scrap_atp_match).

Sauvegarde progressive + reprise automatique, comme pour les etapes
precedentes.

Utilisation :
    python scrape_atp_stats_batch.py matches_2010.csv --out stats_2010.csv
============================================================
"""

import argparse
import os
import random
import re
import sys
import time
from pathlib import Path

import pandas as pd
from playwright.sync_api import sync_playwright
from playwright_stealth import Stealth

# Ajuste cet import si ton fichier de scraping de match s'appelle differemment
from scrape_atp_match import scrape_atp_match


def load_input_matches(path):
    df = pd.read_csv(path)
    if "Stats_URL" not in df.columns:
        raise ValueError(f"Le CSV d'entree doit contenir une colonne 'Stats_URL'. Colonnes trouvees : {list(df.columns)}")
    # Retire les lignes "placeholder" (tournois sans match trouve, cf. etape precedente)
    return df[df["Stats_URL"].notna()].reset_index(drop=True)


def _split_paths(out_path):
    """Deduit les 2 (ou 3) chemins de sortie a partir du chemin de base demande."""
    base, ext = os.path.splitext(out_path)
    return {
        "ATP + Grand Chelem": f"{base}_atpgs{ext}",
        "Challenger": f"{base}_challenger{ext}",
        "_other": f"{base}_other{ext}",  # filet de securite si tournament_type est absent/inattendu
    }


def load_accumulated_rows(out_path):
    """Recharge les lignes deja scrapees depuis LES DEUX fichiers (pour reprise),
    sous forme de liste de dicts unique en memoire."""
    rows = []
    for path in _split_paths(out_path).values():
        if Path(path).exists():
            try:
                rows.extend(pd.read_csv(path).to_dict("records"))
            except Exception:
                pass
    return rows


def _reorder_columns(df):
    """Force un ordre de colonnes logique plutot que l'ordre d'apparition
    (qui place winner_jeux_set3 n'importe ou selon quand ce match a ete
    traite dans le batch)."""
    identity_cols = [c for c in ["url", "Winner", "Loser", "Winner_url", "Loser_url"] if c in df.columns]

    set_cols = sorted(
        [c for c in df.columns if re.match(r"(winner|loser)_(jeux|tb)_set\d+$", c)],
        key=lambda c: (int(re.search(r"set(\d+)", c).group(1)), c.startswith("loser"))
    )

    context_cols = [c for c in [
        "Round", "Duration", "year", "tournament_type", "tourney_id",
        "tournament_name", "category", "slug", "tournament_results_url",
    ] if c in df.columns]

    other_cols = [c for c in df.columns if c not in identity_cols + set_cols + context_cols]

    return df[identity_cols + set_cols + other_cols + context_cols]


def save_all_rows(rows, out_path):
    """Reecrit les CSV en entier (un par tournament_type) a partir de TOUTES
    les lignes accumulees. pandas fait l'union des colonnes et remplit avec
    NaN plutot que de decaler (cf. bug winner_jeux_set3 corrige precedemment)."""
    if not rows:
        return

    df = _reorder_columns(pd.DataFrame(rows))
    paths = _split_paths(out_path)

    if "tournament_type" not in df.columns:
        df.to_csv(paths["_other"], index=False, encoding="utf-8-sig")
        return

    for label, path in paths.items():
        if label == "_other":
            subset = df[~df["tournament_type"].isin(["ATP + Grand Chelem", "Challenger"])]
        else:
            subset = df[df["tournament_type"] == label]
        if not subset.empty:
            subset.to_csv(path, index=False, encoding="utf-8-sig")


def run_batch(input_csv, out_path, delay_range=(2, 5), headless=True, debug=False, flush_every=1):
    input_df = load_input_matches(input_csv)

    accumulated_rows = load_accumulated_rows(out_path)

    # Les matchs en echec (scrape_error non vide) ne comptent PAS comme "deja
    # faits" -> ils seront retentes automatiquement au prochain lancement,
    # sans avoir a intervenir manuellement. On les retire aussi de la liste
    # accumulee pour eviter d'avoir deux lignes pour la meme URL apres retry.
    success_rows = [
        r for r in accumulated_rows
        if not (isinstance(r.get("scrape_error"), str) and r.get("scrape_error").strip())
    ]
    n_failed_previously = len(accumulated_rows) - len(success_rows)
    accumulated_rows = success_rows

    already_done = {r["url"] for r in accumulated_rows if "url" in r and pd.notna(r["url"])}

    if n_failed_previously:
        print(f"{n_failed_previously} match(s) en echec lors d'un run precedent -> seront retentes.")

    to_scrape = input_df[~input_df["Stats_URL"].isin(already_done)].reset_index(drop=True)
    print(f"{len(input_df)} matchs au total, {len(already_done)} deja scrapes, {len(to_scrape)} restants.")

    if len(to_scrape) == 0:
        print("Rien a faire, tout est deja scrape.")
        return

    context_cols = [c for c in input_df.columns if c != "Stats_URL"]
    n_errors = 0

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

        for i, input_row in to_scrape.iterrows():
            url = input_row["Stats_URL"]
            print(f"[{i + 1}/{len(to_scrape)}] {url}")

            try:
                row = scrape_atp_match(page, url, debug=debug)
            except Exception as e:
                print(f"  [ERREUR] {e}", file=sys.stderr)
                n_errors += 1
                row = {"url": url, "Winner": None, "Loser": None, "scrape_error": str(e)}

            for col in context_cols:
                if col not in row:
                    row[col] = input_row[col]

            accumulated_rows.append(row)

            if (i + 1) % flush_every == 0:
                save_all_rows(accumulated_rows, out_path)

            if i < len(to_scrape) - 1:
                time.sleep(random.uniform(*delay_range))

        browser.close()

    # Sauvegarde finale, au cas ou flush_every > 1 laisse des lignes non ecrites
    save_all_rows(accumulated_rows, out_path)

    print(f"\nTermine. {len(to_scrape) - n_errors} matchs scrapes avec succes, {n_errors} erreurs.")
    paths = _split_paths(out_path)
    for label in ["ATP + Grand Chelem", "Challenger"]:
        p = paths[label]
        if Path(p).exists():
            n = len(pd.read_csv(p))
            print(f"  {label:20s} -> {p} ({n} lignes)")


def main():
    parser = argparse.ArgumentParser(description="Scrape en batch les stats de tous les matchs d'un CSV")
    parser.add_argument("input_csv", help="CSV contenant une colonne 'Stats_URL' (+ contexte Round/Duration/etc.)")
    parser.add_argument("--out", default="stats_all_matches.csv")
    parser.add_argument("--delay-min", type=float, default=2.0)
    parser.add_argument("--delay-max", type=float, default=5.0)
    parser.add_argument("--no-headless", action="store_true")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--flush-every", type=int, default=1, help="Reecrit le CSV complet toutes les N lignes (1 = a chaque match, plus sur pour de tres gros volumes)")
    args = parser.parse_args()

    run_batch(
        args.input_csv,
        args.out,
        delay_range=(args.delay_min, args.delay_max),
        headless=not args.no_headless,
        debug=args.debug,
        flush_every=args.flush_every,
    )


if __name__ == "__main__":
    main()