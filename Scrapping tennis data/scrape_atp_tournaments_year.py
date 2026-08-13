"""
============================================================
LISTE DES TOURNOIS (ATP + CHALLENGER) D'UNE ANNEE DONNEE
============================================================
Source : https://www.atptour.com/en/scores/results-archive?year=YYYY

Structure confirmee via devtools :
    div.atp_results-archive > div.tournament-list
      ul.events (plusieurs, un par lot/mois selon le rendu de la page)
        li
          div.tournament-info      -> nom, pays, dates, categorie (ATP/Challenger), vainqueurs
          div.cta-holder
          div.non-live-cta
            a.results[href="/en/scores/archive/{slug}/{tourney_id}/{year}/results"]

Meme protection Cloudflare que pour le reste du site -> reutilise
Stealth() + vrai Chrome, comme scrape_atp_matches_stealth.py.

Installation :
    pip install playwright-stealth pandas
    playwright install chrome
    playwright install-deps chrome

Utilisation :
    python scrape_atp_tournaments_year.py 2010 --out tournois_2010.csv
    python scrape_atp_tournaments_year.py 2010 2011 2012 --out tournois_multi.csv
============================================================
"""

import argparse
import re
import time
from pathlib import Path

import pandas as pd
from playwright.sync_api import sync_playwright
from playwright_stealth import Stealth

BASE_URL = "https://www.atptour.com/en/scores/results-archive?tournamentType={tournament_type}&year={year}"

TOURNAMENT_TYPES = {
    "atpgs": "ATP + Grand Chelem",
    "ch": "Challenger",
}


def scrape_tournaments_for_year(page, year, tournament_type):
    url = BASE_URL.format(tournament_type=tournament_type, year=year)
    print(f"Chargement : {url}")
    page.goto(url, wait_until="domcontentloaded", timeout=45000)

    # Laisser le temps au challenge Cloudflare de se resoudre
    page.wait_for_timeout(8000)
    title = page.title()
    if "moment" in title.lower() or "cloudflare" in title.lower():
        print("  [!] Challenge Cloudflare detecte, attente supplementaire...")
        page.wait_for_timeout(15000)
        title = page.title()
    print(f"  Titre de la page : {title!r}")

    # Cookies
    try:
        page.click("#onetrust-accept-btn-handler", timeout=3000)
    except Exception:
        pass

    try:
        page.wait_for_selector("a.results[href*='/results']", timeout=20000)
    except Exception:
        print("  [!] Timeout : aucun lien de resultats detecte apres 20s.")

    page.wait_for_timeout(1500)

    records = []
    links = page.locator("a.results[href*='/results']")
    n = links.count()
    print(f"  {n} lien(s) de resultats trouve(s).")

    for i in range(n):
        link = links.nth(i)
        try:
            href = link.get_attribute("href")
            full_url = f"https://www.atptour.com{href}" if href and href.startswith("/") else href

            # Remonter au conteneur li pour recuperer nom + categorie du tournoi
            li = link.locator("xpath=ancestor::li[1]")
            tournament_name = None
            category = None
            if li.count() > 0:
                info = li.locator("div.tournament-info")
                if info.count() > 0:
                    info_text = info.first.inner_text()
                    lines = [l.strip() for l in info_text.split("\n") if l.strip()]
                    if lines:
                        tournament_name = lines[0]
                    cat_match = [l for l in lines if re.search(r"challenger|atp\s*\d+|grand slam|masters", l, re.I)]
                    if cat_match:
                        category = cat_match[0]

            # tourney_id extrait de l'URL elle-meme (utile pour recouper avec
            # les scripts de liste de matchs / stats deja construits)
            m = re.search(r"/archive/([^/]+)/(\w+)/(\d{4})/results", full_url)
            slug, tourney_id, url_year = (m.group(1), m.group(2), m.group(3)) if m else (None, None, None)

            records.append({
                "year": year,
                "tournament_type": TOURNAMENT_TYPES.get(tournament_type, tournament_type),
                "tournament_name": tournament_name,
                "category": category,
                "tourney_id": tourney_id,
                "slug": slug,
                "results_url": full_url,
            })
        except Exception as e:
            print(f"  [!] Erreur sur un lien : {e}")
            continue

    return records


def scrape_multiple_years(years, categories=("atpgs", "ch"), headless=True):
    all_records = []
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

        for year in years:
            for tournament_type in categories:
                try:
                    records = scrape_tournaments_for_year(page, year, tournament_type)
                    all_records.extend(records)
                except Exception as e:
                    print(f"[ERREUR] Annee {year} / {tournament_type} : {e}")
                time.sleep(3)  # pause entre deux requetes

        browser.close()

    df = pd.DataFrame(all_records).drop_duplicates(subset=["results_url"])
    return df


def main():
    parser = argparse.ArgumentParser(description="Liste les tournois ATP+Challenger d'une ou plusieurs annees")
    parser.add_argument("years", nargs="+", type=int, help="Annee(s) a scraper, ex: 2010 2011 2012")
    parser.add_argument(
        "--category", choices=["atpgs", "ch", "both"], default="both",
        help="Categorie a scraper : atpgs (ATP + Grand Chelem), ch (Challenger), ou both (les deux, par defaut)"
    )
    parser.add_argument("--out", default="tournois_atp_challenger.csv")
    parser.add_argument("--no-headless", action="store_true")
    args = parser.parse_args()

    categories = ["atpgs", "ch"] if args.category == "both" else [args.category]

    df = scrape_multiple_years(args.years, categories=categories, headless=not args.no_headless)
    df.to_csv(args.out, index=False)
    print(f"\n{len(df)} tournoi(s) trouve(s) -> {args.out}")
    if not df.empty:
        print(df.groupby(["year", "tournament_type"]).size())


if __name__ == "__main__":
    main()