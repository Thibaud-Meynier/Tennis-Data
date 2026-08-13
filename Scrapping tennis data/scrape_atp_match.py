"""
============================================================
SCRAPING D'UN MATCH ATP OFFICIEL (atptour.com)
============================================================
URL type :
    https://www.atptour.com/en/scores/match-stats/archive/{year}/{tourney_id}/{match_id}

Structure du site confirmee via devtools (Vue.js, data-v-app -> rendu
JS necessaire, meme approche Playwright que pour Flashscore) :

    div.stats-score > div.scores
        div.tournament                              -> nom/lieu/annee du tournoi
        div.match > div.match-content
            div.stats-item (x2, un par joueur)
                div.player-info
                    div.name                         -> nom du joueur
                    div.winner > span.icon-checkmark  -> present SEULEMENT si vainqueur
                div.scores
                    div.score-item > span             -> jeux gagnes, un par set

    div.stats-vs-stats div.stats-internal
        div.stats-group (un par section : "Service Stats", etc.)
            div.stats-group-title                    -> nom de la section (non retenu)
            ul > li
                div.player-stats-item > div.value     -> valeur joueur 1
                div.stats-item-legend                 -> nom du stat (ex: "Aces")
                div.opponent-stats-item > div.value    -> valeur joueur 2

Installation : identique a scrap_flashscore.py (playwright + pandas)
    pip install playwright pandas
    playwright install chromium
    playwright install-deps chromium   # si libs systeme manquantes

Utilisation :
    python scrape_atp_match.py "https://www.atptour.com/en/scores/match-stats/archive/2010/580/ms001" --out match_atp.csv
============================================================
"""

import argparse
import re
import sys
import time
import random
from pathlib import Path

import pandas as pd
from playwright.sync_api import sync_playwright, TimeoutError as PWTimeout


# ------------------------------------------------------------
# 1. Chargement de la page rendue
# ------------------------------------------------------------

def get_rendered_page(page, url, timeout=20000, max_retries=3):
    last_error = None
    for attempt in range(1, max_retries + 1):
        try:
            page.goto(url, wait_until="domcontentloaded", timeout=timeout)
            break
        except PWTimeout as e:
            last_error = e
            print(f"  [!] Tentative {attempt}/{max_retries} : timeout au chargement, nouvel essai...", file=sys.stderr)
            time.sleep(2 * attempt)  # backoff progressif : 2s, 4s, 6s...
    else:
        raise last_error

    try:
        page.wait_for_selector(".stats-item, .match-stats", timeout=timeout)
    except PWTimeout:
        print("  [!] Timeout en attendant .stats-item -> structure differente ou page non chargee.", file=sys.stderr)

    # Cookies (bandeau generique)
    for sel in ["#onetrust-accept-btn-handler", ".cookie-accept", "[data-testid='accept-all']"]:
        try:
            page.click(sel, timeout=1500)
            break
        except PWTimeout:
            continue

    try:
        page.wait_for_load_state("networkidle", timeout=6000)
    except PWTimeout:
        pass

    return page.content()


# ------------------------------------------------------------
# 2. Extraction joueurs + score par set (les 2 blocs .stats-item)
# ------------------------------------------------------------

def extract_players_and_sets(page):
    blocks = page.locator("div.match-stats div.stats-item")
    n = blocks.count()
    if n < 2:
        print(f"  [!] {n} bloc(s) .stats-item trouve(s) (2 attendus) -> structure inattendue.", file=sys.stderr)
        return None

    # Attend que le nombre de score-item (sets) se stabilise avant de lire,
    # pour eviter de rater un set qui se rend un peu plus tard que les autres
    # (race condition : .count() ne fait AUCUNE attente, contrairement a
    # _safe_text qui utilise wait_for()).
    _wait_for_stable_count(page.locator("div.match-stats div.score-item"))

    players = []
    for i in range(min(n, 2)):
        block = blocks.nth(i)

        name = _safe_text(block, "div.player-info div.name")
        player_url = _safe_attr(block, "div.player-info div.name a", "href")
        if player_url and player_url.startswith("/"):
            player_url = f"https://www.atptour.com{player_url}"
        is_winner = block.locator("div.winner span.icon-checkmark").count() > 0

        sets = []
        score_items = block.locator("div.scores div.score-item")
        for j in range(score_items.count()):
            txt = score_items.nth(j).inner_text().strip()
            sets.append(_first_int(txt))

        players.append({
            "name": _clean_text(name),
            "player_url": player_url,
            "is_winner": is_winner,
            "sets": sets,
        })

    return players


def _wait_for_stable_count(locator, checks_needed=3, interval_ms=400, max_wait_ms=6000):
    """Attend que locator.count() renvoie la meme valeur 'checks_needed' fois
    de suite avant de continuer (evite de lire un DOM encore en train de se
    completer, ex: un 3e set qui se rend un peu apres les 2 premiers)."""
    last_count = -1
    stable_streak = 0
    elapsed = 0
    while elapsed < max_wait_ms:
        current = locator.count()
        if current == last_count:
            stable_streak += 1
            if stable_streak >= checks_needed:
                return current
        else:
            stable_streak = 0
        last_count = current
        time.sleep(interval_ms / 1000)
        elapsed += interval_ms
    return last_count


def _safe_attr(locator, selector, attr, timeout=8000):
    try:
        el = locator.locator(selector).first
        el.wait_for(timeout=timeout)
        return el.get_attribute(attr)
    except Exception:
        print(f"  [!] Timeout/echec sur le selecteur '{selector}' (attribut {attr})", file=sys.stderr)
        return None


def _safe_text(locator, selector, timeout=8000):
    try:
        el = locator.locator(selector).first
        el.wait_for(timeout=timeout)
        return el.inner_text()
    except Exception:
        print(f"  [!] Timeout/echec sur le selecteur '{selector}' (texte)", file=sys.stderr)
        return None


def _first_int(s):
    m = re.match(r"^\d+", s or "")
    return int(m.group()) if m else None


# ------------------------------------------------------------
# 3. Extraction des stats (Service Stats, Return Stats, etc.)
# ------------------------------------------------------------

def extract_stats(page):
    # La page a des sous-onglets "Match / Set 1 / Set 2 / Set 3" dont le
    # contenu est TOUS present dans le DOM simultanement (juste cache en
    # CSS selon l'onglet actif). Sans scope, on melange les lignes du
    # total du match avec celles de chaque set -> decalage de colonnes
    # selon que le match fait 2 ou 3 sets (nombre de repetitions du
    # meme libelle de stat different d'un match a l'autre).
    #
    # Le conteneur du tab "Match" (agregat) porte la classe
    # "stas-internal--match" (coquille cote ATP : "stas" au lieu de
    # "stats", presente telle quelle dans le DOM).
    scoped = page.locator("div.stas-internal--match div.stats-group-items li")
    rows = scoped if scoped.count() > 0 else page.locator("div.stats-group-items li")
    n = rows.count()
    if n == 0:
        print("  [!] Aucune ligne de stats trouvee -> verifier que la page a bien charge les stats.", file=sys.stderr)
        return []

    stats = []
    for i in range(n):
        row = rows.nth(i)
        legend = _row_text(row, "div.stats-item-legend")
        p1_val = _row_text(row, "div.player-stats-item div.value")
        p2_val = _row_text(row, "div.opponent-stats-item div.value")
        if legend:
            stats.append({
                "stat": _clean_text(legend),
                "player1_value": _clean_text(p1_val),
                "player2_value": _clean_text(p2_val),
            })
    return stats


def _row_text(row_locator, selector):
    try:
        return row_locator.locator(selector).first.inner_text()
    except Exception:
        return None


def _clean_text(s):
    return re.sub(r"\s+", " ", (s or "")).strip()


# ------------------------------------------------------------
# 4. Metadonnees depuis l'URL (annee, tourney_id, match_id)
# ------------------------------------------------------------

def parse_url_metadata(url):
    m = re.search(r"/archive/(\d{4})/(\w+)/(\w+)", url)
    if not m:
        return {"year": None, "tourney_id": None, "match_id": None}
    return {"year": m.group(1), "tourney_id": m.group(2), "match_id": m.group(3)}


# ------------------------------------------------------------
# 5. Orchestration pour une URL de match
# ------------------------------------------------------------

def scrape_atp_match(page, url, debug=False):
    print(f"Scraping : {url}")
    html = get_rendered_page(page, url)

    if debug:
        Path("debug_atp_match.html").write_text(html, encoding="utf-8")
        print("  [debug] HTML sauvegarde dans debug_atp_match.html")

    meta = parse_url_metadata(url)
    players = extract_players_and_sets(page)
    stats_ = extract_stats(page)

    row = {"url": url, **meta}

    if players is None or len(players) < 2:
        row["Winner"] = None
        row["Loser"] = None
        return row

    winner_idx = 0 if players[0]["is_winner"] else 1
    loser_idx = 1 - winner_idx
    winner, loser = players[winner_idx], players[loser_idx]

    row["Winner"] = winner["name"]
    row["Loser"] = loser["name"]
    row["Winner_url"] = winner["player_url"]
    row["Loser_url"] = loser["player_url"]

    n_sets = max(len(winner["sets"]), len(loser["sets"]))
    for s in range(n_sets):
        row[f"winner_jeux_set{s + 1}"] = winner["sets"][s] if s < len(winner["sets"]) else None
        row[f"loser_jeux_set{s + 1}"] = loser["sets"][s] if s < len(loser["sets"]) else None

    seen = {}
    for st in stats_:
        base = re.sub(r"\s+", "_", st["stat"])
        seen[base] = seen.get(base, 0) + 1
        key = base if seen[base] == 1 else f"{base}.{seen[base] - 1}"

        # player1/player2 correspondent a l'ordre DOM (winner_idx determine lequel est qui)
        p_vals = [st["player1_value"], st["player2_value"]]
        row[f"{key}_winner_value"] = p_vals[winner_idx]
        row[f"{key}_loser_value"] = p_vals[loser_idx]

    return row


# ------------------------------------------------------------
# 6. Batch + CLI
# ------------------------------------------------------------

def scrape_matches_batch(urls, delay_range=(2, 5), debug=False, headless=True):
    results = []
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless)
        context = browser.new_context(
            user_agent=(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
                "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
            )
        )
        page = context.new_page()

        for i, url in enumerate(urls):
            try:
                row = scrape_atp_match(page, url, debug=debug)
            except Exception as e:
                print(f"  [ERREUR] Echec sur {url} : {e}", file=sys.stderr)
                row = {"url": url, "Winner": None, "Loser": None, "scrape_error": str(e)}
            results.append(row)

            if i < len(urls) - 1:
                time.sleep(random.uniform(*delay_range))

        browser.close()

    return pd.DataFrame(results)


def main():
    parser = argparse.ArgumentParser(description="Scraping de match(s) ATP officiel")
    parser.add_argument("url", nargs="?", help="URL unique d'un match")
    parser.add_argument("--urls", help="Fichier texte contenant une URL par ligne")
    parser.add_argument("--out", default="match_atp.csv")
    parser.add_argument("--no-headless", action="store_true")
    parser.add_argument("--debug", action="store_true")
    args = parser.parse_args()

    if args.urls:
        urls = [u.strip() for u in Path(args.urls).read_text(encoding="utf-8").splitlines() if u.strip()]
    elif args.url:
        urls = [args.url]
    else:
        parser.error("Fournir soit une URL en argument, soit --urls fichier.txt")

    df = scrape_matches_batch(urls, debug=args.debug, headless=not args.no_headless)
    df.to_csv(args.out, index=False)
    print(f"\n{len(df)} match(s) exporte(s) vers {args.out}")


if __name__ == "__main__":
    main()