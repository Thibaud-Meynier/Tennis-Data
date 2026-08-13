"""
============================================================
EXTRACTION DES MATCHS D'UN TOURNOI ATP AVEC LIENS DE STATS + ROUND
(avec contournement du challenge anti-bot Cloudflare)
============================================================
Utilisable seul (CLI, une URL) ou importe comme fonction par
scrape_atp_tournament_matches_batch.py pour boucler sur plusieurs
tournois dans la meme session de navigateur.

Installation prealable :
    pip install playwright-stealth
    playwright install chrome        # installe le vrai Chrome (pas Chromium)
    playwright install-deps chrome   # si libs systeme manquantes
============================================================
"""
import argparse
import time
from pathlib import Path

import pandas as pd
from playwright.sync_api import sync_playwright
from playwright_stealth import Stealth


def _wait_for_stable_link_count(page, selector, checks_needed=3, interval_ms=500, max_wait_ms=15000):
    """Attend que page.locator(selector).count() renvoie la meme valeur
    'checks_needed' fois de suite avant de continuer (evite de lire un DOM
    encore en train de se completer, ex: accordeons de qualifs clique en
    dernier qui n'ont pas fini de rendre leurs cartes de match)."""
    last_count = -1
    stable_streak = 0
    elapsed = 0
    while elapsed < max_wait_ms:
        current = page.locator(selector).count()
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


def get_tournament_match_urls(page, tournament_url, debug=False):
    """Scrape la liste des matchs (Round, Duration, Stats_URL) d'un tournoi,
    en reutilisant une page/session de navigateur deja ouverte."""
    print(f"Chargement : {tournament_url}")
    page.goto(tournament_url, wait_until="domcontentloaded", timeout=45000)

    page.wait_for_timeout(8000)
    title = page.title()
    if "moment" in title.lower() or "cloudflare" in title.lower():
        print("  [!] Challenge Cloudflare detecte, attente supplementaire...")
        page.wait_for_timeout(15000)
        title = page.title()
    print(f"  Titre de la page : {title!r}")

    try:
        page.click("#onetrust-accept-btn-handler", timeout=3000)
    except Exception:
        pass

    # On ne clique QUE sur les rounds fermes (data-default-state="close").
    # Certains rounds (souvent la Finale, parfois les demies) sont OUVERTS
    # PAR DEFAUT (data-default-state="open") -> cliquer dessus sans
    # verification les REFERME (bouton toggle), ce qui fait disparaitre
    # leurs matchs de la requete finale ciblant ".atp_accordion-content--expanded".
    all_items = page.locator("div.atp_accordion-item")
    n_total = all_items.count()
    print(f"  {n_total} round(s) au total.")

    # Boucle auto-corrective : a CHAQUE iteration, on re-cherche le premier
    # round ferme-par-defaut ET PAS ENCORE ouvert (plutot que de figer une
    # liste d'index au debut). Necessaire car ouvrir un round injecte
    # beaucoup de DOM (les cartes de match), ce qui peut invalider des
    # index de position calcules a l'avance.
    unexpanded_selector = "div.atp_accordion-item[data-default-state='close']:not(:has(.atp_accordion-content--expanded))"
    max_attempts = n_total * 3  # marge large : la boucle se re-corrige a chaque iteration
    stalled_count = 0
    last_remaining = -1

    for attempt in range(max_attempts):
        remaining = page.locator(unexpanded_selector)
        n_remaining = remaining.count()
        if n_remaining == 0:
            break

        if n_remaining == last_remaining:
            stalled_count += 1
            if stalled_count >= 5:
                print(f"    - [!] Plus aucune progression apres {attempt} tentatives ({n_remaining} round(s) restant(s)), abandon.")
                break
        else:
            stalled_count = 0
        last_remaining = n_remaining

        item = remaining.first
        h4 = item.locator("div.atp_accordion-header h4")
        round_label = h4.first.inner_text().strip() if h4.count() > 0 else f"round_inconnu_{attempt}"

        try:
            header = item.locator("div.atp_accordion-header").first
            header.scroll_into_view_if_needed(timeout=8000)
            header.click(force=True, timeout=8000)
        except Exception:
            try:
                item.locator("div.atp_accordion-header").first.evaluate("el => el.click()")
            except Exception as e:
                print(f"    - [!] Clic impossible sur '{round_label}' : {type(e).__name__}")

        # Pas de wait_for(visible) ici : trop lent/peu fiable sur cette page.
        # On verifie juste la PRESENCE de la classe (pas le rendu visuel),
        # via un court sondage, puis on boucle -> la re-requete au tour
        # suivant confirmera naturellement si ca a fonctionne ou non.
        for _ in range(6):
            if item.locator(".atp_accordion-content--expanded").count() > 0:
                print(f"    - Ouvert : '{round_label}'")
                break
            time.sleep(0.3)
        # Si pas confirme dans ce court sondage, pas grave : l'attente de
        # stabilisation finale (apres la boucle) rattrape le coup dans la
        # grande majorite des cas -> pas la peine de logger un avertissement
        # a chaque fois, ca n'indique pas un vrai probleme.

    # Attend que le nombre de liens de stats arrete d'augmenter avant de les
    # recuperer (marge de securite supplementaire, budget genereux vu la
    # lenteur generale observee sur cette page).
    link_selector = ".atp_accordion-content--expanded a[href*='stats'], .atp_accordion-content--expanded a[href*='match-stats']"
    _wait_for_stable_link_count(page, link_selector, max_wait_ms=20000)

    if debug:
        print("  --- Diagnostic round par round ---")
        for i in range(n_total):
            item = all_items.nth(i)
            h4 = item.locator("div.atp_accordion-header h4")
            round_label = h4.first.inner_text().strip() if h4.count() > 0 else "???"
            is_expanded = item.locator(".atp_accordion-content--expanded").count() > 0
            n_links = item.locator("a[href*='stats'], a[href*='match-stats']").count()
            print(f"    '{round_label}' : expanded={is_expanded}, liens={n_links}")

    stats_links = page.locator(link_selector).all()
    print(f"  {len(stats_links)} lien(s) de stats trouve(s).")

    records = []
    for link in stats_links:
        try:
            href = link.get_attribute("href")
            full_link = f"https://www.atptour.com{href}" if href and href.startswith("/") else href

            match_div = link.locator(
                "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' match ')][1]"
            )
            round_name = None
            duration = None
            if match_div.count() > 0:
                header_span = match_div.locator("div.match-header span").first
                if header_span.count() > 0:
                    strong_el = header_span.locator("strong")
                    if strong_el.count() > 0:
                        round_name = strong_el.first.inner_text().strip().rstrip(" -").strip()
                duration_spans = match_div.locator("div.match-header span")
                if duration_spans.count() > 1:
                    duration = duration_spans.nth(1).inner_text().strip()

            records.append({
                "Round": round_name,
                "Duration": duration,
                "Stats_URL": full_link,
            })
        except Exception:
            continue

    if len(stats_links) == 0 and debug:
        Path("debug_atp_blocked.html").write_text(page.content(), encoding="utf-8")
        print("  [debug] HTML sauvegarde dans debug_atp_blocked.html pour diagnostic")

    return records


def _launch_stealth_page(headless=True):
    """Cree un navigateur+page avec stealth pour un usage standalone (CLI)."""
    cm = Stealth().use_sync(sync_playwright())
    p = cm.__enter__()
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
    return cm, browser, page


def main():
    parser = argparse.ArgumentParser(description="Liste les matchs (round, duree, URL stats) d'un tournoi ATP")
    parser.add_argument("url", help="URL de la page resultats du tournoi (.../archive/{slug}/{id}/{year}/results)")
    parser.add_argument("--out", default="matches_tournoi.csv")
    parser.add_argument("--no-headless", action="store_true")
    parser.add_argument("--debug", action="store_true")
    args = parser.parse_args()

    cm, browser, page = _launch_stealth_page(headless=not args.no_headless)
    try:
        records = get_tournament_match_urls(page, args.url, debug=args.debug)
    finally:
        browser.close()
        cm.__exit__(None, None, None)

    df = pd.DataFrame(records).drop_duplicates(subset=["Stats_URL"])
    if not df.empty:
        df.to_csv(args.out, index=False, encoding="utf-8-sig")
        print(f"\n[OK] {len(df)} matchs avec liens de stats enregistrés dans '{args.out}' !")
        print(df["Round"].value_counts())
    else:
        print("\n[!] Aucun lien de stats extrait.")


if __name__ == "__main__":
    main()