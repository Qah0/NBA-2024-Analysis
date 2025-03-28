import requests
from bs4 import BeautifulSoup
import pandas as pd

HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
}

def scrape_player_stats(year):
    url = f"https://www.basketball-reference.com/leagues/NBA_{year}_per_game.html"
    
    # Requête HTTP
    response = requests.get(url, headers=HEADERS)
    if response.status_code != 200:
        print(f"Erreur: Impossible d'accéder à la page (statut {response.status_code})")
        return pd.DataFrame()
    
    # Parsing HTML
    soup = BeautifulSoup(response.text, 'html.parser')
    table = soup.find('table', {'id': 'per_game_stats'})
    if not table:
        print("Erreur: Tableau des statistiques non trouvé")
        return pd.DataFrame()
    
    # Extraction des en-têtes
    headers = [th.get('aria-label') or th.get_text() for th in table.find_all('th')][:30]
    
    # Extraction des données
    rows = []
    for row in table.find_all('tr')[1:]:
        cols = row.find_all(['th', 'td'])
        if len(cols) > 20:  # Filtre les lignes vides
            rows.append([col.get_text(strip=True) for col in cols[:30]])
    
    if not rows:
        print("Avertissement: Aucune donnée de joueur trouvée")
        return pd.DataFrame()
    
    # Création du DataFrame
    df = pd.DataFrame(rows, columns=headers)
    
    # Colonnes numériques à convertir
    numeric_cols = ['Age', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG%', '3P', '3PA', 
                   '3P%', '2P', '2PA', '2P%', 'FT', 'FTA', 'FT%', 'ORB', 
                   'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS']
    
    # Conversion des types
    for col in numeric_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    
    return df

if __name__ == "__main__":
    # Paramètre: année de la saison (2024 pour la saison 2023-2024)
    YEAR = 2024
    
    # Scraping des stats joueurs
    player_stats = scrape_player_stats(YEAR)
    
    if not player_stats.empty:
        # Sauvegarde en CSV
        filename = f'nba_player_stats_{YEAR-1}_{YEAR}.csv'
        player_stats.to_csv(filename, index=False)
        print(f"Statistiques des joueurs sauvegardées dans {filename}")
        print(f"Joueurs trouvés: {len(player_stats)}")
        print("Colonnes disponibles:", list(player_stats.columns))
    else:
        print("Aucune donnée n'a été récupérée")