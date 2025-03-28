import requests
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
import time
from datetime import datetime

def get_monthly_games(month, year):
    url = f"https://www.basketball-reference.com/leagues/NBA_{year}_games-{month}.html"
    response = requests.get(url)
    
    if response.status_code == 404:
        return pd.DataFrame()
    
    soup = BeautifulSoup(response.text, 'html.parser')
    table = soup.find('table', {'id': 'schedule'})
    
    if not table:
        return pd.DataFrame()
    
    rows = table.find_all('tr')[1:]  
    data = []
    
    for row in rows:
        cols = row.find_all('td')
        if len(cols) >= 8:  # Vérifie qu'on a bien une ligne de match
            date = row.find('th').get_text()
            visitor = cols[1].get_text()
            pts_visitor = cols[2].get_text()
            home = cols[3].get_text()
            pts_home = cols[4].get_text()
            box_score = cols[5].find('a')['href'] if cols[5].find('a') else ''
            overtime = cols[6].get_text()
            
            data.append([
                date, visitor, pts_visitor, home, pts_home, 
                box_score, overtime, 
            ])
    
    columns = [
        'Date', 'Visitor', 'PTS_Visitor', 'Home', 'PTS_Home',
        'Box_Score', 'OT'
    ]
    
    return pd.DataFrame(data, columns=columns)

# Mois de la saison régulière NBA 2023-2024 (octobre 2023 à avril 2024)
months = ['october', 'november', 'december', 'january', 'february', 'march', 'april']
year = 2024

# Récupération de tous les matchs
all_games = pd.DataFrame()

for month in months:
    print(f"Récupération des données pour {month} {year}...")
    monthly_games = get_monthly_games(month, year)
    if not monthly_games.empty:
        all_games = pd.concat([all_games, monthly_games], ignore_index=True)
    time.sleep(5)  # Pour éviter de surcharger le serveur

# Conversion des données de points en données numériques
all_games['PTS_Visitor'] = pd.to_numeric(all_games['PTS_Visitor'], errors='coerce')
all_games['PTS_Home'] = pd.to_numeric(all_games['PTS_Home'], errors='coerce')

# Suppression des lignes ayant des valeurs manquantes
all_games = all_games.dropna(subset=['PTS_Visitor', 'PTS_Home'])

# Conversion de la date en format datetime
all_games['Date'] = pd.to_datetime(all_games['Date'])

# Ajout d'une colonne pour avoir le gagnant du match
all_games['Winner'] = np.where(
    all_games['PTS_Visitor'] > all_games['PTS_Home'],
    all_games['Visitor'],
    all_games['Home']
)

# Sauvegarde des données vers un fichier CSV
all_games.to_csv('nba_games_2023_2024.csv', index=False)

print("Extraction terminée. Données sauvegardées dans nba_games_2023_2024.csv")
