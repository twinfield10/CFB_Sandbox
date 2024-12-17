import time
from datetime import datetime, timedelta
import requests
import polars as pl
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import TimeoutException

chrome_options  = webdriver.ChromeOptions()
chrome_options .add_argument('--ignore-certificate-errors')
chrome_options .add_argument('--ignore-ssl-errors')

def scrape_slim_pinny(period):
    start_time = time.time()
    url = f"https://www.pinnacle.com/en/football/ncaa/matchups/#period:{period}"

    if period == 1:
        lab = "1H"
    elif period == 0:
        lab = "GAME"
    elif period == 3:
        lab = "1Q"
    elif period == "team_total":
        lab = "TT"
    else:
        lab = ""

    driver = webdriver.Chrome(options=chrome_options)
    
    # Load URL
    driver.get(url)
    driver.maximize_window()
    driver.execute_script("document.body.style.zoom = '75%'")

    # Get Game Date
    try:
        # Let Page Load
        #time.sleep(10)
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR,'div[class="contentBlock square"]')))

        # Get the page source after elements are loaded
        html = driver.page_source
        soup = BeautifulSoup(html, 'html.parser')

        # Date Handling:
        date_bars = [div.get_text() for div in soup.find_all('div', class_=lambda x: x and 'dateBar-' in x)]
        today_date = datetime.now().date()
        generated_dates = []

        # Index Rows Between Dates
        all_divs = soup.find_all('div')
        date_bar_indices = [
            idx for idx, div in enumerate(all_divs) if div.get('class') and any('dateBar-' in cls for cls in div['class'])
        ]
        row_indices = [
            idx for idx, div in enumerate(all_divs) if div.get('class') and any('row-' in cls for cls in div['class'])
        ]

        # Get Number of Games Between Date Bars
        rows_between_dates = []
        for i in range(len(date_bar_indices)):
            start_index = date_bar_indices[i]
            if i == len(date_bar_indices) - 1:
                rows_between = sum(idx > start_index for idx in row_indices)
            else:
                end_index = date_bar_indices[i + 1]
                rows_between = sum(start_index < idx < end_index for idx in row_indices)
            rows_between_dates.append(rows_between)

        # Clean Dates + Duplicate For List
        generated_dates = []
        for item, rows_between in zip(date_bars, rows_between_dates):
            parts = item.split('(')
            day_info = parts[0].strip()
            num_repeats = rows_between - 1

            if day_info.lower() == 'today':
                base_date = today_date
            elif day_info.lower() == 'tomorrow':
                base_date = today_date + timedelta(days=1)
            else:
                base_date = datetime.strptime(day_info, "%a, %b %d, %Y")
            
            # Format
            repeated_dates = [base_date.strftime('%Y-%m-%d') for _ in range(num_repeats)]
            generated_dates.extend(repeated_dates)

        print(generated_dates)

        # Begin Betting Information Pull
        rows = soup.select('div[class^="row-"]')

        # Game Information
        links_list = []
        team_list = []
        price_list = []
        label_list = []

        # Iterate over each row
        for row in rows:
            # Find the <a> tag
            a_tag = row.find('a', href=True)
        
            # LINK
            if a_tag:
                link = a_tag['href']
                if link is not None and "games" not in link:
                    live_text = [span.get_text() for span in row.find_all('span', class_=lambda x: x and 'live__' in x)]
                    if not live_text:
                        links_list.append(link)
        
                        ## TEAMS
                        team_list.append([span.get_text() for span in row.find_all('span', class_=lambda x: x and 'ellipsis gameInfoLabel' in x)])
                        team_list = [lst for lst in team_list if lst]
    
                        ## LINES
                        price_list.append([span.get_text() for span in row.find_all('span', class_=lambda x: x and 'price-' in x)])
                        label_list.append([span.get_text() for span in row.find_all('span', class_=lambda x: x and 'label-' in x)])

        df = pl.DataFrame({
                'link': links_list,
                'teams': team_list,
                'prices': price_list,
                'labels': label_list,
                'officialDate': generated_dates
            })
    except TimeoutException:
        print(f"Failed to retrieve date for {url}")
        
    end_time = time.time()
    elap_time = round(end_time - start_time, 1)
    print(f"{lab} Pinny Data Loaded in {elap_time} Seconds")

    driver.quit()
    return df

    


# Convert the lists to a Polars DataFrame
fg_df = scrape_slim_pinny(period = 0) \
    .with_columns(
    pl.col("teams").list.get(0).alias("Away"),
    pl.col("teams").list.get(1).alias("Home"),
    pl.col("prices").list.get(0).cast(pl.Float32).alias("Away_Moneyline_Price"),
    pl.col("prices").list.get(1).cast(pl.Float32).alias("Home_Moneyline_Price"),
    pl.col("prices").list.get(2).cast(pl.Float32).alias("Away_Spread_Price"),
    pl.col("prices").list.get(3).cast(pl.Float32).alias("Home_Spread_Price"),
    pl.col("prices").list.get(4).cast(pl.Float32).alias("Over_Total_Price"),
    pl.col("prices").list.get(5).cast(pl.Float32).alias("Under_Total_Price"),
    pl.col("labels").list.get(0).str.replace("//+", "").cast(pl.Float32).alias("Away_Spread_Value"),
    pl.col("labels").list.get(1).str.replace("//+", "").cast(pl.Float32).alias("Home_Spread_Value"),
    pl.col("labels").list.get(2).str.replace("//+", "").cast(pl.Float32).alias("Over_Total_Value"),
    pl.col("labels").list.get(3).str.replace("//+", "").cast(pl.Float32).alias("Under_Total_Value")
) \
.drop('teams', 'prices', 'labels')

print(fg_df.head())
clean_path = f'Data/Pinnacle/Pinnacle_Base.csv'
fg_df.write_csv(clean_path)

# Convert the lists to a Polars DataFrame
#h1_df = scrape_slim_pinny(period = 1) \
#    .with_columns(
#    pl.when(pl.col('prices').list.lengths == 6).then(pl.col("prices").list.get(0).cast(pl.Float32)).otherwise(pl.lit(None)).alias("HALF1_Away_ML_Price"),
#    pl.when(pl.col('prices').list.lengths == 6).then(pl.col("prices").list.get(1).cast(pl.Float32)).otherwise(pl.lit(None)).alias("HALF1_Home_ML_Price"),
#    pl.when(pl.col('prices').list.lengths == 6).then(pl.col("prices").list.get(2).cast(pl.Float32)).otherwise(pl.col("prices").list.get(0).cast(pl.Float32)).alias("HALF1_Away_Spread_Price"),
#    pl.when(pl.col('prices').list.lengths == 6).then(pl.col("prices").list.get(3).cast(pl.Float32)).otherwise(pl.col("prices").list.get(1).cast(pl.Float32)).alias("HALF1_Home_Spread_Price"),
#    pl.when(pl.col('prices').list.lengths == 6).then(pl.col("prices").list.get(4).cast(pl.Float32)).otherwise(pl.col("prices").list.get(2).cast(pl.Float32)).alias("HALF1_Over_Price"),
#    pl.when(pl.col('prices').list.lengths == 6).then(pl.col("prices").list.get(5).cast(pl.Float32)).otherwise(pl.col("prices").list.get(3).cast(pl.Float32)).alias("HALF1_Under_Price"),
#
#    pl.col("labels").list.get(0).str.replace("//+", "").cast(pl.Float32).alias("HALF1_Away_Spread_Value"),
#    pl.col("labels").list.get(1).str.replace("//+", "").cast(pl.Float32).alias("HALF1_Home_Spread_Value"),
#    pl.col("labels").list.get(2).str.replace("//+", "").cast(pl.Float32).alias("HALF1_Over_Value"),
#    pl.col("labels").list.get(3).str.replace("//+", "").cast(pl.Float32).alias("HALF1_Under_Value")
#) \
#.with_columns(
#    pl.when(pl.col('HALF1_Away_Spread_Value') == 0.0)
#      .then(pl.col('HALF1_Away_Spread_Price'))
#      .otherwise(pl.col('HALF1_Away_ML_Price'))
#      .alias('HALF1_Away_ML_Price'),
#
#    pl.when(pl.col('HALF1_Home_Spread_Value') == 0.0)
#      .then(pl.col('HALF1_Home_Spread_Price'))
#      .otherwise(pl.col('HALF1_Home_ML_Price'))
#      .alias('HALF1_Home_ML_Price')
#) \
#.drop('teams', 'prices', 'labels')
#
#
## Combine
#final_df = fg_df.join(h1_df, on = 'link' , how = 'left')
#
#
#clean_path = 'Data/PinnyData/PinnyData_Slim.csv'
#final_df.write_csv(clean_path)
#print(final_df.head())