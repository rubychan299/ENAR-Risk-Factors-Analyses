import os
import requests
import pandas as pd
from bs4 import BeautifulSoup

# NHANES Dietary data page
#url = 'https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary'
#url = "https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination"
#url = 'https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics'
#url = 'https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Laboratory'
url = 'https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Questionnaire'

# Save directory for XPT files
save_directory = "/Users/taehyo/Library/CloudStorage/Dropbox/NYU/Research/Research/Code/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/questionnaire"

def get_xpt_links_filtered(url, year_filter="2021-2023"):
    # Send a GET request to the website
    response = requests.get(url)
    # Parse the HTML content of the page
    soup = BeautifulSoup(response.content, 'html.parser')

    # Find the data table
    table = soup.find('table')
    if not table:
        print("No table found on the webpage.")
        return []

    # Iterate through table rows to find .XPT links for the specified years
    filtered_links = []
    rows = table.find_all('tr')[1:]  # Skip the header row
    for row in rows:
        cells = row.find_all('td')
        if len(cells) < 2:
            continue  # Skip rows that don't have enough cells
        year_text = cells[0].text.strip()  # 'Years' information is in the first cell
        if year_filter in year_text:
            link_tag = cells[3].find('a', href=True)  # 'File' link is in the second cell
            if link_tag and link_tag['href'].endswith('.xpt'):
                filtered_links.append(link_tag['href'])

    return filtered_links

def download_xpt_files(links, base_url, save_directory):
    # Ensure the save directory exists
    if not os.path.exists(save_directory):
        os.makedirs(save_directory)

    for link in links:
        full_url = base_url + link
        response = requests.get(full_url)
        file_name = link.split('/')[-1]
        file_path = os.path.join(save_directory, file_name)
        with open(file_path, 'wb') as file:
            file.write(response.content)
        print(f'Downloaded {file_name} to {file_path}')

# Get filtered .XPT links
xpt_links_filtered = get_xpt_links_filtered(url, year_filter="2021-2023")

# Download the filtered files
download_xpt_files(xpt_links_filtered, base_url='https://wwwn.cdc.gov/', save_directory=save_directory)

