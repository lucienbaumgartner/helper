from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
import pandas as pd
import re

chrome_options = Options()
chrome_options.add_argument('--headless')

df = pd.read_csv('/path/to/csv')

def scrape(page):
    driver =  webdriver.Chrome(executable_path='/path/to/chromedriver', options = chrome_options)
    try:
        driver.get(page)
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, 'some-xpath')))
        elem = driver.find_element_by_xpath('some-xpath')
        txt = elem.get_attribute('innerHTML').encode('utf-8').strip()
        if not isinstance(txt, str): txt = str(txt)
        txt = re.sub('\s{2,}', '', txt) # get rid of excess whitespace
        return txt
    except TimeoutException:
        print('Loading took too much time!')
    finally:
        driver.close()

df['new-column'] = [scrape(str(row)) for row in df['column-name-links']]

df.to_csv('/path/to/wd')
