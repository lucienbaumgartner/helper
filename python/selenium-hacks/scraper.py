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
chrome_options.add_argument("--headless")

df = pd.read_csv('/Users/lucienbaumgartner/share/dev-temp/Link_List_NR_SR.csv')
df = df[1:100]
print(df)

def scrape(page):
    driver =  webdriver.Chrome(executable_path='/Users/lucienbaumgartner/Selenium/chromedriver', options = chrome_options)
    try:
        driver.get(page)
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, '//*[@id="wsCouncilorGeneralInfo"]/div/div[1]/div[2]/h2')))
        elem = driver.find_element_by_xpath('//*[@id="wsCouncilorGeneralInfo"]/div/div[1]/div[2]/h2')
        txt = elem.get_attribute('innerHTML').encode('utf-8').strip()
        if not isinstance(txt, str): txt = str(txt)
        txt = re.sub('\s{2,}', '', txt)
        print(txt)
        return txt
    except TimeoutException:
        print "Loading took too much time!"
    finally:
        driver.close()

df['name'] = [scrape(str(row)) for row in df['Links']]
print(df)
df.to_csv('/Users/lucienbaumgartner/share/dev-temp/pyscraper.csv')
