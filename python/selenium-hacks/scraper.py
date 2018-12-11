from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
import pandas as pd
import re
# some random comment
df = pd.read_csv('path/to/pdf/with/links')

def scrape(page):
    driver =  webdriver.Chrome(executable_path='/Users/lucienbaumgartner/Selenium/chromedriver')
    try:
        driver.get("https://www.parlament.ch/en/biografie/flavia-wasserfallen/4224")
        WebDriverWait(driver, 5).until(EC.presence_of_element_located((By.XPATH, '//*[@id="wsCouncilorGeneralInfo"]/div/div[1]/div[2]/h2')))
        elem = driver.find_element_by_xpath('//*[@id="wsCouncilorGeneralInfo"]/div/div[1]/div[2]/h2')
        txt = elem.get_attribute('innerHTML')
        if not isinstance(txt, str): txt = str(txt)
        txt = re.sub('\s{2,}', '', txt)
        # print(txt)
    except TimeoutException:
        print "Loading took too much time!"
    finally:
        driver.close()

df['name'] = [scrape(str(row)) for row in df['links']]
