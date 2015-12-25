
# Imports
from selenium import webdriver
from selenium.webdriver.common import action_chains, keys
import time

# Includes
execfile("/home/ff/FantasyFootball/Python_Examples/GetCredentials.py")

def getWebDriverLoggedInToFanDuel() :
    driver = webdriver.Firefox()
    driver.implicitly_wait(10)
    driver.get("https://www.fanduel.com/p/login")
    # time.sleep(10)

    credentials =  getCredentials("FanDuel")
    print credentials
    driver.find_element_by_xpath("//input[@type='email']").send_keys(credentials[0])
    driver.find_element_by_xpath("//input[@type='password']").clear()
    driver.find_element_by_xpath("//input[@type='password']").send_keys(credentials[1])
    driver.find_element_by_xpath("//input[@type='submit']").click()
    return driver

#f = open("/home/ff/test.html",'w')
#f.write(driver.page_source.encode('utf-8'))
#f.close()
#print "File /home/ff/test.html written"