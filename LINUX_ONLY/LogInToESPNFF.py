
# Imports
from selenium import webdriver
from selenium.webdriver.common import action_chains, keys
import time

# Includes
execfile("/home/ff/FantasyFootball/LINUX_ONLY/GetCredentials.py")


def getWebDriverLoggedInToESPN() :
    driver = webdriver.Firefox()
    driver.implicitly_wait(10)
    driver.get("http://games.espn.go.com/ffl/signin?redir=http%3A%2F%2Fgames.espn.go.com%2Fffl%2Fleaguerosters%3FleagueId%3D1111554")
    # time.sleep(10)

    credentials =  getCredentials("ESPN")
    driver.switch_to.frame(driver.find_element_by_id("disneyid-iframe"))
    driver.find_element_by_xpath("//input[@type='text']").send_keys(credentials[0])
    driver.find_element_by_xpath("//input[@type='password']").clear()
    driver.find_element_by_xpath("//input[@type='password']").send_keys(credentials[1])
    driver.find_element_by_xpath("//button[@type='submit']").click()
    return driver

# f = open("/home/ff/test.html",'w')
# f.write(driver.page_source.encode('utf-8'))
# f.close()
# print "File /home/ff/test.html written"
