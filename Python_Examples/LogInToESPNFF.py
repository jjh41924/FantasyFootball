from selenium import webdriver
from selenium.webdriver.common import action_chains, keys
import time

driver = webdriver.Firefox()
driver.implicitly_wait(10)
driver.get("http://games.espn.go.com/ffl/signin?redir=http%3A%2F%2Fgames.espn.go.com%2Fffl%2Fleaguerosters%3FleagueId%3D1111554")
# time.sleep(10)
driver.switch_to.frame(driver.find_element_by_id("disneyid-iframe"))
driver.find_element_by_xpath("//input[@type='text']").send_keys("INCORRECT_USERNAME")
driver.find_element_by_xpath("//input[@type='password']").clear()
driver.find_element_by_xpath("//input[@type='password']").send_keys("INCORRECT_PASSWORD")
driver.find_element_by_xpath("//button[@type='submit']").click()
