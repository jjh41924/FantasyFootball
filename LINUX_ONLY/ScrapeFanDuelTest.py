from selenium import webdriver
from selenium.webdriver.common import action_chains, keys
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time

# Includes
execfile("/home/ff/FantasyFootball/LINUX_ONLY/GetCredentials.py")



class FanDuel :
    "This object manages your connection with the Fan Duel website using the Selenium package."
    def __init__(self):
        self.__isLoggedIn = False
        self.__driver = webdriver.Firefox()

    def __checkIsLoggedIn(self):
        if not self.__isLoggedIn :
            raise Exception("You are not logged in.  Please call .login() first")

    def login(self):
        "This will log you into fan duel.  I dont do this in init because I want the user to be able to create one of these without forcing when he logs in.  However it is required for most methods below"
        self.__driver.implicitly_wait(10)
        self.__driver.get("https://www.fanduel.com/p/login")
        # time.sleep(10)

        credentials =  getCredentials("FanDuel")
        self.__driver.find_element_by_xpath("//input[@type='email']").send_keys(credentials[0])
        self.__driver.find_element_by_xpath("//input[@type='password']").clear()
        self.__driver.find_element_by_xpath("//input[@type='password']").send_keys(credentials[1])
        self.__driver.find_element_by_xpath("//input[@type='submit']").click()
        self.__driver.implicitly_wait(10)
        self.navigateToNFL()
        self.__isLoggedIn = True

    def navigateToNFL(self):
        l = self.__driver.find_elements_by_xpath("//li[@class='toggle-menu-item sport-menu-item']")
        for le in l:
            if(le.text.encode('ascii','ignore') == "NFL") :
                le.click()
                return None

    def getDriver(self):
        "This will get you the selenium driver for the site.  I am not entirely necessary that __driver is private."
        return self.__driver

    def getPossibleTabNames(self):
        return ["Featured", "Tournaments", "Leagues", "Head to Heads", "50/50s & Multipliers"]

    def navigateToTab(self, tabName="Featured"):
        "This is a dispatch.  I will Navigate to one of the 5 Tabs based on these strings {Featured, Tournaments, Leagues, HeadToHeads, Multipliers5050}"
        if tabName not in self.getPossibleTabNames():
            raise Exception("Invalid Tab name")

        currentTab = self.__driver.find_element_by_xpath("//li[@class='toggle-menu-item contest-type-menu-item active']").text.encode("ascii","ignore").split("\n")[0]
        if currentTab == tabName:
            return None

        otherTabs = self.__driver.find_elements_by_xpath("//li[@class='toggle-menu-item contest-type-menu-item']")
        for ot in otherTabs:
            if ot.text.encode("ascii","ignore").split("\n")[0] == tabName:
                ot.click()
                return None
        raise Exception("Tab Not Found")


    def getFeaturedContests(self):
        "If logged in this method will loop through the featured contests and create Contest objects for each"
        self.navigateToTab("Featured")
        return self.getContestsOnCurrentTab

    def getHeadtoHeadsContests(self):
        self.navigateToTab("Head to Heads")
        return self.getContestsOnCurrentTab

    def getTournamentsContests(self):
        self.navigateToTab("Tournaments")
        return self.getContestsOnCurrentTab

    def get5050sMultipliersContests(self):
        self.navigateToTab("50/50s & Multipliers")
        return self.getContestsOnCurrentTab

    def getContestsOnCurrentTab(self):
        self.__checkIsLoggedIn()
        #TODO: click the "New Contsts Available Button"
        contestsElems = self.__driver.find_elements_by_xpath("//tr[@class='contest-list-item featured-contest']")
        contests = [FanDuelContest(e) for e in contestsElems]
        return contests


class FanDuelContest :
    "This is an individual contest.  This object should contain all the information needed to select it online"
    def __init__(self, element):
        self.__id = element.get_attribute("id")
        self.__name = element.find_element_by_xpath("descendant::a[@class='contest-name']").text.encode('ascii','ignore')
        se = element.find_element_by_xpath("descendant::a[@class='contest-entries']").text.encode('ascii','ignore').split(" ")
        if len(se) != 3 :
            print(se)
            raise Exception("[Invalid contest entry  and size]")
        self.__entires = se[0]
        self.__size = se[2]
        self.__fee = element.find_element_by_xpath("descendant::span[@class='contest-entry-fee']").text.encode('ascii','ignore')
        self.__prizes = element.find_element_by_xpath("descendant::a[@class='contest-prizes']").text.encode('ascii','ignore')
        self.__starts = element.find_element_by_xpath("descendant::time[@class='contest-start-date']").get_attribute("datetime").encode('ascii','ignore')
        self.__clickable = element.find_element_by_xpath("descendant::a[@class='contest-name']")


    def getAllEntries(self,driver):
        self.click()
        driver.find_elements_by_xpath("//nav[@class='button-group']//a")[1].click() # This gets us to entries
        entries = []
        while(True) :
            driver.implicitly_wait(4)
            try :
                entries.extend([e.text.encode('ascii','ignore') for e  in driver.find_elements_by_xpath("//a[@target='_self']")[0:20]] )
                try : #These errors need to be raised
                    current_page = int(driver.find_element_by_xpath("//a[@class='button mini is-active']").text.encode('ascii','ignore'))
                    next_pages = [p for p in driver.find_elements_by_xpath("//a[@class='button mini']") if p.text.encode('ascii','ignore').isdigit() and int(p.text.encode('ascii','ignore')) > current_page]
                    if(len(next_pages)==0) :
                        break
                    next_pages[0].click()
                except :
                    raise
            except :
                driver.implicitly_wait(4)
        driver.find_element_by_xpath("//a[@class='modal-close']").click()
        return entries


    def click(self):
        self.__clickable.click()

    def __str__(self):
        return "%s:%s      %s/%s    %s/%s    %s" % (self.__id, self.__name, self.__entires, self.__size, self.__fee, self.__prizes, self.__starts)


