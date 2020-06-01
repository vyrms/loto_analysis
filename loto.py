import requests
from bs4 import BeautifulSoup
import codecs
from selenium import webdriver


# loto object for temporary data storage
class Loto:
    def __init__(self, kai="", date=[], honsuuji=[], bonus=[]):
        self.kai = kai
        self.date = date
        self.honsuuji = honsuuji
        self.bonus = bonus


# gets all loto6 number data from their website
def loto6_data():
    n = 60001
    outer_switch = True

    loto6_list = []
    # goes to all the loto6 websites in old form
    while outer_switch:
        # access the loto6 website (in old form)
        url = f"https://www.mizuhobank.co.jp/retail/takarakuji/loto/backnumber/loto{n}.html"
        response = requests.get(url)
        status = response.status_code
        print(status)
        if status != 200:
            outer_switch = False
            print("end!")
            break

        # get the html
        html_doc = response.content
        soup = BeautifulSoup(html_doc, 'html.parser')

        # get table with data in it
        inner = soup.find(class_="typeTK")
        tr = inner.find_next("tr")


        # goes through all contents in the inner table which contains the data
        switch = True
        while switch:
            try:
                tr = tr.find_next("tr")
                row = tr.text.strip("\n").split()

                # tidy 第n回 data
                kai = row[0]
                kai = kai.replace("第", "").replace("回", "")

                # tidy the date data
                date_orig = row[1]
                date = []
                nen = date_orig.index("年")
                tsuki = date_orig.index("月")
                nichi = date_orig.index("日")
                date.append(date_orig[0:nen])
                date.append(date_orig[nen + 1:tsuki])
                date.append(date_orig[tsuki + 1:nichi])

                # get the winning numbers
                honsuuji = row[2:8]
                bonus = row[8]

                # make loto6 object to store these data
                loto = Loto(kai, date, honsuuji, bonus)

                # append the object into output list
                loto6_list.append(loto)

            # end of table
            except IndexError:
                switch = False
                print("index error")
            except ValueError:
                switch = False
                print("value error")
            except AttributeError:
                switch = False
                print("Attribute error")

        # check hwo many rounds of loto6 has been acquired (supposed to go up by 20 every time)
        print(len(loto6_list))

        # loto6 data are stored in html that go up by 20 every time
        n += 20

    # the way loto6 stores their data changes after 461st round
    # after the website format changed
    n += -60000  # make the number not contain any 0s in front
    max = 1478  # this is the current number of rounds of loto6
    print(n)
    url = f"https://www.mizuhobank.co.jp/retail/takarakuji/loto/backnumber/detail.html?fromto={n}_{max}&type=loto6"

    # use headless firefox to browse the website (because the website loads parts by parts)
    firefox_options = webdriver.FirefoxOptions()
    firefox_options.add_argument("-headless")
    browser = webdriver.Firefox(executable_path="D:/softwares/geckodriver.exe", options=firefox_options)
    # needs to have geckodriver.exe in some repository (change the executable_path as needed)

    browser.get(url)

    html_doc = browser.page_source
    soup = BeautifulSoup(html_doc, 'html.parser')

    # save the html to inspect later
    with codecs.open("htmls/loto6_new_response.txt", "w", "utf-8") as outfile:
        outfile.write(soup.prettify())

    inner = soup.find(class_="typeTK")
    tr = inner.find_next("tr")

    switch = True
    while switch:
        try:
            tr = tr.find_next("tr")
            row = tr.text.strip("\n").split()

            # tidy 第n回 data
            kai = row[0]
            kai = kai.replace("第", "").replace("回", "")

            # tidy the date data
            date_orig = row[1]
            date = []
            nen = date_orig.index("年")
            tsuki = date_orig.index("月")
            nichi = date_orig.index("日")
            date.append(date_orig[0:nen])
            date.append(date_orig[nen + 1:tsuki])
            date.append(date_orig[tsuki + 1:nichi])

            # get 本数字 and bonus data (the way they format these values changed)
            numbers = row[2]
            honsuuji = []
            for n in range(0, len(numbers) - 2, 2):
                num = numbers[n:n+2]
                honsuuji.append(num)
            print(honsuuji)
            bonus = numbers[-2:]
            print(bonus)

            # make the loto object and append
            loto = Loto(kai, date, honsuuji, bonus)
            loto6_list.append(loto)

        # at the end of the table
        except IndexError:
            switch = False
            print("index error")
        except ValueError:
            switch = False
            print("value error")
        except AttributeError:
            switch = False
            print("Attribute error")

    # save the data in csv format
    with codecs.open("data/loto6_data.csv", "w", "utf-8") as outfile:
        outfile.write("kai,year,month,day,hon1,hon2,hon3,hon4,hon5,hon6,bonus\n")
        for entry in loto6_list:
            outfile.write(entry.kai + "," + ",".join(entry.date) + "," + ",".join(entry.honsuuji) + "," + str(entry.bonus) + "\n")


def loto7_data():
    # list for all loto7 data
    loto7_list = []

    # get data from n回 to max回
    n = 1
    max = 319
    url = f"https://www.mizuhobank.co.jp/retail/takarakuji/loto/backnumber/detail.html?fromto={n}_{max}&type=loto7"

    #  generate web browser object
    firefox_options = webdriver.FirefoxOptions()
    firefox_options.add_argument("-headless")
    browser = webdriver.Firefox(executable_path="D:/softwares/geckodriver.exe", options=firefox_options)

    browser.get(url)

    html_doc = browser.page_source
    soup = BeautifulSoup(html_doc, 'html.parser')

    # save the html for future reference
    with codecs.open("loto7_new_response.txt", "w", "utf-8") as outfile:
        outfile.write(soup.prettify())

    inner = soup.find(class_="typeTK")
    tr = inner.find_next("tr")

    switch = True
    while switch:
        try:
            tr = tr.find_next("tr")
            row = tr.text.strip("\n").split()

            # tidy 第n回 data
            kai = row[0]
            kai = kai.replace("第", "").replace("回", "")

            # tidy the date data
            date_orig = row[1]
            date = []
            nen = date_orig.index("年")
            tsuki = date_orig.index("月")
            nichi = date_orig.index("日")
            date.append(date_orig[0:nen])
            date.append(date_orig[nen + 1:tsuki])
            date.append(date_orig[tsuki + 1:nichi])

            # get 本数字 and bonus data
            numbers = row[2]
            honsuuji = []
            for n in range(0, len(numbers) - 4, 2):
                num = numbers[n:n + 2]
                honsuuji.append(num)

            # get bonus (there are 2 bonus numbers for loto7)
            bonus = [numbers[-4:-2], numbers[-2:]]
            loto = Loto(kai, date, honsuuji, bonus)
            loto7_list.append(loto)

            print(kai)
            print(date)
            print(honsuuji)
            print(bonus)

        # end of table on website
        except IndexError:
            switch = False
            print("index error")
        except ValueError:
            switch = False
            print("value error")
        except AttributeError:
            switch = False
            print("Attribute error")

    # write into loto7 data file
    with codecs.open("data/loto7_data.csv", "w", "utf-8") as outfile:
        outfile.write("kai,year,month,day,hon1,hon2,hon3,hon4,hon5,hon6,hon7,bonus1,bonus2\n")
        for entry in loto7_list:
            outfile.write(
                entry.kai + "," + ",".join(entry.date) + "," + ",".join(entry.honsuuji) + "," + ",".join(entry.bonus) + "\n")


loto6_data()
loto7_data()
