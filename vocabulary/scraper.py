from bs4 import BeautifulSoup
import re
import requests

baseURL = "http://dictionary.reference.com/browse/"
def clean(inputStr):
	temp = re.sub("[\d.]|;", "", inputStr).strip()
	return [y for y in [x.strip() for x in temp.replace("See", "").split(",")] if " " not in y]


def scrape(word):
	req = requests.get(baseURL+word)
	data = req.text
	soup = BeautifulSoup(data)
	try:
		text = soup.select('.me')[0]['data-syllable'].encode('utf8')
		synonyms, antonyms = [], []
		syllables = ''.join([i if ord(i) < 128 else ' ' for i in text]).split()
		
		if soup.select('.luna-data-header span'):
			pos = soup.select('.luna-data-header span')[0].string.encode('utf8')
		else:
			return ""
		if soup.select('.tail-type-synonyms .tail-elements'):
			synonyms = clean(soup.select('.tail-type-synonyms .tail-elements')[0].text.encode("utf-8"))
		if soup.select('.tail-type-antonyms .tail-elements'):
			antonyms = clean(soup.select('.tail-type-antonyms .tail-elements')[0].text.encode("utf-8"))
		return [word, pos, len(syllables), synonyms, antonyms]
	except (IndexError, KeyError):
		return ""

fout = open('wordsScrapped.txt', 'a')
fin = open('words.txt','r')
line = fin.readline()
fin.close()
wordlist = line.split()
for word in wordlist:
	fout.write(str(scrape(word)) +"\n")
	print "finished", word
fout.close()