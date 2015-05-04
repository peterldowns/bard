scrape.scm: scrape.json
	./scrape_to_scm.py

scrape.json: rhymes.json words.txt
	./scraper.py

rhymes.json: words.txt
	./dump_rhymes.py

clean:
	rm scrame.scm scrape.json rhymes.json
