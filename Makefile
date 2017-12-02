all:
	mkdir -p output
	sassc scss/styles.scss > output/styles.css
	cp js/snippet.js output/snippet.js

.PHONY: all
