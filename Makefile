all:
	mkdir -p output
	sassc -t compressed scss/styles.scss > output/styles.css

.PHONY: all
