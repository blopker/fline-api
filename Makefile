install:
	python3 `which poetry` install

run:
	cd templates && python3 -m http.server