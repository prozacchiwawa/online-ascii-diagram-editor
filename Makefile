all:
	npm run build
	./node_modules/.bin/browserify -o ./static/picmode.js ./src/main.js
