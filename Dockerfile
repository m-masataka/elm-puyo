FROM node:9.6.0

WORKDIR /app

RUN npm init -y

RUN npm install -S \
  elm@0.19.0 \
  elm-format@0.8.0 \
  elm-test@0.19.0-beta5 \
  webpack \
  webpack-cli \
  elm-webpack-loader \
  file-loader \
  css-loader \
  style-loader \
  sass-loader \
  node-sass \
  url-loader

ENV PATH $PATH:/app//node_modules/.bin
RUN sed -i -e "s/\(\"scripts\": {\)/\1\n    \"start\": \"nf start\", /" /app/package.json
RUN sed -i -e "s/\(\"scripts\": {\)/\1\n    \"postinstall\": \"npm rebuild node-sass \&\& npm rebuild elm\", /" /app/package.json
RUN yarn postinstall
