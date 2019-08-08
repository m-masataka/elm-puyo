const path = require('path');

module.exports = {
  mode: 'production',
  entry: path.resolve(__dirname, './src/index.js'),
  output: {
    filename: 'bundle-elm.js',
    path: path.resolve(__dirname, './dist/')
  },
  module: {
    rules: [
      {
        test: /\.scss/,
        use: [
          "style-loader",
          {
            loader: "css-loader",
            options: { 
              url: false,
              importLoaders: 2
            }
          },
          {
            loader: "sass-loader",
            options: {
            }
          }
        ]
      },
      {
        test: /\.elm/,
        exclude: [
          /elm_stuff/,
          /node_modules/
        ],
        use: [
          {
            loader: 'elm-webpack-loader',
            options: {}
          }
        ]
      }
    ]
  }
};

