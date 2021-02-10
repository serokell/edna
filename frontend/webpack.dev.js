const {merge} = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = merge(common, {
  mode: 'development',
  // devtool from here https://github.com/TypeStrong/ts-loader
  devtool: 'inline-source-map',
  devServer: {
    contentBase: './dist',
    host: '0.0.0.0',
    port: 9000,

    hot: true,
    // To fallback on index.html when page not found (needed for client side routing).
    historyApiFallback: true,

    // Redirect api requests to the backend
    proxy: {
      '/api': {
        target: 'http://localhost:8080',
      }
    },

    // Uncomment in case of some troubles to inspect whether dev server generates bundles
    // writeToDisk: true,
  },
});
