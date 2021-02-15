// eslint-disable-next-line @typescript-eslint/no-var-requires
const path = require("path");
// eslint-disable-next-line @typescript-eslint/no-var-requires
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
// eslint-disable-next-line @typescript-eslint/no-var-requires
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  target: "web",
  entry: {
    app: "./src/index.tsx"
  },
  output: {
    // Add hash to make filename different depending on file content
    // to enforce browsers to reload changes in production.
    filename: "[name].[contenthash].js",
    path: path.resolve(__dirname, "dist")
  },

  resolve: {
    alias: {
      "~": path.resolve(__dirname, "src/")
    },
    // Add `.ts` and `.tsx` as a resolvable extension.
    extensions: [".ts", ".tsx", ".js"]
  },

  module: {
    rules: [
      // all files with a `.ts` or `.tsx` extension will be handled by `ts-loader`
      {
        test: /\.tsx?$/,
        loader: "ts-loader"
      },

      {
        test: /\.(sa|sc|c)ss$/,
        use: [
          {
            loader: MiniCssExtractPlugin.loader,
            options: {}
          },
          "css-loader",
          "postcss-loader",
          "sass-loader"
        ]
      },

      {
        test: /\.(png|jpg|jpeg|gif)$/i,
        use: [
          {
            loader: "url-loader",
            options: {
              limit: 10000
            }
          }
        ]
      },

      // Related reading:
      // https://www.pluralsight.com/guides/how-to-load-svg-with-react-and-webpack
      // https://webpack.js.org/loaders/url-loader/#examples
      // https://www.npmjs.com/package/react-svg-loader
      {
        test: /\.svg$/,
        use: [
          {
            loader: "babel-loader",
            options: {
              presets: ["@babel/preset-react"]
            }
          },
          {
            loader: "react-svg-loader",
            options: {
              jsx: true // true outputs JSX tags
            }
          }
        ]
      }
    ]
  },

  plugins: [
    new MiniCssExtractPlugin({
      filename: "style.[contenthash].css"
    }),

    new HtmlWebpackPlugin({
      template: "src/index.template.html",
      filename: "index.html",
      publicPath: "/",
      minify: {
        collapseWhitespace: true,
        removeComments: true,
        removeRedundantAttributes: true,
        useShortDoctype: true
      }
    })
  ]
};
