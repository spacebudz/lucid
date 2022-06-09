const webpack = require("webpack");

module.exports = {
  configureWebpack: {
    experiments: {
      asyncWebAssembly: true,
      topLevelAwait:true,
    },
    plugins: [
      new webpack.ProvidePlugin({
        process: "process/browser",
        Buffer: ["buffer", "Buffer"],
      }),
    ],
  },
    
}
