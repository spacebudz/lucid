const path = require('path');
const webpack = require('webpack');

//TODO webpack is currently not used to bundle for web
// NPM package works for Node.js and Browser environment right now

module.exports = {
  entry: './src/index.ts',
  devtool: 'inline-source-map',
  mode: 'production',
  experiments: {
    asyncWebAssembly: true,
    topLevelAwait: true,
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'],
    alias: {
      '../../custom_modules/cardano-multiplatform-lib-nodejs': false,
    },
  },
  target: 'web',
  output: {
    filename: 'cardano-web3.js',
    path: path.resolve(__dirname, 'build/web'),
    library: 'CardanoWeb3',
    libraryTarget: 'umd',
    publicPath: '/web/',
    globalObject: 'this',
    umdNamedDefine: true,
  },
};
