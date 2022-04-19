module.exports = {
  rollup(config, options) {
    config.output = {
      dir: 'dist',
    };
    config.external = [
      '../../custom_modules/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib.js',
      '../../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib.js',
      'node-fetch',
    ];
    return config;
  },
};
