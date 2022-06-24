const { dependencies } = require('./package.json');

module.exports = {
  rollup(config, options) {
    config.output = {
      dir: 'dist',
    };
    config.external = [
      '../../custom_modules/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib.js',
      '../../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib.js',
      '../../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib.js',
      ...Object.keys(dependencies),
    ];

    return config;
  },
};
