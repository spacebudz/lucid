const { dependencies } = require('./package.json');
const dts = require('rollup-plugin-dts').default;

module.exports = {
  rollup(config, options) {
    config.output = {
      dir: 'dist',
    };
    config.plugins.push(
      dts({
        compilerOptions: {
          baseUrl: 'src',
          // Ensure ".d.ts" modules are generated
          declaration: true,
          // Skip ".js" generation
          noEmit: false,
          emitDeclarationOnly: true,
          // Skip code generation when error occurs
          noEmitOnError: true,
          // Avoid extra work
          checkJs: false,
          declarationMap: false,
          skipLibCheck: true,
          preserveSymlinks: false,
        },
      })
    );
    config.external = [
      '../../custom_modules/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib.js',
      '../../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib.js',
      '../../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib.js',
      ...Object.keys(dependencies),
    ];

    return config;
  },
};
