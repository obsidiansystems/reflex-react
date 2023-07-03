const spawnSync = require('node:child_process').spawnSync;
const path = require('path');
const readFileSync = require('fs').readFileSync;

module.exports = function(source) {
  const result = spawnSync(
    'js-unknown-ghcjs-cabal',
    ['build', path.basename(source)],
    {
      cwd: path.dirname(source),
      stdio: 'inherit',
    }
  );

  return readFileSync('../dist-newstyle/build/js-ghcjs/ghcjs-8.10.7/reflex-react-0.1.0.0/x/reflex-react/build/reflex-react/reflex-react.jsexe/all.js');
}
