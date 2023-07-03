const child_process = require('node:child_process');
const path = require('path');
const readFileSync = require('fs').readFileSync;

module.exports = function(source) {
  const options = this.getOptions();
  const callback = this.async();
  const cabalFilePath = this.resourcePath;
  const cabalFileDir = path.dirname(cabalFilePath);


  let result;
  try {
    if (options.dev) {
      if (options.isServer) {
        this.cacheable(false);
        // no-op
        result = 'export function haskellEngine(arg, global) { };';
      } else { // !options.isServer
        const command = 'ghcid -r -W -c"cabal repl ' + path.basename(cabalFilePath) + '"';
        const ghcid_process = child_process.spawn(
          'nix-shell',
          ['-A', 'shells.ghc', '--run', command],
          {
            cwd: cabalFileDir,
            stdio: 'inherit',
          }
        );
        // ghcid_process should not stop
        ghcid_process.on('close', (code) => {
          throw("ghcid process stopped");
        });
        // TODO: we should ideally wait for the ghcid to successfully start the jsaddle server

        //TODO: xhr.onerror
        const evalJSaddleJs =
              'new Promise((resolve, reject) => {' +
              '  const jsaddleRoot = "http://localhost:3001";' +
              '  const xhr = new XMLHttpRequest();' +
              '  xhr.open("GET", jsaddleRoot + "/jsaddle.js");' +
              '  xhr.onload = () => {' +
              '    eval("(function(JSADDLE_ROOT, arg) {" + xhr.response + "})")(jsaddleRoot, arg);' +
              '  };' +
              '  xhr.send();' +
              '})';

        result = 'export function haskellEngine(arg, global) { ' +
          'var doXhr = ' + evalJSaddleJs + ';' +
          'doXhr;' +
          '};';
      }
    } else {  // !options.dev
      if(options.isServer) {
        // no-op
        result = '';
      } else {
        const build_command = 'js-unknown-ghcjs-cabal build ' + path.basename(cabalFilePath);
        const build_result = child_process.spawnSync(
          'nix-shell',
          ['-A', 'shells.ghcjs', '--run', build_command],
          {
            cwd: cabalFileDir,
            stdio: 'inherit',
          }
        );
        if (build_result.error != null) {
          throw(build_result.error);
        }

        // If the cabal build has no changes to build, it only prints "Up to date"
        // In order to get the output dir we currently have only the cabal run command
        // The cabal list-bins command is present in cabal v3.4
        const run_command = 'js-unknown-ghcjs-cabal run ' + path.basename(cabalFilePath) + ' || true';
        const run_result = child_process.spawnSync(
          'nix-shell',
          ['-A', 'shells.ghcjs', '--run', run_command],
          {
            cwd: cabalFileDir,
            stdio: 'pipe',
            encoding: 'utf8',
          }
        );
        if (run_result.error != null) {
          throw(run_result.error);
        }

        // The output of cabal run prints this in the end of stderr
        // <dir>: createProcess: posix_spawnp: does not exist (No such file or directory)
        // We need to get the <dir> from this line
        // The end of strerr has '\n', so second last item
        const last_line = run_result.stderr.split('\n').at(-2);
        const out_dir = last_line.split(': createProcess:')[0] + '.jsexe';

        const allJs = readFileSync(out_dir + '/all.js');
        result = "export function haskellEngine(arg, global) { function getProgramArg() { return arg; };" + allJs + "};";
      }
    }
  } catch (error) {
    callback(error);
    return;
  }

  callback(null, result);
}
