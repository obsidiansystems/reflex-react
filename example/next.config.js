const path = require('path');

/** @type {import('next').NextConfig} */
const nextConfig = {
  webpack: (config, options) => {
    config.resolve.fallback = {
      ...config.resolve.fallback,
      fs: false,
      child_process: false
    };
    config.module.rules.push({
      test: /\.cabal/,
      use: [
        {
          loader: path.resolve('./loaders/haskell.js'),
        }
      ],
    })

    return config;
  },
}

module.exports = nextConfig
