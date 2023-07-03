'use client';

import { useProps } from 'react';
import { ImportHaskellComponent } from './Haskell';

import { default as haskell } from './haskell-app/reflex-react.cabal';

export function SimplePropsTypescript(props) {
  return (
    <>
      {JSON.stringify(props)}
    </>
  );
}

export const SimplePropsHaskell = haskell.simplePropsHaskell;

export const SimplePropsReflex = haskell.simplePropsReflex;
