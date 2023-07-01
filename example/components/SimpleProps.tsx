'use client';

import { useProps } from 'react';
import { ImportHaskellComponent } from './Haskell';

export function SimplePropsTypescript(props) {
  return (
    <>
      {JSON.stringify(props)}
    </>
  );
}

export const SimplePropsHaskell = ImportHaskellComponent('simplePropsHaskell');

export const SimplePropsReflex = ImportHaskellComponent('simplePropsReflex');
