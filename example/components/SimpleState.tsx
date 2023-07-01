'use client';

import { useState } from 'react';
import { ImportHaskellComponent } from './Haskell';

export function SimpleStateTypescript() {
  const [v, setV] = useState(0);
  return (
    <>
      <button onClick={() => setV(v+1)} >+</button>
      {v.toString()}
    </>
  );
}

export const SimpleStateHaskell = ImportHaskellComponent('simpleStateHaskell');

export const SimpleStateReflex = ImportHaskellComponent('simpleStateReflex');
