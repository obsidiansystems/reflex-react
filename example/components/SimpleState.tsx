'use client';

import { useState } from 'react';
import { ImportHaskellComponent } from './Haskell';

export function SimpleStateTypescript(props) {
  const [v, setV] = useState(0);
  return (
    <>
      <h1>React-Typescript</h1>
      <p>
        <strong>Props: </strong>
        {JSON.stringify(props)}
      </p>
      <p>
        <strong>State: </strong>
        {v.toString()}
        <button onClick={() => setV(v+1)} >+</button>
      </p>
    </>
  );
}

export const SimpleStateHaskell = ImportHaskellComponent('simpleStateHaskell');

export const SimpleStateReflex = ImportHaskellComponent('simpleStateReflex');
