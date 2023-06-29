'use client';

import * as react from 'react';
import { useContext } from 'react';
import { HaskellContext } from './Haskell';

export default function Reflex(props) {
  const haskell = useContext(HaskellContext);
  if(haskell) {
    return <haskell.comp {...props} />
  } else {
    return <i>Haskell is still loading</i>
  }
}
