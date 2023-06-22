'use client';

import * as react from 'react';
import { useContext } from 'react';
import { HaskellContext } from './Haskell';

export default function Reflex() {
  const haskell = useContext(HaskellContext);
  const ValComp = () => {
    if(haskell) {
      return <haskell.comp test="test" />
    } else {
      return <i>Blah</i>
    }
  };
  return <ValComp />
}
