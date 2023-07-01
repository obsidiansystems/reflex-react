"use client";

import { WithHaskell, HaskellComponent } from './Haskell'
import { useState, useMemo, useCallback, Suspense } from 'react';

export default function Test() {
  const [v, setV] = useState(0);
  return (
    <WithHaskell fallback={<i>Loading Haskell...</i>}>
      <button onClick={() => setV((old) => old+1)}>Increment</button>
      <div id={v.toString()} />
      <ul>
        <li><Suspense fallback={<i>Loading Haskell...</i>}><HaskellComponent name="reflex" v={v} /></Suspense></li>
        <li><Suspense fallback={<i>Loading Haskell...</i>}><HaskellComponent name="reflex" v={v} /></Suspense></li>
        <li><Suspense fallback={<i>Loading Haskell...</i>}><HaskellComponent name="reflex" v={v} /></Suspense></li>
        <li><Suspense fallback={<i>Loading Haskell...</i>}><HaskellComponent name="simple" v={v} /></Suspense></li>
        <li><Suspense fallback={<i>Loading Haskell...</i>}><HaskellComponent name="simple" v={v} /></Suspense></li>
        <li><Suspense fallback={<i>Loading Haskell...</i>}><HaskellComponent name="simple" v={v} /></Suspense></li>
      </ul>
    </WithHaskell>
  )
}
