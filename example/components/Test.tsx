'use client';

import { WithHaskell, HaskellComponent } from './Haskell'
import { useState, useMemo, useCallback, Suspense } from 'react';
import { SimpleStateTypescript, SimpleStateHaskell, SimpleStateReflex } from './SimpleState';

export default function Test() {
  const [v, setV] = useState(0);
  return (
    <WithHaskell fallback={<i>Loading...</i>}>
      <input type="number" value={v.toString()} onChange={e => setV(Number(e.target.value))} />
      <ul>
        <li><SimpleStateTypescript v={v} /></li>
        <li><SimpleStateHaskell v={v} /></li>
        <li><SimpleStateReflex v={v} /></li>
      </ul>
    </WithHaskell>
  )
}
