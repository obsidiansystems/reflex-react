"use client";

import Reflex from './Reflex'
import WithHaskell from './Haskell'
import { useState, useMemo, useCallback } from 'react';

export default function Test() {
  const [v, setV] = useState(0);
  const e = useMemo(() => document.createElement("input"))
  return (
    <WithHaskell>
      <button onClick={() => setV((old) => old+1)}>Increment</button>
      <div id={v} />
      <ul>
        <li><Reflex v={v}></Reflex></li>
        <li><Reflex v={v}></Reflex></li>
        <li><Reflex v={v}></Reflex></li>
      </ul>
    </WithHaskell>
  )
}
