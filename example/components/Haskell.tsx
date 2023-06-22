'use client';

import * as react from 'react';
import { createContext, useState, useEffect } from 'react';

export const HaskellContext = createContext(null);

export default function WithHaskell({children}) {
  const [haskell, setHaskell] = useState(null);
  useEffect((ref) => {
    const jsaddleRoot = "http://localhost:3001";
    const xhr = new XMLHttpRequest();
    xhr.open("GET", jsaddleRoot + "/jsaddle.js");
    xhr.onload = () => {
      eval("(function(JSADDLE_ROOT, arg) {" + xhr.response + "})")(jsaddleRoot, { react: react, setVal: setHaskell });
    };
    //TODO: xhr.onerror
    xhr.send();
  }, []);
  return (
    <HaskellContext.Provider value={haskell}>
      {children}
    </HaskellContext.Provider>
  );
}
