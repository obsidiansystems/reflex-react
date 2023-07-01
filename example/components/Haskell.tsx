// @ts-nocheck
'use client';

import * as react from 'react';
import { createElement, createContext, useRef, useEffect, useState, Suspense, useContext } from 'react';

/**
 * A React Context that holds a Promise that yields a Haskell engine.  If this is null, that means no ancestor component is providing thix Context.
 */
export const HaskellContext = createContext(null);

/**
 * Use an existing Haskell engine, which must have been provided by a parent component via HaskellContext.Provider.  This function takes care of waiting for the Haskell engine to load, whereas if you use `useContext(HaskellContext)`, you must use the resulting Promise to do that yourself.
 *
 * @returns {Object} The loaded Haskell engine
 */
export function useHaskell() {
  const haskell = useContext(HaskellContext);
  if(haskell === null) {
    throw new Error("No HaskellContext provided");
  } else if(!haskell.ready) {
    throw haskell;
  } else {
    return haskell.val;
  }
}

/**
 * Initialize a new Haskell engine.  Generally, you only need one of these per app.  This function does not do anything with React; for a hook that invokes this function, use `useLoadHaskellEngine`.
 *
 * @returns {Promise<Object>} A promise that yields the loaded Haskell engine
 */
export function loadHaskellEngine() {
  return new Promise((resolve, reject) => {
    const jsaddleRoot = "http://localhost:3001";
    const xhr = new XMLHttpRequest();
    xhr.open("GET", jsaddleRoot + "/jsaddle.js");
    xhr.onload = () => {
      eval("(function(JSADDLE_ROOT, arg) {" + xhr.response + "})")(jsaddleRoot, { react: react, setVal: resolve });
    };
    //TODO: xhr.onerror
    xhr.send();
  })
}

/**
 * A React hook that initializes a new Haskell engine.  Generally, you only need one of these per app.  You can use a `HaskellContext` together with `useHaskell` to share the resulting engine.  Note that you MUST put a `Suspense` between this call and any `useHaskell` invocations you write; otherwise the system will repeatedly start new Haskell engines rather than finishing loading the first one.
 *
 * @returns {Promise<Object>} When the Haskell engine has finished loading, the Promise provides an Object with all of its exported values.
 */
export function useLoadHaskellEngine() {
  // {ryantrinkle} This seems overly-complicated to me, but I don't know how to do better.  I want to create this Promise only once and return it synchronously.  useMemo doesn't guarantee only-once, useEffect doesn't return any value synchronously, and useRef doesn't accept an initializer function (only a value, so it must be pointlessly reconstructed each time).  useState seems to have the right semantics, so we just ignore the setter function, since we don't need it.
  const [[engineLoaded, resolveEngineLoaded], _] = useState(() => {
    var resolveEngineLoadedInner;
    const engineLoadedInner = new Promise((resolve, reject) => {
      resolveEngineLoadedInner = (val) => {
        resolve(val);
        engineLoadedInner.val = val;
        engineLoadedInner.ready = true;
      };
    });
    engineLoadedInner.ready = false;
    return [engineLoadedInner, resolveEngineLoadedInner];
  });
  useEffect(() => {
    loadHaskellEngine().then(resolveEngineLoaded);
  }, []);
  return engineLoaded;
}

/**
 * A React component that loads a Haskell engine and provides it to all child elements.  Generally you just need one of these near the top of your app.
 *
 * This component includes a Suspense; you may also include smaller Suspense scopes around Haskell-using widgets to provide a loading indicator while the Haskell engine is starting up.  If you need different loading indicator behavior, use `useLoadHaskellEngine` and `HaskellContext.Provider` directly.
 *
 * You may provide a `fallback` prop to this component to display while children are not ready (whether they are not ready because of the Haskell engine or any other reason).
 */
export function WithHaskell({children, fallback}) {
  const engineLoaded = useLoadHaskellEngine();

  // Note that we must include Suspense here, otherwise our
  // `useLoadHaskellEngine` will be continuously retried instead
  // of being allowed to finish
  return (
    <Suspense fallback={fallback}>
      <HaskellContext.Provider value={engineLoaded}>
        {children}
      </HaskellContext.Provider>
    </Suspense>
  );
}

/**
 * Import a React component written in Haskell.  You must do this at the top level of your code or otherwise ensure that this doesn't get re-run, since each invocation will produce a different component object.
 */
export function ImportHaskellComponent(name) {
  return (props) => {
    const haskell = useHaskell();
    return createElement(haskell[name], props);
  };
}
