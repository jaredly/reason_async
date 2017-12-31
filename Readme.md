# Async/await for Reason/OCaml!

[Clone the example repo](https://github.com/jaredly/reason_async_example) to get started quickly.

## Example usage

With promises

```
let doSomething = () => {
  let module Let_syntax = Reason_async.Promise;
  [%await let x = somethingPromisy
  and y = anotherPromise];
  /* ... */
  [%awaitWrap let z = getFileContents()];
  x + y + z
};
```

## Installation / setup

NOTE: You currently need to be using `bsb-native` in your project in order for weird bugs not to happen on recompilation. You'll want `"bs-platform": "git+https://github.com/bsansouci/bsb-native"` in your package.json.

- `yarn add reason_async` (or npm)
- add `reason_async` to your bs-dependencies in `bsconfig.json`
- add `reason_async` to your `ppx-flags` in `bsconfig.json`

Example `bsconfig.json`:

```json
{
  "name": "myapp",
  "refmt": 3,
  "sources": "./src",
  "bs-dependencies": ["reason_async"],
  "ppx-flags": ["reason_async"]
}
```
