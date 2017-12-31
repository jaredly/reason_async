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

- `yarn add reason_async` (or npm)
- add `reason_async` to your dependencies in `bsconfig.json`
- add `reason_async` to your `ppx-flags` in `bsconfig.json`

Example `bsconfig.json`:

```json
{
  "name": "myapp",
  "refmt": 3,
  "sources": "./src",
  "dependencies": ["reason_async"],
  "ppx-flags": ["reason_async"]
}
```
