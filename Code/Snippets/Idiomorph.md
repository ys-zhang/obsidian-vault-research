

# 1 Concepts

>[!def] persistent node
> - Given the new and old version of the DOM, a node is _persistent_ if both new and old version has a node with the same `tagName` and `id`;
> - a _persistent-id_ is the `id` of some persistent node.

>[!def] id-set
> for each node in either the new or old version is associated with a _id-set_, which is a set of _persistent-ids_

>[!def] pantry
> pantry is a node that holds persistent nodes temporarily during merge.

>[!tldr]
>Given an id set, you can now adopt a broader sense of "matching" than simply using id matching: if the intersection between the id sets of element 1 and element 2 is non-empty, they match.
> - `findBestMatch`
>     - `isSoftMatch`: tagName & nodeType & id matches
>     - `isIdSetMatch`: id-set overlaps
> - xxx

- `config.restoreFocus` only take effect when active element is `input` or `textarea`

# 2 Main algorithm

```ts
const morphChildren : (ctx: MorphContext, oldNode: Element, newNode: Element) => Node[] = (

)
```


# 3 Code 

### 3.1.1 Persistent id

```js
/**
 * This function computes the set of ids that 
 * persist between the two contents excluding 
 * duplicates
 *
 * @param {Element[]} oldIdElements
 * @param {Element[]} newIdElements
 * @returns {Set<string>}
 */
function createPersistentIds(oldIdElements, newIdElements) {
  let duplicateIds = new Set();
  /** @type {Map<string, string>} */
  let oldIdTagNameMap = new Map();
  for (const { id, tagName } of oldIdElements) {
    if (oldIdTagNameMap.has(id)) {
      duplicateIds.add(id);
    } else {
      oldIdTagNameMap.set(id, tagName);
    }
  }
  let persistentIds = new Set();
  for (const { id, tagName } of newIdElements) {
    if (persistentIds.has(id)) {
      duplicateIds.add(id);
    } else if (oldIdTagNameMap.get(id) === tagName) {
      persistentIds.add(id);
    }
    /* skip if tag types mismatch 
       because its not possible to 
       morph one tag into another */
  }
  for (const id of duplicateIds) {
    persistentIds.delete(id);
  }
  return persistentIds;
}
```

### 3.1.2 id-set

```js
/**
 * A bottom-up algorithm that populates a map of
 *   Element -> IdSet.
 * The idSet for a given element is the set of 
 *   all IDs contained within its subtree.
 * As an optimzation, we filter these IDs 
 *   through the given list of persistent IDs,
 *   because we don't need to bother considering 
 *   IDed elements that won't be in the new content.
 *
 * @param {Map<Node, Set<string>>} idMap
 * @param {Set<string>} persistentIds
 * @param {Element} root
 * @param {Element[]} elements
 */
function populateIdMapWithTree(idMap, persistentIds, root, elements) {
  for (const elt of elements) {
    if (persistentIds.has(elt.id)) {
      /** @type {Element|null} */
      let current = elt;
      // walk up the parent hierarchy 
      // of that element, adding the id
      // of element to the parent's id set
      while (current) {
        let idSet = idMap.get(current);
        // if the id set doesn't exist, 
        // create it and insert it in the map
        if (idSet == null) {
          idSet = new Set();
          idMap.set(current, idSet);
        }
        idSet.add(elt.id);
        if (current === root) break;
        current = current.parentElement;
      }
    }
  }
}
```


### 3.1.3 Deep Merge Objects
```js
/**
 * Deep merges the config object and the
 * Idiomorph.defaults object to
 * produce a final configuration object
 * @param {Config} config
 * @returns {ConfigInternal}
 */
function mergeDefaults(config) {
  let finalConfig = Object.assign({}, defaults);
  // copy top level stuff into final config
  Object.assign(finalConfig, config);
  // copy callbacks into final config 
  //   (do this to deep merge the callbacks)
  finalConfig.callbacks = Object.assign(
    {},
    defaults.callbacks,
    config.callbacks,
  );
  // copy head config into final config  
  //   (do this to deep merge the head)
  finalConfig.head = Object.assign({}, defaults.head, config.head);
  return finalConfig;
}
```