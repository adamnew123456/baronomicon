/* Removes all child elements of a DOM node. */
function clearChildren(node) {
  while (node.firstChild) {
    node.removeChild(node.firstChild);
  }
}

/* Removes all duplicate values from a list. Uses the string representation of
   the values for comparisons. */
function toSet(list) {
  let set = {};
  list.forEach(value => set[value] = value);

  let result = [];
  for (let valueKey in set) {
    result.push(set[valueKey]);
  }

  return result;
}

/* Parses a string into a series of search words. These search words ignore all
   non-word characters and strip plurals. */
function parseKeywords(text) {
  let keywords = text
    .split(/\W+/)
    .map(term => term.replace(/s*$/, '').toLowerCase())
    .filter(term => term != '');
  return toSet(keywords);
}

/* Schedules an event to be invoked, but only if the same event isn't invoked
   within a specified interval. */
function debounce(intervalMS, target) {
  let currentTimeout = null;

  let invoke = () => {
    currentTimeout = null;
    target();
  };

  return () => {
    if (currentTimeout !== null) {
      clearTimeout(currentTimeout);
    }
    currentTimeout = setTimeout(invoke, intervalMS);
  };
}
