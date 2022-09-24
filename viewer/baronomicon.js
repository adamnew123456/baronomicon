const NO_SELECTED_TITLE = "No Item Selected";
const NO_SELECTED_DESCRIPTION = "Please search and select an item from the left panel";

const EMPTY_DESCRIPTION = "(Item has no description)";

const EMPTY_FABRICATOR = "Item has an empty fabrication recipe. This usually means it can be purchased from a vending machine or merchant.";
const EMPTY_DECONSTRUCTOR = "Item has an empty deconstruction recipe. You can deconstruct it but it will yield no items.";

const ROOT_ELEM = document.getElementById('main-ui');
const TITLE_ELEM = document.getElementById('item-title');
const DESC_ELEM = document.getElementById('item-description');
const FABRICATE_LIST = document.getElementById('fabricate-list');
const DECONSTRUCT_LIST = document.getElementById('deconstruct-list');
const USED_BY_LIST = document.getElementById('used-by-list');
const YIELDED_BY_LIST = document.getElementById('yield-by-list');

const SEARCH_INPUT = document.getElementById('search');
const SEARCH_LIST = document.getElementById('search-results');

const MODAL_ROOT = document.getElementById('file-prompt');
const MODAL_FILE = document.getElementById('upload-file');
const MODAL_UPLOAD = document.getElementById('upload-button');
const MODAL_ERROR = document.getElementById('upload-error');

let ITEMS_BY_ID = {};
let ITEMS_BY_TAG = {};
let ITEMS_BY_KEYWORD = {};
let ITEMS_INDIRECT_FABRICATE = {};
let ITEMS_INDIRECT_DECONSTRUCT = {};

/* Parses the contents of the item dictionary in localstorage, or prompts the user for
 * a file if there are none in localstorage.
 */
function initItems() {
  if (!localStorage.getItem('items')) {
    // We'll get called when the user has performed the upload
    MODAL_ROOT.style.display = 'block';
    return;
  }

  MODAL_ROOT.style.display = 'none';

  let items = JSON.parse(localStorage.getItem('items'));
  ITEMS_BY_ID = {};
  ITEMS_BY_TAG = {};
  ITEMS_BY_KEYWORD = {};
  ITEMS_INDIRECT_FABRICATE = {};
  ITEMS_INDIRECT_DECONSTRUCT = {};

  // Initial pass - add the information that we can get by looking at a single item
  items.forEach(item => {
    ITEMS_BY_ID[item.id] = item;
    ITEMS_INDIRECT_FABRICATE[item.id] = [];
    ITEMS_INDIRECT_DECONSTRUCT[item.id] = [];

    item.tags.forEach(tag => {
      let tagItems = ITEMS_BY_TAG[tag] || [];
      tagItems.push(item);
      ITEMS_BY_TAG[tag] = tagItems;
    });

    let nameKeywords = [];
    if (item.label) {
      nameKeywords = parseKeywords(item.label);
    } else {
      nameKeywords = parseKeywords(item.id);
    }

    let descriptionKeywords = [];
    if (item.description) {
      descriptionKeywords = parseKeywords(item.description);
    }

    let allKeywords = toSet(nameKeywords.concat(descriptionKeywords));
    allKeywords.forEach(keyword => {
      let keywordItems = ITEMS_BY_KEYWORD[keyword] || [];
      keywordItems.push(item);
      ITEMS_BY_KEYWORD[keyword] = keywordItems;
    });
  });

  // Second pass - now that all items are present, we can index the indirect
  // dependencies between items
  items.forEach(item => {
    let allUsedByTargets = [];
    let allYieldFromTargets = [];

    item.fabricate.forEach(recipe => {
      recipe.forEach(ingredient => {
        let itemId = ingredient.item.id;
        let itemTag = ingredient.item.tag;

        if (itemId) {
          allUsedByTargets.push(itemId);
        } else {
          ITEMS_BY_TAG[itemTag].forEach(item => allUsedByTargets.push(item.id));
        }
      });
    });

    item.deconstruct.forEach(recipe => {
      recipe.forEach(ingredient => {
        let itemId = ingredient.item.id;
        let itemTag = ingredient.item.tag;

        if (itemId) {
          allYieldFromTargets.push(itemId);
        } else {
          ITEMS_BY_TAG[itemTag].forEach(item => allYieldFromTargets.push(item.id));
        }
      });
    });

    toSet(allUsedByTargets).forEach(id => {
      ITEMS_INDIRECT_FABRICATE[id].push(item);
    });

    toSet(allYieldFromTargets).forEach(id => {
      ITEMS_INDIRECT_DECONSTRUCT[id].push(item);
    });
  });

  displayItem();
}

/* Updates the item display pane with the currently selected item, stored in the
   current URL's fragment. */
function displayItem() {
  clearChildren(FABRICATE_LIST);
  clearChildren(DECONSTRUCT_LIST);
  clearChildren(USED_BY_LIST);
  clearChildren(YIELDED_BY_LIST);

  let itemId = location.hash;
  if (itemId) itemId = itemId.substring(1);

  if (!itemId || !(itemId in ITEMS_BY_ID)) {
    TITLE_ELEM.innerText = NO_SELECTED_TITLE;
    DESC_ELEM.innerText = NO_SELECTED_DESCRIPTION;
    return;
  }

  let item = ITEMS_BY_ID[itemId];
  TITLE_ELEM.innerText = item.label || item.id;
  DESC_ELEM.innerText = item.description || EMPTY_DESCRIPTION;

  item.fabricate.forEach(recipe => {
    FABRICATE_LIST.appendChild(createRecipeNode(recipe, EMPTY_FABRICATOR));
  });

  item.deconstruct.forEach(recipe => {
    DECONSTRUCT_LIST.appendChild(createRecipeNode(recipe, EMPTY_DECONSTRUCTOR));
  });

  ITEMS_INDIRECT_FABRICATE[item.id].forEach(item => {
    USED_BY_LIST.appendChild(createSearchResultNode(item));
  });

  ITEMS_INDIRECT_DECONSTRUCT[item.id].forEach(item => {
    YIELDED_BY_LIST.appendChild(createSearchResultNode(item));
  });
}

/* Re-runs the given keyword search, and populates the search output list again. */
function updateSearchResults() {
  clearChildren(SEARCH_LIST);
  let terms = parseKeywords(SEARCH_INPUT.value);

  let matches = [];
  let firstTerm = terms.shift();
  if (firstTerm in ITEMS_BY_KEYWORD) {
    matches = ITEMS_BY_KEYWORD[firstTerm].concat();
  }

  terms.forEach(term => {
    if (!(term in ITEMS_BY_KEYWORD)) {
      matches = [];
      return;
    }

    let termKeys = {};
    ITEMS_BY_KEYWORD[term].forEach(item => termKeys[item.id] = item);
    matches = matches.filter(item => item.id in termKeys);
  });

  matches.sort((a, b) => a.id.localeCompare(b.id));
  matches.forEach(item => {
    let node = createSearchResultNode(item);
    SEARCH_LIST.appendChild(node);
  });
}

/* Creates the UI fragment for an item found by searching. */
function createSearchResultNode(item) {
  let container = document.createElement('div');
  container.className = 'search-result-entry';

  container.appendChild(createItemLink(item));
  return container;
}

/* Creates a UI fragment for a recipe entry. */
function createRecipeNode(recipe, fallbackMessage) {
  let container = document.createElement('div');

  if (recipe.length == 0) {
    container.className = 'label';
    container.innerText = fallbackMessage;
    return container;
  }

  container.className = 'recipe-entry';

  recipe.forEach(ingredient => {
    let node = ingredient.item.tag ? createTagIngredientNode(ingredient) : createIngredientNode(ingredient);
    container.appendChild(node);
  });

  return container;
}

/* Creates a uI fragment for a recipe ingredient that is an item. */
function createIngredientNode(ingredient) {
  let container = document.createElement('div');
  container.className = 'ingredient-container';

  let node = createItemLink(ITEMS_BY_ID[ingredient.item.id], describeIngredient(ingredient));
  container.appendChild(node);

  return container;
}

/* Creates a UI fragment for a recipe ingredient that is a tag. */
function createTagIngredientNode(ingredient) {
  let tag = ingredient.item.tag;
  let container = document.createElement('div');
  container.className = 'ingredient-container';

  let tagText = document.createElement('span');
  tagText.innerText = describeIngredient(ingredient);

  let itemList = document.createElement('ul');
  ITEMS_BY_TAG[tag].forEach(item => {
    let itemEntry = document.createElement('li');
    itemEntry.appendChild(createItemLink(item));
    itemList.appendChild(itemEntry);
  });

  container.appendChild(tagText);
  container.appendChild(itemList);
  return container;
}

/* Creates a link that changes the current item to the given one. */
function createItemLink(item, label) {
  let nameLink = document.createElement('a');
  nameLink.innerText = label || item.label || item.id;
  nameLink.href = '#' + item.id;
  return nameLink;
}

/* Creates a description for an ingredient */
function describeIngredient(ingredient) {
  let description;
  if (ingredient.item.id) {
    let item = ITEMS_BY_ID[ingredient.item.id];
    description = item.label || item.id;
  } else {
    description = ingredient.item.tag;
  }

  if (ingredient.amount > 1) {
    description = ingredient.amount + " " + description;
  }

  if (ingredient.min_condition !== null && ingredient.max_condition !== null) {
    description += " (condition: " + ingredient.min_condition + " - " + ingredient.max_condition + ")";
  } else if (ingredient.min_condition !== null) {
    description += " (condition: >= " + ingredient.min_condition + ")";
  } else if (ingredient.max_condition !== null) {
    description += " (condition: <= " + ingredient.max_condition + ")";
  }

  return description;
}

initItems();
if (SEARCH_INPUT.value) {
  // Saved value from a previous visit to the page
  updateSearchResults();
}

MODAL_UPLOAD.addEventListener('click', () => {
  if (!MODAL_FILE.files) {
    MODAL_ERROR.innerText = 'No file selected!';
    return;
  }

  MODAL_FILE.files[0].text().then(json => {
    let compactJSON = JSON.stringify(JSON.parse(json));
    localStorage.setItem('items', compactJSON);
    initItems();
  }, error => {
    MODAL_ERROR.innerText = error;
  });
});

SEARCH_INPUT.addEventListener('input', debounce(250, updateSearchResults));

window.addEventListener('hashchange', displayItem);
