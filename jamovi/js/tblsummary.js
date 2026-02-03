const events = {
  // Fires when user adds/removes continuous variables
  varsCont_changed: function (ui) {
    updateList(ui, ui.varsCont, ui.statsContSpecific, "use_global", "stat");
    updateList(ui, ui.varsCont, ui.testsContSpecific, "use_global", "test");
  },

  // Fires when user adds/removes categorical variables
  varsCat_changed: function (ui) {
    updateList(ui, ui.varsCat, ui.statsCatSpecific, "use_global", "stat");
    updateList(ui, ui.varsCat, ui.testsCatSpecific, "use_global", "test");
  },
};

/**
 * Generic function to update a specific settings list based on a variable source list.
 * @param {Object} ui - The UI object.
 * @param {Object} sourceCtrl - The control containing the list of variables (e.g., varsCont).
 * @param {Object} targetCtrl - The control to update (e.g., statsContSpecific).
 * @param {string} defaultValue - The default value for the setting (e.g., "use_global").
 * @param {string} keyName - The key name for the setting in the item object (e.g., "stat" or "test").
 */
let updateList = function (ui, sourceCtrl, targetCtrl, defaultValue, keyName) {
  let variableList = sourceCtrl.value();
  if (!variableList) variableList = [];

  let currentList = targetCtrl.value();
  if (!currentList) currentList = [];

  let newList = [];
  for (let i = 0; i < variableList.length; i++) {
    let found = null;
    for (let j = 0; j < currentList.length; j++) {
      if (currentList[j] && currentList[j].var === variableList[i]) {
        found = currentList[j];
        break;
      }
    }

    if (found === null) {
      // Create new item with correct key
      let newItem = { var: variableList[i] };
      newItem[keyName] = defaultValue;
      newList.push(newItem);
    } else {
      // Keep existing item
      newList.push(found);
    }
  }

  targetCtrl.setValue(newList);
};

module.exports = events;
