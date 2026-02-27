/**
 * Syncs a settings list to match the selected variables.
 * @param {Object} source - The source control with variable names
 * @param {Object} target - The target control to update
 * @param {string} targetIdName - Name of the identifier element in target (e.g. "var")
 * @param {string} targetValName - Name of the value element in target (e.g. "stat")
 * @param {string} defaultVal - Value for new entries (e.g. "useDefault")
 */
const sync = function (source, target, targetIdName, targetValName, defaultVal) {
  const sourceList = source.value() || [];
  const targetList = target.value() || [];

  const updated = sourceList.map(function (sourceItem) {
    const existing = targetList.find(function (targetItem) {
      return targetItem[targetIdName] === sourceItem;
    });
    return existing || { [targetIdName]: sourceItem, [targetValName]: defaultVal };
  });

  target.setValue(updated);
};

module.exports = {
  varsCont_changed: function (ui) {
    sync(ui.varsCont, ui.statContSpecific, "var", "stat", "useDefault");
    sync(ui.varsCont, ui.testContSpecific, "var", "test", "useDefault");
    sync(ui.varsCont, ui.ciContSpecific, "var", "method", "useDefault");
    sync(ui.varsCont, ui.diffContSpecific, "var", "method", "useDefault");
  },

  varsCat_changed: function (ui) {
    sync(ui.varsCat, ui.statCatSpecific, "var", "stat", "useDefault");
    sync(ui.varsCat, ui.testCatSpecific, "var", "test", "useDefault");
    sync(ui.varsCat, ui.sortCatSpecific, "var", "sort", "useDefault");
    sync(ui.varsCat, ui.ciCatSpecific, "var", "method", "useDefault");
    sync(ui.varsCat, ui.diffDichotSpecific, "var", "method", "useDefault");
  },
};
