/**
 * Syncs a settings list to match the selected variables.
 * @param {Object} source - The source control with variable names
 * @param {Object} target - The target control to update
 * @param {string} targetIdName - Name of the identifier element in target (e.g. "var")
 * @param {string} targetValName - Name of the value element in target (e.g. "stat")
 * @param {string} defaultVal - Value for new entries (e.g. "use_default")
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
    sync(ui.varsCont, ui.statsContSpecific, "var", "stat", "use_default");
    sync(ui.varsCont, ui.testsContSpecific, "var", "test", "use_default");
    sync(ui.varsCont, ui.ciMethodsContSpecific, "var", "method", "use_default");
    sync(ui.varsCont, ui.diffMethodsContSpecific, "var", "method", "use_default");
  },

  varsCat_changed: function (ui) {
    sync(ui.varsCat, ui.statsCatSpecific, "var", "stat", "use_default");
    sync(ui.varsCat, ui.testsCatSpecific, "var", "test", "use_default");
    sync(ui.varsCat, ui.sortCatSpecific, "var", "sort", "use_default");
    sync(ui.varsCat, ui.ciMethodsCatSpecific, "var", "method", "use_default");
    sync(ui.varsCat, ui.diffMethodsDichotSpecific, "var", "method", "use_default");
  },
};
