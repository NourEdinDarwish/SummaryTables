/**
 * Syncs a settings list to match the selected variables.
 *
 * When the user adds/removes variables in a slot (e.g. varsCont),
 * jamovi fires the _changed event. This function makes sure the
 * corresponding settings array (e.g. statContSpecific) has one row
 * per variable — adding new rows for new variables, and removing
 * rows for variables that were taken out.
 *
 * @param {Object} source         - The source control (e.g. ui.varsCont)
 * @param {Object} target         - The target array control (e.g. ui.statContSpecific)
 * @param {string} targetIdName   - Key that identifies which variable ("var")
 * @param {string} targetValName  - Key that holds the user's choice ("stat", "test", etc.)
 * @param {string} defaultVal     - Default value for new entries ("useDefault")
 */
const sync = function (
  source,
  target,
  targetIdName,
  targetValName,
  defaultVal,
) {
  const sourceList = source.value() || [];
  const targetList = target.value() || [];

  const updated = sourceList.map(function (sourceItem) {
    const existing = targetList.find(function (targetItem) {
      return targetItem[targetIdName] === sourceItem;
    });
    return (
      existing || { [targetIdName]: sourceItem, [targetValName]: defaultVal }
    );
  });

  target.setValue(updated);
};

/**
 * Like sync(), but only includes variables with exactly 2 levels (dichotomous).
 *
 * Uses jamovi's requestData API to ask the dataset for each variable's
 * level count. Since requestData returns a Promise (the answer comes later,
 * not immediately), we use Promise.all to wait for all answers.
 *
 * @param {Object} view           - The View instance (provides requestData)
 * @param {Object} source         - The source control (e.g. ui.varsCat)
 * @param {Object} target         - The target array control (e.g. ui.diffDichotSpecific)
 * @param {string} targetIdName   - Key that identifies which variable ("var")
 * @param {string} targetValName  - Key that holds the user's choice ("method")
 * @param {string} defaultVal     - Default value for new entries ("useDefault")
 */
const syncDichotomous = function (
  view,
  source,
  target,
  targetIdName,
  targetValName,
  defaultVal,
) {
  const sourceList = source.value() || [];
  const targetList = target.value() || [];

  if (sourceList.length === 0) {
    target.setValue([]);
    return;
  }

  var promises = sourceList.map(function (varName) {
    return view
      .requestData("column", { columnName: varName, properties: ["levels"] })
      .then(function (rData) {
        return {
          varName: varName,
          levelCount: rData.columnFound ? rData.levels.length : 0,
        };
      });
  });

  Promise.all(promises).then(function (results) {
    var dichotVars = results
      .filter(function (r) {
        return r.levelCount === 2;
      })
      .map(function (r) {
        return r.varName;
      });

    var updated = dichotVars.map(function (varName) {
      var existing = targetList.find(function (item) {
        return item[targetIdName] === varName;
      });
      return (
        existing || { [targetIdName]: varName, [targetValName]: defaultVal }
      );
    });

    target.setValue(updated);
  });
};

module.exports = { sync, syncDichotomous };
