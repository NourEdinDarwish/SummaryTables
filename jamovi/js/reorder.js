/**
 * Sync variable insertion order across multiple listboxes.
 *
 * Uses the type-slot pattern: each position in varOrder remembers
 * which listbox (type) it came from — inferred by checking which
 * source list currently contains the variable.
 * Within-box reordering fills the type's slots in the new order.
 * New variables are appended at the end.
 *
 * @param {Array} sources - Array of UI controls (e.g. [ui.varsCont, ui.varsCat]).
 *   The array index is the type.
 * @param {Object} orderCtrl - The varOrder UI control.
 */
const syncOrder = function (sources, orderCtrl) {
  // Collect current values from each source, keyed by type (index)
  const sourceLists = [];
  const sourceMap = {};
  for (let s = 0; s < sources.length; s++) {
    const vals = sources[s].value() || [];
    sourceLists.push(vals.slice());
    for (let v = 0; v < vals.length; v++) {
      sourceMap[vals[v]] = s;
    }
  }

  const currentOrder = orderCtrl.value() || [];

  // Track how many items we have pulled from each source list
  const consumedCounts = [];
  for (let s = 0; s < sources.length; s++) consumedCounts.push(0);

  const newOrder = [];

  // Fill existing slots
  for (let i = 0; i < currentOrder.length; i++) {
    const varName = currentOrder[i];
    const sourceIndex = sourceMap[varName];
    if (sourceIndex === undefined) continue; // removed variable
    if (consumedCounts[sourceIndex] < sourceLists[sourceIndex].length) {
      newOrder.push(sourceLists[sourceIndex][consumedCounts[sourceIndex]]);
      consumedCounts[sourceIndex] += 1;
    }
  }

  // Append remaining new variables
  for (let t = 0; t < sourceLists.length; t++) {
    for (let j = consumedCounts[t]; j < sourceLists[t].length; j++) {
      newOrder.push(sourceLists[t][j]);
    }
  }

  orderCtrl.setValue(newOrder);
};

module.exports = { syncOrder };
