/**
 * Sync the Supplier and auto-add/remove terms.
 *
 * When the user adds or removes a variable, this function:
 *   1. Updates the Supplier so the user can drag variables into terms
 *   2. Auto-adds main effects for newly added variables
 *   3. Removes terms whose source variable was removed
 *
 * @param {Object} ui      - The UI controls object.
 * @param {Object} context - The View instance (`this` in handlers).
 */
const updateModelTerms = function (ui, context) {
  // 1. Combine both variable slots into one list
  const covsList = utils.clone(ui.covs.value(), []);
  const factorsList = utils.clone(ui.factors.value(), []);
  const allVars = covsList.concat(factorsList);

  // 2. Feed the combined list to the Supplier
  ui.modelSupplier.setValue(
    utils.valuesToItems(allVars, FormatDef.variable),
  );

  // 3. Diff against previous state to detect added / removed variables
  const diff = context.findChanges(
    "modelSupplier_vars",
    allVars,
    true,
    FormatDef.variable,
  );

  const terms = utils.clone(ui.modelTerms.value(), []);
  let changed = false;

  // 4. Remove terms that contain any removed variable
  for (let i = 0; i < diff.removed.length; i++) {
    for (let j = terms.length - 1; j >= 0; j--) {
      if (FormatDef.term.contains(terms[j], diff.removed[i])) {
        terms.splice(j, 1);
        changed = true;
      }
    }
  }

  // 5. Auto-add main effects for newly added variables
  for (let i = 0; i < diff.added.length; i++) {
    const newTerm = [diff.added[i]];
    if (!utils.listContains(terms, newTerm, FormatDef.term)) {
      terms.push(newTerm);
      changed = true;
    }
  }

  // 6. Sort terms by length (main effects before interactions)
  if (utils.sortArraysByLength(terms)) {
    changed = true;
  }

  if (changed) {
    ui.modelTerms.setValue(terms);
  }
};

/**
 * Re-sort terms after manual drag/drop inside the ListBox.
 *
 * Without this, the user can drag an interaction above its constituent
 * main effects — violating the hierarchy principle.
 *
 * @param {Object} ui - The UI controls object.
 */
const enforceTermOrder = function (ui) {
  const terms = utils.clone(ui.modelTerms.value(), []);
  if (utils.sortArraysByLength(terms)) {
    ui.modelTerms.setValue(terms);
  }
};

module.exports = { updateModelTerms, enforceTermOrder };
