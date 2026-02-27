const sync = require("./sync");
const enable = require("./enable");

module.exports = {
  // Set initial ListBox enabled state when the panel first opens
  view_loaded: function (ui) {
    const hasCat = (ui.varsCat.value() || []).length > 0;
    const hasGroup = !!ui.groupVar.value();
    enable.enable(hasCat, [ui.sortCatSpecific]);
    enable.enable(hasGroup && ui.addDifference.value(), [
      ui.diffContSpecific,
      ui.diffDichotSpecific,
    ]);
    enable.enable(hasGroup && ui.addPvalue.value(), [
      ui.testContSpecific,
      ui.testCatSpecific,
    ]);
    enable.enable(ui.addCi.value(), [
      ui.ciContSpecific,
      ui.ciCatSpecific,
    ]);
  },

  // Fires when the user adds/removes variables in the Continuous slot
  varsCont_changed: function (ui) {
    sync.sync(ui.varsCont, ui.statContSpecific, "var", "stat", "useDefault");
    sync.sync(ui.varsCont, ui.testContSpecific, "var", "test", "useDefault");
    sync.sync(ui.varsCont, ui.ciContSpecific, "var", "method", "useDefault");
    sync.sync(ui.varsCont, ui.diffContSpecific, "var", "method", "useDefault");
  },

  // Fires when the user adds/removes variables in the Categorical slot
  varsCat_changed: function (ui) {
    const hasCat = (ui.varsCat.value() || []).length > 0;
    enable.enable(hasCat, [ui.sortCatSpecific]);

    sync.sync(ui.varsCat, ui.statCatSpecific, "var", "stat", "useDefault");
    sync.sync(ui.varsCat, ui.testCatSpecific, "var", "test", "useDefault");
    sync.sync(ui.varsCat, ui.sortCatSpecific, "var", "sort", "useDefault");
    sync.sync(ui.varsCat, ui.ciCatSpecific, "var", "method", "useDefault");

    // Only show 2-level variables in the dichotomous difference methods
    sync.syncDichotomous(
      this,
      ui.varsCat,
      ui.diffDichotSpecific,
      "var",
      "method",
      "useDefault",
    );
  },

  // Toggle ListBox enabled state when parent checkboxes change
  addDifference_changed: function (ui) {
    const hasGroup = !!ui.groupVar.value();
    enable.enable(hasGroup && ui.addDifference.value(), [
      ui.diffContSpecific,
      ui.diffDichotSpecific,
    ]);
  },

  addPvalue_changed: function (ui) {
    const hasGroup = !!ui.groupVar.value();
    enable.enable(hasGroup && ui.addPvalue.value(), [
      ui.testContSpecific,
      ui.testCatSpecific,
    ]);
  },

  addCi_changed: function (ui) {
    enable.enable(ui.addCi.value(), [
      ui.ciContSpecific,
      ui.ciCatSpecific,
    ]);
  },

  // Toggle all groupVar-dependent ListBoxes when groupVar changes
  groupVar_changed: function (ui) {
    const hasGroup = !!ui.groupVar.value();
    enable.enable(hasGroup && ui.addDifference.value(), [
      ui.diffContSpecific,
      ui.diffDichotSpecific,
    ]);
    enable.enable(hasGroup && ui.addPvalue.value(), [
      ui.testContSpecific,
      ui.testCatSpecific,
    ]);
  },

  // Fires when the dataset changes (e.g. user adds data creating new levels)
  view_remoteDataChanged: function (ui, event) {
    if (event.dataType === "columns" && event.dataInfo.levelsChanged) {
      sync.syncDichotomous(
        this,
        ui.varsCat,
        ui.diffDichotSpecific,
        "var",
        "method",
        "useDefault",
      );
    }
  },
};
