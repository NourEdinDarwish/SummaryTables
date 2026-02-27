var sync = require("./sync");

module.exports = {
  // Fires when the user adds/removes variables in the Continuous slot
  varsCont_changed: function (ui) {
    sync.sync(ui.varsCont, ui.statContSpecific, "var", "stat", "useDefault");
    sync.sync(ui.varsCont, ui.testContSpecific, "var", "test", "useDefault");
    sync.sync(ui.varsCont, ui.ciContSpecific, "var", "method", "useDefault");
    sync.sync(ui.varsCont, ui.diffContSpecific, "var", "method", "useDefault");
  },

  // Fires when the user adds/removes variables in the Categorical slot
  varsCat_changed: function (ui) {
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
