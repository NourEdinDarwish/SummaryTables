const sync = require("./sync");
const enable = require("./enable");

module.exports = {
  // Set initial ListBox enabled state when the panel first opens
  view_loaded: function (ui) {
    const noGroup = !ui.groupVar.value();
    enable.enable(noGroup && ui.addPvalue.value(), [ui.testSpecific]);
  },

  // Fires when the user adds/removes variables in the Categorical Variables slot
  varsCat_changed: function (ui) {
    sync.sync(ui.varsCat, ui.statSpecific, "var", "stat", "useDefault");
    sync.sync(ui.varsCat, ui.sortCatSpecific, "var", "sort", "useDefault");
    sync.sync(ui.varsCat, ui.testSpecific, "var", "test", "useDefault");
  },

  // Toggle testSpecific enabled state when addPvalue changes
  addPvalue_changed: function (ui) {
    const noGroup = !ui.groupVar.value();
    enable.enable(noGroup && ui.addPvalue.value(), [ui.testSpecific]);
  },

  // Toggle groupVar-dependent controls when groupVar changes
  groupVar_changed: function (ui) {
    const noGroup = !ui.groupVar.value();
    enable.enable(noGroup && ui.addPvalue.value(), [ui.testSpecific]);
  },
};
