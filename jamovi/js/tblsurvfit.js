const sync = require("./sync");
const enable = require("./enable");

module.exports = {
  // Set initial ListBox enabled state when the panel first opens
  view_updated: function (ui) {
    const hasStrata = (ui.strata.value() || []).length > 0;
    enable.enable(hasStrata && ui.addPvalue.value(), [ui.testSpecific]);
  },

  // Fires when the user adds/removes variables in the Strata slot.
  // strata both supplies ListBox rows AND controls enabled state
  // (unlike tblcontinuous where varsCat supplies and groupVar enables)
  strata_changed: function (ui) {
    sync.sync(ui.strata, ui.testSpecific, "var", "test", "useDefault");
    const hasStrata = (ui.strata.value() || []).length > 0;
    enable.enable(hasStrata && ui.addPvalue.value(), [ui.testSpecific]);
  },

  // Toggle testSpecific enabled state when addPvalue changes
  addPvalue_changed: function (ui) {
    const hasStrata = (ui.strata.value() || []).length > 0;
    enable.enable(hasStrata && ui.addPvalue.value(), [ui.testSpecific]);
  },
};
