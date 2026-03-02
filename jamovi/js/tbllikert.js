const sync = require("./sync");

module.exports = {
  vars_changed: function (ui) {
    sync.sync(ui.vars, ui.statSpecific, "var", "stat", "useDefault");
  },
};
