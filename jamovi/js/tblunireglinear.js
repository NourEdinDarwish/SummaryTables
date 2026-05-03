const reorder = require("./reorder");

module.exports = {
  // Fires when the user adds/removes variables in Covariates
  covs_changed: function (ui) {
    reorder.syncOrder([ui.covs, ui.factors], ui.varOrder);
  },

  // Fires when the user adds/removes variables in Factors
  factors_changed: function (ui) {
    reorder.syncOrder([ui.covs, ui.factors], ui.varOrder);
  },
};
