const regression = require("./regression");

module.exports = {
  // Fires when the Supplier needs to refresh its available items
  modelSupplier_updated: function (ui) {
    regression.updateModelTerms(ui, this);
  },

  // Fires when the user adds/removes variables in Covariates
  covs_changed: function (ui) {
    regression.updateModelTerms(ui, this);
  },

  // Fires when the user adds/removes variables in Factors
  factors_changed: function (ui) {
    regression.updateModelTerms(ui, this);
  },
};
