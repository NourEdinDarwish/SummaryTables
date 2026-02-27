/**
 * Workaround for ListBox enable/disable.
 *
 * Jamovi's enable binding is commented out for ListBox controls
 * (optionlistcontrol.ts lines 189, 234). This utility toggles the
 * framework's own "disabled-list" CSS class which applies
 * grayscale + opacity + pointer-events:none.
 */

/**
 * Enable or disable one or more ListBox controls.
 *
 * @param {boolean} enabled - true to enable, false to disable
 * @param {Array}   lists   - array of ui ListBox controls (e.g. [ui.diffContSpecific])
 */
const enable = function (enabled, lists) {
  for (let i = 0; i < lists.length; i++) {
    if (enabled) lists[i].el.classList.remove("disabled-list");
    else lists[i].el.classList.add("disabled-list");
  }
};

module.exports = { enable };
