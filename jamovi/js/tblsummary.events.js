const events = {

    update: function(ui) {
        updateContArray(ui);
        updateCatArray(ui);
    },

    onChange_vars_cont: function(ui) {
        updateContArray(ui);
    },

    onChange_vars_cat: function(ui) {
        updateCatArray(ui);
    }
};

let updateContArray = function(ui) {
    let variableList = ui.vars_cont.value() || [];
    let currentList = ui.stats_cont_specific.value() || [];

    let newList = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j] && currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            newList.push({ var: variableList[i], stat: "use_global" });
        else
            newList.push(found);
    }

    ui.stats_cont_specific.setValue(newList);
};

let updateCatArray = function(ui) {
    let variableList = ui.vars_cat.value() || [];
    let currentList = ui.stats_cat_specific.value() || [];

    let newList = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j] && currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            newList.push({ var: variableList[i], stat: "use_global" });
        else
            newList.push(found);
    }

    ui.stats_cat_specific.setValue(newList);
};

module.exports = events;

