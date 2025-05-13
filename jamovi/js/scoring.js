const events = {
  
    view_updated: function(ui) {
         console.log("Updating analysis");

    },
    
    covs_changed: function(ui ) {
      
         console.log("covs changed");

         var a = utils.clone(ui.forced.value(), []);
         var b = utils.clone(ui.covs.value(), []);

         a = a.filter(entry => b.includes(entry.var));

         const existingVars = new Set(a.map(entry => entry.var));

         b.forEach(val => {
            if (!existingVars.has(val)) {
              a.push({ var: val, type: 'auto' });
          }
         });
         ui.forced.setValue(a);

         console.log("work on included");

        var facs = utils.clone(ui.factors.value(),[])
        var combined = b.concat(facs)

        ui.includedSupplier.setValue(utils.valuesToItems(combined, FormatDef.term));
        return
         
//          var c = utils.clone(ui.includedSupplier.value(), []);
//          console.log("clone done")
//         
//         c = c.filter(item => b.includes(item));
//
//          b.forEach(item => {
//                if (!c.includes(item)) {
//                 c.push(item);
//                }
//          });

//         console.log(c)
//         ui.includedSupplier.setValue(utils.valuesToItems(c, FormatDef.term));
         

    },
    
    factors_changed: function(ui) {
      
      var covs = utils.clone(ui.covs.value(),[])
      var facs = utils.clone(ui.factors.value(),[])
      var combined = covs.concat(facs)
      ui.includedSupplier.setValue(utils.valuesToItems(combined, FormatDef.term));
    },
    
    includedSupplier_updated: function(ui) {
      
      console.log("included supplier updated");
      var covs = utils.clone(ui.covs.value(),[])
      var facs = utils.clone(ui.factors.value(),[])
      var combined = covs.concat(facs);
      ui.includedSupplier.setValue(utils.valuesToItems(combined, FormatDef.term));


    },
    includedSupplier_changed: function(ui) {

     console.log("includedSupplier changed"); 

      let values = utils.itemsToValues(ui.includedSupplier.value());
      utils.checkValue(ui.included, true, values, FormatDef.term);

      },
    included_changed: function(ui) {
      
     console.log("included changed"); 
     var included_values=utils.clone(ui.included.value(),[]);
     var ok_values=included_values.filter((v,i,a)=>a.indexOf(v)==i);
     if (included_values.length !== ok_values.length) {
        ui.included.setValue(ok_values);
     }
    }

      
    
};

module.exports = events;

