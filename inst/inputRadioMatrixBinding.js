var radioMatrixInputBinding = new Shiny.InputBinding();

$.extend(radioMatrixInputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-radiomatrix-container');
  },

  getValue: function(el) {
    // find rows in the matrix
  	var $rows = $(el).find("tr.shiny-radiomatrix-row");
    var values = {};

    $rows.each(function (){
     var $ch = $(this).find("input:radio:checked").val();
     if ($ch === undefined){ // always keep the length of output constant, but return NULLs for unchecked rows
       values[$(this).attr("name")] = null;
     } else {
        values[$(this).attr("name")] = $(this).find("input:radio:checked").val();
     }

    });

      return(values);
    },

  setValue: function(el, value) {
    // TODO: implement
  },

  receiveMessage: function(el, data) {
    //TODO: implement
  },

  subscribe: function(el, callback) {
    $(el).on('change.radioMatrixInputBinding', function(event) {
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off('.radioMatrixInputBinding');
  },

});

Shiny.inputBindings.register(radioMatrixInputBinding, 'shiny.radioMatrixInput');
