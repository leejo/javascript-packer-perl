function filteredData ()
{
    var data = [];

    $$("pivot").$$("data").data.each(function(item){
        //item is a visible item
        var source = item.$source;
        if(source){
            var raw = [].concat(source).map(id => $$("pivot").data.getItem(id));
            data = data.concat(raw);
        }
    });
    return data;
};
