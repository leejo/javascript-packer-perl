function filteredData(){var d=[]; $$("pivot").$$("data").data.each(function(a){var c=a.$source;if(c){var b=[].concat(c).map(id=> $$("pivot").data.getItem(id));d=d.concat(b)}});return d};
