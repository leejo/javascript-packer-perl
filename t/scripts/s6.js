var CONCAT_ARGUMENTS_BUGGY = (function() {
    return [].concat(arguments)[0][0] !== 1;
})( 1, 2 )

if (CONCAT_ARGUMENTS_BUGGY) arrayProto.concat = concat;