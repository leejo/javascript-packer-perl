# NAME

JavaScript::Packer - Perl version of Dean Edwards' Packer.js

<div>

    <a href='https://travis-ci.org/leejo/javascript-packer-perl?branch=master'><img src='https://travis-ci.org/leejo/javascript-packer-perl.svg?branch=master' alt='Build Status' /></a>
    <a href='https://coveralls.io/r/leejo/javascript-packer-perl'><img src='https://coveralls.io/repos/leejo/javascript-packer-perl/badge.png?branch=master' alt='Coverage Status' /></a>
</div>

# VERSION

Version 2.04

# DESCRIPTION

A JavaScript Compressor

This module is an adaptation of Dean Edwards' Packer.js.

Additional information: http://dean.edwards.name/packer/

# SYNOPSIS

    use JavaScript::Packer;

    my $packer = JavaScript::Packer->init();

    $packer->minify( $javascript, $opts );

To return a scalar without changing the input simply use (e.g. example 2):

    my $ret = $packer->minify( $javascript, $opts );

For backward compatibility it is still possible to call 'minify' as a function:

    JavaScript::Packer::minify( $javascript, $opts );

The first argument must be a scalarref of javascript-code.

Second argument must be a hashref of options. Possible options are:

- compress

    Defines compression level. Possible values are 'clean', 'shrink', 'obfuscate'
    and 'best'.
    Default value is 'clean'.
    'best' uses 'shrink' or 'obfuscate' depending on which result is shorter. This
    is recommended because especially when compressing short scripts the result
    will exceed the input if compression level is 'obfuscate'.

- copyright

    You can add a copyright notice at the top of the script.

- remove\_copyright

    If there is a copyright notice in a comment it will only be removed if this
    option is set to a true value. Otherwise the first comment that contains the
    word "copyright" will be added at the top of the packed script. A copyright
    comment will be overwritten by a copyright notice defined with the copyright
    option.

- no\_compress\_comment

    If not set to a true value it is allowed to set a JavaScript comment that
    prevents the input being packed or defines a compression level.

        /* JavaScript::Packer _no_compress_ */
        /* JavaScript::Packer shrink */

# EXAMPLES

- Example 1

    Common usage.

        #!/usr/bin/perl

        use strict;
        use warnings;

        use JavaScript::Packer;

        my $packer = JavaScript::Packer->init();

        open( UNCOMPRESSED, 'uncompressed.js' );
        open( COMPRESSED, '>compressed.js' );

        my $js = join( '', <UNCOMPRESSED> );

        $packer->minify( \$js, { compress => 'best' } );

        print COMPRESSED $js;
        close(UNCOMPRESSED);
        close(COMPRESSED);

- Example 2

    A scalar is requested by the context. The input will remain unchanged.

        #!/usr/bin/perl

        use strict;
        use warnings;

        use JavaScript::Packer;

        my $packer = JavaScript::Packer->init();

        open( UNCOMPRESSED, 'uncompressed.js' );
        open( COMPRESSED, '>compressed.js' );

        my $uncompressed = join( '', <UNCOMPRESSED> );

        my $compressed = $packer->minify( \$uncompressed, { compress => 'best' } );

        print COMPRESSED $compressed;
        close(UNCOMPRESSED);
        close(COMPRESSED);

# AUTHOR

Merten Falk, `<nevesenin at cpan.org>`. Now maintained by Lee
Johnson (LEEJO)

# BUGS AND CAVEATS

This module will break code that relies on ASI, see [https://github.com/leejo/javascript-packer-perl/issues/5](https://github.com/leejo/javascript-packer-perl/issues/5)
for more information.

Please report any bugs or feature requests through the web interface at
[http://github.com/leejo/javascript-packer-perl/issues](http://github.com/leejo/javascript-packer-perl/issues).

# SUPPORT

You can find documentation for this module with the perldoc command.

perldoc JavaScript::Packer

# COPYRIGHT & LICENSE

Copyright 2008 - 2012 Merten Falk, all rights reserved.

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.
