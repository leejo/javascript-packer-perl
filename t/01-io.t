#!perl -T

# =========================================================================== #
#
# All these tests are stolen from JavaScript::Minifier
#
# =========================================================================== #

use Test::More;

my $not = 15;

SKIP: {
    eval( 'use JavaScript::Packer' );

    skip( 'JavaScript::Packer not installed!', $not ) if ( $@ );

    plan tests => $not;

    fileTest( 's1', 'clean', 'compression level "clean"' );
    fileTest( 's2', 'shrink', 'compression level "shrink"' );
    fileTest( 's3', 'obfuscate', 'compression level "obfuscate"' );
    fileTest( 's4', 'best', 'compression level "best" whith short javascript' );
    fileTest( 's5', 'best', 'compression level "best" whith long javascript' );
    fileTest( 's7', 'clean', 'compression level "clean" function as argument' );
    fileTest( 's8', 'shrink', 'compression level "shrink" function as argument' );
    fileTest( 's9', 'shrink', 'compression level "shrink" with _no_shrink_ argument' );

    my $packer = JavaScript::Packer->init();

    my $var = 'var x = 2;';
    $packer->minify( \$var );
    is( $var, 'var x=2;', 'string literal input and ouput' );

    $var = "var x = 2;\n;;;alert('hi');\nvar x = 2;";
    $packer->minify( \$var );
    is( $var, 'var x=2;var x=2;', 'scriptDebug option' );

    $var = "var x = 2;";
    $packer->minify( \$var, { copyright => 'BSD' } );
    is( $var, '/* BSD */' . "\n" . 'var x=2;', 'copyright option');

    $var = "/* Copyright BSD */var x = 2;";
    $packer->minify( \$var, { remove_copyright => 1 } );
    is( $var, 'var x=2;', 'copyright comment with remove_copyright option');

    $var = "/* Copyright BSD */var x = 2;";
    $packer->minify( \$var );
    is( $var, '/* Copyright BSD */' . "\n" . 'var x=2;', 'copyright comment without remove_copyright option');

    $var = "/* JavaScript::Packer _no_compress_ */\n\nvar x = 1;\n\n\nvar y = 2;";
    $packer->minify( \$var );
    is( $var, "/* JavaScript::Packer _no_compress_ */\n\nvar x = 1;\n\n\nvar y = 2;", '_no_compress_ comment');

    $var = "/* JavaScript::Packer _no_compress_ */\n\nvar x = 1;\n\n\nvar y = 2;";
    $packer->minify( \$var, { no_compress_comment => 1 } );
    is( $var, "var x=1;var y=2;", '_no_compress_ comment with no_compress_comment option');

}

sub filesMatch {
    my $file1 = shift;
    my $file2 = shift;
    my $a;
    my $b;

    while (1) {
        $a = getc($file1);
        $b = getc($file2);

        if (!defined($a) && !defined($b)) { # both files end at same place
            return 1;
        }
        elsif (
            !defined($b) || # file2 ends first
            !defined($a) || # file1 ends first
            $a ne $b
        ) {     # a and b not the same
            return 0;
        }
    }
}

sub fileTest {
    my $filename    = shift;
    my $compress    = shift || 'minify';
    my $comment    = shift || '';

    open(INFILE, 't/scripts/' . $filename . '.js') or die("couldn't open file");
    open(GOTFILE, '>t/scripts/' . $filename . '-got.js') or die("couldn't open file");

    my $js = join( '', <INFILE> );

    my $packer = JavaScript::Packer->init();

    $packer->minify( \$js, { compress => $compress } );
    print GOTFILE $js;
    close(INFILE);
    close(GOTFILE);

    open(EXPECTEDFILE, 't/scripts/' . $filename . '-expected.js') or die("couldn't open file");
    open(GOTFILE, 't/scripts/' . $filename . '-got.js') or die("couldn't open file");
    ok( filesMatch(GOTFILE, EXPECTEDFILE), $comment );
    close(EXPECTEDFILE);
    close(GOTFILE);
}

