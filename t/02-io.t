#!perl -T

# =========================================================================== #
#
# Most of these tests are stolen from JavaScript::Minifier
#
# =========================================================================== #

use Test::More;
use Test::File::Contents;

my $not = 30;

SKIP: {
    eval( 'use JavaScript::Packer' );

    skip( 'JavaScript::Packer not installed!', $not ) if ( $@ );

    plan tests => $not;

    fileTest( 's1',  'clean',     'compression level "clean"' );
    fileTest( 's2',  'shrink',    'compression level "shrink"' );
    fileTest( 's3',  'obfuscate', 'compression level "obfuscate"' );
    fileTest( 's4',  'best',      'compression level "best" whith short javascript' );
    fileTest( 's5',  'best',      'compression level "best" whith long javascript' );
    fileTest( 's7',  'clean',     'compression level "clean" function as argument' );
    fileTest( 's8',  'shrink',    'compression level "shrink" function as argument' );
    fileTest( 's9',  'shrink',    'compression level "shrink" with _no_shrink_ argument' );
    fileTest( 's10', 'shrink',    'compression level "shrink" with quoted args' );
    fileTest( 's11', 'shrink',    'compression level "shrink" with associative array' );

    my $packer = JavaScript::Packer->init();

    my $var = 'var x = 2;';
    $packer->minify( \$var );
    is( $var, 'var x=2;', 'string literal input and ouput' );

    $var = "var x = 2;\n;;;alert('hi');\nvar x = 2;";
    $packer->minify( \$var );
    is( $var, 'var x=2;var x=2;', 'scriptDebug option' );

    $var = "var x = 2;";
    $packer->copyright( 'BSD' );
    $packer->minify( \$var );
    is( $var, '/* BSD */' . "\n" . 'var x=2;', 'copyright option compression level "clean"' );
    $packer->compress( 'shrink' );
    $packer->minify( \$var );
    is( $var, '/* BSD */' . "\n" . 'var x=2;', 'copyright option compression level "shrink"' );
    $packer->compress( 'best' );
    $packer->minify( \$var );
    is( $var, '/* BSD */' . "\n" . 'var x=2;', 'copyright option compression level "best"' );
    $packer->compress( 'obfuscate' );
    $packer->minify( \$var );
    is(
        $var,
        '/* BSD */'
            . "\neval(function(p,a,c,k,e,r){e=String;if('0'.replace(0,e)==0){while(c--)r[e(c)]=k[c];k=[function(e){return r[e]||e}];e=function(){return'[01]'};c=1};while(c--)if(k[c])p=p.replace(new RegExp('\\\\b'+e(c)+'\\\\b','g'),k[c]);return p}('1 0=2;',[],2,'x|var'.split('|'),0,{}))",
        'copyright option compression level "obfuscate"'
    );

    $packer = JavaScript::Packer->init();

    $var = "/* Copyright BSD */var x = 2;";
    $packer->minify( \$var, { remove_copyright => 1 } );
    is( $var, 'var x=2;', 'copyright comment with remove_copyright option' );

    $var = "/* Copyright BSD */var x = 2;";
    $packer->minify( \$var, { remove_copyright => 0 } );
    is( $var, '/* Copyright BSD */' . "\n" . 'var x=2;', 'copyright comment without remove_copyright option' );

    $packer = JavaScript::Packer->init();

    $var = "/* JavaScript::Packer _no_compress_ */\n\nvar x = 1;\n\n\nvar y = 2;";
    $packer->minify( \$var );
    is( $var, "/* JavaScript::Packer _no_compress_ */\n\nvar x = 1;\n\n\nvar y = 2;", '_no_compress_ comment' );

    $var = "/* JavaScript::Packer _no_compress_ */\n\nvar x = 1;\n\n\nvar y = 2;";
    $packer->minify( \$var, { no_compress_comment => 1 } );
    is( $var, "var x=1;var y=2;", '_no_compress_ comment with no_compress_comment option' );

    $var = "var foo = \"foo\" + \"bar\" + \"baz\" + 'foo' + 'bar' + 'baz' + \"foo\" + \"bar\" + \"baz\";";
    $packer->minify( \$var );
    is( $var, "var foo=\"foobarbaz\"+'foobarbaz'+\"foobarbaz\";", 'concat' );

    $var = "var foo = \" \"; var bar = \"+\";";
    JavaScript::Packer::minify( \$var );
    is( $var, "var foo=\" \";var bar=\"+\";", 'concat with plus' );

    $var = "var foo = \" \"; var bar = \"+\"; var baz = \"-\";";
    JavaScript::Packer::minify( \$var );
    is( $var, "var foo=\" \";var bar=\"+\";var baz=\"-\";", 'concat with plus and three strings' );

    $var = "!/foo/";
    $packer->minify( \$var );
    is( $var, "!/foo/", 'regexp preceeded by negation' );

    $var = "!/foo/";
    JavaScript::Packer::minify( \$var );
    is( $var, "!/foo/", 'regexp preceeded by negation, subroutine invocation' );

    $var = "!/foo/";
    $packer->minify( \$var, { compress => 'shrink', } );
    is( $var, "!/foo/", 'regexp preceeded by negation, with shrink' );

    $var = "!/foo/";
    JavaScript::Packer::minify( \$var, { compress => 'shrink', } );
    is( $var, "!/foo/", 'regexp preceeded by negation, with shrink, subroutine invocation' );

    $var = "var foo = /bar/;";
    JavaScript::Packer::minify( \$var );
    is( $var, "var foo=/bar/;", 'building Regexp object implictly' );

    $var = "var foo = /bar/;";
    JavaScript::Packer::minify( \$var, { compress => 'shrink', } );
    is( $var, "var foo=/bar/;", 'building Regexp object implictly with shrink' );

    $var = q~var foo = new RegExp("bar");~;
    JavaScript::Packer::minify( \$var );
    is( $var, q~var foo=new RegExp("bar");~, 'building Regexp object explictly' );

    $var = q~var foo = new RegExp("bar");~;
    JavaScript::Packer::minify( \$var );
    JavaScript::Packer::minify( \$var, { compress => 'shrink', } );
    is( $var, q~var foo=new RegExp("bar");~, 'building Regexp object explictly with shrink' );
}

sub fileTest {
    my $filename = shift;
    my $compress = shift || 'minify';
    my $comment  = shift || '';

    open( INFILE,  't/scripts/' . $filename . '.js' )      or die( "couldn't open file" );
    open( GOTFILE, '>t/scripts/' . $filename . '-got.js' ) or die( "couldn't open file" );

    my $js = join( '', <INFILE> );

    my $packer = JavaScript::Packer->init();

    $packer->minify( \$js, { compress => $compress } );
    print GOTFILE $js;
    close( INFILE );
    close( GOTFILE );

	files_eq_or_diff(
		't/scripts/' . $filename . '-got.js',
		't/scripts/' . $filename . '-expected.js',
		{ style => 'Unified' }
	);

	return;
}
