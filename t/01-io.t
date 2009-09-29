#!perl -T

# =========================================================================== #
#
# All these tests are stolen from JavaScript::Minifier
#
# =========================================================================== #

use Test::More;

my $not = 8;

SKIP: {
	eval( 'use JavaScript::Packer' );
	
	skip( 'JavaScript::Packer not installed!', $not ) if ( $@ );
	
	plan tests => $not;
	
	fileTest( 's1', 'clean', 'compression level "clean"' );
	fileTest( 's2', 'shrink', 'compression level "shrink"' );
	fileTest( 's3', 'obfuscate', 'compression level "obfuscate"' );
	fileTest( 's4', 'best', 'compression level "best" whith short javascript' );
	fileTest( 's5', 'best', 'compression level "best" whith long javascript' );
	
	my $var = 'var x = 2;';
	JavaScript::Packer::minify( \$var );
	is( $var, 'var x=2;', 'string literal input and ouput' );
	$var = "var x = 2;\n;;;alert('hi');\nvar x = 2;";
	JavaScript::Packer::minify( \$var );
	is( $var, 'var x=2;var x=2;', 'scriptDebug option' );
	$var = "var x = 2;";
	JavaScript::Packer::minify( \$var, { 'copyright' => 'BSD' } );
	is( $var, '/* BSD */var x=2;', 'copyright option');
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
	my $filename	= shift;
	my $compress	= shift || 'minify';
	my $comment	= shift || '';
	
	open(INFILE, 't/scripts/' . $filename . '.js') or die("couldn't open file");
	open(GOTFILE, '>t/scripts/' . $filename . '-got.js') or die("couldn't open file");
	
	my $js = join( '', <INFILE> );
	JavaScript::Packer::minify( \$js, { 'compress' => $compress } );
	print GOTFILE $js;
	close(INFILE);
	close(GOTFILE);
	
	open(EXPECTEDFILE, 't/scripts/' . $filename . '-expected.js') or die("couldn't open file");
	open(GOTFILE, 't/scripts/' . $filename . '-got.js') or die("couldn't open file");
	ok( filesMatch(GOTFILE, EXPECTEDFILE), $comment );
	close(EXPECTEDFILE);
	close(GOTFILE);
}

