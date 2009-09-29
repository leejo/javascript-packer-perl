package JavaScript::Packer;

use 5.008;
use warnings;
use strict;
use Carp;

use vars qw/$VERSION $COMMENT $DATA $WHITESPACE $CLEAN $BLOCK $WORD/;

# =========================================================================== #

$VERSION = '0.02';

$WORD = qr/((?>[a-zA-Z0-9_]+))/;

$BLOCK = qr/(function\s*[a-zA-Z0-9_\x24]*\s*\(\s*(([a-zA-Z0-9_\x24][a-zA-Z0-9_\x24, ]*[a-zA-Z0-9_\x24])*)\s*\)\s*)?(\{([^{}]*)\})/;

$COMMENT = [
	{
		'regexp'	=> qr/(\/\/|;;;)[^\n]*\n?/,
		'replacement'	=> 'sprintf( " " )'
	},
	{
		'regexp'	=> qr/(\/\*[^*]*\*+([^\/][^*]*\*+)*\/)/,
		'replacement'	=> 'sprintf( " " )'
	}
];

$DATA = [
	{
		'regexp'	=> qr/\s+(\/(\\[\/\\]|[^*\/])(\\.|[^\/\n\\])*\/[gim]*)/,
		'replacement'	=> 'sprintf( "%s", $1 )'
	},
	{
		'regexp'	=> qr/([^a-zA-Z0-9_\x24\/'"*)\?:])(\/(\\[\/\\]|[^*\/])(\\.|[^\/\n\\])*\/[gim]*)/,
		'replacement'	=> 'sprintf( "%s%s", $1, $2 )'
	},
	{
		'regexp'	=> qr/"(?>(?:(?>[^"\\]+)|\\.|\\")*)"|'(?>(?:(?>[^'\\]+)|\\.|\\')*)'/,
		'replacement'	=> '$&'
	},
	{
		'regexp'	=> qr/\/\*@|@\*\/|\/\/@[^\n]*\n/,
		'replacement'	=> '$&'
	}
];

$WHITESPACE = [
	{
		'regexp'	=> qr/(\d)\s+(\.\s*[a-z\x24_\[(])/,
		'replacement'	=> 'sprintf( "%s %s", $1, $2 )'
	},
	{
		'regexp'	=> qr/([+-])\s+([+-])/,
		'replacement'	=> 'sprintf( "%s %s", $1, $2 )'
	},
	{
		'regexp'	=> qr/\b\s+(\x24)\s+\b/,
		'replacement'	=> 'sprintf( " %s ", $1 )'
	},
	{
		'regexp'	=> qr/(\x24)\s+\b/,
		'replacement'	=> 'sprintf( "%s ", $1 )'
	},
	{
		'regexp'	=> qr/\b\s+(\x24)/,
		'replacement'	=> 'sprintf( " %s", $1 )'
	},
	{
		'regexp'	=> qr/\b\s+\b/,
		'replacement'	=> 'sprintf( " " )'
	},
	{
		'regexp'	=> qr/\s+/,
		'replacement'	=> ''
	},
];

$CLEAN = [
	{
		'regexp'	=> qr/\(\s*;\s*;\s*\)/,
		'replacement'	=> '(;;)'
	},
	{
		'regexp'	=> qr/;+\s*([};])/,
		'replacement'	=> '$1'
	},
];

# =========================================================================== #

sub minify {
	my ( $input, $opts ) = @_;
	
	if ( ref( $input ) and ref( $input ) ne 'SCALAR' ) {
		carp( 'First argument must be a scalar or scalarref!' );
		return undef;
	}
	
	my $javascript = ref( $input ) ? $input : \$input;
	
	if ( ref( $opts ) ne 'HASH' ) {
		carp( 'Second argument must be a hashref of options! Using defaults!' ) if ( $opts );
		$opts = { 'compress' => 'clean', 'copyright' => '' };
	}
	else {
		$opts->{'compress'} ||= 'clean';
		unless (
			grep(
				$_ eq $opts->{'compress'},
				( 'clean', 'minify', 'shrink', 'base62', 'obfuscate', 'best' )
			)
		) {
			$opts->{'compress'} = 'clean';
		}
		
		if ( $opts->{'compress'} eq 'minify' ) {
			$opts->{'compress'} = 'clean';
		}
		elsif ( $opts->{'compress'} eq 'base62' ) {
			$opts->{'compress'} = 'obfuscate';
		}
		
		$opts->{'copyright'}	= ( $opts->{'copyright'} and $opts->{'compress'} eq 'clean' ) ? ( '/* ' . $opts->{'copyright'} . ' */' ) : '';
	}
	
	my $data	= [];
	
	${$javascript} =~ s/~jmv_\d+~/ /;
	${$javascript} =~ s/~jmb_\d+~/ /;
	
	${$javascript} =~ s/\\\r?\n//gsm;
	${$javascript} =~ s/\r//gsm;
	
	local *_store = sub {
		my ( $regexp, $match ) = @_;
		
		my @match = $match =~ /$regexp->{'regexp'}/;
		my $replacement = $regexp->{'replacement'};
		
		for ( my $i = 0; $i < scalar( @match ); $i++ ) {
			my $rep_char	= $i + 1;
			my $rep_match	= $match[$i] || '';
			
			$rep_match =~ s/[\/\\]/\\$&/g;
			
			$replacement =~ s/\$$rep_char/q\/$rep_match\//g;
		}
		
		$match =~ s/[\/\\]/\\$&/g;
		
		$replacement =~ s/\$&/q\/$match\//g;
		
		my $store = eval( $replacement . ';' );
		
		return '' unless ( $store );
		
		my $ret = '~jmv_' . scalar( @{$data} ) . '~';
		
		if ( $store =~ /^([^'"])(\/.*)$/sm ) {
			$ret = $1 . $ret;
			$store = $2;
		}
	
		foreach my $regexp ( @$DATA ) {
			next unless ( $regexp->{'regexp'} and $store !~ /$regexp->{'regexp'}/ );
			$store =~ s/$regexp->{'regexp'}/_store( $regexp, $& )/egsm;
		}
		
		push( @{$data}, $store );
		
		return $ret;
	};
	
	foreach my $regexp ( @$DATA ) {
		next unless ( $regexp->{'regexp'} );
		${$javascript} =~ s/$regexp->{'regexp'}/_store( $regexp, $& )/egsm;
	}
	
	foreach my $regexp ( @$COMMENT ) {
		next unless ( $regexp->{'regexp'} );
		${$javascript} =~ s/$regexp->{'regexp'}/ /gsm;
	}
	
	foreach my $regexp ( @$WHITESPACE ) {
		next unless ( $regexp->{'regexp'} );
		${$javascript} =~ s/$regexp->{'regexp'}/_store( $regexp, $& )/egsm;
	}
	
	foreach my $regexp ( @$CLEAN ) {
		next unless ( $regexp->{'regexp'} );
		${$javascript} =~ s/$regexp->{'regexp'}/_store( $regexp, $& )/egsm;
	}
	
	while ( ${$javascript} =~ /~jmv_(\d+)~/ ) {
		${$javascript} =~ s/~jmv_(\d+)~/$data->[$1]/eg;
	}
	
	if ( $opts->{'compress'} ne 'clean' ) {
		my $block	= [];
		$data		= [];
		
		foreach my $regexp ( @$DATA ) {
			next unless ( $regexp->{'regexp'} );
			${$javascript} =~ s/$regexp->{'regexp'}/_store( $regexp, $& )/egsm;
		}
		
		local *_decode = sub {
			my $match = shift;
			
			while ( $match =~ /~jmb_\d+~/ ) {
				$match =~ s/~jmb_(\d+)~/$block->[$1]/eg;
			}
			
			return $match;
		};
		
		local *_encode = sub {
			my $match = shift;
			
			my ( $func, undef, $args ) = $match =~ /$BLOCK/;
			
			if ( $func ) {
				$match = _decode( $match );
				
				$args ||= '';
				
				my %block_vars = map { $_ => 1 } ( $match =~ /var\s+((?>[a-zA-Z0-9_\x24]+))/g ), split( /\s*,\s*/, $args );

				local *_encode52 = sub {
					my $c = shift;
					
					my $m = $c % 52;
					
					my $ret = $m > 25 ? chr( $m + 39 ) : chr( $m + 97 );
					
					if ( $c >= 52 ) {
						$ret = _encode52( int( $c / 52 ) ) . $ret;
					}
					
					return $ret;
				};
				
				my $cnt = 0;
				foreach my $block_var ( keys( %block_vars ) ) {
					if ( length( $block_var ) > 1 ) {
						my $short_id = _encode52( $cnt );
						while ( $match =~ /[^a-zA-Z0-9_\x24\.]\Q$short_id\E[^a-zA-Z0-9_\x24:]/ ) {
							$short_id = _encode52( $cnt++ );
						}
						$match =~ s/(?<![a-zA-Z0-9_\x24\.])\Q$block_var\E(?![a-zA-Z0-9_\x24:])/$short_id/g;
						$match =~ s/(?<![\{,a-zA-Z0-9_\x24\.])\Q$block_var\E:/sprintf( '%s:', $short_id )/eg;
					}
				}
			}
			
			my $ret = '~jmb_' . scalar( @{$block} ) . '~';
			
			push( @{$block}, $match );
			
			return $ret;
		};
		
		while( ${$javascript} =~ /$BLOCK/ ) {
			${$javascript} =~ s/$BLOCK/_encode( $& )/egsm;
		}
		
		${$javascript} = _decode( ${$javascript} );
		
		while ( ${$javascript} =~ /~jmv_(\d+)~/ ) {
			${$javascript} =~ s/~jmv_(\d+)~/$data->[$1]/egsm;
		}
	}
	else {
		${$javascript} = $opts->{'copyright'} . ${$javascript} if ( $opts->{'copyright'} );
	}
	
	if ( $opts->{'compress'} eq 'obfuscate' or $opts->{'compress'} eq 'best' ) {
		my $words = {};
		
		my @words = ${$javascript} =~ /$WORD/g;
		
		my $idx = 0;
		
		map {
			if ( exists( $words->{$_} ) ) {
				$words->{$_}->{'count'}++;
			}
			else {
				$words->{$_}->{'count'} = 1;
			}
		} @words;
		
		WORD: foreach my $word ( sort { $words->{$b}->{'count'} <=> $words->{$a}->{'count'} } keys( %{$words} ) ) {
			
			if ( exists( $words->{$word}->{'encoded'} ) and $words->{$word}->{'encoded'} eq $word ) {
				next WORD;
			}
			
			local *_encode62 = sub {
				my $c = shift;
				
				my $m = $c % 62;
				
				my $ret = $m > 35 ? chr( $m + 29 ) : $m > 9 ? chr( $m + 87 ) : $m;
				
				if ( $c >= 62 ) {
					$ret = _encode62( int( $c / 62 ) ) . $ret;
				}
				
				return $ret;
			};
			
			my $encoded = _encode62( $idx );
			
			if ( exists( $words->{$encoded} ) ) {
				my $next = 0;
				if ( exists( $words->{$encoded}->{'encoded'} ) ) {
					my %enc = %{$words->{$encoded}};
					$words->{$word}->{'encoded'} = $words->{$encoded}->{'encoded'};
					$words->{$word}->{'index'} = $words->{$encoded}->{'index'};
					$words->{$word}->{'minus'} = length( $word ) - length( $words->{$word}->{'encoded'} );
					$next = 1;
				}
				$words->{$encoded}->{'encoded'} = $encoded;
				$words->{$encoded}->{'index'} = $idx;
				$words->{$encoded}->{'minus'} = 0;
				$idx++;
				next WORD if ( $next );
				redo WORD;
			}
			
			$words->{$word}->{'encoded'} = $encoded;
			$words->{$word}->{'index'} = $idx;
			$words->{$word}->{'minus'} = length( $word ) - length( $encoded );
			
			$idx++;
		}
		
		my $packed_length = length( ${$javascript} );
		
		map {
			$packed_length -= ( $words->{$_}->{'count'} * $words->{$_}->{'minus'} );
		} keys( %{$words} );
		
		my $pc = scalar( keys( %{$words} ) );
		$packed_length += length( $pc );
		
		my $pa = $pc > 2 ? $pc < 62 ? $pc : 62 : 2;
		$packed_length += length( $pa );
		
		my $pk = join(
			'|',
			map {
				$words->{$_}->{'encoded'} ne $_ ? $_ : '';
			} sort { 
				$words->{$a}->{'index'} <=> $words->{$b}->{'index'}
			} keys( %{$words} )
		);
		$packed_length += length( $pk );
		
		my $pe = 'String';
		
		my $pr = 'c';
		
		if ( $pa > 10 ) {
			if ( $pa <= 36 ) {
				$pe = q~function(c){return c.toString(a)}~;
			}
			$pr = q~e(c)~;
		}
		if ( $pa > 36 ) {
			$pe = q~function(c){return(c<a?'':e(parseInt(c/a)))+((c=c%a)>35?String.fromCharCode(c+29):c.toString(36))}~;
		}
		$packed_length += length( $pe );
		$packed_length += length( $pr ) * 2;
		
		my $f_str = q~eval(function(p,a,c,k,e,r){e=%s;if(!''.replace(/^/,String)){while(c--)r[%s]=k[c]~;
		$f_str .= q~||%s;k=[function(e){return r[e]}];e=function(){return'\\\\w+'};c=1};while(c--)if(k[c])p=p.~;
		$f_str .= q~replace(new RegExp('\\\\b'+e(c)+'\\\\b','g'),k[c]);return p}('%s',%s,%s,'%s'.split('|'),0,{}))~;
		
		$packed_length += length( $f_str );
		$packed_length -= ( $f_str =~ s/%s/%s/g ) * 2;
		
		map {
			$packed_length -= length( $_ ) - 3;
		} ${$javascript} =~ s/((?>[\r\n]+))/$1/g;
		
		$packed_length += ${$javascript} =~ tr/\\\'/\\\'/;
		
		if ( $opts->{'compress'} eq 'obfuscate' or $packed_length <= length( ${$javascript} ) ) {
		
			${$javascript} =~ s/$WORD/sprintf( "%s", $words->{$1}->{'encoded'} )/eg;
			
			${$javascript}	=~ s/([\\'])/\\$1/g;
			${$javascript}	=~ s/[\r\n]+/\\n/g;
			
			my $pp = ${$javascript};
			
			${$javascript} = sprintf( $f_str, $pe, $pr, $pr, $pp, $pa, $pc, $pk );
		}
	}
}

1;

__END__

=head1 NAME

JavaScript::Packer - Perl version of Dean Edwards' Packer.js

=head1 VERSION

Version 0.02

=cut

=head1 SYNOPSIS

A JavaScript Compressor

This module does exactly the same as Dean Edwards' Packer.js

Take a look at http://dean.edwards.name/packer/

use JavaScript::Packer;

JavaScript::Packer::minify( $javascript, $opts );

First argument must be a scalarref of JavaScript-Code.
Second argument must be a hashref of options. Possible options are

=over 4

=item compress

Defines compression level. Possible values are 'clean', 'shrink',
'obfuscate' and 'best'.
Default value is 'clean'.
'best' uses 'shrink' or 'obfuscate' depending on which result is shorter. This is
because short scripts will grow if compression level is 'obfuscate'.

For backward compatibility 'minify' and 'base62' will still work.

=item copyright

You can add a copyright notice on top of the script. The copyright notice will
only be added if the compression value is 'clean'.

=back

=head1 AUTHOR

Merten Falk, C<< <nevesenin at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-javascript-packer at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JavaScript-Packer>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

perldoc JavaScript::Packer


=head1 COPYRIGHT & LICENSE

Copyright 2009 Merten Falk, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut
