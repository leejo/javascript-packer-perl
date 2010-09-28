package JavaScript::Packer;

use 5.008;
use warnings;
use strict;
use Carp;
use Regexp::RegGrp;

# =========================================================================== #

our $VERSION = '0.05_06';

our $PACKER_COMMENT     = '\/\*\s*JavaScript::Packer\s*(\w+)\s*\*\/';
our $COPYRIGHT_COMMENT  = '(\/\*(?>[^\*]|\*[^\/])*copyright(?>[^\*]|\*[^\/])*\*\/)';

our $MISSING_SEMICOLON  = qr/(\)\([^\(\)]*\))([^;\.\),])/;

our $SHRINK_VARS = {
    ENCODED_DATA    => qr~\x01(\d+)\x01~,
    BLOCK           => qr/(((catch|do|if|while|with|function)\b[^~{};]*(\(\s*[^{};]*\s*\))\s*)?(\{[^{}]*\}))/,  # function ( arg ) { ... }
    ENCODED_BLOCK   => qr/~#?(\d+)~/,
    CALLER          => qr/((?>[a-zA-Z0-9_\x24\.]+)\s*\([^\(\)]*\))(?=[,\)])/,                                   # do_something( arg1, arg2 ) as argument of another function call
    ENCODED_CALLER  => qr/~%(\d+)%~/,
    BRACKETS        => qr/\{[^{}]*\}|\[[^\[\]]*\]|\([^\(\)]*\)|~[^~]+~/,
    IDENTIFIER      => qr~[a-zA-Z_\x24][a-zA-Z_0-9\\x24]*~,
    SCOPED          => qr/~#(\d+)~/,
    VARS            => qr~\b(?:var|function)\s+((?>[a-zA-Z0-9_\x24]+))~,                                        # var x, funktion blah
    PREFIX          => qr~\x02~,
    SHRUNK          => qr~\x02\d+\b~
};

our $BASE62_VARS = {
    WORDS       => qr~(\b[0-9a-zA-Z]\b|(?>[a-zA-Z0-9_]{2,}))~,
    ENCODE10    => 'String',
    ENCODE36    => 'function(c){return c.toString(36)}',
    ENCODE62    => q~function(c){return(c<62?'':e(parseInt(c/62)))+((c=c%62)>35?String.fromCharCode(c+29):c.toString(36))}~,
    UNPACK      => q~eval(function(p,a,c,k,e,r){e=%s;if('0'.replace(0,e)==0){while(c--)r[e(c)]=k[c];k=[function(e){return r[e]||e}];e=function(){return'%s'};c=1};while(c--)if(k[c])p=p.replace(new RegExp('\\\\b'+e(c)+'\\\\b','g'),k[c]);return p}('%s',%s,%d,'%s'.split('|'),0,{}))~
};

our $DICTIONARY = {
    STRING1     => qr~"(?>(?:(?>[^"\\]+)|\\.|\\")*)"~,
    STRING2     => qr~'(?>(?:(?>[^'\\]+)|\\.|\\')*)'~,
    REGEXP      => qr~\/(\\[\/\\]|[^*\/])(\\.|[^\/\n\\])*\/[gim]*~,
    OPERATOR    => qr'return|typeof|[\[(\^=,{}:;&|!*?]',
    CONDITIONAL => qr~\/\*\@\w*|\w*\@\*\/|\/\/\@\w*|\@(?>\w+)~,
    COMMENT1    => qr~\/\/(\@)?([^\n]*)?\n~,
    COMMENT2    => qr~\/\*[^*]*\*+(?:[^\/][^*]*\*+)*\/~
};

our $DATA = [
    {
        regexp      => $DICTIONARY->{STRING1}
    },
    {
        regexp      => $DICTIONARY->{STRING2}
    },
    {
        regexp      => $DICTIONARY->{CONDITIONAL}
    },
    {
        regexp      => '(' . $DICTIONARY->{OPERATOR} . ')\s*(' . $DICTIONARY->{REGEXP} . ')',
        replacement => sub {
            return sprintf( "%s%s", $_[0]->{submatches}->[0], $_[0]->{submatches}->[1] );
        }
    }
];

our $COMMENTS = [
    {
        regexp      => ';;;[^\n]*\n',
        replacement => ''
    },
    {
        regexp      => $DICTIONARY->{COMMENT1} . '\s*(' . $DICTIONARY->{REGEXP} . ')?',
        replacement => sub { return sprintf( "\n%s", $_[0]->{submatches}->[2] ); }
    },
    {
        regexp      => '(' . $DICTIONARY->{COMMENT2} . ')\s*(' . $DICTIONARY->{REGEXP} . ')?',
        replacement => sub { return sprintf( " %s", $_[0]->{submatches}->[1] ); }
    }
];

our $CLEAN = [
    {
        regexp      => '\(\s*([^;)]*)\s*;\s*([^;)]*)\s*;\s*([^;)]*)\)',
        replacement => sub { return sprintf( "(%s;%s;%s)", @{$_[0]->{submatches}} ); }
    },
    {
        regexp      => 'throw[^};]+[};]'
    },
    {
        regexp      => ';+\s*([};])',
        replacement => sub { return $_[0]->{submatches}->[0]; }
    }
];

our $WHITESPACE = [
    {
        regexp      => '\/\/@[^\n]*\n'
    },
    {
        regexp      => '@\s+\b',
        replacement => '@ '
    },
    {
        regexp      => '\b\s+@',
        replacement => ' @'
    },
    {
        regexp      => '(\d)\s+(\.\s*[a-z\x24_\[(])',
        replacement => sub { return sprintf( "%s %s", @{$_[0]->{submatches}} ); }
    },
    {
        regexp      => '([+-])\s+([+-])',
        replacement => sub { return sprintf( "%s %s", @{$_[0]->{submatches}} ); }
    },
    {
        regexp      => '(?>\s+)(\x24)(?>\s+)',
        replacement => sub { return sprintf( " %s ", $_[0]->{submatches}->[0] ); }
    },
    {
        regexp      => '(\x24)(?>\s+)(?!=)',
        replacement => sub { return sprintf( "%s ", $_[0]->{submatches}->[0] ); }
    },
    {
        regexp      => '(?<!=)(?>\s+)(\x24)',
        replacement => sub { return sprintf( " %s", $_[0]->{submatches}->[0] ); }
    },
    {
        regexp      => '\b\s+\b',
        replacement => ' '
    },
    {
        regexp      => '\s+',
        replacement => ''
    }
];

 our $TRIM = [
    {
        regexp      => '(\d)(?:\|\d)+\|(\d)',
        replacement => sub { return sprintf( "%d-%d", $_[0]->{submatches}->[0] || 0, $_[0]->{submatches}->[1] || 0 ); }
    },
    {
        regexp      => '([a-z])(?:\|[a-z])+\|([a-z])',
        replacement => sub { return sprintf( "%s-%s", $_[0]->{submatches}->[0], $_[0]->{submatches}->[1] ); }
    },
    {
        regexp      => '([A-Z])(?:\|[A-Z])+\|([A-Z])',
        replacement => sub { return sprintf( "%s-%s", $_[0]->{submatches}->[0], $_[0]->{submatches}->[1] ); }
    },
    {
        regexp      => '\|',
        replacement => ''
    }
];

sub init {
    my $class   = shift;
    my $self    = {};

    map {
        my $what = $_;

        map {
            push(
                @{$self->{$what}->{reggrp_data}},
                {
                    regexp      => $_->{regexp},
                    replacement => $_->{replacement}
                }
            );
        } @$DATA;

        if ( $what eq 'comments' ) {
            map {
                push(
                    @{$self->{comments}->{reggrp_data}},
                    {
                        regexp      => $_->{regexp},
                        replacement => $_->{replacement}
                    }
                );
            } @$COMMENTS;
        }
        elsif ( $what eq 'clean' ) {
            map {
                push(
                    @{$self->{clean}->{reggrp_data}},
                    {
                        regexp      => $_->{regexp},
                        replacement => $_->{replacement}
                    }
                );
            } @$CLEAN;
        }
        elsif ( $what eq 'whitespace' ) {
            map {
                push(
                    @{$self->{whitespace}->{reggrp_data}},
                    {
                        regexp      => $_->{regexp},
                        replacement => $_->{replacement}
                    }
                );
            } @$WHITESPACE;
        }
    } ( 'comments', 'clean', 'whitespace', 'concat' );

    splice( @{$self->{comments}->{reggrp_data}}, 2, 1 );
    splice( @{$self->{whitespace}->{reggrp_data}}, 2, 1 );
    splice( @{$self->{concat}->{reggrp_data}}, 2, 1 );

    map {
        push(
            @{$self->{data_store}->{reggrp_data}},
            {
                regexp  => $_->{regexp},
                store   => sub { return sprintf( "%s", $_[0]->{match} ); }
            }
        );
    } @$DATA;

    for ( my $i = 1; $i >= 0; $i-- ) {
        unshift(
            @{$self->{concat}->{reggrp_data}},
            {
                regexp => $DATA->[$i]->{regexp}
            }
        );
        unshift(
            @{$self->{concat}->{reggrp_data}},
            {
                regexp      => '(' . $DATA->[$i]->{regexp} . ')((?:\+' . $DATA->[$i]->{regexp} . ')+)',
                replacement => sub {
                    my $submatches = $_[0]->{submatches};
                    my $ret = $submatches->[0];

                    my $next_str = '^\+(' . $DATA->[0]->{regexp} . '|' . $DATA->[1]->{regexp} . ')';

                    while ( my ( $next ) = $submatches->[1] =~ /$next_str/ ) {
                        chop( $ret );
                        $ret .= substr( $next, 1 );
                        $submatches->[1] =~ s/$next_str//;
                    }

                    return $ret;
                }
            }
        );
    }

    push(
        @{$self->{concat}->{reggrp_data}},
        {
            regexp      => $DATA->[3]->{regexp},
            replacement => $DATA->[3]->{replacement}
        }
    );

    map {
        push(
            @{$self->{trim}->{reggrp_data}},
            {
                regexp      => $_->{regexp},
                replacement => $_->{replacement}
            }
        );
    } @$TRIM;

    $self->{comments}->{reggrp_data}->[-2]->{replacement} = sub {
        my $submatches = $_[0]->{submatches};
        if ( $submatches->[0] eq '@' ) {
            # I don't like this, but
            # $self->{comments}->{reggrp}->exec( \$_[1]->[1] ); ...
            # will not work. It isn't initialized jet.
            # If someone has a better idea, please let me know
            $self->_process_wrapper( 'comments', \$submatches->[1] );
            $self->_process_wrapper( 'clean', \$submatches->[1] );
            $self->_process_wrapper( 'whitespace', \$submatches->[1] );

            return sprintf( "//%s%s\n%s", @{$submatches} );
        }
        return sprintf( "\n%s", $submatches->[2] );
    };

    $self->{comments}->{reggrp_data}->[-1]->{replacement} = sub {
        my $submatches = $_[0]->{submatches};
        if ( $submatches->[0] =~ /^\/\*\@(.*)\@\*\/$/sm ) {
            my $cmnt = $1;
            # Same as above
            $self->_process_wrapper( 'comments', \$cmnt );
            $self->_process_wrapper( 'clean', \$cmnt );
            $self->_process_wrapper( 'whitespace', \$cmnt );

            return sprintf( '/*@%s@*/ %s', $cmnt, $submatches->[1] );
        }
        return sprintf( " %s", $submatches->[1] );
    };

    $self->{data_store}->{reggrp_data}->[-1]->{replacement} = sub {
        return sprintf( "%s\x01%d\x01", $_[0]->{submatches}->[0], $_[0]->{store_index} );
    };

    $self->{data_store}->{reggrp_data}->[-1]->{store} = sub {
        return $_[0]->{submatches}->[1];
    };

    map {
        $self->{$_}->{reggrp} = Regexp::RegGrp->new( { reggrp => $self->{$_}->{reggrp_data} } );
    } ( 'comments', 'clean', 'whitespace', 'concat', 'trim' );

    $self->{data_store}->{reggrp} = Regexp::RegGrp->new(
        {
            reggrp          => $self->{data_store}->{reggrp_data},
            restore_pattern => $SHRINK_VARS->{ENCODED_DATA}
        }
    );

    $self->{block_data}     = [];
    $self->{caller_data}    = [];

    bless( $self, $class );

    return $self;
}

sub minify {
    my ( $self, $input, $opts );

    unless (
        ref( $_[0] ) and
        ref( $_[0] ) eq __PACKAGE__
    ) {
        $self = __PACKAGE__->init();

        shift( @_ ) unless ( ref( $_[0] ) );

        ( $input, $opts ) = @_;
    }
    else {
        ( $self, $input, $opts ) = @_;
    }

    if ( ref( $input ) ne 'SCALAR' ) {
        carp( 'First argument must be a scalarref!' );
        return undef;
    }

    my $javascript    = \'';
    my $cont    = 'void';

    if ( defined( wantarray ) ) {
        my $tmp_input = ref( $input ) ? ${$input} : $input;

        $javascript    = \$tmp_input;
        $cont        = 'scalar';
    }
    else {
        $javascript = ref( $input ) ? $input : \$input;
    }

    if ( ref( $opts ) ne 'HASH' ) {
        carp( 'Second argument must be a hashref of options! Using defaults!' ) if ( $opts );
        $opts = { compress => 'clean', copyright => '', no_compress_comment => 0, remove_copyright => 0 };
    }
    else {
        $opts->{compress} ||= 'clean';
        unless (
            grep(
                $_ eq $opts->{compress},
                ( 'clean', 'minify', 'shrink', 'base62', 'obfuscate', 'best' )
            )
        ) {
            $opts->{compress} = 'clean';
        }

        if ( $opts->{compress} eq 'minify' ) {
            $opts->{compress} = 'clean';
        }
        elsif ( $opts->{compress} eq 'base62' ) {
            $opts->{compress} = 'obfuscate';
        }

        $opts->{remove_copyright}       = $opts->{remove_copyright} ? 1 : 0;
        $opts->{no_compress_comment}    = $opts->{no_compress_comment} ? 1 : 0;
        $opts->{copyright}              = '' if ( ref( $opts->{copyright} ) );
        $opts->{copyright}              = ( $opts->{copyright} and $opts->{compress} eq 'clean' ) ? ( '/* ' . $opts->{copyright} . ' */' ) : '';
    }

    if ( not $opts->{remove_copyright} and not $opts->{copyright} and ${$javascript} =~ /$COPYRIGHT_COMMENT/ism ) {
        $opts->{copyright} = $1;
    }

    if ( not $opts->{no_compress_comment} and ${$javascript} =~ /$PACKER_COMMENT/ ) {
        my $compress = $1;
        if ( $compress eq '_no_compress_' ) {
            return ( $cont eq 'scalar' ) ? ${$javascript} : undef;
        }

        $opts->{compress} = grep( $compress, ( 'clean', 'shrink', 'obfuscate', 'best' ) ) ? $compress : $opts->{compress};
    }

    ${$javascript} =~ s/\r//gsm;
    ${$javascript} .= "\n";

    $self->{comments}->{reggrp}->exec( $javascript );
    $self->{clean}->{reggrp}->exec( $javascript );
    $self->{whitespace}->{reggrp}->exec( $javascript );
    $self->{concat}->{reggrp}->exec( $javascript );

    # Do something about the f**king missing semicolon.
    # It's rediculous to correct invalid javascript and I don't like it,
    # but some well known libraries e.g. prototype.js can't be compressed correctly
    # without it.

    $self->{data_store}->{reggrp}->exec( $javascript );

    while( ${$javascript} =~ /$SHRINK_VARS->{CALLER}/ ) {
        ${$javascript} =~ s/$SHRINK_VARS->{CALLER}/$self->_store_caller_data( $1 )/egsm;
    }

    ${$javascript} =~ s/$MISSING_SEMICOLON/sprintf( '%s;%s', $1, $2 )/egsm;

    $self->_restore_data( $javascript, 'caller_data', $SHRINK_VARS->{ENCODED_CALLER} );

    $self->{data_store}->{reggrp}->restore_stored( $javascript );

    # end of f**king missing semicolon fix

    if ( $opts->{compress} ne 'clean' ) {
        $self->{data_store}->{reggrp}->exec( $javascript );

        while( ${$javascript} =~ /$SHRINK_VARS->{BLOCK}/ ) {
            ${$javascript} =~ s/$SHRINK_VARS->{BLOCK}/$self->_store_block_data( $1 )/egsm;
        }

        $self->_restore_data( $javascript, 'block_data', $SHRINK_VARS->{ENCODED_BLOCK} );

        my %shrunk_vars = map { $_ => 1 } ( ${$javascript} =~ /$SHRINK_VARS->{SHRUNK}/g );

        my $cnt = 0;
        foreach my $shrunk_var ( keys( %shrunk_vars ) ) {
            my $short_id;
            do {
                $short_id = $self->_encode52( $cnt++ );
            } while (
                ${$javascript} =~ /[^a-zA-Z0-9_\\x24\.]\Q$short_id\E[^a-zA-Z0-9_\\x24:]/
            );

            ${$javascript} =~ s/$shrunk_var/$short_id/g;
        }

        $self->{data_store}->{reggrp}->restore_stored( $javascript );

        $self->{block_data} = [];
    }

    if ( $opts->{compress} eq 'obfuscate' or $opts->{compress} eq 'best' ) {
        my $words = {};

        my @words = ${$javascript} =~ /$BASE62_VARS->{WORDS}/g;

        my $idx = 0;

        map {
            if ( exists( $words->{$_} ) ) {
                $words->{$_}->{count}++;
            }
            else {
                $words->{$_}->{count} = 1;
            }
        } @words;

        WORD: foreach my $word ( sort { $words->{$b}->{count} <=> $words->{$a}->{count} } keys( %{$words} ) ) {

            if ( exists( $words->{$word}->{encoded} ) and $words->{$word}->{encoded} eq $word ) {
                next WORD;
            }

            my $encoded = $self->_encode62( $idx );

            if ( exists( $words->{$encoded} ) ) {
                my $next = 0;
                if ( exists( $words->{$encoded}->{encoded} ) ) {
                    $words->{$word}->{encoded} = $words->{$encoded}->{encoded};
                    $words->{$word}->{index} = $words->{$encoded}->{index};
                    $words->{$word}->{minus} = length( $word ) - length( $words->{$word}->{encoded} );
                    $next = 1;
                }
                $words->{$encoded}->{encoded} = $encoded;
                $words->{$encoded}->{index} = $idx;
                $words->{$encoded}->{minus} = 0;
                $idx++;
                next WORD if ( $next );
                redo WORD;
            }

            $words->{$word}->{encoded} = $encoded;
            $words->{$word}->{index} = $idx;
            $words->{$word}->{minus} = length( $word ) - length( $encoded );

            $idx++;
        }

        my $packed_length = length( ${$javascript} );

        my ( @pk, @pattern ) = ( (), () );

        foreach (
            sort {
                $words->{$a}->{index} <=> $words->{$b}->{index}
            } keys( %{$words} )
        ) {
            $packed_length -= ( $words->{$_}->{count} * $words->{$_}->{minus} );

            if ( $words->{$_}->{encoded} ne $_ ) {
                push( @pk, $_ );
                push( @pattern, $words->{$_}->{encoded} );
            }
            else {
                push( @pk, '' );
                push( @pattern, '' );
            }
        }

        my $size = scalar( @pattern );

        splice( @pattern, 62 ) if ( scalar( @pattern ) > 62 );

        my $pd = join( '|', @pattern );

        $self->{trim}->{reggrp}->exec( \$pd );

        unless ( $pd ) {
            $pd = '^$';
        }
        else {
            $pd = '[' . $pd . ']';

            if ( $size > 62 ) {
                $pd = '(' . $pd . '|';

                my $enc = $self->_encode62( $size );

                my ( $c ) = $enc =~ /(^.)/;
                my $ord = ord( $c );

                my $mul = length( $enc ) - 1;

                my $is62 = 0;

                if ( $ord >= 65 ) {
                    if ( $c eq 'Z' ) {
                        $mul += 1;
                        $is62 = 1;
                    }
                    else {
                        $pd .= '[0-9a';
                        if ( $ord > 97 ) {
                            $pd .= '-' . $c;
                        }
                        elsif ( $ord > 65 ) {
                            $pd .= '-zA-' . $c;
                        }
                        $pd .= ']';
                    }
                }
                elsif ( $ord == 57 ) {
                    $pd .= '0-9';
                }
                elsif ( $ord == 50 ) {
                    $pd .= '[12]';
                }
                elsif ( $ord == 49 ) {
                    $pd .= '1';
                }
                else {
                    $pd .= '[0-' . ( $ord - 48 ) . ']';
                }

                $pd .= '[0-9a-zA-Z]' . ( ( $mul > 1 ) ? '{' . $mul . '}' : '' );

                $mul-- if ( $is62 );

                if ( $mul > 1 ) {
                    for ( my $i = $mul; $i >= 2; $i-- ) {
                        $pd .= '|[0-9a-zA-Z]{' . $i . '}';
                    }
                }

                $pd .= ')';
            }
        }
        $packed_length += length( $pd );

        my $pk = join( '|', @pk );
        $pk =~ s/(?>\|+)$//;
        $packed_length += length( $pk );

        my $pc = length( $pk ) ? ( ( $pk =~ tr/|/|/ ) + 1 ) : 0;
        $packed_length += length( $pc );

        my $pa = '[]';
        $packed_length += length( $pa );

        my $pe = $BASE62_VARS->{'ENCODE' . ( $pc > 10 ? $pc > 36 ? 62 : 36 : 10 )};
        $packed_length += length( $pe );


        $packed_length += length( $BASE62_VARS->{UNPACK} );
        $packed_length -= ( $BASE62_VARS->{UNPACK} =~ s/(%s|%d)/$1/g ) * 2;

        map {
            $packed_length -= length( $_ ) - 3;
        } ${$javascript} =~ s/((?>[\r\n]+))/$1/g;

        $packed_length += ${$javascript} =~ tr/\\\'/\\\'/;

        if ( $opts->{compress} eq 'obfuscate' or $packed_length <= length( ${$javascript} ) ) {

            ${$javascript} =~ s/$BASE62_VARS->{WORDS}/sprintf( "%s", $words->{$1}->{encoded} )/eg;

            ${$javascript}    =~ s/([\\'])/\\$1/g;
            ${$javascript}    =~ s/[\r\n]+/\\n/g;

            my $pp = ${$javascript};

            ${$javascript} = sprintf( $BASE62_VARS->{UNPACK}, $pe, $pd, $pp, $pa, $pc, $pk );
        }

    }

    ${$javascript} = $opts->{copyright} . "\n" . ${$javascript} if ( $opts->{copyright} );

    return ${$javascript} if ( $cont eq 'scalar' );
}

sub _process_wrapper {
    my ( $self, $reg_name, $in ) = @_;

    $self->{$reg_name}->{reggrp}->exec( $in );
}

sub _restore_data {
    my ( $self, $string_ref, $data_name, $pattern ) = @_;

    while ( ${$string_ref} =~ /$pattern/ ) {
        ${$string_ref} =~ s/$pattern/$self->{$data_name}->[$1]/egsm;
    }
}

sub _store_caller_data {
    my ( $self, $match ) = @_;

    my $replacement = sprintf( "~%%%d%%~", scalar( @{$self->{caller_data}} ) );

    push( @{$self->{caller_data}}, $match );

    return $replacement;
}

sub _store_block_data {
    my ( $self, $match ) = @_;

    my ( undef, $prefix, $blocktype, $args, $block ) = $match =~ /$SHRINK_VARS->{BLOCK}/;

    $prefix ||= '';
    $blocktype ||= '';
    $args ||= '';
    my $replacement = '';
    if ( $blocktype eq 'function' ) {

        $self->_restore_data( \$block, 'block_data', $SHRINK_VARS->{SCOPED} );

        $args =~ s/\s*//g;

        $block = $args . $block;
        $prefix =~ s/$SHRINK_VARS->{BRACKETS}//;

        $args =~ s/^\(|\)$//g;

        while( $args =~ /$SHRINK_VARS->{CALLER}/ ) {
            $args =~ s/$SHRINK_VARS->{CALLER}//gsm;
        }

        my @vars = grep( $_, split( /\s*,\s*/, $args ) );
        my $do_shrink = grep( $_ eq '_no_shrink_', @vars ) ? 0 : 1;

        my %block_vars = ();
        if ( $do_shrink ) {
            %block_vars = map { $_ => 1 } ( $block =~ /$SHRINK_VARS->{VARS}/g ), grep( $_ ne '$super', @vars );
        }

        $self->_restore_data( \$block, 'block_data', $SHRINK_VARS->{ENCODED_BLOCK} );

        if ( $do_shrink ) {

            my $cnt = 0;
            foreach my $block_var ( keys( %block_vars ) ) {
                if ( length( $block_var ) ) {
                    while ( $block =~ /$SHRINK_VARS->{PREFIX}\Q$cnt\E\b/ ) {
                        $cnt++;
                    }

                    while ( $block =~ /[^a-zA-Z0-9_\\x24\.]\Q$block_var\E[^a-zA-Z0-9_\\x24:]/ ) {
                        $block =~ s/([^a-zA-Z0-9_\\x24\.])\Q$block_var\E([^a-zA-Z0-9_\\x24:])/sprintf( "%s\x02%d%s", $1, $cnt, $2 )/eg;
                    }

                    $block =~ s/([^{,a-zA-Z0-9_\\x24\.])\Q$block_var\E:/sprintf( "%s\x02%d:", $1, $cnt )/eg;

                    $cnt++;
                }
            }
        }
        $replacement = sprintf( "%s~%d~", $prefix, scalar( @{$self->{block_data}} ) );

        push( @{$self->{block_data}}, $block );
    }
    else {
        $replacement = sprintf( "~#%d~", scalar( @{$self->{block_data}} ) );

        push( @{$self->{block_data}}, $prefix . $block );
    }

    return $replacement;
};

sub _encode52 {
    my ( $self, $c ) = @_;

    my $m = $c % 52;

    my $ret = $m > 25 ? chr( $m + 39 ) : chr( $m + 97 );

    if ( $c >= 52 ) {
        $ret = $self->_encode52( int( $c / 52 ) ) . $ret;
    }

    $ret = substr( $ret, 1 ) . '0' if ( $ret =~ /^(do|if|in)$/ );

    return $ret;
};


sub _encode62 {
    my ( $self, $c ) = @_;

    my $m = $c % 62;

    my $ret = $m > 35 ? chr( $m + 29 ) : $m > 9 ? chr( $m + 87 ) : $m;

    if ( $c >= 62 ) {
        $ret = $self->_encode62( int( $c / 62 ) ) . $ret;
    }

    return $ret;
};

1;

__END__

=head1 NAME

JavaScript::Packer - Perl version of Dean Edwards' Packer.js

=head1 VERSION

Version 0.05_06

=head1 DESCRIPTION

A JavaScript Compressor

This module is an adaptation of Dean Edwards' Packer.js.

Additional information: http://dean.edwards.name/packer/

=head1 SYNOPSIS

    use JavaScript::Packer;

    my $packer = JavaScript::Packer->init();

    $packer->minify( $javascript, $opts );

To return a scalar without changing the input simply use (e.g. example 2):

    my $ret = $packer->minify( $javascript, $opts );

For backward compatibility it is still possible to call 'minify' as a function:

    JavaScript::Packer::minify( $javascript, $opts );

The first argument must be a scalarref of javascript-code.

Second argument must be a hashref of options. Possible options are:

=over 4

=item compress

Defines compression level. Possible values are 'clean', 'shrink', 'obfuscate'
and 'best'.
Default value is 'clean'.
'best' uses 'shrink' or 'obfuscate' depending on which result is shorter. This
is recommended because especially when compressing short scripts the result
will exceed the input if compression level is 'obfuscate'.

For backward compatibility 'minify' and 'base62' will still work.

=item copyright

You can add a copyright notice on top of the script.

=item remove_copyright

If there is a copyright notice in a comment it will only be removed if this
option is set to a true value. Otherwise the first comment that contains the
word "copyright" will be added at the top of the packed script. A copyright
comment will be overwritten by a copyright notice defined with the copyright
option.

=item no_compress_comment

If not set to a true value it is allowed to set a JavaScript comment that
prevents the input being packed or defines a compression level.

    /* JavaScript::Packer _no_compress_ */
    /* JavaScript::Packer shrink */

Is set by default.

=back

=head1 EXAMPLES

=over 4

=item Example 1

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

=item Example 2

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

=back

=head1 AUTHOR

Merten Falk, C<< <nevesenin at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-javascript-packer at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JavaScript-Packer>.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

perldoc JavaScript::Packer


=head1 COPYRIGHT & LICENSE

Copyright 2008-2010 Merten Falk, all rights reserved.

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
