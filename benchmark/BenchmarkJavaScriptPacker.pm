package BenchmarkJavaScriptPacker;

use strict;
use warnings;

use JavaScript::Packer;

my $init_map = sub {
    my $class   = shift;
    my $self    = {};

    bless( $self, $class );

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
        } @$JavaScript::Packer::DATA;

        if ( $what eq 'comments' ) {
            map {
                push(
                    @{$self->{comments}->{reggrp_data}},
                    {
                        regexp      => $_->{regexp},
                        replacement => $_->{replacement}
                    }
                );
            } @$JavaScript::Packer::COMMENTS;
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
            } @$JavaScript::Packer::CLEAN;
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
            } @$JavaScript::Packer::WHITESPACE;
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
    } @$JavaScript::Packer::DATA;

    for ( my $i = 1; $i >= 0; $i-- ) {
        unshift(
            @{$self->{concat}->{reggrp_data}},
            {
                regexp => $JavaScript::Packer::DATA->[$i]->{regexp}
            }
        );
        unshift(
            @{$self->{concat}->{reggrp_data}},
            {
                regexp      => '(' . $JavaScript::Packer::DATA->[$i]->{regexp} . ')((?:\+' . $JavaScript::Packer::DATA->[$i]->{regexp} . ')+)',
                replacement => sub {
                    my $submatches = $_[0]->{submatches};
                    my $ret = $submatches->[0];

                    my $next_str = '^\+(' . $JavaScript::Packer::DATA->[0]->{regexp} . '|' . $JavaScript::Packer::DATA->[1]->{regexp} . ')';

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
            regexp      => $JavaScript::Packer::DATA->[3]->{regexp},
            replacement => $JavaScript::Packer::DATA->[3]->{replacement}
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
    } @$JavaScript::Packer::TRIM;

    $self->{comments}->{reggrp_data}->[-2]->{replacement} = sub {
        my $submatches = $_[0]->{submatches};
        if ( $submatches->[0] eq '@' ) {
            $self->reggrp_comments()->exec( \$submatches->[1] );
            $self->reggrp_clean()->exec( \$submatches->[1] );
            $self->reggrp_whitespace()->exec( \$submatches->[1] );

            return sprintf( "//%s%s\n%s", @{$submatches} );
        }
        return sprintf( "\n%s", $submatches->[2] );
    };

    $self->{comments}->{reggrp_data}->[-1]->{replacement} = sub {
        my $submatches = $_[0]->{submatches};
        if ( $submatches->[0] =~ /^\/\*\@(.*)\@\*\/$/sm ) {
            my $cmnt = $1;

            $self->reggrp_comments()->exec( \$cmnt );
            $self->reggrp_clean()->exec( \$cmnt );
            $self->reggrp_whitespace()->exec( \$cmnt );

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
        $self->{ '_reggrp_' . $_ } = Regexp::RegGrp->new( { reggrp => $self->{$_}->{reggrp_data} } );
    } @JavaScript::Packer::REGGRPS;

    $self->{block_data} = [];

    return $self;
};

my $init_foreach = sub {
    my $class   = shift;
    my $self    = {};

    bless( $self, $class );

    @{$self->{clean}->{reggrp_data}}        = ( @$JavaScript::Packer::DATA, @$JavaScript::Packer::CLEAN );
    @{$self->{whitespace}->{reggrp_data}}   = ( @$JavaScript::Packer::DATA[ 0, 1, 3 ], @$JavaScript::Packer::WHITESPACE );
    @{$self->{trim}->{reggrp_data}}         = @$JavaScript::Packer::TRIM;

    @{$self->{data_store}->{reggrp_data}}   = map {
        {
            regexp  => $_->{regexp},
            store   => sub { return sprintf( "%s", $_[0]->{match} ); }
        }
    } @$JavaScript::Packer::DATA;

    $self->{data_store}->{reggrp_data}->[-1]->{replacement} = sub {
        return sprintf( "%s\x01%d\x01", $_[0]->{submatches}->[0], $_[0]->{store_index} );
    };

    $self->{data_store}->{reggrp_data}->[-1]->{store} = sub {
        return $_[0]->{submatches}->[1];
    };

    @{$self->{concat}->{reggrp_data}}       = map {
        my $data = $_;
        {
            regexp      => '(' . $data->{regexp} . ')((?:\+' . $data->{regexp} . ')+)',
            replacement => sub {
                my $submatches = $_[0]->{submatches};
                my $ret = $submatches->[0];

                my $next_str = '^\+(' . $data->{regexp} . ')';

                while ( my ( $next ) = $submatches->[1] =~ /$next_str/ ) {
                    chop( $ret );
                    $ret .= substr( $next, 1 );
                    $submatches->[1] =~ s/$next_str//;
                }

                return $ret;
            }
        };
    } @$JavaScript::Packer::DATA[ 0, 1 ];

    @{$self->{comments}->{reggrp_data}}     = ( @$JavaScript::Packer::DATA[ 0, 1, 3 ], @$JavaScript::Packer::COMMENTS );

    $self->{comments}->{reggrp_data}->[-2]->{replacement} = sub {
        my $submatches = $_[0]->{submatches};
        if ( $submatches->[0] eq '@' ) {
            $self->reggrp_comments()->exec( \$submatches->[1] );
            $self->reggrp_clean()->exec( \$submatches->[1] );
            $self->reggrp_whitespace()->exec( \$submatches->[1] );

            return sprintf( "//%s%s\n%s", @{$submatches} );
        }
        return sprintf( "\n%s", $submatches->[2] );
    };

    $self->{comments}->{reggrp_data}->[-1]->{replacement} = sub {
        my $submatches = $_[0]->{submatches};
        if ( $submatches->[0] =~ /^\/\*\@(.*)\@\*\/$/sm ) {
            my $cmnt = $1;

            $self->reggrp_comments()->exec( \$cmnt );
            $self->reggrp_clean()->exec( \$cmnt );
            $self->reggrp_whitespace()->exec( \$cmnt );

            return sprintf( '/*@%s@*/ %s', $cmnt, $submatches->[1] );
        }
        return sprintf( " %s", $submatches->[1] );
    };

    foreach ( @JavaScript::Packer::REGGRPS ) {
        $self->{ '_reggrp_' . $_ } = Regexp::RegGrp->new( { reggrp => $self->{$_}->{reggrp_data} } );
    }

    $self->{block_data} = [];

    return $self;
};

sub set_foreach {
    no strict 'refs';
    no warnings 'redefine';

    *{'JavaScript::Packer::init'} = $init_foreach;
}

sub set_map {
    no strict 'refs';
    no warnings 'redefine';

    *{'JavaScript::Packer::init'} = $init_map;
}

1;