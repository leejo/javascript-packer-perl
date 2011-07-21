package BenchmarkRegGrp;

use strict;
use warnings;

use Regexp::RegGrp;

my $process_map = sub {
    my ( $self, $in_ref ) = @_;

    my %match_hash  = %{$in_ref->{match_hash}};
    my $opts        = $in_ref->{opts};

    my $match_key   = ( keys( %match_hash ) )[0];
    my ( $midx )    = $match_key =~ /^_(\d+)$/;
    my $match       = $match_hash{$match_key};

    my $reggrp = $self->reggrp_by_idx( $midx );

    my @submatches = $match =~ $reggrp->regexp();
    map {
        $_ .= '';
    } @submatches;

    my $ret = $match;

    my $replacement = $reggrp->replacement();

    if (
        defined( $replacement ) and
        not ref( $replacement )
    ) {
        $ret = $replacement;
    }
    elsif ( ref( $replacement ) eq 'CODE' ) {
        $ret = $replacement->(
            {
                match       => $match,
                submatches  => \@submatches,
                opts        => $opts,
                store_index => $self->store_data_count()
            }
        );
    }

    my $store = $reggrp->store();

    if ( $store ) {
        my $tmp_match = $match;
        if ( not ref( $store ) ) {
            $tmp_match = $store;
        }
        elsif ( ref( $store ) eq 'CODE' ) {
            $tmp_match = $store->(
                {
                    match       => $match,
                    submatches  => \@submatches,
                    opts        => $opts
                }
            );
        }

        $self->store_data_add( $tmp_match );
    }

    return $ret;
};

my $process_foreach = sub {
    my ( $self, $in_ref ) = @_;

    my %match_hash  = %{$in_ref->{match_hash}};
    my $opts        = $in_ref->{opts};

    my $match_key   = ( keys( %match_hash ) )[0];
    my ( $midx )    = $match_key =~ /^_(\d+)$/;
    my $match       = $match_hash{$match_key};

    my $reggrp = $self->reggrp_by_idx( $midx );

    my @submatches = $match =~ $reggrp->regexp();
    foreach ( @submatches ) {
        $_ .= '';
    }

    my $ret = $match;

    my $replacement = $reggrp->replacement();

    if (
        defined( $replacement ) and
        not ref( $replacement )
    ) {
        $ret = $replacement;
    }
    elsif ( ref( $replacement ) eq 'CODE' ) {
        $ret = $replacement->(
            {
                match       => $match,
                submatches  => \@submatches,
                opts        => $opts,
                store_index => $self->store_data_count()
            }
        );
    }

    my $store = $reggrp->store();

    if ( $store ) {
        my $tmp_match = $match;
        if ( not ref( $store ) ) {
            $tmp_match = $store;
        }
        elsif ( ref( $store ) eq 'CODE' ) {
            $tmp_match = $store->(
                {
                    match       => $match,
                    submatches  => \@submatches,
                    opts        => $opts
                }
            );
        }

        $self->store_data_add( $tmp_match );
    }

    return $ret;
};

my $new_map = sub {
    my ( $class, $in_ref )  = @_;
    my $self                = {};

    bless( $self, $class );

    if ( ref( $in_ref ) ne 'HASH' ) {
        carp( 'First argument must be a hashref!' );
        return;
    }

    unless ( exists( $in_ref->{reggrp} ) ) {
            carp( 'Key "reggrp" does not exist in input hashref!' );
            return;
    }

    if ( ref( $in_ref->{reggrp} ) ne 'ARRAY' ) {
        carp( 'Value for key "reggrp" must be an arrayref!' );
        return;
    }

    if (
        ref( $in_ref->{restore_pattern} ) and
        ref( $in_ref->{restore_pattern} ) ne 'Regexp'
    ) {
        carp( 'Value for key "restore_pattern" must be a scalar or regexp!' );
        return;
    }

    my $no = 0;

    map {
        $no++;

        my $reggrp_data = Regexp::RegGrp::Data->new(
            {
                regexp          => $_->{regexp},
                replacement     => $_->{replacement},
                store           => $_->{store},
                modifier        => $_->{modifier},
                restore_pattern => $in_ref->{restore_pattern}
            }
        );

        unless ( $reggrp_data ) {
            carp( 'RegGrp No ' . $no . ' in arrayref is malformed!' );
            return;
        }

        $self->reggrp_add( $reggrp_data );
    } @{ $in_ref->{reggrp} };

    my $restore_pattern         = $in_ref->{restore_pattern} || qr~\x01(\d+)\x01~;
    $self->{_restore_pattern}   = qr/$restore_pattern/;

    my $offset  = 1;
    my $midx    = 0;

    # In perl versions < 5.10 hash %+ doesn't exist, so we have to initialize it
    $self->{_re_str} = ( ( $] < 5.010000 ) ? '(?{ %+ = (); })' : '' ) . join(
        '|',
        map {
            my $re = $_->regexp();
            # Count backref brackets
            $re =~ s/${\(Regexp::RegGrp::ESCAPE_CHARS)}//g;
            $re =~ s/${\(Regexp::RegGrp::ESCAPE_BRACKETS)}//g;
            my @nparen = $re =~ /${\(Regexp::RegGrp::BRACKETS)}/g;

            $re = $_->regexp();

            my $backref_pattern = '\\g{%d}';

            if ( $] < 5.010000 ) {
                $backref_pattern = '\\%d';
            }

            $re =~ s/${\(Regexp::RegGrp::BACK_REF)}/sprintf( $backref_pattern, $offset + ( $1 || $2 ) )/eg;

            my $ret;

            if ( $] < 5.010000 ) {
                # In perl versions < 5.10 we need to fill %+ hash manually
                # perl 5.8 doesn't reset the %+ hash correctly if there are zero-length submatches
                # so this is also done here
                $ret = '(' . $re . ')' . '(?{ %+ = ( \'_' . $midx++ . '\' => $^N ); })';
            }
            else {
                $ret = '(?\'_' . $midx++ . '\'' . $re . ')';
            }

            $offset += scalar( @nparen ) + 1;

            $ret;

        } $self->reggrp_array()
    );

    return $self;
};

my $new_foreach = sub {
    my ( $class, $in_ref )  = @_;
    my $self                = {};

    bless( $self, $class );

    if ( ref( $in_ref ) ne 'HASH' ) {
        carp( 'First argument must be a hashref!' );
        return;
    }

    unless ( exists( $in_ref->{reggrp} ) ) {
            carp( 'Key "reggrp" does not exist in input hashref!' );
            return;
    }

    if ( ref( $in_ref->{reggrp} ) ne 'ARRAY' ) {
        carp( 'Value for key "reggrp" must be an arrayref!' );
        return;
    }

    if (
        ref( $in_ref->{restore_pattern} ) and
        ref( $in_ref->{restore_pattern} ) ne 'Regexp'
    ) {
        carp( 'Value for key "restore_pattern" must be a scalar or regexp!' );
        return;
    }

    my $no = 0;

    foreach ( @{ $in_ref->{reggrp} } ) {
        $no++;

        my $reggrp_data = Regexp::RegGrp::Data->new(
            {
                regexp          => $_->{regexp},
                replacement     => $_->{replacement},
                store           => $_->{store},
                modifier        => $_->{modifier},
                restore_pattern => $in_ref->{restore_pattern}
            }
        );

        unless ( $reggrp_data ) {
            carp( 'RegGrp No ' . $no . ' in arrayref is malformed!' );
            return;
        }

        $self->reggrp_add( $reggrp_data );
    }

    my $restore_pattern         = $in_ref->{restore_pattern} || qr~\x01(\d+)\x01~;
    $self->{_restore_pattern}   = qr/$restore_pattern/;

    my $offset  = 1;
    my $midx    = 0;

    # In perl versions < 5.10 hash %+ doesn't exist, so we have to initialize it
    $self->{_re_str} = ( ( $] < 5.010000 ) ? '(?{ %+ = (); })' : '' ) . join(
        '|',
        map {
            my $re = $_->regexp();
            # Count backref brackets
            $re =~ s/${\(Regexp::RegGrp::ESCAPE_CHARS)}//g;
            $re =~ s/${\(Regexp::RegGrp::ESCAPE_BRACKETS)}//g;
            my @nparen = $re =~ /${\(Regexp::RegGrp::BRACKETS)}/g;

            $re = $_->regexp();

            my $backref_pattern = '\\g{%d}';

            if ( $] < 5.010000 ) {
                $backref_pattern = '\\%d';
            }

            $re =~ s/${\(Regexp::RegGrp::BACK_REF)}/sprintf( $backref_pattern, $offset + ( $1 || $2 ) )/eg;

            my $ret;

            if ( $] < 5.010000 ) {
                # In perl versions < 5.10 we need to fill %+ hash manually
                # perl 5.8 doesn't reset the %+ hash correctly if there are zero-length submatches
                # so this is also done here
                $ret = '(' . $re . ')' . '(?{ %+ = ( \'_' . $midx++ . '\' => $^N ); })';
            }
            else {
                $ret = '(?\'_' . $midx++ . '\'' . $re . ')';
            }

            $offset += scalar( @nparen ) + 1;

            $ret;

        } $self->reggrp_array()
    );

    return $self;
};

sub set_foreach {
    no strict 'refs';
    no warnings 'redefine';

    *{'Regexp::RegGrp::new'} = $new_foreach;
    *{'Regexp::RegGrp::_process'} = $process_foreach;
}

sub set_map {
    no strict 'refs';
    no warnings 'redefine';

    *{'Regexp::RegGrp::new'} = $new_map;
    *{'Regexp::RegGrp::_process'} = $process_map;
}

'uuuhhhuuuuhuuu, it\'s so true!!!!!';