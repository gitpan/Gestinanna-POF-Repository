package Alzabo::SQLMaker::SQLite;

use strict;
use vars qw($VERSION $AUTOLOAD @EXPORT_OK %EXPORT_TAGS);

use Alzabo::Exceptions;

use Alzabo::SQLMaker;
use base qw(Alzabo::SQLMaker);

use Params::Validate qw( :all );
Params::Validate::validation_options( on_fail => sub { Alzabo::Exception::Params->throw( error => join '', @_ ) } );

$VERSION = 2.0;

my $MADE_FUNCTIONS;

sub import
{
    _make_functions() unless $MADE_FUNCTIONS;

    # used to export function functions
    require Exporter;
    *_import = \&Exporter::import;

    goto &_import;
}

sub _make_functions
{
    local *make_function = \&Alzabo::SQLMaker::make_function;

    foreach ( [ RANDOM => [ 'math' ] ],
	    )
    {
	make_function( function => $_->[0],
		      min => 0,
		      max => 1,
		      quote => [0],
		      groups => $_->[1]
		    );
    }

    foreach ( [ ROUND => [0,0], [ 'math' ] ],

	      [ IFNULL => [0,1], [ 'control' ] ],
	      [ NULLIF => [0,0], [ 'control' ] ],
	    )
    {
	make_function( function => $_->[0],
		      min => 2,
		      max => 2,
		      quote => $_->[1],
		      groups => $_->[2],
		    );
    }

# # comparison (but not functions):
# GLOB
# LIKE

    foreach ( [ ABS  => [0], [ 'math' ] ], 

	      [ LENGTH  => [1], [ 'string' ] ],
	      [ LOWER  => [1], [ 'string' ] ],
	      [ UPPER  => [1], [ 'string' ] ],

              [ COALESCE => [0], [ 'aggregate' ] ],
	      [ COUNT  => [0], [ 'aggregate', 'common' ] ],
	      [ AVG  => [0], [ 'aggregate', 'common' ] ],
	      [ MIN  => [0], [ 'aggregate', 'common' ] ],
	      [ MAX  => [0], [ 'aggregate', 'common' ] ],
	      [ SUM  => [0], [ 'aggregate', 'common' ] ],
	    )
    {
	make_function( function => $_->[0],
		       min => 1,
		       max => 1,
		       quote => $_->[1],
		       groups => $_->[2],
		     );
    }

    $MADE_FUNCTIONS = 1;
}

sub init
{
    1;
}

sub _subselect
{
    Alzabo::Exception::SQL->throw( error => "SQLite does not support subselects" );
}

sub limit
{
    my $self = shift;
    my ($max, $offset) = @_;

    $self->_assert_last_op( qw( from function where and or condition order_by group_by ) );

    if ($offset)
    {
	$self->{sql} .= " LIMIT $offset, $max";
    }
    else
    {
	$self->{sql} .= " LIMIT $max";
    }

    $self->{last_op} = 'limit';

    return $self;
}

sub get_limit
{
    return undef;
}

sub sqlmaker_id
{
    return 'SQLite';
}

1;

__END__

=head1 NAME

Alzabo::SQLMaker::SQLite - Alzabo SQL making class for SQLite

=head1 SYNOPSIS

  use Alzabo::SQLMaker;

  my $sql = Alzabo::SQLMaker->new( sql => 'SQLite' );

=head1 DESCRIPTION

SQLite-specific SQL creation.  It is worth noting that SQLite does not
allow non-constant subselects.  Any attempt to use a subselect (by passing an
C<Alzabo::SQMaker> object in as parameter to a method) will result in
an L<C<Alzabo::Exception::SQL>|Alzabo::Exceptions> error.

=head1 METHODS

Almost all of the functionality inherited from Alzabo::SQLMaker is
used as is.  The only overridden methods are C<limit> and
C<get_limit>, as SQLite does allow for a C<LIMIT> clause in its SQL.

=head1 EXPORTED SQL FUNCTIONS

SQL may be imported by name or by tags.  They take arguments as
documented in the SQLite documentation.  The
functions (organized by tag) are:

=head2 :math

 RANDOM
 ROUND
 ABS

=head2 :string

 LENGTH
 LOWER
 UPPER


=head2 :aggregate

These are functions which operate on an aggregate set of values all at
once.

 COALESCE
 COUNT
 AVG
 MIN
 MAX
 SUM

=head2 :control

These are flow control functions:

 IFNULL
 NULLIF

=head2 :common

These are functions from other groups that are most commonly used.

 COUNT
 AVG
 MIN
 MAX
 SUM

=head1 AUTHOR

Dave Rolsky, <dave@urth.org> 
with SQLite-specific modifications by James Smith, <jsmith@cpan.org>.

=cut
