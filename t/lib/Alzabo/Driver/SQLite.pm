package Alzabo::Driver::SQLite;

use strict;
use vars qw($VERSION);

use Alzabo::Driver;

use DBD::SQLite;
use DBI;

use File::Spec ();

use Params::Validate qw( :all );
Params::Validate::validation_options( on_fail => sub { Alzabo::Exception::Params->throw( error => join '', @_ ) } );

$VERSION = 1.0;

use base qw(Alzabo::Driver);

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;

    return bless {}, $class;
}

sub connect
{
    my $self = shift;
    my %p = @_;

    $self->{tran_count} = undef;

    return if $self->{dbh} && $self->{dbh}->ping;

    $self->disconnect if $self->{dbh};
    $self->{dbh} = $self->_make_dbh( %p, name => $self->{schema}->name );
}

sub supports_referential_integrity { 0 }

sub schemas
{
    my $self = shift;

    my %p = @_;

    %p = validate( @_, { dbname => { type => SCALAR | UNDEF,
                                     optional => 1 },
		       } );

    # really need to list the databases in the dbname directory, probably
    my @schemas = ( map { if ( defined )
                          {
                              /dbi:\w+:dbname=(\w+)/i;
                              $1 ? $1 : ();
                          }
                          else
                          {
                              ();
                          }
                        }
                    DBI->data_sources( $self->dbi_driver_name ) );

    return @schemas;

}

sub create_database
{
    my $self = shift;

    my $dbh;

    eval { $dbh = $self->_make_dbh( @_, name => $self->{schema}->name ); };

    my $e = $@;
    if ($e)
    {
	eval { $dbh->disconnect; };
	Alzabo::Exception::Driver->throw( error => $e ) if $e;
    }
    else
    {
	eval { $dbh->disconnect; };
	Alzabo::Exception::Driver->throw( error => $@ ) if $@;
    }
}

sub drop_database
{
    my $self = shift;

    my %p = @_;

    my $dbh = $self->_make_dbh( %p, name => $self->{schema}->name );

    eval { $dbh->disconnect; };  # clean up anything this process might have created

    # not great, but enough for testing right now
    my $filename = File::Spec -> catfile($p{dbname} || File::Spec->curdir, $self -> {schema} -> name);
    eval { unlink($filename); };

    Alzabo::Exception::Driver->throw( error => $@ ) if $@;
}

sub _make_dbh
{
    my $self = shift;

    my %p = @_;

    %p = validate( @_, { name => { type => SCALAR },
			 dbname => { type => SCALAR | UNDEF,
				   optional => 1 },
		       } );

    my $dsn = "dbi:SQLite:dbname=" . File::Spec -> catfile($p{dbname} || File::Spec->curdir, $p{name});

    my $dbh;
    eval
    {
	$dbh = DBI->connect( $dsn,
                             "", "",
			     { RaiseError => 1,
			       AutoCommit => 1,
			       PrintError => 0,
			     }
			   );
    };

    Alzabo::Exception::Driver->throw( error => $@ ) if $@;
    Alzabo::Exception::Driver->throw( error => "Unable to connect to database\n" ) unless $dbh;

    return $dbh;
}

sub next_sequence_number
{
    # This will cause an auto_increment column to go up (because we're
    # inserting a NULL into it).
    return undef;
}

sub get_last_id
{
    my $self = shift;
    return $self->{last_id} = $self->one_row( sql => "SELECT last_insert_rowid()" );
}

sub driver_id
{
    return 'SQLite';
}

sub dbi_driver_name
{
    return 'SQLite';
}

1;

__END__

=head1 NAME

Alzabo::Driver::SQLite - SQLite specific Alzabo driver subclass

=head1 SYNOPSIS

  use Alzabo::Driver::SQLite;

=head1 DESCRIPTION

This provides some SQLite specific implementations for the virtual
methods in Alzabo::Driver.

=head1 METHODS

=head2 connect, create_database, drop_database

These methods only accept the following parameters:

=over 4

=item * dbname

This may be a filesystem path designating a directory in which to 
place the database.

=item * name

This is the name of the schema.  This is also the name of the database 
appended to the C<dbname> filesystem path.

=back

=head2 schemas

This method only accepts the C<dbname> parameter described above.

=head2 get_last_id

Returns the last id created for a sequenced column.

=head1 BUGS

There are most likely several.  The current code passes all the tests 
except those involving the cache/syncing code and the reverse engineering.
This driver probably should not be used except for testing code that uses Alzabo.


=head1 AUTHOR

James Smith, <jsmith@cpan.org> (SQLite-specific code);

=cut
