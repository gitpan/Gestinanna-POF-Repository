package Alzabo::RDBMSRules::SQLite;

use strict;
use vars qw($VERSION);

use Alzabo::RDBMSRules;

use base qw(Alzabo::RDBMSRules);

$VERSION = 1.0;

our $TEMPORARY_TABLE_SUFFIX = '_TEMP';

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;

    return bless {}, $class;
}

sub validate_schema_name
{
    my $self = shift;
    my $name = shift->name;

    Alzabo::Exception::RDBMSRules->throw( error => "Schema name must be at least one character long" )
	unless length $name;

    # These are characters that are illegal in a dir name.  I'm trying
    # to accomodate both Win32 and UNIX here.
    foreach my $c ( qw( : \ / ) )
    {
	Alzabo::Exception::RDBMSRules->throw( error => "Schema name contains an illegal character ($c)" )
	    if index($name, $c) != -1;
    }
}

sub validate_table_name
{
    my $self = shift;
    my $name = shift->name;

    Alzabo::Exception::RDBMSRules->throw( error => "Table name must be at least one character long" )
	unless length $name;

    Alzabo::Exception::RDBMSRules->throw( error => "Table names that begin with `sqlite_' are reserved for use by the engine" )
        if $name =~ m{^sqlite_};

    Alzabo::Exception::RDBMSRules->throw( error => "Table names that end with `$TEMPORARY_TABLE_SUFFIX' are reserved for use by Alzabo" )
        if $name =~ m{$TEMPORARY_TABLE_SUFFIX$};

    #foreach my $c ( qw( : \ / ) )
    #{
#	Alzabo::Exception::RDBMSRules->throw( error => "Table name contains an illegal character ($c)" )
#	    if index($name, $c) != -1;
#    }
    # following aren't really required
    Alzabo::Exception::RDBMSRules->throw( error => "Table name is too long.  Names must be 64 characters or less." )
	if length $name >= 64;
    Alzabo::Exception::RDBMSRules->throw( error => "Table name must only contain alphanumerics or underscore(_)." )
	if $name =~ /\W/;
}

sub validate_column_name
{
    my $self = shift;
    my $name = shift->name;

    Alzabo::Exception::RDBMSRules->throw( error => "Column name must be at least one character long" )
	unless length $name;
    Alzabo::Exception::RDBMSRules->throw( error => 'Name is too long.  Names must be 64 characters or less.' )
	if length $name >= 64;
    Alzabo::Exception::RDBMSRules->throw( error =>
					  'Name contains characters that are not alphanumeric or the dollar sign ($).' )
	if $name =~ /[^\w\$]/;
    Alzabo::Exception::RDBMSRules->throw( error =>
					  'Name contains only digits.  Names must contain at least one alpha character.' )
	unless $name =~ /[^\W\d]/;
}

sub validate_column_type
{
    my $self = shift;
    my $type = shift;
    my $table = shift;

    return 'INTEGER' if uc $type eq 'INT';

    my %sql_types = (
        SQL_BIT => 'BIT',
        SQL_BIT_VARYING => 'BIT VARYING',
        SQL_BOOLEAN => 'BOOLEAN',
        SQL_CHAR => 'CHAR',
        SQL_CHARACTER => 'CHARACTER',
        SQL_CHARACTER_VARYING => 'CHARACTER VARYING',
        SQL_VARCHAR => 'VARCHAR',
        SQL_DATE => 'DATE',
        SQL_DOUBLE_PRECISION => 'DOUBLE PRECISION',
        SQL_INTEGER => 'INTEGER',
        SQL_INTERVAL => 'INTERVAL',
        SQL_NUMERIC => 'NUMERIC',
        SQL_DECIMAL => 'DECIMAL',
        SQL_REAL => 'REAL',
        SQL_SMALLINT => 'SMALLINT',
        SQL_TIME_WITH_TZ => 'TIMETZ',
        SQL_TIME_WITHOUT_TZ => 'TIME',
        SQL_TIMESTAMP_WITH_TZ => 'TIMESTAMPTZ',
        SQL_TIMESTAMP_WITHOUT_TZ => 'TIMESTAMP',

        # SQL99 - String (BLOB), Character (CBLOB)
        SQL_BLOB => 'BLOB',
        SQL_CBLOB => 'TEXT',
    );

    $type = $sql_types{uc $type} if exists $sql_types{uc $type};

    # Columns which take no modifiers.
    my %simple_types = map {$_ => 1} ( qw( 
        BIT
        BOOLEAN
        CHAR
        CHARACTER
        VARCHAR
        DATE
        DOUBLE
        INTEGER
        INTERVAL
        NUMERIC
        DECIMAL
        REAL
        SMALLINT
        TIMETZ
        TIME
        TIMESTAMPTZ
        TIMESTAMP
        BLOB
        TEXT
    ) );

    return uc $type if $simple_types{uc $type};

    return 'DOUBLE' if $type =~ /DOUBLE\s+PRECISION/i;

    return $type if $type =~ /BIT\s+VARYING/;

    return $type if $type =~ /CHARACTER\s+VARYING/;

    return uc $type if $type;

    Alzabo::Exception::RDBMSnules->throw( error => "Unrecognized type: $type" );
}

sub validate_column_length
{
    my $self = shift;
    my $column = shift;

    # integer column
    if ( $column->type =~ /\A(?:(?:(?:TINY|SMALL|MEDIUM|BIG)?INT)|INTEGER)/i )
    {
	Alzabo::Exception::RDBMSRules->throw( error => "Max display value is too long.  Maximum allowed value is 255." )
	    if defined $column->length && $column->length > 255;

	Alzabo::Exception::RDBMSRules->throw( error => $column->type . " columns cannot have a precision." )
	    if defined $column->precision;
	return;
    }

    if ( $column->type =~ /\A(?:FLOAT|DOUBLE(?:\s+PRECISION)?|REAL)/i )
    {
	if (defined $column->length)
	{
	    Alzabo::Exception::RDBMSRules->throw( error => "Max display value is too long.  Maximum allowed value is 255." )
		if $column->length > 255;

	    Alzabo::Exception::RDBMSRules->throw( error => "Max display value specified without floating point precision." )
		unless defined $column->precision;

	    Alzabo::Exception::RDBMSRules->throw( error =>
						  "Floating point precision is too high.  The maximum value is " .
						  "30 or the maximum display size - 2, whichever is smaller." )
		if $column->precision > 30 || $column->precision > ($column->length - $column->precision);
	}

	return;
    }

    if ( $column->type =~ /\A(?:DECIMAL|NUMERIC)\z/i )
    {
	Alzabo::Exception::RDBMSRules->throw( error => "Max display value is too long.  Maximum allowed value is 255." )
	    if defined $column->length && $column->length > 255;
	Alzabo::Exception::RDBMSRules->throw( error =>
					      "Floating point precision is too high.  The maximum value is " .
					      "30 or the maximum display size - 2, whichever is smaller." )
	    if defined $column->precision && ($column->precision > 30 || $column->precision > ($column->length - 2) );
	return;
    }

    if ( uc $column->type eq 'TIMESTAMP' )
    {
	Alzabo::Exception::RDBMSRules->throw( error => "Max display value is too long.  Maximum allowed value is 14." )
	    if defined $column->length && $column->length > 14;
	Alzabo::Exception::RDBMSRules->throw( error => $column->type . " columns cannot have a precision." )
	    if defined $column->precision;
	return;
    }

    if ( $column->type =~ /\A(?:(?:NATIONAL\s+)?VAR)?CHAR/i )
    {
	Alzabo::Exception::RDBMSRules->throw( error => "CHAR and VARCHAR columns must have a length provided." )
	    unless defined $column->length && $column->length > 0;
	Alzabo::Exception::RDBMSRules->throw( error => "Max display value is too long.  Maximum allowed value is 255." )
	    if $column->length > 255;
	Alzabo::Exception::RDBMSRules->throw( error => $column->type . " columns cannot have a precision." )
	    if defined $column->precision;
	return;
    }

    if ( uc $column->type eq 'YEAR' )
    {
	Alzabo::Exception::RDBMSRules->throw( error => "Valid values for the length specification are 2 or 4." )
	    if defined $column->length && ($column->length != 2 && $column->length != 4);
	return;
    }

    Alzabo::Exception::RDBMSRules->throw( error => $column->type . " columns cannot have a length or precision." )
	if defined $column->length || defined $column->precision;
}

sub validate_column_attribute
{
    my $self = shift;
    my %p = @_;

    my $column = $p{column};
    my $a = uc $p{attribute};
    $a =~ s/\A\s//;
    $a =~ s/\s\z//;

    if ( $a eq 'UNSIGNED' || $a eq 'ZEROFILL' )
    {
	Alzabo::Exception::RDBMSRules->throw( error => "$a attribute can only be applied to numeric columns" )
	    unless $column->is_numeric;
	return;
    }

    if ( $a eq 'AUTO_INCREMENT' )
    {
	Alzabo::Exception::RDBMSRules->throw( error => "$a attribute can only be applied to integer columns" )
	    unless $column->is_integer;
	return;
    }

    if ($a eq 'BINARY')
    {
	Alzabo::Exception::RDBMSRules->throw( error => "$a attribute can only be applied to character columns" )
	    unless $column->is_character;
	return;
    }
    return if $a =~ /\AREFERENCES/i;

    Alzabo::Exception::RDBMSRules->throw( error => "Unrecognized attribute: $a" );
}

sub validate_primary_key
{
    my $self = shift;
    my $col = shift;

    Alzabo::Exception::RDBMSRules->throw( error => 'Blob columns cannot be part of a primary key' )
	if $col->type =~ /\A(?:TINY|MEDIUM|LONG)?(?:BLOB|TEXT)\z/i;
}

sub validate_sequenced_attribute
{
    my $self = shift;
    my $col = shift;

    Alzabo::Exception::RDBMSRules->throw( error => 'Non-integer columns cannot be sequenced' )
	unless $col->is_integer;

    Alzabo::Exception::RDBMSRules->throw( error => 'Only one sequenced column per table is allowed.' )
	if grep { $_ ne $col && $_->sequenced } $col->table->columns;
}

sub validate_index
{
    my $self = shift;
    my $index = shift;

    foreach my $c ( $index->columns )
    {
	my $prefix = $index->prefix($c);
	if (defined $prefix)
	{
	    Alzabo::Exception::RDBMSRules->throw( error => "Invalid prefix specification ('$prefix')" )
		unless $prefix =~ /\d+/ && $prefix > 0;

	    Alzabo::Exception::RDBMSRules->throw( error => 'Non-character/blob columns cannot have an index prefix' )
		unless $c->is_blob || $c->is_character;
	}

	if ( $c->is_blob )
	{
	    Alzabo::Exception::RDBMSRules->throw( error => 'Blob columns must have an index prefix' )
		unless $prefix || $index->fulltext;
	}

	if ( $index->fulltext )
	{
	    Alzabo::Exception::RDBMSRules->throw( error => 'A fulltext index can only include text or char columns' )
		unless $c->is_character || $c->type =~ /\A(?:TINY|MEDIUM|LONG)?TEXT\z/i;
	}
    }

    Alzabo::Exception::RDBMSRules->throw( error => 'An fulltext index cannot be unique' )
	if $index->unique && $index->fulltext;

}

sub type_is_integer
{
    my $self = shift;
    my $col  = shift;
    my $type = uc $col->type;

    return 1 if $type =~ /\A(?:(?:TINY|SMALL|MEDIUM|BIG)?INT|INTEGER)\z/;
}

sub type_is_floating_point
{
    my $self = shift;
    my $col  = shift;
    my $type = uc $col->type;

    return 1 if $type =~ /\A(?:NUMERIC|FLOAT|DOUBLE|REAL|DECIMAL)\z/;
}

sub type_is_char
{
    my $self = shift;
    my $col  = shift;
    my $type = uc $col->type;

    return 1 if $type =~ /\A(?:(?:NATIONAL\s+)?VAR)?CHAR\z/;
    return 1 if $type =~ /\A(?:(?:VAR)?CHAR|CHARACTER)\z/;
}

sub type_is_date
{
    my $self = shift;
    my $col  = shift;
    my $type = uc $col->type;

    return 1 if $type =~ /\A(?:DATE|DATETIME|TIMESTAMP)\z/;
}

sub type_is_datetime
{
    my $self = shift;
    my $col  = shift;
    my $type = uc $col->type;

    if ( $type eq 'TIMESTAMP' )
    {
        return $col->length > 8 || !defined $col->length;
    }

    return 1 if $type eq 'DATETIME';
}

sub type_is_time
{
    my $self = shift;
    my $col  = shift;
    my $type = uc $col->type;

    if ( $type eq 'TIMESTAMP' )
    {
        return $col->length > 8 || !defined $col->length;
    }

    return 1 if $type eq /\A(?:DATETIME|TIME)\z/;
}

sub type_is_blob
{
    my $self = shift;
    my $col  = shift;
    my $type = uc $col->type;


    return 1 if $type =~ /\A(?:TINY|MEDIUM|LONG)?(?:TEXT|BLOB)\z/;
}

sub blob_type { return 'BLOB' }

sub column_types
{
    return (qw(

        INTEGER
        SMALLINT

        DOUBLE
        NUMERIC
        DECIMAL
        REAL

        CHAR
        CHARACTER
        VARCHAR

        DATE
        TIMETZ
        TIME
        TIMESTAMPTZ
        TIMESTAMP
        INTERVAL

        TEXT

        BLOB

        BOOLEAN

        BIT
    ) );
}

my %features = map { $_ => 1 } qw (
				  );
sub feature
{
    shift;
    return $features{+shift};
}

sub schema_sql
{
    my $self = shift;
    my $schema = shift;

    my @sql;

    foreach my $t ( $schema->tables )
    {
	push @sql, $self->table_sql($t);
    }

    return @sql;
}

sub table_sql
{
    my $self = shift;
    my $table = shift;

    # @_ now has a list of column names to ignore
    my %ignore = map { $_ => 1 } @_;

    my $sql = "CREATE TABLE " . $table->name . " (\n  ";

    # since we use this for dropping columns from a table
    $sql .= join ",\n  ", map { $self->column_sql($_) } grep { !$ignore{$_ -> name} } $table->columns;

    if (my @pk = grep { !$_ -> sequenced } $table->primary_key)
    {
	$sql .= ",\n";
	$sql .= '  PRIMARY KEY (';
	$sql .= join ', ', map {$_->name} @pk;
	$sql .= ")";

        # foreign keys are parsed but ignored - not doing them makes it easier for now
	my @fk = (); # map { $self->foreign_key_sql($_) } $table->all_foreign_keys;

	if (@fk)
	{
	    $sql .= ",\n  ";
	    $sql .= join ",\n  ", @fk;
	}

	$sql .= "\n";
    }
    $sql .= ")";

    my @sql = ($sql);
    foreach my $i ( $table->indexes )
    {
        next if grep { $ignore{$_ -> name} } $i -> columns;
	push @sql, $self->index_sql($i);
    }

    return @sql;
}

sub column_sql
{
    my $self = shift;
    my $col = shift;
    my $p = shift; # for skip_name

    # make sure each one only happens once
    my %attr = map { uc $_ => $_ } (
        $col -> sequenced ? ( 'primary key' )
                          : ( $col->attributes,
			        ($col->nullable ? 'NULL' : 'NOT NULL'),
                            )
    );

    # unsigned attribute has to come right after type declaration,
    # same with binary.  No column could have both.
    my @unsigned = $attr{UNSIGNED} ? delete $attr{UNSIGNED} : ();
    my @binary   = $attr{BINARY} ? delete $attr{BINARY} : ();

    my @default;
    if ( defined $col->default )
    {
	my $def = ( $col->is_numeric ? $col->default :
		    do { my $d = $col->default; $d =~ s/"/""/g; $d } );

	@default = ( qq|DEFAULT "$def"| );
    }

    my $type = $col->type;
    my @length;
    if ( defined $col->length )
    {
	my $length = '(' . $col->length;
	$length .= ', ' . $col->precision if defined $col->precision;
	$length .= ')';
	$type .= $length;
    }

    my @name = $p->{skip_name} ? () : $col->name;
    my $sql .= join '  ', ( @name,
			    ($col -> sequenced ? 'integer' : $type),
			    @unsigned,
			    @binary,
			    @default,
			    sort values %attr );

    return $sql;
}

sub index_sql
{
    my $self = shift;
    my $index = shift;

    my $index_name = $self->_make_index_name( $index->id );

    my $sql = 'CREATE';
    $sql .= ' UNIQUE' if $index->unique;
    $sql .= " INDEX $index_name ON " . $index->table->name . ' ( ';

    $sql .= join ', ', ( map {  $_->name } $index->columns );

    $sql .= ' )';

    # SQLite supports additional `ON CONFLICT {ABORT|COPY|INSERT|UPDATE}' - default is ABORT
    #   only makes sense if CREATE UNIQUE INDEX ...

    return $sql;
}

sub _make_index_name
{
    shift;
    return substr(shift, 0, 64);
}

sub foreign_key_sql
{
    return; # SQLite parses the sql, but ignores it
}

sub _alter_table {
    my $self = shift;
    my $old = shift;
    my $new = shift;
    my $columns;
    my $columns2;  # useful for renaming a column
    if(@_) 
    {
        $columns = shift;
    }
    else
    {
        # get the common set
        $columns = join(", ", grep { eval { $old->column($_); } && !$@ } map { $_ -> name } $new->columns );
    }
    if(@_)
    {
        $columns2 = shift;
    }
    else
    {
        $columns2 = $columns;
    }


    my @sql = (
        "BEGIN TRANSACTION " . $old -> name . "_TRANS_ALTER",
        "CREATE TEMP TABLE " . $new -> name . "$TEMPORARY_TABLE_SUFFIX AS SELECT * FROM " . $old -> name,
        "DROP TABLE " . $old -> name,
        $self -> table_sql($new, @_),
        "INSERT INTO " . $new -> name . " ($columns) SELECT $columns2 FROM " . $new -> name . $TEMPORARY_TABLE_SUFFIX,
        "DROP TABLE " . $new -> name . $TEMPORARY_TABLE_SUFFIX,
        "END TRANSACTION " . $old -> name . "_TRANS_ALTER",
    );

    return @sql;
}

sub drop_column_sql
{
    my $self = shift;
    my %p = @_;

    # ALTER TABLE is not support :(
    # have to copy everything to a temporary table, drop/add table, copy back
    # may be a pain, but what else can we do ?

    my $columns = join(", ", grep { $_ ne $p{old} -> name } map { $_ -> name } $p{old}->table->columns);
    return join(";\n", $self -> _alter_table($p{old}->table, $p{old}->table, $columns));
}

sub drop_foreign_key_sql
{
    return;
}

sub drop_index_sql
{
    my $self = shift;
    my $index = shift;

    return 'DROP INDEX ' . $self->_make_index_name( $index->id );
}

sub column_sql_add
{
    my $self = shift;
    my $col = shift;

    my $columns = join(", ", grep { $_ ne $col->name } map { $_ -> name } $col->table->columns );
    return join(";\n", $self -> _alter_table($col->table, $col->table, $columns));
}

sub column_sql_diff
{
    my $self = shift;
    my %p = @_;
    my $new = $p{new};
    my $old = $p{old};

    my $columns = join(", ", grep { eval { $p{old}->table->column($_); } && !$@ } map { $_ -> name } $p{new}->table->columns );
    return $self -> _alter_table($p{new} -> table, $p{new} -> table, $columns);
}

sub foreign_key_sql_diff
{
    return ();
}

# still need to change
sub alter_primary_key_sql
{
    my $self = shift;
    my %p = @_;

    return $self -> _alter_table($p{old}, $p{new}, "*");
}

sub change_table_name_sql
{
    my $self = shift;
    my $table = shift;

    my $sql = "BEGIN TRANSACTION " . $table->name . "_TRANS_RENAME;\n";
    $sql .= "CREATE TEMP TABLE " . $table->name . "$TEMPORARY_TABLE_SUFFIX AS SELECT * FROM " . $table->former_name . ";\n";
    $sql .= "DROP TABLE " . $table->former_name . ";\n";
    $sql .= join ";\n", $self -> table_sql($table);
    $sql .= ";\n"; # copy temp table into new table
    $sql .= "INSERT INTO " . $table->name . " SELECT * FROM " . $table->name . "$TEMPORARY_TABLE_SUFFIX;\n";
    $sql .= "DROP TABLE " . $table->name . "$TEMPORARY_TABLE_SUFFIX;\n"; 
    $sql .= "END TRANSACTION " . $table->name . "_TRANS_RENAME;\n";
    return $sql;
}

sub change_column_name_sql
{
    my $self = shift;
    my $column = shift;

    my %names = map { $_ -> name => $_ -> name } $column->table->columns;

    $names{$column -> name} = $column -> former_name;

    my $new_columns = join(", ", keys %names);
    my $old_columns = join(", ", values %names);

    return join(";\n", $self -> _alter_table($column -> table, $column -> table, $new_columns, $old_columns));
}

sub reverse_engineer
{
    Alzabo::Exception::RDBMSRules->throw( error => "Alzabo::RDBMSRules::SQLite::reverse_engineer is currently not supported." );
}

sub rules_id
{
    return 'SQLite';
}

1;

__END__

=head1 NAME

Alzabo::RDBMSRules::SQLite - SQLite specific database rules.

=head1 SYNOPSIS

  use Alzabo::RDBMSRules::SQLite;

=head1 DESCRIPTION

This module implements all the methods descibed in Alzabo::RDBMSRules
for the SQLite database.  The syntax rules are based on the MySQL rules 
and may be a little more restrictive then necessary.

=head1 DATA TYPES

Even though SQLite is a typeless database (for the most part), the RDBMS 
rules do define a limited number of accepted data types in order to 
support some of the other features of Alzabo, such as querying which 
type of data is expected for a particular column.

=over 4

=item Numeric Types

 BIT
 BOOLEAN
 DECIMAL
 DOUBLE [PRECISION]
 INT[EGER]
 NUMERIC
 REAL
 SMALLINT

=item Text Types

 CHAR [VARYING]
 CHARACTER [VARYING]
 VARCHAR
 TEXT

=item Blob Types

 BLOB
 TEXT

=item Time and Date Types

 DATE
 INTERVAL
 TIMETZ
 TIME
 TIMESTAMPTZ
 TIMESTAMP

=back

=head1 AUTHOR

James Smith, <jsmith@cpan.org> (SQLite-specific code);
Dave Rolsky, <dave@urth.org>

=cut
