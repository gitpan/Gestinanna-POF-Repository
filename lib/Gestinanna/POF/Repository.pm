# define base class for Repositories
package Gestinanna::POF::Repository;

use Gestinanna::POF::Repository::Object ();
use Gestinanna::POF::Repository::Tag ();
use Gestinanna::POF::Repository::Description ();

use base q(Class::Container);

__PACKAGE__ -> valid_params(
    schema =>  { isa => q(Alzabo::Runtime::Schema) },
    _factory => { isa => 'Gestinanna::POF' },
);

our $VERSION = '0.01';
our $REVISION = substr(q$Revision: 1.2 $, 10);

use Carp;

use strict;
no strict 'refs';

sub import {
    my $class = shift;

    if(@_) {
        my $rep = shift;
        my $lcrep = lc $rep;

        my $package = caller;

        my $pcode = <<1HERE1;
package $package;

use base qw($class);

use constant repository => qq{$rep\E};
use constant repository_key => qq{\Q$lcrep\E};
1HERE1

        eval $pcode;
        warn "Error defining package $package: $@\n" if $@;

        my %classes = (
            Tag => "${package}::Tag",
            Object => "${package}::Object",
            Description => "${package}::Description",
        );

        foreach my $subclass (keys %classes) {


            eval "require " . $classes{$subclass} . ";";
            #warn "require $classes{$subclass} failed: $@\n" if $@;

            my $base_class = qq{${class}::${subclass}};
            eval "require $base_class;";
            warn "require $base_class failed: $@\n" if $@;

            if(!defined $base_class -> VERSION) {
                eval { eval "require $base_class;" };
                ${$base_class . "::VERSION"} = '-1, defined by ' . __PACKAGE__
                    unless defined $base_class -> VERSION;
            }

            my $inc_entry = $classes{$subclass};
            $inc_entry =~ s{::}{/}g;
            $inc_entry .= ".pm";

            $INC{$inc_entry} = $inc_entry unless exists $INC{$inc_entry};  # trick Class::Factory into thinking it's already loaded

            my $code = <<1HERE1;
package $classes{$subclass};
use base qw($base_class);

\$${classes{$subclass}}::VERSION = $class -> VERSION;

*repository = \\&${package}::repository;
*repository_key = \\&${package}::repository_key;

1HERE1

            foreach (keys %classes) {
                my $lcc = lc $_;
                my $clc = $classes{$_};
                $code .= "use constant ${lcc}_class => qq{\Q$clc\E};\n";
            }

            eval $code;
            carp "Unable to create class $classes{$subclass}: $@" if $@;
        }
    }
    else {
        $class -> SUPER::import;
    }
}

sub register_factory_types {
    my($self, $factory, $key) = @_;

    my $package = ref $self || $self;
    $key ||= $self -> repository_key;

    for my $class (qw(Tag Description)) {
        $factory -> register_factory_type( $key . "_" . lc $class, "${package}::$class" );
    }

    $factory -> register_factory_type( $key, "${package}::Object" );
    $factory -> register_factory_type( $key. "_repository", $package );
}

sub add_factory_types {
    my($self, $factory, $key) = @_;

    my $package = ref $self || $self;
    $key ||= $self -> repository_key;

    for my $class (qw(Tag Description)) {
        $factory -> add_factory_type( $key. "_" . lc $class, $package . "::" . $class );
    }

    $factory -> add_factory_type( $key, "${package}::Object" );
    $factory -> add_factory_type( $key . "_repository", $package );
}

sub load {
    # we don't actually load anything for the repository object
    # repository objects support searching and `directory listings'

    #diag("We're loading!");
}

sub init {
    my $self = shift;

    my %params = @_;

    @{$self}{keys %params} = values %params;

    return $self;
}

sub listing {
    my($self, $path) = @_;

    # return a list of files and directories
    # returns [ @files ], [ @directories ]
    return if $path =~ m{%};
    my $prefix = "/".$path . "/";
    $prefix =~ s{/+}{/}g;
    $prefix = qr{\Q$prefix\E};
    #warn "prefix: $prefix\n";
    $path = "/$path";
    $path =~ s{/+}{/};
    $path =~ s{_}{\\_};
    $path .= "%";
    #warn "path: [$path]\n";

    my $desc_table = $self -> {schema} -> table($self -> repository . "_Description");
    my $obj_table = $self -> {schema} -> table($self -> repository);

    my(%objects);
    my($name, $obj);

    my @res = $obj_table -> function(
        select => [ 'DISTINCT(name)' ],
        where => [
            [ $obj_table -> column('name'), 'LIKE', $path  ],
        ],
    );

    @objects{(map { (s{^$prefix}{} => $_)[1] } (@res))} = (undef);

    @res = $desc_table -> function(
        select => [ 'DISTINCT(name)' ],
        where => [
            [ $desc_table -> column('name'), 'LIKE', $path  ],
        ],
    );

    @objects{(map { (s{^$prefix}{} => $_)[1] } (@res))} = (undef);

    my @dirs = sort 
               keys %{ +{
                    map { $_ => undef } 
                        (map { (split(/\//, $_, 2))[0] } 
                             (grep { $_ =~ m{^[^/]+/} } 
                                   keys %objects
                             )
                        )
                     }  };

    my @files = sort 
                keys %{ +{
                     map { $_ => undef } 
                         (grep { $_ =~ m{^[^/]+$} } 
                               keys %objects
                         )
                      }  };

    return(\@files, \@dirs);
}

1;

__END__

=head1 NAME

Gestinanna::POF::Repository - basic support for a revision controlled repository

=head1 SYNOPSIS

Creating a repository based on the C<Files> table:

 package My::Files;

 use Gestinanna::POF::Repository qw(Files);


Using the above repository:

 My::Files -> register_factory_types($factory => file);

 my $factory = Factory -> new(_factory => (
     schema => $alzabo_schema,
     tag_path => [ qw(list of tags) ],
 ) );

 my $object = $factory -> ( file => (object_id => '/name') );
 my $description = $factory -> ( file_description => (object_id => '/name') );
 my $tag = $factory -> ( file_tag => (object_id => '/name', tag => 'tag name') );
 my $repository = $factory -> ( file_repository );

 my($files, $folders) = $repository -> listing($path);

=head1 DESCRIPTION

A repository (for the purposes of this module) is a collection of 
revision-controlled objects in an RDBMS accessed through L<Alzabo|Alzabo>.

Each repository is managed by four Perl classes: the repository, the 
object store, the description, and the tags.  See 
L<Gestinanna::POF::Repository::Object>,
L<Gestinanna::POF::Repository::Description>, and
L<Gestinanna::POF::Repository::Tag> for more information.

=head1 SCHEMA

The repository expects the following schema.  This assumes 
C<use Gestinanna::POF::Repository qw(Prefix)>.  
The tests in the distribution have an example of the Alzabo code to
create these tables.  Additional columns that are not under revision 
control will be treated as additional attributes as if the object were 
a simple Gestinanna::POF::Alzabo-based object.

=head2 Table `Prefix'

This is the primary object store and has the following columns.  
Additional text columns named C<data> or starting with C<data_> 
may be added.  These columns are then the default set of 
revision-controlled columns.

The names of the revision controlled columns are returned by the object 
method C<data_columns> in the __PACKAGE__::Object class.

=over 4

=item prefix

This is the unique id of the object revision.  This may be used 
to establish relationships with other tables.

=item name

This is the name of the object.  The name and revision (together) 
must be unique.

=item revision

This is the revision of the object.  This is a unicode string of 
characters (e.g., the Perl code C<v1.1>) following the RCS/CVS convention for 
denoting branches and versions.

=item modify_timestamp

This is the timestamp of the revision (when it was saved to the RDBMS).

=item log

This is any log text added by the user when the revision was created.

=back

=head2 Table `Prefix_Tag'

This table maps tags and names to revisions.  Any tag and name combination must be unique.

=over 4

=item tag

This is the name of the tag.  This is part of the primary key.

=item name

This is the name of the tagged object.  This is also part of the primary key.

=item revision

This is the revision of the object the tag points to (in the same unicode format).

=back

=head2 Table `Prefix_Drescription'

This table is for convenience.  It holds metadata that is not tied to a particular revision.

=over 4

=item name

This is the name of the object being described.

=item description

This is a text description of the object.

=back

=head1 METHODS

While the repository classes are based on 
L<Gestinanna::POF::Alzabo|Gestinanna::POF::Alzabo>, they extend 
the functionality to allow management of the repository while 
still allowing simple use of the objects within the repository.

=head2 add_factory_types

 __PACKAGE__ -> add_factory_types($factory, $type);

This will add the various object types to the factory, mapping them to 
the appropriate Perl classes.  The type is optional and will default 
the the primary table name (except all lowercase).  For example:

 package My::Repository;

 use Gestinanna::POF::Repository qw(Foo);

 package main;

 My::Repository -> add_factory_types($factory);

This will add the following object mappings:

 foo             => My::Repository::Object
 foo_description => My::Repository::Description
 foo_repository  => My::Repository
 foo_tag         => My::Repository::Tag

L<Class::Factory|Class::Factory> will try to load each of the Perl 
classes immediately.

=head2 register_factory_types

This is the same as C<add_factory_types> except that L<Class::Factory|Class::Factory> 
will delay loading the Perl classes until it creates an object of that 
particular type.

=head2 listing

 ($files, $folders) = $repository -> listing($path);

This will return a list of the objects and the folders that match the 
path.  The objects and folders are a union of the names in the object 
table and the description table.

This method assumes that path components are separated in a UNIX 
fashion by C</>.

=head1 AUTHOR
    
James G. Smith, <jsmith@cpan.org>

=head1 COPYRIGHT
            
Copyright (C) 2002-2003 Texas A&M University.  All Rights Reserved.
        
This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.  
