# define base class for Repositories
package Gestinanna::POF::Repository;

use Gestinanna::POF::Repository::Object ();
use Gestinanna::POF::Repository::Tag ();
use Gestinanna::POF::Repository::Description ();

use base q(Class::Container);

__PACKAGE__ -> valid_params(
    alzabo_schema =>  { isa => q(Alzabo::Runtime::Schema) },
    _factory => { isa => 'Gestinanna::POF' },
);

our $VERSION = '0.04';
our $REVISION = substr(q$Revision: 1.6 $, 10);

our $RESOURCE = 'alzabo_schema';

our @XML_ATTRIBUTES = qw(
    resource
    repository
);

use Carp;

use strict;
no strict 'refs';

sub set_factory_resources {
    my($provider_class, %params) = @_;

    my $type = $params{type};

    $params{factory} -> set_resources(
        $type => "${provider_class}::Object" -> resource_requirements(
            params => $params{params},
            config => $params{config},
        )
    );

    $params{factory} -> set_resources(
        $type."_tag" => "${provider_class}::Tag" -> resource_requirements(
            params => $params{params},
            config => $params{config},
        )
    );

    $params{factory} -> set_resources(
        $type."_description" => "${provider_class}::Description" -> resource_requirements(
            params => $params{params},
            config => $params{config},
        )
    );

    $params{factory} -> set_resources(
        $type."_repository" => $provider_class -> resource_requirements(
            params => $params{params},
            config => $params{config},
        )
    );
}

sub import {
    my $class = shift;

    if(@_) {
        return ;
        my $rep = shift;
        my $lcrep = lc $rep;

        my %params = @_;

        my $package = caller;

        my $pcode = <<EOF;
package $package;

use base qw($class);

use constant repository => qq{$rep\E};
use constant repository_key => qq{\Q$lcrep\E};
EOF

        eval $pcode;
        warn "Error defining package $package: $@\n" if $@;

        my %classes = (
            Tag => "${package}::Tag",
            Object => "${package}::Object",
            Description => "${package}::Description",
        );

        foreach my $subclass (keys %classes) {


            eval "require $classes{$subclass};";
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
            my $base_classes = join("\n    ", @{$params{lc($subclass)."_classes"}||[]}, $base_class);

            my $code = <<EOF;
package $classes{$subclass};
use base qw(
    $base_classes
);

\$${classes{$subclass}}::VERSION = $class -> VERSION;

*repository = \\&${package}::repository;
*repository_key = \\&${package}::repository_key;

EOF

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
    $path = '' unless defined $path;
    return { files => [], folders => [] } if $path =~ m{%};
    my $prefix = "/".$path . "/";
    $prefix =~ s{/+}{/}g;
    $prefix = qr{\Q$prefix\E};
    #warn "prefix: $prefix\n";
    $path = "/$path";
    $path =~ s{/+}{/};
    $path =~ s{\\}{\\\\}g;
    $path =~ s{_}{\\_};
    $path =~ s{%}{\\%}g;
    $path .= "%";
    #warn "path: [$path]\n";

    my $desc_table = $self -> {alzabo_schema} -> table($self -> repository . "_Description");
    my $obj_table = $self -> {alzabo_schema} -> table($self -> repository);

    my(%objects);
    my($name, $obj);


    my @res = $obj_table -> function(
        select => [ 'DISTINCT(name)' ],
        where => [
            [ $obj_table -> column('name'), 'LIKE', $path  ],
#              'and',
#            [ $obj_table -> column('name'), 'NOT LIKE', $path . "/%" ],
        ],
    );

    @objects{(map { (s{^$prefix}{} => $_)[1] } (@res))} = (undef);

    @res = $desc_table -> function(
        select => [ 'DISTINCT(name)' ],
        where => [
            [ $desc_table -> column('name'), 'LIKE', $path  ],
#              'and',
#            [ $desc_table -> column('name'), 'NOT LIKE', $path . "/%" ],
        ],
    );

    @objects{(map { (s{^$prefix}{} => $_)[1] } (@res))} = (undef);

    # now do the same with the Folder table, if it exists
    if($self -> {alzabo_schema} -> has_table('Folder')) {
        $desc_table = $self -> {alzabo_schema} -> table('Folder');
        my @res = $desc_table -> function(
            select => [ 'DISTINCT(name)' ],
            where => [
                [ $desc_table -> column('name'), 'LIKE', $path  ],
#              'and',
#            [ $desc_table -> column('name'), 'NOT LIKE', $path . "/%" ],
            ],
        );
        @objects{(map { (s{^$prefix}{} => $_)[1] . '/' } (@res))} = (undef);
    }

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



    return { files => \@files, folders => \@dirs };
}

#        $class -> build_object_class(
#            params => $data_providers -> {$type} -> {params},
#            class => $provider_class,
#            config => $data_providers -> {$type} -> {config},
#            site => $self,

sub build_object_class {
    my($class, %params) = @_;

    no strict 'refs';

    my $rep = $params{params}{repository};
    my $lcrep = lc $rep;

    my $package = $params{class};
             
    my $pcode = <<EOF;
package $package;
            
use base qw($class);
            
our \$VERSION = 1;
use constant repository => qq{\Q$rep\E};
use constant repository_key => qq{\Q$lcrep\E};
EOF
  
    eval $pcode;
    if($@) {
        warn "Error defining package $package: $@\n";
        return 0;
    }

    my %classes = (
        Tag => "${package}::Tag",
        Object => "${package}::Object",
        Description => "${package}::Description",
    );

    my %tables = (
        Tag => $params{params}{repository} . "_Tag",
        Object => $params{params}{repository},
        Description => $params{params}{repository} . "_Description",
    );

    foreach my $subclass (keys %classes) {

        eval "require $classes{$subclass};";

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
        my $base_classes = join("\n    ", @{$params{lc($subclass)."_classes"}||[]}, $base_class);

        my $code = <<EOF;
package $classes{$subclass};
use base qw(
    $base_classes
);

\$${classes{$subclass}}::VERSION = $class -> VERSION;
    
*repository = \\&${package}::repository;
*repository_key = \\&${package}::repository_key;
        
EOF
        
        foreach (keys %classes) {
            my $lcc = lc $_;
            my $clc = $classes{$_};
            $code .= "use constant ${lcc}_class => qq{\Q$clc\E};\n";
        }

        eval $code;
        if($@) {
            warn "Unable to create class $classes{$subclass}: $@\n";
            return 0;
        }

        $base_class -> build_object_class(
            class => $classes{$subclass},
            params => { %{$params{params}},
                        table => $tables{$subclass},
                      },
        ) or return 0;

    }
    return 1;
}

sub resource_requirements {
    my $class = shift;

    $class = ref $class || $class;
    my %params = @_;

    my $resource_attr;
    no strict 'refs';
    foreach my $c ($class, Class::ISA::super_path($class)) {
        $resource_attr = ${"${c}::RESOURCE"};
        last if defined $resource_attr;
    }
     
    return { } unless defined $resource_attr;

    return { $resource_attr => $params{params} -> {resource} };
}

1;

__END__

=head1 NAME

Gestinanna::POF::Repository - basic support for a revision controlled repository

=head1 SYNOPSIS

Creating a repository based on the C<Files> table:

 use Gestinanna::POF::Repository;
                
 Gestinanna::POF::Repository -> build_object_class(
     class => 'My::Files',
     params => {
         repository => 'Files',
     },
 );


Using the above repository:

 My::Files -> register_factory_types($factory => file);

 my $factory = Factory -> new(_factory => (
     alzabo_schema => $alzabo_schema,
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
            
Copyright (C) 2002-2004 Texas A&M University.  All Rights Reserved.
        
This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.  
