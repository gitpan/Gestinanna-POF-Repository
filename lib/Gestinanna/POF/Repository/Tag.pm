# define base class for Repositories
package Gestinanna::POF::Repository::Tag;

use base qw(Gestinanna::POF::Alzabo);
use Params::Validate qw(:types);

__PACKAGE__ -> valid_params (
    revision => { type => SCALAR, optional => 1 },
    name => { type => SCALAR, optional => 1 },
    tag => { type => SCALAR, optional => 1 },
);

__PACKAGE__ -> mk_ro_accessors(qw(
    name
    tag
));

use Carp;

use strict;
no strict 'refs';

sub table { $_[0] -> repository . "_Tag" };

sub load {
    my $self = shift;

    if(defined $self -> {object_id}) {
        $self -> {name} = $self -> {object_id};
        delete $self -> {object_id};
    }

    if(defined $self -> {tag_path}) {
        #warn "Tag path: [", join(", ", @{$self -> {tag_path}||[]}), "]\n";
        # find a revision based on tag
        my $t;
        foreach my $tag (@{$self -> {tag_path}}) {
            $t = $self -> new(
                schema => $self -> {schema},
                tag => $tag,
                name => $self -> {name},
            );
            last if $t -> is_live;
        }
        if($t -> is_live) {
            $self -> {revision} = $t -> revision;
            $self -> {_row} = $t;
        }
    }

    #warn "Tag: $$self{tag}; Name: $$self{name}\n";
    
    $self -> SUPER::load unless $self -> {_row};
}

sub tags {
    my $self = shift;
    my %args = @_;

    # by name and revision => tags
    # by name => tags

    # need _factory, name, (revision)?
    return unless $args{_factory};
    return unless $args{name};

    my $table = $args{_factory} -> {schema} -> table($self -> table);
    my $cursor = $table -> rows_where(
        where => [
            [ $table -> column('name'), '=', $args{name} ],
            ($args{revision} 
                ?  [ $table -> column('revision'), '=', $args{revision} ]
                :  ( )
            )
        ],
    );

    my @tags;
    my $tag;
    my $t;

    while($tag = $cursor -> next) {
        $t = $self -> new(
            schema => $args{_factory} -> {schema},
            tag => $tag -> tag,
            name => $tag -> name,
            revision => $tag -> revision,
            _factory => $args{_factory},
        );
        $t -> {_row} = $tag;
        push @tags, $t;
    }

    return @tags;
}

1;

__END__

=head1 NAME

Gestinanna::POF::Repository::Tag - tag support for repositories

=head1 SYNOPSIS

 $tag = $factory -> new(foo_tag => (
     object_id => '/name', 
     tag => 'tag name'
 ));

 $tag = $factory -> new(foo_tag => (
     object_id => '/name', 
     tag_path => [qw(list of tags)],
 ));

=head1 DESCRIPTION

This class is used to manage tags for a repository.  The object_id and 
tag combination must be unique.

=head1 METHODS

=head2 tags

 Repository::Tag -> tags(
     _factory => $factory,
     name => $name,
     revision => $revision,
 );

This class method will return a list of tag objects.  If only a name 
is given, then all tags for all revisions of that named object are 
returned.  If a name and a revision are given, then only the tags for 
that particular revision and name are returned.

=head1 AUTHOR

James G. Smith, <jsmith@cpan.org>

=head1 COPYRIGHT
    
Copyright (C) 2002-2003 Texas A&M University.  All Rights Reserved.
    
This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.
