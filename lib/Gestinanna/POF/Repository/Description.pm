# define base class for Repositories
package Gestinanna::POF::Repository::Description;

use base qw(Gestinanna::POF::Alzabo);
use Carp;

use strict;
no strict 'refs';

sub table { $_[0] -> repository . "_Description" };

sub delete {
    croak "Objects can not be deleted from a repository";
}

1;

__END__

=head1 NAME

Gestinanna::POF::Repository::Description - metadata support for repositories

=head1 SYNOPSIS

 $desc = $factory -> new(foo_description => (object_id => '/foo'));

 # use $desc as any other Gestinanna::POF::Alzabo object instance

=head1 DESCRIPTION

This class provides an easy way to manage metadata associated with a 
particular revision controlled object.

Use this as you would any other L<Gestinanna::POF::Alzabo|Gestinanna::POF::Alzabo>-based object.
Objects cannot be deleted though.

Objects created here will appear in the listing returned by the repository 
object.  To create folders without creating an actual revision-controlled 
object, create an object description for the folder (the name ending with C</>).

=head1 AUTHOR

James G. Smith, <jsmith@cpan.org>

=head1 COPYRIGHT
    
Copyright (C) 2002-2003 Texas A&M University.  All Rights Reserved.
    
This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.
