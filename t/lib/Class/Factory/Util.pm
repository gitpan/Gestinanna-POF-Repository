package Class::Factory::Util;

use strict;
use vars qw($VERSION);

use Carp qw(confess);

$VERSION = '1.5';

1;

sub import
{
    my $caller = caller(0);

    {
        no strict 'refs';
        *{"${caller}::subclasses"} = \&_subclasses;
    }
}

# deprecated
sub subclasses { _subclasses(@_) }

sub _subclasses
{
    my $base = shift;

    $base =~ s,::,/,g;
    $base .= '.pm';

    # remove '.pm'
    my $dir = substr( $INC{$base}, 0, (length $INC{$base}) - 3 );

    my $inc_dir = substr( $INC{$base}, 0, (length $INC{$base}) - (length $base) - 1);
    my $sub_dir = substr( $dir, (length $INC{$base}) - (length $base) );

    my $was_from_inc = 0;
    foreach my $d (@INC) {
        next if ref $d;
        if($d eq $inc_dir) {
            $was_from_inc = 1;
            last;
        }
    }

    my @dirs;
    if($was_from_inc) {
        @dirs = map { $_ . '/' . $sub_dir } grep { !ref $_ } @INC;
    }
    else {
        @dirs = ($dir);
    }

    my @packages;

    foreach $dir (@dirs) {
        next unless -d $dir;
        opendir DIR, $dir
	    or confess ("Cannot open directory $dir: $!");

        push @packages, map { substr($_, 0, length($_) - 3) } grep { substr($_, -3) eq '.pm' && -f "$dir/$_" } readdir DIR;

        closedir DIR
	    or confess("Cannot close directory $dir: $!" );
    }

    return keys %{ +{ map { $_ => undef } @packages } };
}

__END__

=head1 NAME

Class::Factory::Util - Provide utility methods for factory classes

=head1 SYNOPSIS

  package My::Class;

  use Class::Factory::Util;

  My::Class->subclasses;

=head1 DESCRIPTION

This module exports a method that is useful for factory classes.

=head1 USAGE

When this module is loaded, it creates a method in its caller named
C<subclasses()>.  This method returns a list of the available
subclasses for the package.  It does this by looking in the library
directory containing the caller, and finding any modules in its
immediate subdirectories.

So if you have the modules "Foo::Base", "Foo::Base::Bar", and
"Foo::Base::Baz", then the return value of C<< Foo::Base->subclasses()
>> would be "Bar" and "Baz".

=head1 SUPPORT

Please submit bugs to the CPAN RT system at
http://rt.cpan.org/NoAuth/ReportBug.html?Queue=class-factory-util or
via email at bug-class-factory-util@rt.cpan.org.

=head1 AUTHOR

Dave Rolsky, <autarch@urth.org>.

Removed from Alzabo and packaged by Terrence Brannon,
<tbone@cpan.org>.

=head1 COPYRIGHT

Copyright (c) 2003 David Rolsky.  All rights reserved.  This program
is free software; you can redistribute it and/or modify it under the
same terms as Perl itself.

The full text of the license can be found in the LICENSE file included
with this module.

=cut
