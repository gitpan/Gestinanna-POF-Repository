use Test::More;

use File::Spec;
use File::Find qw(find);
use strict;

eval {
    require Test::Pod;
    Test::Pod->import;
};

if ($@) {
    plan skip_all => "Test::Pod required for testing POD";
}
else {
    my @files;
    my $fh;

    my $pwd = `pwd`;  chomp $pwd;
    $pwd =~ s{[/\\:]t$}{};
    chdir($pwd);

    open $fh, "<", "MANIFEST" or do {
        plan skip_all => "Unable to read MANIFEST";
        exit 0;
    };

    @files = <$fh>;

    close $fh;

    chomp @files;
    @files = grep { m!\.p(m|od|l)$! } @files;

    plan tests => scalar @files;

    pod_file_ok("$_", "Testing POD in $_") foreach @files;
}

