use Test::More;

require "t/schema";

local($SIG{__WARN__});

my $e; # $@

eval {
    require Alzabo::Create;
    require Alzabo::Runtime;
    require Alzabo::MethodMaker;
};

if($@) {
    plan skip_all => 'Alzabo is required to test Gestinanna::POF::Repository';
    exit 0;
}

eval {
    require Gestinanna::POF::Alzabo;
    require Gestinanna::POF;
};

if($@) {
    plan skip_all => 'Gestinanna::POF and Gestinanna::POF::Alzabo are required to test Gestinanna::POF::Repository';
    exit 0;
}

eval {
    require DBI;
    require DBD::SQLite;
};

if($@) {
    plan skip_all => 'DBI support for SQLite (DBD::SQLite) is required to test Gestinanna::POF::Repository';
    exit 0;
};

eval {
    require Alzabo::Driver::SQLite;
};

if($@) {
    plan skip_all => 'Alzabo must support SQLite in order to test Gestinanna::POF::Repository';
    exit 0;
}

# create a schema - errors here make us skip the tests

my $create_schema;
my $schema;
my $schema_name = 'gst_pof_test_alzabo_schema';
eval {
    $create_schema = Alzabo::Create::Schema -> new(
        name => 'gst_pof_test_alzabo_schema',
        rdbms => 'SQLite'
    );

    # schema skeleton
    create_repository($create_schema, 'Foo');

    # data under revision control
    $create_schema -> table('Foo') -> make_column(
        name => 'data',
        type => 'text',
    );

    $create_schema -> create;
    $create_schema -> save_to_file;

    $schema = Alzabo::Runtime::Schema -> load_from_file(
        name => 'gst_pof_test_alzabo_schema',
    );

    $schema -> connect;
};

if($e = $@) {
    diag($e);
    eval { $create_schema -> drop; $create_schema -> delete; };
    plan skip_all => 'Unable to create an Alzabo schema: ' . $e;
    exit 0;
}

my @Tests;

$INC{'My/Rep/Foo.pm'} = 1;

my $factory;
my $object;
my $repository;


push @Tests, [ undef,
               sub {
                   My::Rep::Foo -> register_factory_types(Gestinanna::POF => 'test');
               },
             ];

push @Tests, [ undef,
               sub { $factory = Gestinanna::POF -> new(_factory => ( alzabo_schema => $schema ) ); },
             ];

push @Tests, [ undef,
               sub {
                   # create a few objects and description entries
                   $object = $factory -> new(test_description => (object_id => '/test'));

                   $object -> description("Test Document");
                   $object -> save;

                   $object = $factory -> new(test => ( object_id => '/test/foo' ));
                   $object -> data("Foo");  $object -> log("Testing");
                   $object -> save;

                   $object = $factory -> new(test_description => ( object_id => '/foo/' ));
                   $object -> description("Test folder");
                   $object -> save;

                   $object = $factory -> new(test_description => ( object_id => '/bar/this/that' ));
                   $object -> description("Test folder");
                   $object -> save;
 
                   $object = $factory -> new(test => ( object_id => '/this/that/bar/foo' ));
                   $object -> data("Bar");  $object -> log("Testing");
                   $object -> save;

#                   $object = $factory -> new(test => ( object_id => '/this/%/bar/foo' ));
#                   $object -> data("Bar");  $object -> log("Testing");
#                   $object -> save;
               },
             ];

push @Tests, [ undef,
               sub {
                   $repository = $factory -> new('test_repository');
               },
               sub{ ok(defined $repository); },
               sub{ ok(UNIVERSAL::isa($repository, 'Gestinanna::POF::Repository')); },
             ];

my %results = (
    '/' => [ [ qw( bar foo test this ) ], [ qw( test ) ] ],
    '/bar/' => [ [ qw( this ) ], [ ] ],
    '/bar/this/' => [ [ ], [ qw( that ) ] ],
    '/foo/' => [ [ ], [ ] ],
    '/test/' => [ [ ], [ qw( foo ) ] ],
    '/this/' => [ [ qw( that ) ], [ ] ],
    '/this/that/' => [ [ qw( bar ) ] , [ ] ],
    '/this/that/bar/' => [ [ ], [ qw( foo ) ] ],
#    '/this/%/' => [ [ qw( bar ) ], [ ] ],
);

my($dirs, $files);

foreach my $dir (keys %results) {
    push @Tests, [ undef,
                   sub {
                       ($files, $dirs) = @{$repository -> listing($dir)}{qw(files folders)};
                   },
                   sub { is_deeply([ $dirs, $files ], $results{$dir}); },
                 ];
}
              

#push @Tests, [ undef,
#               sub {
#                   $object -> delete;
#               },
#               qr{Objects can not be deleted from a repository},
#             ];

#push @Tests, [ undef, # figure out how many later -- no tests run in the
#                      # actual test code (otherwise, this is the number
#                      # of tests in the test code
#               sub {
#                   # do nothing -- this is where the test code goes
#               },   
#               # third element may be a regex (qr//) to replace the ok(!$e) test
#               #   if this is qr//, then the test is ok($e =~ m{qr//});
#               # otherwise, all subs after the test code are tests of the results
#             ];


### end tests
    
# go through and figure out how many tests we are actually running
my $num_tests;

foreach my $test (@Tests) {
    if(defined($test->[0])) {
        $num_tests += $test->[0];
        $num_tests += scalar(@$test) - 2; # no test of ok(!$e)
    }
    else {
        $num_tests += scalar(@$test) - 1;
        $num_tests -- if UNIVERSAL::isa($test -> [2], 'SCALAR'); # replaces ok(!$e)
    } 
}
  
if($num_tests) {
    plan tests => $num_tests;
}
else {
    plan skip_all => 'No tests are defined';
    exit 0;
}
  
my $testloc;
foreach my $test (@Tests) {
    eval { $test -> [1] -> () };
    $e = $@;

    if(defined($test->[0])) {
        $testloc = 2;
    }
    elsif(UNIVERSAL::isa($test->[2], 'SCALAR')) {  
        diag($e) unless $e =~ m{$test->[2]};
        ok($e =~ m{$test->[2]});
        $testloc = 3; 
    }
    else {
        diag($e) if $e;
        ok(!$e);
        $testloc = 2;
    }
    while($testloc < @$test) {
        $test -> [$testloc++] -> ();
    }
}

# clean up the schema - errors here are warnings, not failed tests

eval {
    $create_schema -> drop;
    $create_schema -> delete;
};

exit 0;

package My::Rep::Foo;

use Gestinanna::POF::Repository qw(Foo);

1;
