use Test::More;

#require "t/schema";
require "t/test-utils";
#require "t/api_tests";

local($SIG{__WARN__});

my $e; # $@

start_tests(
    Gestinanna::POF::Repository => (
        Alzabo::Create => undef,
        Alzabo::Runtime => undef,
        Alzabo::MethodMaker => undef,
        Gestinanna::POF::Alzabo => undef,
        Gestinanna::POF => undef,
        DBI => undef,
        DBD::SQLite => undef,
        Alzabo::Driver::SQLite => 'Alzabo must support SQLite in order to test __TARGET__',
    ),
);

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
my %data = reverse (
    foo => v1.1,
    bar => v1.2,
    this => v1.3,
    that => v1.4,
);
my $object;


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
                   # create a revision so we can create a tag
                   $object = $factory -> new(test => (object_id => '/test'));

                   # create revisions 1.1 .. 1.7
                   for my $d (map { $data{$_} } sort keys %data) {
                       $object -> data($d);
                       $object -> log('Testing');
               
                       $object -> save;
                   }
               },
             ];

push @Tests, [ undef,
               sub {
                   foreach my $rev (keys %data) {
                       my $tag = $factory -> new(test_tag => (
                           tag => 'Tag_' . join(".", unpack("U*", $rev)), 
                           object_id => '/test',
                       #    revision => $rev,
                           ) );
                       $tag -> revision($rev);
                       $tag -> save;
                   }
               },
             ];

eval "require Algorithm::Permute;";

unless($@) {
    my $permuter = Algorithm::Permute -> new( [ map { 'Tag_' . join(".", unpack("U*", $_)) } keys %data ]);
    my $num = 1;
    $num *= $_ for 2..scalar(keys %data);

    my @tags;
    push @Tests, [ $num,
                   sub {
                       while(@tags = $permuter -> next) {
                           $object = $factory -> new(test => (object_id => '/test', tag_path => \@tags));
                           is(join(".", unpack("U*", $object -> revision)),
                              substr($tags[0], 4)
                             );
                       }
                   },
                 ];
}

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

run_tests(\@Tests);

eval {
    $create_schema -> drop;
    $create_schema -> delete;
};

exit 0;

BEGIN {
    use Gestinanna::POF::Repository;
                
Gestinanna::POF::Repository -> build_object_class(
    class => 'My::Rep::Foo',
    params => {
        repository => 'Foo',
    },
);

}

1;
