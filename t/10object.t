use Test::More;

require "t/test-utils";
use strict;
#require "t/schema";

local($SIG{__WARN__});

my $e; # $@

my $create_schema;
my $schema;
my $schema_name = 'gst_pof_test_alzabo_schema';

start_tests(
    'Gestinanna::POF::Repository' => (
        'Alzabo::Create' => undef,
        'Alzabo::Runtime' => undef,

        'Gestinanna::POF' => undef,
        'Gestinanna::POF::Alzabo' => undef,

        __TARGET__ => 'Unable to load __TARGET__ for testing',

        DBI => undef,
        'DBD::SQLite' => undef,

        'Alzabo::Driver::SQLite' => 'Alzabo must support SQLite in order to test __TARGET__',
    
        sub {
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
        } => sub {
            diag($_[0]);
            eval { $create_schema -> drop; $create_schema -> delete; };
            return 'Unable to create an Alzabo schema: ' . $_[0];
        },
    ),
);

# see if we can do what we need to

my @Tests;

my $factory;
$INC{'My/Rep/Foo.pm'} = 1;

push @Tests,
    sub { My::Rep::Foo -> register_factory_types('Gestinanna::POF' => 'test'); },
      
    [ sub { $factory = Gestinanna::POF -> new(_factory => ( alzabo_schema => $schema ) ); },
      sub { ok(defined $factory); }, #diag("Factory: $factory") },
    ],

    run_api_tests(\$factory, '/test', 'data'),

    #run_api_tests(\$factory, '/second-test', 'data'),

    ;


run_tests(\@Tests);


# clean up the schema - errors here are warnings, not failed tests
eval {
    $create_schema -> drop;
    $create_schema -> delete;
};

exit 0;


sub run_api_tests {
    my($factory, $object_id, $field) = @_;

    my $object;
    my $other_object;
    my $t;

    my $save_object = sub { $object -> save; };
    my $log_msg = sub { $object -> log("Testing"); };
    my $object_is_live = [ sub { $t = $object -> is_live; }, sub { is($t, 1, "Object is live"); } ];
    my $load_object = [ 
        sub { 
            $object = undef;
            #main::diag("Factory: $$factory;  object_id: $object_id"); 
            $object = $$factory -> new(test => (object_id => $object_id)); 
            #main::diag("Loaded $object");
        },
        sub { ok(defined $object, "Loaded object is defined"); },
    ];

    return (
        $load_object,

        [ sub { $t = $object -> is_live; },
          sub { is($t, 0, "Object is not live") },
          sub { is(defined($object -> {alzabo_schema}), 1, "Alzabo schema is defined in object") },
        ],

        sub { $object -> $field('other'); },

        [ sub { $t = $object -> $field; },
          sub { is($t, 'other'); },
        ],

        [ $save_object,
          qr{Must set a log message before saving},
        ],

        $log_msg,

        $save_object,

#( 0 ? (

        $object_is_live,

        $load_object,
        
        $object_is_live,

        [ sub { $t = $object -> $field; },
          sub { is($t, 'other'); },
        ],

        sub { $object -> $field("another"); },

        [ sub { $t = $object -> $field; },
          sub { is($t, 'another'); },
        ],

        $log_msg,

        $save_object,

        $object_is_live,

        $load_object,

        $object_is_live,

        [ sub { $t = $object -> $field; },
          sub { is($t, 'another'); },
        ],

        [ sub { $object -> delete; },
          qr{Objects can not be deleted from a repository},
        ],

        $object_is_live,

        $load_object,

        $object_is_live,

        sub { $object -> $field('foo'); },

        $log_msg,

        $save_object,


#    eval {
#        $other_object = $factory -> new(test => (object_id => $object_id));
#    };
#      
#    $e = $@; diag($e) if $e;
#    ok($other_object && !$e, "Loading object");
#
#    eval {
#        $t = $other_object -> revision;
#        $t = join(".", unpack("U*", $t));
#    };
#    $e = $@; diag($e) if $e;
#    is($t, "1.3");
#
#    eval {
#        $t = $other_object -> $field;
#    };
#
#    $e = $@; diag($e) if $e;
#    is($t, "foo");

    ## 24

        sub { $object -> $field("bar"); },

        $log_msg,

        $save_object,

        $load_object,

        [ sub { $t = $object -> revision; },
          sub { is($t, "1.4"); },
        ],

        [ sub { $t = $object -> $field; },
          sub { is($t, 'bar'); },
        ],

        [ sub {
            $object = undef;
            #main::diag("Factory: $$factory;  object_id: $object_id");
            $object = $$factory -> new(test => (object_id => "name=$object_id, revision=1.1"));
            #main::diag("Loaded $object");
          },
          sub { is($object -> revision, '1.1'); },
          sub { is($object -> $field, 'other'); },
        ],

        $load_object,

        [ sub { for(my $i = 5; $i < 100; $i++) { 
                    main::diag(" ... $i") if $i % 10 == 0; 
                    $object -> $field("$i"); 
                    $object -> log("test $i"); 
                    $object -> save; 
                } 
              },
          sub { is($object -> revision, "1.99"); },
        ],

        $load_object,

        sub { $object -> $field("This\nis\nmulti\nline\ntext"); },

        $log_msg,

        $save_object,


        $load_object,

        [ sub { $t = $object -> $field },
          sub { is($t, "This\nis\nmulti\nline\ntext"); },
        ],

        sub { $object -> $field("This\nis\nsome\nother\ntext"); },

        $log_msg,

        $save_object,
#) : ( ) ),

        $load_object,

        [ sub { $t = $object -> $field },
          sub { is($t, "This\nis\nsome\nother\ntext"); },
        ],

        sub { $object -> $field("This\nis\nsome\nlong\nlist\nof\nthings"); },

        $log_msg,

        $save_object,

        $load_object,

        [ sub { $t = $object -> $field },
          sub { is($t, "This\nis\nsome\nlong\nlist\nof\nthings"); },
        ],

        sub { $object -> $field("This\nlong\nlist\nof\nis\nsome\nthings"); },

        $log_msg,

        $save_object,

        $load_object,

        [ sub { $t = $object -> $field },
          sub { is($t, "This\nlong\nlist\nof\nis\nsome\nthings"); },
        ],
    );
}

1;

package My::Rep::Foo;

use Gestinanna::POF::Repository qw(Foo);



1;
