use Module::Build;

my $m = Module::Build -> new(
    module_name => 'Gestinanna::POF::Repository',
    #module_version_from => 'lib/Gestinanna.pm',
    license => 'perl',
    requires => {
        Algorithm::Diff => 1.15,
        Alzabo => 0.67,
        Gestinanna::POF => 0.06,
        Data::Serializer => 0,
    },
    recommends => {
        DBD::SQLite => 0,
    },
) -> create_build_script;
