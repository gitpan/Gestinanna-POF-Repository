use Test::More;

# do things load?

my @classes = qw(
    Gestinanna::POF::Repository
    Gestinanna::POF::Repository::Object
    Gestinanna::POF::Repository::Tag
    Gestinanna::POF::Repository::Description
);

plan tests => scalar(@classes);

foreach my $class (@classes) {
    eval "require $class;";
    ok(!$@, "Requiring $class");
}
