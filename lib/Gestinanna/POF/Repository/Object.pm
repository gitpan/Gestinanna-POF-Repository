package Gestinanna::POF::Repository::Object;

use Carp;

use strict;
no strict 'refs';

use base qw(Gestinanna::POF::Alzabo);
use Algorithm::Diff;
use Data::Serializer;
use Params::Validate qw(:types);
#use version;

__PACKAGE__ -> valid_params (
    tag_path   => { type => ARRAYREF, optional => 1},
    revision => { type => SCALAR, optional => 1 },
);

sub _encode_rev {
    shift;
    #main::diag("Encoding : " . join(".", @_));
    #return version -> new(join(".", @_)) if @_ % 2 == 0;
    return join(".", @_);
}

sub _decode_rev {
    #main::diag("Decoding : $_[1]");
    return unless defined $_[1];
    return split(/\./, "" . $_[1]);
}

sub version_cmp {
    #my $a = ref($_[0]) ? $_[0] : split(/\./, $_[0]);
    #my $b = ref($_[1]) ? $_[1] : split(/\./, $_[1]);
    my($a, $b) = @_[0,1];

    my($i, $r);

    for($i = 0; $i < @$a && $i < @$b; $i++) {
        next unless $r = $a -> [$i] <=> $b -> [$i];
        return $r;
    }

    return 1 if @$b < @$a;
    return -1 if @$a < @$b;
    return 0;
}

sub table { $_[0] -> repository };

sub log {
    my $accessor if 0; # static code reference
    $accessor ||= $_[0] -> make_accessor('log');
    #my $self = shift;

    $_[0] -> {_added_log_message} = 1 if @_;
    goto &$accessor;
    #$self -> SUPER::log(@_);
}

# override this to change what columns are revision-controlled
# default: any blob columns named `data' or `data_.*'
sub data_columns {
    my $self = shift;
    return $self -> {_data_columns} if $self -> {_data_columns};
    #main::diag "Schema: $$self{alzabo_schema}\n";
    return [ ] unless defined $self -> {alzabo_schema};
    my $table = $self -> {alzabo_schema} -> table($self -> table);
    $self -> {_data_columns} =
        [ grep { $_ =~ m{^data(_.*)?$} } map { $_ -> name } grep { $_ -> is_character || $_ -> is_blob } $table -> columns ];
}

# does this document/object already exist in the repository 
# -- not does this revision already exist
sub is_live {
    my $self = shift;
    #main::diag("is_live: _row: " . ($self -> {_row} && $self -> {_row} -> is_live) . " _prev_row: " . ($self -> {_prev_row} && $self -> {_prev_row} -> is_live));
    return 1 if $self -> {_row}      && $self -> {_row}      -> is_live
             || $self -> {_prev_row} && $self -> {_prev_row} -> is_live;
    return 0;
}

sub tags {
    my $self = shift;

    return $self -> tag_class -> tags(%$self);
}

sub process_object_id {
    my($self, $objectid, %params) = @_;

    #main::diag("$self -> process_object_id($objectid, ...)");
    #warn("$self -> process_object_id($objectid, ...)\n");

    $self -> SUPER::process_object_id($objectid, %params);

    #if(!$self -> SUPER::process_object_id($objectid, %params)) {
    if(!defined $self -> {name}) {
        ##main::diag("SUPER::process_object_id returned <false>");
        return 0 if $objectid =~ m{=};

        #warn("Setting self->{name} to $objectid");
        $self -> {name} = $objectid;
    }
    #warn("self -> {name}: $$self{name}");

    if(!exists $self -> {revision} || !defined $self -> {revision}) {
        my $t;        
        if(defined $self -> {tag_path}) {
            # find a revision based on tag
            #my $class = $self -> tag_class; #(ref $self || $self) . "::Tag";
            my $tag_type = $self -> {_factory} -> get_object_type($self -> tag_class);
            my $pk = $self -> table;
            foreach my $tag (@{$self -> {tag_path}}) {
                #warn "Looking at tag [$tag]\n";
                $t = $self -> {_factory} -> new ( $tag_type => (
                      tag => $tag,
                      object_id => $self -> {name},
                ));
                last if $t -> is_live;
            }
        }
        if($t && $t -> is_live) {
            $self -> {revision} = $t -> revision;
        }
        else {
            #my @revisions = sort { $a cmp $b } $self -> revisions;
            my @revisions = map { join(".", @$_) } sort { version_cmp($a, $b) } map { [ split /\./ ] } $self -> revisions;
            #main::diag("Revisions: " . join("; ", @revisions));
            $self -> {revision} = pop @revisions if @revisions;
        }
    }

    #warn("self -> {name}: $$self{name}");
    return 1 if exists $self -> {revision} && defined $self -> {revision};
    return 0;
}

sub make_accessor {
    my $self = shift;
    my $field = shift;

    my %data_columns = map { $_ => undef } @{$self -> data_columns};
    #my($field) = $AUTOLOAD =~ /::([^:]+)$/;
    #my $super_field = "SUPER::$field";
    #Carp::cluck("Looking at $field: " . join(", ", keys %data_columns));
    #main::diag("Caller: ", join("; ", caller));
    my $super_accessor = $self -> SUPER::make_accessor($field);

    return $super_accessor unless exists $data_columns{$field};

    #return sub {
    ##    $_[0] -> post_load if @_ > 1; # on modification
    #    goto &$super_accessor;
    #} unless exists $data_columns{$field};

    return sub {
        $_[0] -> post_load; # on reading or modification
        goto &$super_accessor;
    };
}

use Data::Dumper;
sub load {
    my $self = shift;

    #main::diag "Before load: " . Data::Dumper -> Dump([$self]);

    $self -> SUPER::load unless $self -> {_row};

    #delete $self -> {_prev_row};
}

sub post_load {
    my $self = shift;

    return if $self -> {_prev_row};

# we want to delay the next bit until we need it
    #main::diag("$self -> post_load");
    #main::diag "After load: " . Data::Dumper -> Dump([$self]);

    #main::diag "Revision (after SUPER::load): " . $self -> {revision};

    if($self -> {_row} && $self -> {_row} -> is_live) {
        # this will create a new entry in the RDBMS when it is saved
        my $row = $self -> {_prev_row} 
                = $self -> {_row};

        eval {
            $self -> {_row} = $self -> {alzabo_schema} -> table($self -> table) -> potential_row(
                values => {
                    name => $self -> name,
                    revision => $row -> select('revision'),
                },
            );
        };
        croak "$@" if $@;

        my $target_rev = $self -> revision;
        # [0..1], [2..3], ... == [branch, revision]
        # we need to trace back to the first revision in the closest 
        # branch for the original document and then apply all succeeding 
        # patches


        my $table = $self -> {alzabo_schema} -> table($self -> table);
        my $name_col = $table -> column('name');
        my $rev_col  = $table -> column('revision');
        #my $branch = substr($target_rev, 0, -1);
        my @bits = $self -> _decode_rev($target_rev);
        my $branch = $self -> _encode_rev(@bits[0..$#bits-1]);

        my %revisions = %{$self -> revisions_hash($branch)};
        #my @revs = sort { $a cmp $b } keys %revisions;
        my @revs = map { join(".", @$_) } 
                   sort { version_cmp($a, $b) } 
                   grep { version_cmp($_, \@bits) <= 0 } 
                   map { [ split /\./ ] } 
                   keys %revisions;

        #warn("Keys to \%revisions: " . join(", ", keys %revisions) . "\n");
        #warn("Revisions: " . join(", ", @revs) . "\n");

        my @columns = @{$self -> data_columns};
        #warn "Data columns: ", join(", ", @columns), "\n";
        if(@revs) {
            #warn "init: revision is of type: ", ref($revisions{$revs[0]}), "\n";
            $self -> initialize_diffs($revisions{$revs[0]});
            shift @revs;

            #main::diag __LINE__ . ": Revision list: ". join(",", map { join(".", $self -> _decode_rev($_)) } @revs);

            $self -> apply_diff($revisions{$_}) for grep { defined $revisions{$_} } @revs;

            $self -> finish_diffs;
        }
    }
    else {
        # this is the initial document
    }

    my @columns = @{$self -> data_columns};
    foreach my $col (@columns) {
        $self -> {_prev_obj} -> {$col} = $self -> {$col};
    }

    #warn "data: ", Data::Dumper -> Dump([ [ @{$self}{@columns} ] ]);

    #main::diag "revision: ". $self -> revision;
    #warn "load finished\n";
}

#sub get_revision { return version -> new($_[0] -> {revision}); }

#sub set_revision { $_[0] -> {revision} = "" . $_[1]; }

sub save {
    # we create a new revision and save the diff

    # we want to move actor->object_id tag when we save
    # here we do merges and branches
# we probably want to lock on $self -> name  before going through with this
    my $self = shift;

#    warn "Entering ", __PACKAGE__, "::save for $self\n";

    croak "Must set a log message before saving" unless $self -> {_added_log_message};

    #return unless $self -> {_prev_row}; # no change made to revision-controlled data
    $self -> post_load;

    #unless($self -> {_prev_row}) {
    #    $self -> post_load;
    #}

    my @columns = @{$self -> data_columns};
    my %data;
    @data{@columns} = @$self{@columns};

#    $self -> post_load;

    my $target_rev = $self -> {revision};
    my @target_rev_a = $self -> _decode_rev($target_rev);
    my $branch = $self -> _encode_rev(@target_rev_a[0..$#target_rev_a-1]);

    #warn("bits: <" . join("><", @bits) . ">\n");
    #warn("target rev: $target_rev; branch: $branch\n");

    my $table = $self -> {alzabo_schema} -> table($self -> table);
    my $name_col = $table -> column('name');
    my $rev_col  = $table -> column('revision');

    unless($self -> {_merge} || $self -> {_branch}) {
        # we have the next revision in our current branch
        # but, if there is a more recent revision than ours, we want to create a new branch
        my @revs = $self -> revisions($branch);

        #main::diag "Revs: ". join(", ", map { join(".", $self -> _decode_rev($_)) } @revs);
        #$self -> {_branch} = 1 if $revs[0] gt $target_rev;
        $self -> {_branch} = 1 if defined $revs[0] && version_cmp([split(/\./, $revs[0])], \@target_rev_a) > 0;
    }


   #use Data::Dumper;
    #warn Data::Dumper -> Dump([\%data]);

    if($self -> {_branch}) {
        # we want our revision, but a new branch
        # depends on how many other branches are out there
        my $new_rev = $self -> revision;
        # find greatest branch

        my @revs = $self -> revisions($new_rev);

        #warn "Found ", scalar(@revs), " revisions\n";
        $new_rev .= '.' . $self -> _encode_rev(($self -> _decode_rev($revs[0]))[-2] + 1, 1);
        #main::diag "Moving from $revs[0] to $new_rev\n";
        $self -> {revision} = $new_rev;
        # no diffs for the first entry in a branch
    }
    else {
        if(defined $target_rev && length($target_rev)) {
            my @bits = $self -> _decode_rev($target_rev);
            $self -> {revision} = $branch . "." . ($bits[$#bits] + 1);
            #$self -> {revision} = $branch . $self -> _encode_rev($self -> _decode_rev($target_rev))+1);
            # we also need to construct diffs
            #warn "Data columns: ", join(" ", @columns), "\n";
            # TODO: optimize $obj out (perhaps)
            my $obj = { };
            foreach my $col (@columns) {
                $obj->{$col} = $self -> {_prev_obj} -> {$col};
            }
            my $diffs = $self -> create_diff($obj);
            #warn Data::Dumper -> Dump([$diffs]), "\n";
            my @d = grep { defined $diffs->{$_} } keys %$diffs;
            #main::diag "There are ". scalar(@d). " fields with differences\n";
            unless(@d) {
                $self -> {revision} = $target_rev;
                return;
            }
            foreach my $k (keys %$diffs) {
                $self -> {$k} = $diffs -> {$k};
            }
        }
        else {
            #main::diag "Creating new object";
            $self -> {revision} = "1.1";  # new object
        }
    }

    delete $self -> {$self -> repository_key}; # force a new one

    eval {
        #main::diag "_row ref before save: " . ref($self -> {_row});
        #main::diag "  revision: $$self{revision}\n";
        $self -> {modify_timestamp} = undef;

        $self -> SUPER::save;

        #main::diag "_row ref after save: " . ref($self -> {_row});

        if($self -> {_row} && $self -> {_row} -> is_live) {
            $self -> {_prev_row} = $self -> {_row};

            $self -> {_row} = $self -> {alzabo_schema} -> table($self -> table) -> potential_row(
                values => {
                    name => $self -> {name},
                    revision => $self -> {revision},
                },
            );

            foreach my $col (@columns) {
                $self -> {_prev_obj} -> {$col} = $data{$col};
            }
        }
    };

    my $e = $@;

    #main::diag "Saved revision: ". join(".", $self -> _decode_rev($self -> revision));
    #main::diag "Saved columns: ". Data::Dumper -> Dump([ { map { $_ => $self->{$_} } @columns } ]);

    @$self{@columns} = @data{@columns};

    if($e) {
        croak $e;
        return 0;
    }
    else {
        delete $self -> {_added_log_message};
    }

    #warn "Returning 1 from ", __PACKAGE__, "::save for $self\n";
    return 1;
}

sub revisions {
    my($self, $min, $max) = @_;

    return unless $self -> {name};  # no name, no revisions

    my $table = $self -> {alzabo_schema} -> table($self -> table);
    my $name_col = $table -> column('name');
    my $rev_col  = $table -> column('revision');
    my $branch;

#    warn "revisions($min, $max)\n";

    if(defined $min && length($min) % 2 == 1) {
        $branch = $min;
        $min = "1"; #pack("U*", 1);
        $max = undef;
    }
    else {
        $max = $self -> {revision} unless defined $max;
        my @bits = $self -> _decode_rev($max);
        $branch = $self -> _encode_rev(@bits[0..$#bits-1]);
        $max = $bits[$#bits] if @bits > 1;
        if(defined $min) {
            @bits = $self -> _decode_rev($min);
            $min = $bits[$#bits] if @bits > 1;
        }
        else {
            $min = '1';
        }
    }

    #main::diag("$self -> revisions($min, $max)");

    my $rev;
    my @revs;

    if(@_ > 1) {
#        $revs = $table -> rows_where(
#            where => [
#                [ $name_col, '=', $self -> name ],
#                [ $rev_col, 'LIKE', $self -> escape_sql_meta($branch) . ('_' x (2 - length($branch)%2)) ],
#            ],
#        );
#        while(my $rev = $revs -> next) {
#            #main::diag __LINE__ . ": Found " . join(".", $self -> _decode_rev($rev -> select('revision')));
#            push @revs, $rev -> select('revision');
#        }
        if(defined $max) {
            foreach my $r ( $min .. $max ) {
                #main::diag("Looking at $branch.$r");
                eval {
                    $rev = $table -> one_row(
                        where => [
                            [ $name_col, '=', $self -> {name} ],
                            [ $rev_col, '=', $branch . ".$r" ],
                        ],
                    );
                };
                #push @revs, version -> new($branch . $r) unless $@;
                push @revs, $branch . $r unless $@;
            }
        }
        else {
            my $r = $min;
            eval {
                $rev = $table -> one_row(
                    where => [
                        [ $name_col, '=', $self -> {name} ],
                        [ $rev_col, '=', $branch . ".$r" ],
                    ],
                );
            };
            while($rev && !$@) {
                #main::diag("Looking at $branch.$r");
                push @revs, $branch . ".$r";
                $r++;
                eval {
                    $rev = $table -> one_row(
                        where => [
                            [ $name_col, '=', $self -> {name} ],
                            [ $rev_col, '=', $branch . ".$r" ],
                        ],
                    );
                };
            }
        }
    }
    else {
        my $revs = $table -> rows_where(
            where => [
                [ $name_col, '=', $self -> {name} ],
            ],
        );
        while(my $rev = $revs -> next) {
            my $r = $rev -> select('revision');
            #main::diag("Looking at $r");
            next unless $r =~ tr[.][.] == 1; # one dot, two parts
            #main::diag __LINE__ . ": Found " . join(".", $self -> _decode_rev($r));
            push @revs, $r;
        }
    }

    #sort { $b cmp $a } keys %{ +{ map { $_ => undef } @revs } };
    map { join(".", @$_) } sort { version_cmp($b, $a) } map { [ split /\./ ] } keys %{ +{ map { $_ => undef } @revs } };
}

sub revisions_hash {
    my($self, $branch) = @_;

    my $table = $self -> {alzabo_schema} -> table($self -> table);
    my $name_col = $table -> column('name');
    my $rev_col  = $table -> column('revision');

    my $revs;
    my %revs;

    if(defined $branch) {
        my $length = 1 + ($branch =~ tr[.][.]) + ($branch =~ tr[.][.])%2; # desired length (in dots)

        $revs = $table -> rows_where(
            where => [
                [ $name_col, '=', $self -> name ],
                [ $rev_col, 'LIKE', "$branch.%" ],
            ],
        );
        while(my $rev = $revs -> next) {
            my $r = $rev -> select('revision');
            #main::diag __LINE__ . ": Found " . join(".", $self -> _decode_rev($r));
            #main::diag("Looking at rev $r in branch $branch (length $length)");
            next unless $length == ($r =~ tr[.][.]);
            $revs{$r} = $rev;
        }
    }
    else {
        $revs = $table -> rows_where(
            where => [
                [ $name_col, '=', $self -> name ],
            ],
        );
        while(my $rev = $revs -> next) {
            my $r = $rev -> select('revision');
            next unless 1 == $r =~ tr[.][.];
            #main::diag __LINE__ . ": Found " . join(".", $self -> _decode_rev($r));
            $revs{$r} = $rev;
        }
    }

    return \%revs;
}


sub delete {
    croak "Objects can not be deleted from a repository";
}

sub initialize_diffs {
    my($self, $obj) = @_;

    my @columns = @{$self -> data_columns};
    if($obj) {
        foreach my $col (@columns) {
            $self -> {$col} = [ split(/\n/, $obj -> select($col)) ];
        }
    }
    else {
        foreach my $col (@columns) {
            $self -> {$col} = [ split(/\n/, $self -> {$col}) ];
        }
    }
}

sub _freeze {
    my($self, $data) = @_;

    my $serializer = Data::Serializer -> new(
        compress => 1,
    );
#    warn "$self -> _freeze: ", Data::Dumper -> Dump([$data]), "\n";
    return $serializer -> serialize($data);
}

sub _thaw {
    my($self, $string) = @_;

    return undef unless defined $string;
    my $serializer = Data::Serializer -> new(
        compress => 1,
    );
    my $data = $serializer -> deserialize($string);
#    warn "$self -> _thaw: ", Data::Dumper -> Dump([$data]), "\n";
    return $data; #$serializer -> deserialize($string);
}

sub apply_diff {
    my($self, $obj) = @_;

    # here, we apply the diffs in $obj to the current object data
    # we need to move to context diffs so we can support merging more easily
    #warn "Applying diff for revision ", join(".", $self -> _decode_rev($obj -> select("revision"))), "\n";
    foreach my $col (@{$self -> data_columns}) {
        my $diff = $obj -> select($col);
        next unless defined $diff;
        $diff = $self -> _thaw($diff);
        #warn "Diff: ", Data::Dumper -> Dump([$diff]);
        #my $offset = 0;
        #my $hunk_offset;
        foreach my $hunk (@{$diff}) {
            splice @{$self -> {$col}}, $hunk -> [0], $hunk -> [1], @{$hunk}[2 .. $#{$hunk}];
        }
    }

    #warn "finished applying diff\n";
}

# if \n changes, the diffs could change :/
# need to add context
# $self -> create_diff($old, $new)
# or 
# $self -> create_diff($old)
sub create_diff {
    my($self, $obj, $obj2) = @_;

    $obj2 ||= $self;

    my @columns = @{$self -> data_columns};
    my $diffs = { };
    foreach my $col (@columns) {
       #warn "Looking at column `$col'\n";
        my $diff = Algorithm::Diff::diff(
            [ split(/\n/, $obj ->{$col}) ],   # previous version
            [ split(/\n/, $obj2 -> {$col}) ] # current version
        );
        my @splices;
        my $pos_difference = 0;
        foreach my $hunk (@{$diff}) {
            my @pos = grep { $_ -> [0] eq '+' } @{$hunk};
            my @neg = grep { $_ -> [0] eq '-' } @{$hunk};
            my $being_replaced = scalar(@neg);
            my @replacing = map { $_ -> [2] } @pos;
            my $position = @pos ? ($pos[0] -> [1])
                                : ($neg[0] -> [1] + $pos_difference);
            $pos_difference += scalar(@pos) - $being_replaced;
            push @splices, [ $position, $being_replaced, @replacing ];
        }
        #foreach my $hunk (@{$diff}) {
        #    @{$hunk} = sort { $b->[0] cmp $a->[0] || $a->[1] <=> $b->[1] } @{$hunk};
        #}
        #warn "There are ", scalar(@{$diff}), " hunks\n";
        if(0 != @splices) {
            $diffs -> {$col} = $self -> _freeze(\@splices);
        }
        else {
            $diffs -> {$col} = undef;
        }
    }

    return $diffs;
}

sub create_context_diff {
    my($self, $obj, $obj2, $c) = @_;

    if(@_ < 4) {
        $c = $obj2;
        $obj2 = $self;
    }

    my @columns = @{$self -> data_columns};
    my $diffs = { };
    foreach my $col (@columns) {
        my $diff = Algorithm::Diff::sdiff(
            [ split(/\n/, $obj ->{$col}) ],   # previous version
            [ split(/\n/, $obj2 -> {$col}) ] # current version
        );

        my $in_u = 0;
        my @pos;
        my $lim = scalar(@{$diff});
        my @cnt;
        for(my $i = 0; $i < $lim; $i++) {
            if($diff -> [$i] -> [0] eq '+') {
                push @cnt, $cnt[-1]+0;
            }
            else {
                push @cnt, $cnt[-1]+1;
            }

            if($diff->[$i]->[0] eq 'u') {
                push @pos, $i - 1 unless $in_u || !$i;
                $in_u = 1;
            }
            else {
                push @pos, $i if $in_u || !$i;
                $in_u = 0;
            }
        }

        push @pos, $lim-1 if @pos%2 == 1;

        if($c) {
            for(my $i = 0; $i < @pos; $i+=2) {
                $pos[$i] -= $c;
                $pos[$i+1] += $c;

                $pos[$i] = 0 if $pos[$i] < 0;
                $pos[$i+1] = $lim-1 if $pos[$i+1] > $lim-1;
            }
        }

        my $i = 0;
        while($i < @pos-2) {
            splice @pos, $i+1, 2 while @pos-2 && $pos[$i+1] > $pos[$i+2];  
            $i+=2;
        }

        # @pos now has (first, last) pairs for context-surrounded sets of changes

        $diffs -> {$col} = [ ];
        while(@pos) {
            my($s, $e) = splice @pos, 0, 2;
            push @{$diffs -> {$col}}, [ [ $cnt[$s], $cnt[$e] + 1 ], [ $s, $e + 1 ], [ @{$diff}[$s .. $e] ] ];
        }
    }

    return $diffs;
}

sub finish_diffs {
    my $self = shift;

    foreach my $col (@{$self -> data_columns}) {
        $self -> {$col} = join("\n", @{$self -> {$col} || []});
    }
}

sub branch {
    # create a new branch when this revision is saved
    my $self = shift;
    $self -> {_branch} = 1;
}

sub unbranch {
    # undoes $self -> branch;
    my $self = shift;
    delete $self -> {_branch};
}
    
1;

__END__

=head1 NAME

Gestinanna::POF::Repository::Object - objects within the repository

=head1 SYNOPSIS

 $object = $factory => new(foo => (object_id => '/name') );

 $data = $object -> data();
 $object->data($new_data);

 $object -> log($log_message);

 $object -> save()

=head1 DESCRIPTION

The object_id is the name of the object (C<object_id> is an alias 
for C<name> in the constructor).  If C<revision> is given, then 
that revision is loaded.  If no revision is given but a tag_path 
is supplied, then the tags are searched until one is found 
pointing to a revision of the named object.

Saving an object will not modify any existing row in the RDBMS 
but will create a new row storing the differences between that 
version of the object and the previous version.  If a more recent 
version of the object exists in the branch, then a new branch is 
created.  Otherwise, a new revision is created in the same branch.
A new branch may be forced at the next save by calling the C<branch>
method.

Revision numbers are Perl ordinal strings.  For example, C<v1.1> 
is the first revision of the first branch.  C<v1.1.1.1> is the 
first revision of the first branch of the first revision of the 
first branch.  Etc.  This is an added feature in Perl 5.6.0.

=head1 METHODS

In addition to the usual L<Gestinanna::POF::Alzabo|Gestinanna::POF::Alzabo> 
methods, the following support this class's role in the repository.

=head2 branch

 $object -> branch

Force a branch at the next C<save()>.  Multiple calls do not create 
multiple branches without intervening saves.

=head2 data_columns

 @columns = $object -> data_columns

This returns an array of column names that should be treated as under 
revision control.  Override this method if you do not want the default 
columns (C<data>, and any column name beginning with C<data_>).

=head2 revisions

 @revisions = $object -> revisions()
 @revisions = $object -> revisions($branch)
 @revisions = $object -> revisions($min, $max)

This method is fairly DWIMy.

Given zero or one arguments, this will return similar results as 
C<revisions_hash> except it will not try to load each object.

If both C<$min> and C<$max> are given, then the branch is taken to be
the branch indicated by C<$max>.  If C<$min> is undefined, 
then it is assumed to be the first revision in the branch, with the 
last revision being the revision indicated by C<$max>.

N.B.: This method does not return the actual objects, but a list of 
revision ordinals.  To retrieve an object, use the following code:

 $object = $factory -> new(foo => (
      object_id => '/name-of-object',
      revision => $revision
 ) );

Where C<$revision> is one of the revision ordinal strings returned by 
the C<revisions()> and C</name-of-object> is the name of the object.

=head2 revisions_hash

 %revisions = $object -> revisions();
 %revisions = $object -> revisions($branch);

This will return a hash mapping the revision ordinal strings to objects 
for a particular branch.  If no branch is given, then this will assume 
all branches.  Be careful.  If there are a lot of revisions, this can 
consume a fair amount of memory.

=head2 unbranch

 $object -> unbranch

Forget the forced branching at the next save.  This undoes what C<branch()> does.
This will not prevent a branching if the there is a newer revision than 
the object's revision (e.g., if the object is revision 1.2 and there is 
already a 1.3, then nothing will prevent the creation of 1.2.x.x on the 
next save--1.2 won't leap frog 1.3 to create 1.4).

=head1 BUGS

=head2 Iterators for revision lists

It would be nice to return an iterator instead of an entire list of 
revisions.  This code was developed before L<Gestinanna::POF::Iterator|Gestinanna::POF::Iterator> 
came into existance.  This may happen in the near future, in which 
case the behavior of the C<revisions> and C<revisions_hash> methods 
will either change completely or depend on the calling context (scalar 
returns iterator).

=head1 AUTHOR

James G. Smith, <jsmith@cpan.org>
            
=head1 COPYRIGHT

Copyright (C) 2002-2003 Texas A&M University.  All Rights Reserved.
            
This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

