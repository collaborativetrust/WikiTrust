#!/usr/bin/perl

my $last = undef;
while(<>) {
    my ($key, $val) = split(' ', $_, 2);
    $last = $val if $key eq 'ARTICLE';
    print $last if $key eq 'REVS' && $val+0 > 0;
}
exit(0);

