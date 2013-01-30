#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use Time::HiRes qw(gettimeofday tv_interval);

my %words = ();
my $filename = $ARGV[0] || "words";
my $load_file_attempts = $ARGV[1] || 15;
my $find_attemts = $ARGV[2] || 100;

my ($load_average_time, $find_average_time);

for(1..$load_file_attempts){
    my $start_time = [gettimeofday()];
    loadfile($filename);
    my $diff_mks = int(tv_interval($start_time) * 1000000);
    $load_average_time = $load_average_time?int(($load_average_time+$diff_mks)/2):$diff_mks;
    print "."
}
print "\nLoad average time ${load_average_time} mks, ${find_attemts} load attemts \n";

while(1){
    print "enter word:\n";
    my $line = <STDIN>;
    chomp($line);
    last if $line eq 'quit';
    my $ret;
    for(1..$find_attemts){
        my $start_time = [gettimeofday()];
        $ret = finder($line);
        my $diff_mks = int(tv_interval($start_time) * 1000000);
        $find_average_time = $find_average_time?int(($find_average_time+$diff_mks)/2):$diff_mks;
    }
    print $ret."\n";
    print "Average find time: ${find_average_time} mks, ${find_attemts} find attempts \n";
    undef($find_average_time)
}

sub loadfile{
    %words = ();
    open(DATA, shift) or die "cannot open file";
    while(<DATA>){chomp; push @{$words{join("",sort(split(//,lc($_))))}},$_}
    close(DATA);
}

sub finder{
    my $line = shift;
    my $key = join("",sort(split(//,lc($line))));
    my $anagrams = $words{$key};
    $anagrams?"Found anagrams: ".join(", ", grep {lc($_) ne lc($line)} @$anagrams):"not found";
}
