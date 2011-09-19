#!/usr/bin/env perl

$ghc = '/usr/bin/ghc'; # where is ghc

@ghc_options  = qw(-Wall);   # e.g. ('-fglasgow-exts')
@ghc_packages = qw(ghc);          # e.g. ('QuickCheck')    
### the following should not been edited ###
my $src = $ARGV[0];
my @command = ($ghc, qw{--make -fbyte-code}, $src, @ghc_options, join(' -package ','', @ghc_packages));
open(MESSAGE, "@command 2>&1 |");
while (<MESSAGE>) {
    chomp;
    if (/(^\S+\.l?hs)(:\d*:\d*:)\s?(.*)$/) {
	print "\n$1$2$3";
    } elsif (/^\s+(.+)$/) {
	print "$1 ";
    }
}
close MESSAGE;
