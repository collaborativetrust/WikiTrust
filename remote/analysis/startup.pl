use Apache::DBI;
use DBI;
use Cwd;
use Compress::Zlib;
use strict;

unless (defined $ENV{MOD_PERL}){
  die "\$ENV{MOD_PERL} not set!";
}

## ToDo Bo -- is there a better way to do this?
use lib qw(/mnt/bigspace/ipye/git/wikitrust/remote/analysis);

## Setup the DB.
Apache::DBI->connect_on_init(
  $ENV{WT_DBNAME},
  $ENV{WT_DBUSER},
  $ENV{WT_DBPASS},
  {AutoCommit => 1}
);

## Require the WikiTrust module
use WikiTrust;

1;
