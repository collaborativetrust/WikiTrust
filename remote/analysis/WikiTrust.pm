package WikiTrust;

use constant DEBUG => 0;

use strict;
use warnings;
use DBI;
use Error qw(:try);
use Apache2::RequestRec ();
use Apache2::Const -compile => qw( OK );
use Apache2::Connection ();
use CGI;
use CGI::Carp;
use IO::Zlib;
use Compress::Zlib;
use Cache::Memcached;
use Data::Dumper;
#use Time::HiRes qw(gettimeofday tv_interval);

use constant QUEUE_PRIORITY => 10; # wikitrust_queue is now a priority queue.
use constant SLEEP_TIME => 3;
use constant NOT_FOUND_TEXT_TOKEN => "TEXT_NOT_FOUND";

our %methods = (
	'edit' => \&handle_edit,
	'vote' => \&handle_vote,
	'gettext' => \&handle_gettext,
	'wikiorhtml' => \&handle_wikiorhtml,
	'sharehtml' => \&handle_sharehtml,
	'stats' => \&handle_stats,
	'miccheck' => \&handle_miccheck,
    );

our $cache = undef;	# will get initialized when needed

sub handler {
  my $r = shift;
  my $cgi = CGI->new($r);

  my $dbh = DBI->connect(
    $ENV{WT_DBNAME},
    $ENV{WT_DBUSER},
    $ENV{WT_DBPASS},
    { RaiseError => 1, AutoCommit => 1 }
  );

  my $result = "";
  try {
    my ($pageid, $title, $revid, $time, $username, $method);
    $method = $cgi->param('method');
    if (!$method) {
      $method = 'gettext';
      if ($cgi->param('vote')) {
        $method = 'vote';
      } elsif ($cgi->param('edit')) {
        $method = 'edit';
      }
    }

    throw Error::Simple("Bad method: $method") if !exists $methods{$method};
    my $func = $methods{$method};
    $result = $func->($dbh, $cgi, $r);
  } otherwise {
    my $E = shift;
    print STDERR $E;
    $r->no_cache(1);
    $r->content_type('text/plain; charset=utf-8');
    $r->print('ERROR detected.');
  };
  return $result;
}

sub get_stdargs {
    my $cgi = shift @_;
    my ($pageid, $title, $revid, $time, $username);
    my $method = $cgi->param('method');
    if (!$method) {
	# old parameter names
	$pageid = $cgi->param('page') || 0;
	$title = $cgi->param('page_title') || '';
	$revid = $cgi->param('rev') || -1;
	$time = $cgi->param('time') || timestamp();
	$username = $cgi->param('user') || '';
    } else {
	# new parameter names
	$pageid = $cgi->param('pageid') || 0;
	$title = $cgi->param('title') || '';
	$revid = $cgi->param('revid') || -1;
	$time = $cgi->param('time') || timestamp();
	$username = $cgi->param('username') || '';
    }
    return ($revid, $pageid, $username, $time, $title);
}

sub timestamp {
    my @time = localtime();
    return sprintf("%04d%02d%02d%02d%02d%02d", $time[5]+1900, $time[4]+1, $time[3], $time[2], $time[1], $time[0]);
}

sub secret_okay {
    my $cgi = shift @_;
    my $secret = $cgi->param('secret') || '';
    my $true_secret = $ENV{WT_SECRET} || '';
    return ($secret eq $true_secret);
}

# To fix atomicity errors, wrapping this in a procedure.
sub mark_for_coloring {
  my ($page, $page_title, $dbh) = @_;

  my $sth = $dbh->prepare(
    "INSERT INTO wikitrust_queue (page_id, page_title, priority) VALUES (?, ?, ?)"
	." ON DUPLICATE KEY UPDATE requested_on = now()"
  ) || die $dbh->errstr;
  $sth->execute($page, $page_title, QUEUE_PRIORITY) || die $dbh->errstr;
}

sub handle_vote {
  my ($dbh, $cgi, $r) = @_;
  my ($rev, $page, $user, $time, $page_title) = get_stdargs($cgi);

  $r->content_type('text/plain');

  # can't trust non-verified submitters
  $user = 0 if !secret_okay($cgi);

  my $sel_sql = "SELECT voter_name FROM wikitrust_vote WHERE voter_name = ? AND revision_id = ?";
  if (my $ref = $dbh->selectrow_hashref($sel_sql, {}, ($user, $rev))){
    $r->print('dup');
    return Apache2::Const::OK;
  }

  my $sth = $dbh->prepare("INSERT INTO wikitrust_vote (revision_id, page_id, "
    . "voter_name, voted_on) VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE "
    . "voted_on = ?") || die $dbh->errstr;
  $sth->execute($rev, $page, $user, $time, $time) || die $dbh->errstr;
  # Not a transaction: $dbh->commit();

  if ($sth->rows > 0){
    mark_for_coloring($page, $page_title, $dbh);
  }

  # Token saying things are ok
  # Votes do not return anything. This just sends back the ACK
  # that the vote was recorded.
  # We could change this to be the re-colored wiki-text, reflecting the
  # effect of the vote, if you like.
  $r->print('good');
  return Apache2::Const::OK;
}

sub get_median {
  my $dbh = shift;
  my $median = 0.0;
  my $sql = "SELECT median FROM wikitrust_global";
  if (my $ref = $dbh->selectrow_hashref($sql)){
    $median = $$ref{'median'};
  }
  return $median;
}

sub util_getRevFilename {
  my ($pageid, $blobid) = @_;
  my $path = $ENV{WT_BLOB_PATH};
  return undef if !defined $path;

  my $page_str = sprintf("%012d", $pageid);
  my $blob_str = sprintf("%09d", $blobid);

  for (my $i = 0; $i <= 3; $i++){
    $path .= "/" . substr($page_str, $i*3, 3);
  }
  if ($blobid >= 1000){
    $path .= "/" . sprintf("%06d", $blobid);
  }
  $path .= "/" . $page_str . "_" . $blob_str . ".gz";
  return $path;
}

# Extract text from a blob
sub util_extractFromBlob {
  my ($rev_id, $blob_content) = @_;
  my @parts = split(/:/, $blob_content, 2);
  my $offset = 0;
  my $size = 0;
  while ($parts[0] =~ m/\((\d+) (\d+) (\d+)\)/g){
    if ($1 == $rev_id){
      $offset = $2;
      $size = $3;
      return substr($parts[1], $offset, $size);
    }
  }
  throw Error::Simple("Unable to find $rev_id in blob");
}

sub fetch_colored_markup {
  my ($page_id, $rev_id, $dbh) = @_;

  my $median = get_median($dbh);

  ## Get the blob id
  my $sth = $dbh->prepare ("SELECT blob_id FROM "
      . "wikitrust_revision WHERE "
      . "revision_id = ?") || die $dbh->errstr;
  my $blob_id = -1;
  $sth->execute($rev_id) || die $dbh->errstr;
  if ((my $ref = $sth->fetchrow_hashref())){
    $blob_id = $$ref{'blob_id'};
  } else {
    return NOT_FOUND_TEXT_TOKEN;
  }

  my $file = util_getRevFilename($page_id, $blob_id);
  if ($file) {
    warn "fetch_colored_markup: file=[$file]\n" if DEBUG;
    throw Error::Simple("Unable to read file($file)") if !-r $file;

    my $fh = IO::Zlib->new();
    $fh->open($file, "rb") || die "open($file): $!";
    my $text = join("", $fh->getlines());
    $fh->close();
    return $median.",".util_extractFromBlob($rev_id, $text);
  }

  my $new_blob_id = sprintf("%012d%012d", $page_id, $blob_id);
  $sth = $dbh->prepare ("SELECT blob_content FROM "
      . "wikitrust_blob WHERE "
      . "blob_id = ?") || die $dbh->errstr;
  my $result = NOT_FOUND_TEXT_TOKEN;
  $sth->execute($new_blob_id) || die $dbh->errstr;
  if ((my $ref = $sth->fetchrow_hashref())){
    my $blob_c = Compress::Zlib::memGunzip($$ref{'blob_content'});
    $result = $median.",".util_extractFromBlob($rev_id, $blob_c);
  }
  return $result;
}

sub handle_edit {
  my ($dbh, $cgi, $r) = @_;
  my ($rev, $page, $user, $time, $page_title) = get_stdargs($cgi);
  # since we still need to download actual text,
  # it's safe to not verify the submitter
  mark_for_coloring($page, $page_title, $dbh);
  $r->content_type('text/plain');
  $r->print('good');
  return Apache2::Const::OK;
}

sub util_ColorIfAvailable { 
  my ($dbh, $page, $rev, $page_title) = @_;
  my $result = fetch_colored_markup($page, $rev, $dbh);
  if ($result eq NOT_FOUND_TEXT_TOKEN){
    # If the revision is not found among the colored ones,
    # we mark it for coloring,
    # and it wait a bit, in the hope that it got colored.
    mark_for_coloring($page, $page_title, $dbh);
    sleep(SLEEP_TIME);
    # Tries again to get it, to see if it has been colored.
    $result = fetch_colored_markup($page, $rev, $dbh);
  }
  return $result;
}

sub handle_gettext {
  my ($dbh, $cgi, $r) = @_;
  my ($rev, $page, $user, $time, $page_title) = get_stdargs($cgi);
  my $result = util_ColorIfAvailable($dbh, $page, $rev, $page_title);
  if ($result eq NOT_FOUND_TEXT_TOKEN) {
    $r->headers_out->{'Cache-Control'} = "max-age=" . 60;
  } else {
    $r->headers_out->{'Cache-Control'} = "max-age=" . 5*24*60*60;
  }
  $r->content_type('text/plain; charset=utf-8');
  $r->print($result);
  return Apache2::Const::OK;
}

sub handle_wikiorhtml {
  my ($dbh, $cgi, $r) = @_;
  my ($rev, $page, $user, $time, $page_title) = get_stdargs($cgi);
  my $cache = util_getCache();
  my $data = $cache->get($rev);
  if (defined $data && ref $data eq 'HASH') {
    $r->headers_out->{'Cache-Control'} = "max-age=" . 30*24*60*60;
    $r->content_type('text/plain; charset=utf-8');
    $r->print('H');
    $r->print($data->{html});
    return Apache2::Const::OK;
  }

  my $result = util_ColorIfAvailable($dbh, $page, $rev, $page_title);
  if ($result eq NOT_FOUND_TEXT_TOKEN) {
    $r->headers_out->{'Cache-Control'} = "max-age=" . 15;
  } else {
    $r->headers_out->{'Cache-Control'} = "max-age=" . 30;
  }
  $r->content_type('text/plain; charset=utf-8');
  $r->print('W');
  $r->print($result);
  return Apache2::Const::OK;
}

sub handle_stats {
  my ($dbh, $cgi, $r) = @_;

  $r->content_type('text/plain; charset=utf-8');

  my $sth = $dbh->prepare ("SELECT * FROM wikitrust_queue") || die $dbh->errstr;
  $sth->execute() || die $dbh->errstr;
  $r->print("Processing queue for WikiTrust dispatcher:\n\n");
  my $found_header = 0;
  while ((my $ref = $sth->fetchrow_hashref())){
    if (!$found_header) {
      foreach (sort keys %$ref) { $r->print(sprintf("%20s ", $_)); }
      $r->print("\n");
      foreach (sort keys %$ref) { $r->print('='x21); }
      $r->print("\n");
      $found_header = 1;
    }
    foreach (sort keys %$ref) { $r->print(sprintf("%20s ", $ref->{$_})); }
    $r->print("\n");
  }
  $r->print("\n\nMemCache statistics:\n\n");
  my $cache = util_getCache();
  my $stats = $cache->stats();
  foreach my $key (keys %$stats) {
    $r->print("\t$key = ". Dumper($stats->{$key}) . "\n");
  }
  return Apache2::Const::OK;
}

# miccheck: Called by the plugin to see if this language is
# actually implemented.  Basically, it tests that the DNS
# is configured and working correctly from user to us.
sub handle_miccheck {
  my ($dbh, $cgi, $r) = @_;

  $r->headers_out->{'Cache-Control'} = "max-age=" . 30*24*60*60;
  $r->content_type('text/plain; charset=utf-8');
  $r->print("OK");
  return Apache2::Const::OK;
}

sub util_getCache {
    return $cache if defined $cache;
    my $server = $ENV{WT_MEMCACHED} || '127.0.0.1:11211';
    return new Cache::Memcached {
	    'servers' => [ $server ],
	    'debug' => DEBUG,
	    'compress_threshold' => 1000,
	    'namespace' => $ENV{WT_NAMESPACE} || '',
	};
}

sub handle_sharehtml {
    my ($dbh, $cgi, $r) = @_;

    my $myhtml = $cgi->param('myhtml') || '';
    my $revid = $cgi->param('revid') || 0;
    if (($myhtml eq '') || ($revid == 0)) {
      $r->content_type('text/plain');
      $r->print('Thanks, but no thanks.');
      return Apache2::Const::OK;
    }

    my $c = $r->connection;
    my $remoteip = $c->remote_ip();

warn "Received share from $remoteip";

    my $cache = util_getCache();

    my $changed = 0;
    my $data = $cache->get($revid);
    if (!defined $data || (ref $data ne 'HASH')) {
	$changed = 1;
	$data = {
	    'html' => $myhtml,
	    'src' => { $remoteip => 1 },
	    'srcs' => 1,
	    };
    }
    if ($data->{srcs}<5 && !exists $data->{src}->{$remoteip}) {
	$changed = 1;
	$data->{srcs}++;
	$data->{src}->{$remoteip} = 1;
    }

    $cache->set($revid, $data) if $changed;

    $r->content_type('text/plain');
    $r->print('Thanks.');
    return Apache2::Const::OK;
}

1;
