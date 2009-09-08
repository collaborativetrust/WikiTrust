package WikiTrust;

use constant DEBUG => 0;

use strict;
use warnings;
use DBI;
use Error qw(:try);
use Apache2::RequestRec ();
use Apache2::Const -compile => qw( OK );
use CGI;
use CGI::Carp;
use IO::Zlib;

use constant SLEEP_TIME => 3;
use constant NOT_FOUND_TEXT_TOKEN => "TEXT_NOT_FOUND";

our %methods = (
	'edit' => \&handle_edit,
	'vote' => \&handle_vote,
	'gettext' => \&handle_gettext,
    );


sub handler {
  my $r = shift;
  my $cgi = CGI->new($r);

  my $dbh = DBI->connect(
    $ENV{WT_DBNAME},
    $ENV{WT_DBUSER},
    $ENV{WT_DBPASS}
  );

  my $result = "";
  try {
    my ($pageid, $title, $revid, $time, $userid, $method);
    $method = $cgi->param('method');
    if (!$method) {
	$method = 'gettext';
	if ($cgi->param('vote')) {
	    $method = 'vote';
	} elsif ($cgi->param('edit')) {
	    $method = 'edit';
	}
	# old parameter names
	$pageid = $cgi->param('page') || 0;
	$title = $cgi->param('page_title') || '';
	$revid = $cgi->param('rev') || -1;
	$time = $cgi->param('time') || '';
	$userid = $cgi->param('user') || -1;
    } else {
	# new parameter names
	$pageid = $cgi->param('pageid') || 0;
	$title = $cgi->param('title') || '';
	$revid = $cgi->param('revid') || -1;
	$time = $cgi->param('time') || '';
	$userid = $cgi->param('userid') || -1;
    }

    throw Error::Simple("Bad method: $method") if !exists $methods{$method};
    my $func = $methods{$method};
    $result = $func->($revid, $pageid, $userid, $time, $title, $dbh);
  } otherwise {
    my $E = shift;
    print STDERR $E;
  };
  $r->content_type('text/plain');
  $r->print($result);
  return Apache2::Const::OK;
}


sub mark_for_coloring {
  my ($page, $page_title, $dbh) = @_;
  my $select_sth = $dbh->prepare(
    "SELECT page_title FROM wikitrust_queue WHERE page_title = ? AND"
    . " processed <> 'processed'"
  ) || die $dbh->errstr;
  # This doesn't seem safe.  What if another entry appears and gets
  # marked as 'processing' between these two statements.  This could
  # lead to multiple eval_online_wiki processes running, I think.
  my $ins_sth = $dbh->prepare(
    "INSERT INTO wikitrust_queue (page_id, page_title) VALUES (?, ?) " 
    . "ON DUPLICATE KEY UPDATE requested_on = now(), processed = 'unprocessed'"
  ) || die $dbh->errstr;
  $select_sth->execute(($page_title)) || die $dbh->errstr;
  if (!($select_sth->fetchrow_arrayref())){
    $ins_sth->execute(($page, $page_title)) || die $dbh->errstr;
  }
  #Not a transaction right now! old code: $dbh->commit;
}

sub handle_vote {
  my ($rev, $page, $user, $time, $page_title, $dbh) = @_;

  my $sth = $dbh->prepare("INSERT INTO wikitrust_vote (revision_id, page_id, "
    . "voter_id, voted_on) VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE "
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
  return "good"
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
    my ($pageid, $revid) = @_;
    my $path = $ENV{WT_COLOR_PATH};
    return undef if !defined $path;
    my $page_str = sprintf("%012d", $pageid);
    my $rev_str = sprintf("%012d", $revid);
    for (my $i = 0; $i <= 3; $i++) {
        $path .= "/" . substr($page_str, $i*3, 3);
    }
    $path .= "/" . substr($rev_str, 6, 3);
    $path .= "/" . $page_str . "_" . $rev_str . ".gz";
    return $path;
}

sub fetch_colored_markup {
  my ($page_id, $rev_id, $dbh) = @_;

  my $median = get_median($dbh);

  my $file = util_getRevFilename($page_id, $rev_id);
  if ($file) {
    warn "fetch_colored_markup: file=[$file]\n" if DEBUG;
    throw Error::Simple("Unable to read file($file)") if !-r $file;
    my $fh = IO::Zlib->new();
    $fh->open($file, "rb") || die "open($file): $!";
    my $text = '';
    while (!$fh->eof()) {
	$text .= <$fh>;
    }
    $fh->close();
    return $median.",".$text;
  }

  my $sth = $dbh->prepare ("SELECT revision_text FROM "
      . "wikitrust_colored_markup WHERE "
      . "revision_id = ?") || die $dbh->errstr;
  my $result = NOT_FOUND_TEXT_TOKEN;
  $sth->execute($rev_id) || die $dbh->errstr;
  if ((my $ref = $sth->fetchrow_hashref())){
    $result = $median.",".$$ref{'revision_text'};
  }
  return $result;
}

sub handle_edit {
  my ($rev, $page, $user, $time, $page_title, $dbh) = @_;
  mark_for_coloring($page, $page_title, $dbh);
  return "good"
}

sub handle_gettext {
  my ($rev, $page, $user, $time, $page_title, $dbh) = @_;
  
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

  # Text may or may not have been found, but it's all the same now.
  return $result;
}

1;
