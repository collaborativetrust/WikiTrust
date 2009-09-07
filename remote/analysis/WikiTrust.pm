package WikiTrust;

#
#
#

use strict;
use warnings;
use DBI;

use constant SLEEP_TIME => 3;
use constant NOT_FOUND_TEXT_TOKEN => "TEXT_NOT_FOUND";
use constant FIND_REVS_ON_DISK => 0;


sub mark_for_coloring {
  my ($page, $page_title, $dbh) = @_;
  my $select_sth = $dbh->prepare(
    "SELECT page_title FROM wikitrust_queue WHERE page_title = ? AND"
    . " processed <> 'processed'"
  );
  # This doesn't seem safe.  What if another entry appears and gets
  # marked as 'processing' between these two statements.  This could
  # lead to multiple eval_online_wiki processes running, I think.
  my $ins_sth = $dbh->prepare(
    "INSERT INTO wikitrust_queue (page_id, page_title) VALUES (?, ?) " 
    . "ON DUPLICATE KEY UPDATE requested_on = now(), processed = 'unprocessed'"
  );
  my $rv = $select_sth->execute(($page_title));
  if ($rv && !($select_sth->fetchrow_arrayref())){
    $ins_sth->execute(($page, $page_title));
  }
  $dbh->commit;
}

sub handle_vote {
  my ($rev, $page, $user, $time, $page_title, $dbh) = @_;

  my $sth = $dbh->prepare("INSERT INTO wikitrust_vote (revision_id, page_id, "
    . "voter_id, voted_on) VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE "
    . "voted_on = ?");
  my $rv = $sth->execute($rev, $page, $user, $time, $time);
  $dbh->commit();

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

sub get_median{
  my $dbh = shift;
  my $median = 0.0;
  my $sql = "SELECT median FROM wikitrust_global";
  if (my $ref = $dbh->selectrow_hashref($sql)){
    $median = $$ref{'median'};
  }
  return $median;
}

sub fetch_colored_markup {
  my ($rev_id, $dbh) = @_;
  if (FIND_REVS_ON_DISK){
    return NOT_FOUND_TEXT_TOKEN;
  } else {
    my $sth = $dbh->prepare ("SELECT revision_text FROM "
      . "wikitrust_colored_markup WHERE "
      . "revision_id = ?");
    my $result = NOT_FOUND_TEXT_TOKEN;
    my $rv = $sth->execute(($rev_id));
    if ($rv && (my $ref = $sth->fetchrow_hashref())){
      my $median = get_median($dbh);
      $result = $median.",".$$ref{'revision_text'};
    }
    return $result;
  }
}

sub handle_edit {
  my ($rev, $page, $user, $time, $page_title, $dbh) = @_;
  mark_for_coloring($page, $page_title, $dbh);
  return "good"
}

sub handle_text_request {
  my ($rev, $page, $user, $time, $page_title, $dbh) = @_;
  
  my $result = fetch_colored_markup($rev, $dbh);
  if ($result eq NOT_FOUND_TEXT_TOKEN){
    # If the revision is not found among the colored ones,
    # we mark it for coloring,
    # and it wait a bit, in the hope that it got colored.
    mark_for_coloring($page, $page_title, $dbh);
    sleep(SLEEP_TIME);
    # Tries again to get it, to see if it has been colored.
    $result = fetch_colored_markup($rev, $dbh);
  }

  # Text may or may not have been found, but it's all the same now.
  return $result;
}

1;
