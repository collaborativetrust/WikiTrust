package WikiTrust;

#
#
#

use DBI;
use strict;

my $sleep_time_sec = 3;
our $not_found_text_token = "TEXT_NOT_FOUND";
our $find_revs_on_disk = 0;

sub get_not_found_text_token{
  return $not_found_text_token;
}

sub mark_for_coloring {
  my ($page, $page_title, $dbh) = @_;
  my $select_sth = $dbh->prepare(
    "SELECT page_title FROM wikitrust_queue WHERE page_title = ? AND"
    . " processed <> 'processed'"
  );
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
  if ($find_revs_on_disk){
    return $not_found_text_token;
  } else {
    my $sth = $dbh->prepare ("SELECT revision_text FROM "
      . "wikitrust_colored_markup WHERE "
      . "revision_id = ?");
    my $result = $not_found_text_token;
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
  if ($result eq $not_found_text_token){
    # If the revision is not found among the colored ones,
    # we mark it for coloring,
    # and it wait a bit, in the hope that it got colored.
    mark_for_coloring($page, $page_title, $dbh);
    sleep($sleep_time_sec);
    # Tries again to get it, to see if it has been colored.
    $result = fetch_colored_markup($rev, $dbh);
  }

  if ($result eq $not_found_text_token){
    # No: we will have to wait until it gets colored.  For now, we report not found.
    return $not_found_text_token;
  } else {
    return $result;
  }
}

1;
