#!/usr/bin/perl

# 
# Copyright (c) 2010 B. Thomas Adler
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
# 1. Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
# 
# 3. The names of the contributors may not be used to endorse or promote
# products derived from this software without specific prior written
# permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# 

use strict;
use warnings;

use Date::Manip;
use open qw(:std :utf8);
use constant DB_ENGINE => "mysql";
use constant BASE_DIR => "./";
use constant INI_FILE => BASE_DIR . "db_access_data.ini";

use constant COLOR_PAGE => 1;

use DBI;
use LWP::UserAgent;
use HTTP::Request::Common qw(GET);
use JSON;
use URI::Escape;
use lib '../../remote/analysis';
use WikiTrust;

my %db = readINI(INI_FILE);

my $dbname = join(':', "DBI", DB_ENGINE, 
	"database=".$db{db},
	"host=".$db{host});

my $dbh = DBI->connect($dbname, $db{user}, $db{pass},
	{ RaiseError => 1, AutoCommit => 1 });


my $sth_pid1 = $dbh->prepare('SELECT page_id FROM wikitrust_page WHERE page_title = ?');
my $sth_pid2 = $dbh->prepare('SELECT page_id FROM page WHERE page_title = ?');
my $sth_rid = $dbh->prepare('SELECT revision_id FROM wikitrust_revision WHERE page_id = ? and time_string > ?');

my $oldtime = getOldTimestamp();

while (my $title = <>) {
    chomp($title);
    my $pageid = 0;;
    $pageid ||= getPageidFDb($sth_pid1, $title);
    $pageid ||= getPageidFDb($sth_pid2, $title);
    $pageid ||= getPageidFWpapi($title);
    if (!defined $pageid || $pageid == 0) {
	die "No pageid for \"$title\"";
    }
    $sth_rid->execute($pageid, $oldtime);
    next if $sth_rid->rows() > 0;
    warn "Coloring pageid $pageid, \"$title\"\n";
    COLOR_PAGE && WikiTrust::mark_for_coloring($pageid, $title, $dbh);
}
exit(0);

sub readINI {
    my $ini = shift @_;
    my (%values);
    open(INI, "<$ini") || die "open($ini): $!";
    while (<INI>) {
	chomp;
	next if m/^\s*\[/;
	if (m/^\W*(\w+)\s*=\s*(\w+)\W*(#.*)?$/) {
	    $values{$1} = $2;
	}
    }
    close(INI);
    return %values;
}

sub getPageidFWpapi {
    my $title = shift @_;
warn "Unknown page '$title'; looking up on web.\n";
    my $url = 'http://en.wikipedia.org/w/api.php?action=query&format=json'
		.'&titles='.uri_escape_utf8($title);
    my $ua = LWP::UserAgent->new;
    $ua->agent('Mozilla/4.0 (compatible; MSIE 5.0; Windows 95)');
    my $req = GET $url;
    my $res = $ua->request($req);
    die $res->status_line if !$res->is_success;
    my $answer = decode_json($res->content);
		# don't need decoded_content, since decode_json will decode
    die "no data" if !defined $answer;
    my @pages = keys %{$answer->{query}->{pages}};
    return shift @pages;
}

sub getPageidFDb {
    my $sth = shift @_;
    my $title = shift @_;
    $sth->execute($title) || die "Couldn't execute: ".$sth->errstr;
    my @data = $sth->fetchrow_array();
    return $data[0] if @data > 0;
    return undef;
}

sub getOldTimestamp {
    my $weekago = ParseDate("last month");
    my $timestamp = UnixDate($weekago, "%Y%m%d000000");
    return $timestamp;
}
