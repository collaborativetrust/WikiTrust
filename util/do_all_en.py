#!/usr/bin/python

# Evaluates all the English Wiki

import commands, sys

source_dir = "/home/share/splitwikis/enwiki-20070206-pages-meta-history_split/"
dest_dir   = "/home/share/splitstats/enwiki-20070206-pages-meta-history/"
cmd_dir    = "../"
files = commands.getoutput ("ls " + source_dir).split ()

# Now processes each file, unless it already exists in the destination.
for source_file in files:
    # gets the filename root, called f
    file_root = (source_file.split ("."))[0]
    # and makes the file names
    dest_file = file_root + ".stats"
    full_source_file = source_dir + source_file
    full_dest_file   = dest_dir   + dest_file

    # Tries to process the file if it does not exist
    (missing, _) = commands.getstatusoutput ("test -f " + full_dest_file)
    if missing:
        # Creates or touches the file
        commands.getoutput ("touch " + full_dest_file)
        # Now tries to get a lock on the file
        (lock, _) = commands.getstatusoutput ("lockfile-create --retry 0 " + full_dest_file)
        if not lock:
            # Ok, it's not locked
            print "Processing", file_root
            sys.stdout.flush ()
            (err, s) = commands.getstatusoutput ("gunzip -c " + full_source_file + " | " + cmd_dir + "evalwiki > " + full_dest_file)
            if err > 0:
                print "*** Error:", err, s
                sys.stdout.flush ()
            # Removes the lock
            commands.getoutput ("lockfile-remove " + full_dest_file)
    # Ok, the file is processed.
