# This script is not meant to be run!
# It is simply a collection of commands, to be used as a reminder.

# Computing the statistics (can be parallelized):
/bin/gunzip -c /home/luca/wiki-data/enwiki/wiki-00100000.xml.gz \
    | batch/analysis/evalwiki -compute_stats -si ~/wiki-data/enwork/stats/wiki-00100000.stats

/bin/gunzip -c /home/luca/wiki-data/enwiki/wiki-00100220.xml.gz \
    | batch/analysis/evalwiki -compute_stats -si ~/wiki-data/enwork/stats/wiki-00100220.stats

# Sorting the statistics (use -n_digits 5 for small wikis):
batch/analysis/combinestats \
    -bucket_dir ~/wiki-data/enwork/buckets/ \
    -input_dir ~/wiki-data/enwork/stats/ \
    -n_digits 4 -use_subdirs

# Computing the reputations (whole histories):
batch/analysis/generate_reputation -u ~/wiki-data/enwork/reps/rep_history.txt \
    -buckets -gen_exact_rep ~/wiki-data/enwork/buckets/ 

# Computing the reputations (only the final ones):
batch/analysis/generate_reputation -u ~/wiki-data/enwork/reps/rep_history.txt \
    -buckets ~/wiki-data/enwork/buckets/ -gen_exact_rep -write_final_reps

# ONLY IF NEEDED, remove previous version trees and sql.
rm -rf /home/luca/wiki-data/enwork/sigtree
rm -rf /home/luca/wiki-data/enwork/revtree
rm -f /home/luca/wiki-data/enwork/sql/*

# Generating the colored pages and the sql file for batch-online:
batch/analysis/evalwiki -trust_for_online \
    -historyfile ~/wiki-data/enwork/reps/rep_history.txt \
    -rev_base_path ~/wiki-data/enwork/revtree \
    -sig_base_path ~/wiki-data/enwork/sigtree \
    -n_sigs 8 \
    -d ~/wiki-data/enwork/sql \
    /home/luca/wiki-data/enwiki/wiki-00100000.xml.gz

batch/analysis/evalwiki -trust_for_online \
    -historyfile ~/wiki-data/enwork/reps/rep_history.txt \
    -rev_base_path ~/wiki-data/enwork/revtree \
    -sig_base_path ~/wiki-data/enwork/sigtree \
    -n_sigs 8 \
    -d ~/wiki-data/enwork/sql \
    /home/luca/wiki-data/enwiki/wiki-00100220.xml.gz

# Load the xml files in the wiki db:
cd test-scripts 
python load_data.py --clear_db /home/luca/wiki-data/enwiki/wiki-00100000.xml /home/luca/wiki-data/enwiki/wiki-00100220.xml
# Or simply:
python load_data.py --clear_db /home/luca/wiki-data/enwiki/wiki-00100000.xml

# clears the old wikitrust information:
python truncate_wikitrust_tables.py

# Loads the reputations in the wiki db:
python load_reputations.py --clear_db ~/wiki-data/enwork/reps/rep_history.txt

# Loads the sql in the wiki db:
mysql wikidb -u wikiuser -p < ~/wiki-data/enwork/sql/wiki-00100000.sql

# Then, try to edit some pages.  If you want to run the command 
# eval_online_wiki manually, do something like:
