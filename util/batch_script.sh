# Computing the stats (can be parallelized):
/bin/gunzip -c /home/luca/wiki-data/enwiki/wiki-00100000.xml.gz \
    | ./evalwiki -compute_stats -si ~/wiki-data/enwork/stats/wiki-00100000.stats

# Sorting the statistics:
./combinestats -outfile ~/wiki-data/enwork/sorted_stats/all_sorted.stat \
    -outdir ~/wiki-data/enwork/sorted_stats_temp/ ~/wiki-data/enwork/stats/

# Computing the reputations:
./generate_reputation -use_reputation_cap -use_nix -nix_interval 100000 \
    -local_global_algo -u ~/wiki-data/enwork/reps/rep_history.txt \
    -write_final_reps ~/wiki-data/enwork/sorted_stats/all_sorted.stat
