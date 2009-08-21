# Sample commands.
v="v3.0.pre1"
git-archive --format=tar --prefix=WikiTrust/ $v > WikiTrust-$v.tar
cd ..
tar --append --file=WikiTrust/WikiTrust-$v.tar  WikiTrust/eval_online_wiki
tar --append --file=WikiTrust/WikiTrust-$v.tar  WikiTrust/vote_revision
cd WikiTrust
gzip WikiTrust-$v.tar
