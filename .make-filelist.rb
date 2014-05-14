$EXCLUDE_PATH = %w[
. .. backup backup1 backup2 backup-system lost+found tmp temp rubikitch.lost
.cache w3mtmp proxy-cache trash proc RCS.old
autom4te.cache blib _build .bzr .cdv cover_db CVS _darcs ~.dep ~.dot .git .hg ~.nib .pc ~.plst RCS SCCS _sgbak .svn
]

# Exclude regexps (backup files, core files, and so on)
$EXCLUDE_REGEXP = Regexp.union(/~$/, /\#.+\#$/, /[._].*\.swp$/, /core\.\d+$/, # from ack-grep
  /\.(?:elc|o)$/, /,v$/)

# Set default directories to collect
$LS_DIRS = [
  "~/emacs/init.d", "~/emacs/lisp", "~/.emacs.d", "~/emacs",
  "~/gdgd", "~/memo", "~/book", "~/src", "~/public_html", "~/private_html", "~",
  "/m/usr/share/emacs/23.1.50", "/m/usr/share/emacs/site-lisp", "/m/log/emacswikipages",
  "/m/home/local/lib/ruby19", "/m/home/local/lib/ruby", "/m/log/compile/ruby19", "/m/log/compile/ruby18",
  "/m/log/compile", "/m/log", "/m/home/local", "/m/home/nobackup",
  "/m/usr", "/etc", "/m/home/archives", "/m/l/var",
  "/"
]
