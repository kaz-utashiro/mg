#!/usr/local/bin/perl
'di ';
'ds 00 \"';
'ig 00 ';
##
## mg: multi-line grep
##
## Copyright (c) 2001-2002 Kazumasa Utashiro <utashiro@srekcah.org>
## Copyright (c) 1994-2000 Kazumasa Utashiro <utashiro@iij.ad.jp>
## Internet Initiative Japan Inc.
## 3-13 Kanda Nishiki-cho, Chiyoda-ku, Tokyo 101-0054, Japan
##
## Copyright (c) 1991-1994 Kazumasa Utashiro
## Software Research Associates, Inc.
##
## Original: Mar 29 1991
;; $rcsid = q$Id: mg,v 5.0.1.2 2002/03/02 09:31:00 utashiro Exp $;
##
## EXAMPLES:
##	% mg 'control message protocol' rfc*.txt.Z	# line across search
##	% mg -nRTP '*.[sch]' 'struct vnode' /sys	# recursive search
##	% mg -o sockaddr /usr/include/sys/socket.h	# paragraph mode
##	% mg -Bc0,1 '@(#)' /lib/libc.a			# binary mode
##	% mg -iTB copyright /bin/*			# auto bin/text mode
##	% tset -IQS | mg -ec0 '(so|se)=[^:]+'		# matched part only
##	% echo $path | mg -Q mh				# highlighting
##
## Use and redistribution for ANY PURPOSE are granted as long as all
## copyright notices are retained.  Redistribution with modification
## is allowed provided that you make your modified version obviously
## distinguishable from the original one.  THIS SOFTWARE IS PROVIDED
## BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES ARE
## DISCLAIMED.
##

##
## perl4 on BSD/OS 3.[01] has wrong $] value, oh my...
##
$perl4 = ($] < 5) || (40000 < $]);

if ($perl4) {
    die "Sorry, this version of mg no longer support perl4.\n" .
	"Use version 2 seriese instead.\n";
}

##
## Match in 5.6 or later supports matched offset list: @-, @+
##
$has_matched_offset_list = (!$perl4) && ($] >= 5.006);

require('getopts.pl');
#require('usage.pl');
@opts =('i::ignore case',
	'l::list filename only',
	'n::print line number',
	'N::print byte offset',
	'h::do not display filenames',
	'w::word sensitive (foo becomes \\bfoo\\b)',
	'v:pattern:skip the line if matched with the pattern',
	'e::use pattern as regular expression (space is special)',
	'E::use pattern as regular expression completely',
	'r:pattern:specify search restriction pattern',
	'c:n[,n]|/\d+(,\d+)?/:print n tol/eol before/after matched line',
	'o::paragraph mode',
	'O:string:specify paragraph delimiter string',
	'g:lines|/^\d+$/:page mode',
	'C:chars:continuous characters',
	'u::underline matched string (except JIS)',
	'b::make bold (print twice) matched string (except JIS)',
	'Q::stand-out matched string (except JIS)',
	'Y::yield to -Q option even if stdout is not a terminal',
	'a::print whole file (no filename and line number)',
	's::output filename and line number separately',
	'2::duplicate line if multiple matching is occured',
	'R::search recursively',
	'D:level:descending directory level',
	'P:pattern:specify search file in wildcard (w/-R)',
	'V:pattern:specify exception file in wildcard (w/-R)',
	'F::follow symbolic link of directory (w/-R)',
	'T::search text file only',
	'B::search from binary file',
	'L::print formfeed before each matching',
	'S::get filenames from stdin',
	'm::print only line across matching',
	'M::print only multiple matched line',
	'f:file:file contains search pattern',
	'p:pattern:specify search pattern',
	'A::do not archive search',
	'Z::do not uncompress automatically',
	'J:string:convert newline in matched string to specified string',
	'0:digits:record separator by octal. only first record is searched',
	'W::slurp whole file contents at once',
	'G:#[,#]:maxreadsize and keepsize',
	'1::print first match only',
	'j:code:specify Japanese code of file (jis, sjis, euc)',
	'y::yield to use JIS X0208 alphabet character',
	'z:filter:set filter command',
	'x:exp:input filter (&mime: RFC1342 decoding)',
	'X:filter:output filter command',
	'd:flags:display info (f:file d:dir c:count m:misc s:stat)',
	'-:IHYUt:',
	't::-T takes only ascii files',
	'k::remember passphrase for PGP key',
	'I::ignore error', 'H::manual', 'U::show unused opts',
);
$opts = &Mkopts(@opts);
if ($ENV{'MGOPTS'}) {
    require('shellwords.pl');
    unshift(@ARGV, &shellwords($ENV{'MGOPTS'}));
};
&Getopts($opts) || &usage;
sub usage {
    @usage = &Usage($0, $opts, "pattern [ file ... ]", @opts);
    print "usage: mg [ -options ] pattern [ file... ]\n", pop(@usage);
    print "$rcsid\n" if $rcsid =~ /:/;
    exit 2;
}

&eval(join(' ', grep($_ = "\$db_$_++;", split(//, $opt_d))), $opt_d =~ /e/);

## show unused option characters
if ($opt_U) {
    $_ = join('','0'..'9',"\n",'a'..'z',"\n",'A'..'Z',"\n");
    eval "tr/$opts/./";
    die $_;
}

## show man pages
if ($opt_H) {
    exec "nroff -man $0 |" . ($ENV{'PAGER'} || 'more') . ' -s';
    die;
}

## setup delimitting character
if ($opt_C) {
    $delim = sprintf('[\\s%s]', quotemeta($opt_C));
} elsif ($opt_y && $opt_j eq 'euc') {
    $delim = '(\s|\\241\\241)';		# JIS X 0208 space
} else {
    $delim = '\s';
}
$delim .= $opt_w ? '+' : '*';
$rawdelim = quotemeta($delim);

$in  = join('|', (@in  = ('\e\$\@', '\e\$B')));
$out = join('|', (@out = ('\e\(J',  '\e\(B')));
$shiftcode   = '(' . join('|', @in, @out) . ')';
$optionalseq = '(' . join('|', @in, @out, $opt_C || '\s'). ')*';
$rawoptionalseq = quotemeta($optionalseq);

defined($opt_j) && ($opt_j eq 'is') && ($opt_j = 'jis');

sub mkpat {
    local($pat, $p) = @_;
    if (defined($opt_j)) {
	require('jcode.pl');
	&jcode'convert(*pat, $opt_j);
    }
    for (split(/$shiftcode/, $pat)) {
	if (/$in/o)  { $jis = 1; $p .= $optionalseq if $p; next; }
	if (/$out/o) { $jis = 0; next; }
	if ($jis)    { s/(..)/&jis($1)/eg; $p .= $_; next; }
	if ($opt_e or $opt_E) {
	    s{
		(\(\?\<[=!][^\)]*\))	# look-behind pattern
		|
		([\200-\377]?.)		# normal characters
	    }{
		if ($1) {
		    $1;
		} else {
		    length($2) > 1 ? &mb($2) : &asc($2)
		}
	    }egx;
	} else {
	    s/([\200-\377]?.)/length($1) > 1 ? &mb($1) : &asc($1)/eg;
	}
	$p .= $_;
    }
    $p =~ s/($rawdelim|$rawoptionalseq)+$//;
    length($p) ? $p : undef;
}

## make search pattern
if ($opt_f) {
    local(@opt_f) = `cat $opt_f`;
    @opt_f = grep { !/^#/ } @opt_f;
    for (@opt_f) { s/\n//; }
    $opt_p = join('|', grep($_ = &mkpat($_), @opt_f));
} else {
    defined $opt_p || defined($opt_p = shift(@ARGV)) || &usage;
    $opt_p = &mkpat($opt_p);
}

($opt_v, $opt_r) = (&mkpat($opt_v), &mkpat($opt_r));
($opt_P, $opt_V) = (&wildcard($opt_P), &wildcard($opt_V));
$opt_p = "\\b$opt_p\\b" if $opt_w && !$opt_E;

## some option have to be disabled when searching JIS string
if (defined($jis)) {
    for ('q', 'u', 'b') {
	local(*opt) = "opt_$_";
	$opt = 0, warn "-$_ is disabled.\n" if $opt;
    }
}

##
## make search functions
##
$_i = $opt_i ? 'i' : '';				# ignore case?
($check, $_g) = $opt_1 ? ('if', '') : ('while', 'g');	# first match?
$Q = pack('C', ord($Q) + 1) until index("$opt_p$opt_v$opt_r", $Q) < $[;

if ($has_matched_offset_list) {
    $offset = '$-[0]';
    $length = '$+[0] - $-[0]';
}
else {
    ##
    ## Perl5 has a function pos() which enable to get last matched
    ## position, so we can avoid to use $` and $& to eliminate to cost of
    ## using these variables.  To do that, whole pattern has to be caught
    ## by $1, which requires the pattern encloses by parentheses.
    ##
    ## However, enclosing the whole pattern by parenthese confuses the use
    ## of other parentheses in the original regular expression, because
    ## the order of parantheses incremented by one.  Following code fix
    ## the back-reference number in the form of \n.  I doubt this code
    ## works fine for arbitrary expression. (XXX)
    ##
    $opt_p =~ s/\\([1-9])\b/'\\' . ($1+1)/ge;
    $opt_p = "($opt_p)";
    $offset = 'pos($_) - length($1)';
    $length = 'length($1)';
}
&eval("sub search {
    local(*_, *array) = \@_;
    return m$Q$opt_p$Q$_i if \@_ < 2;
    $check (m$Q$opt_p$Q$_g$_i) {
	push(\@array, $offset, $length);
    }
}
sub match {
    local(*_, \$pattern) = \@_;
    m${Q}\$pattern${Q}$_i;
}");

if ($opt_Q && ($opt_Y || -t STDOUT)) {
    ($ql, $qr, $qd, $qe) = &sose;
}
$nlqsubs = $opt_n ? qq#s/\\n/"$qd\\n\$file".++\$line.":$qe"/ge#
		  : "s/\\n/$qd\\n\$file$qe/g";
$nlqsubs = q|s/\\n/++$line;"| . $opt_J . '"/ge' if defined $opt_J;
$nlsubs  = $opt_n ? 's/\\n/"\\n$file".++$line.":"/ge'
		  : 's/\\n/\\n$file/g';
&eval("sub nlsubs {
    local(*_, \$file, *line, \$matched) = \@_;
    \$matched ? $nlqsubs : $nlsubs;
}");

if ($effect = $opt_b || $opt_u) {
    @ul[1,2] = ("_\cH", "__\cH\cH");
    @bs[1,2] = ("\cH", "\cH\cH");
    &eval('sub effect {
	$_[$[] =~ s/([\\200-\\337]?.)/' .
	'$ul[length($1)].' x $opt_u . '$1.$bs[length($1)].' x $opt_b . '$1/ge;
    }'); 
}

$opt_O = $opt_o ? '\n\n' : '\n' unless defined($opt_O);
$opt_O =~ s/"/\\"/g;
&eval("\$rs = \"$opt_O\";");	# record separator
grep(eval "warn \"opt_$_=/\$opt_$_/\n\" if \$opt_$_;", split(//, 'pvPVCO'))
    if $db_m;

if (defined($_ = $opt_c)) {
    ($pre_c, $post_c) = /,/ ? split(',') : ($_, $_);
}
$pre_c = $opt_o ? -1 : 1 unless defined($pre_c);
$post_c = $opt_o ? -1 : 1 unless defined($post_c);

$ar_header_format = "A16 a12 a6 a6 a8 a10 a2";
($ar_name, $ar_date, $ar_uid, $ar_gid, $ar_mode, $ar_size, $ar_fmag) = (0..6);

$tar_header_format = "a100 a8 a8 a8 a12 a12 a8 a a100 a*";
($tar_name, $tar_mode, $tar_uid, $tar_gid, $tar_size,
    $tar_mtime, $tar_chksum, $tar_linkflag, $tar_linkname, $tar_pad) = (0..9);

unless ($opt_W) {
    local(%units);
    @units{'b', 'k', 'm'} = (512, 1024, 1024 * 1024);
    ($maxreadsize, $keepsize) = (1024 * 512, 1024 * 2);
    $opt_G =~ s/(\d+)([kbm])/$1 * $units{"\l$2"}/gei;
    if ($opt_G =~ m|([\d]+)\D*([\d]+)?|i) {
	$maxreadsize = $1+0 if $1;
	$keepsize = $2+0 if $2;
    }
    warn "maxreadsize = $maxreadsize, keepsize = $keepsize\n" if $db_m;
}

if ($opt_T) {
    local($quick_check, $binary_check) = $opt_t
	? ('/\0/', 'tr/\000-\007\013\016-\032\034-\037\200-\377/./*10>length')
	: ('/[\0\377]/', 'tr/\000-\007\013\016-\032\034-\037/./*10>length');
    &eval('sub binary {
	return 1 if $_[0] =~ ' . $quick_check . ';
	local($_) = substr($_[0], 0, 512);
	' . $binary_check . ';
    }');
}

push(@filter, 's/\.Z$//', 'zcat', 's/\.g?z$//', 'gunzip -c') unless $opt_Z;
push(@filter, split(':', $opt_z)) if $opt_z;

##
## Push PGP decrypt command for input filter.
## Currently pgp version 2 is hard coded, but it have to be fixed soon. XXX
##
if (defined $opt_k and not defined $opt_z) {
    push(@filter, 'pgp -f');
}

for ($_ = ''; @_ = splice(@filter, 0, 2); ) {
    unshift(@_, '1') if @_ == 1;
    $_[0] =~ /^\s*unlink\s*$/ && die "It's too dangerous!! \"$_[0]\"\n";
    $_ .= "\t\push(\@fil, '$_[1]'), next if $_[0];\n";
}
&eval('sub filter {
    local($file, @fil, $prev) = @_;
    local($_) = $file;
    while (($prev ne $_) && ($prev = $_)) {'. "\n" . $_ .'    }
    ($_ = join(" | ", @fil)) =~ s/{}/$file/g;
    $_;
}') if $_;

%zinfo = ('lzh', 'lha pq,   lha l,     \d\d:\d\d\s+(.+)',
	  'zip', 'unzip -p, unzip -l,  \d\d:\d\d\s+(.+)',
	  'zoo', 'zoo xpq,  zoo -list, \d\d:\d\d:\S+\s+\S+\s+(.+)');
$zsuffix = join('|', keys %zinfo);
sub zfiles {
    local($z, @z, *Z) = @_;	# Z will be closed on return
    open(Z, "$zlist $z|") || do { warn "$zlist: $!\n"; return undef; };
    /$zpattern/ && push(@z, "{$z}$+") while <Z>;
    ########## ^ don't put //o here!  $zpattern is set in &open_nextfile.
    @z;
}

if ($opt_x) {			# define input filter
    # do require/use once
    while ($opt_x =~ s/((require|use)\b[^;]+;?)//) {
	&eval($1);
    }
    &eval("sub input_filter { $opt_x }");
}
if ($opt_X) {			# open output filter
    open(STDOUT, "|$opt_X") || die "$opt_X: $!\n";
    $| = 1;
}

if ($opt_k) {
    use Fcntl;

    my $passfile = "/tmp/invisible_passfile_$$";
    my $passphrase;
    
    sysopen(PGPPASS, $passfile, O_RDWR|O_CREAT|O_EXCL, 0000)
	or die "sysopen $passfile: $!\n";
    unlink($passfile)
	or die "can't unlink $passfile: $!\n";
    fcntl(PGPPASS, F_SETFD, 0)
	or die "can't fcntl F_SETFD: $!\n";

    print STDERR "Enter PGP Passphrase> ";
    system "stty -echo";
    $passphrase = <STDIN>;
    system "stty echo";
    print STDERR "\n";

    syswrite(PGPPASS, $passphrase, length $passphrase);
    $passphrase =~ s/./\0/g;
    $passphrase = "";
    seek(PGPPASS, 0, 0);

    $ENV{PGPPASSFD} = fileno(PGPPASS);
}

$hash_t = 1;
$bin = $opt_B;
$dirend = "\0\0";
$p_all = !($opt_m || $opt_M);
$NL = $opt_a ? "\377#+%&^=(*-!" x 2 : "\n";
$showfname = $opt_l || !$opt_h && (@ARGV > 1 || $opt_R || $opt_S);
$/ = !defined($opt_0) ? undef : $opt_0 =~ /^0+$/ ? '' : pack('C', oct($opt_0));
$* = 1;

open(SAVESTDIN, '<&STDIN');
push(@ARGV, '-') unless @ARGV || $opt_S;

sub open_nextfile {
    local($/, $file) = ("\n");
    while (defined($file = shift(@ARGV)) ||
	   defined($file = $opt_S && <SAVESTDIN>)) {
	$file =~ s/\n$// if $opt_S;
	if ($file eq $dirend) {		# end of current directory
	    shift(@dirstack);
	    next;
	}
	$file = $dirstack[$[] . $file;
	next if $opt_V && $file =~ /$opt_V/o;

	stat($file);
	next if -p _ || -c _ || -b _;	# skip FIFO and device files
	if (-d _ && $opt_R && (!-l $file || $opt_F)) {
	    next if defined($opt_D) && @dirstack >= $opt_D;
	    ($ent = -@ARGV) += unshift(@ARGV, &getdirent($file), $dirend) - 1;
	    unshift(@dirstack, $file . ($file =~ m|/$| ? '' : '/'));
	    &chash, warn '=' x @dirstack, "> $file ($ent entries).\n" if $db_d;
	    next;
	}
	if (!$opt_A && $file =~ /\.($zsuffix)$/o) {
	    ($zext, $zlist, $zpattern) = split(/,\s*/, $zinfo{$1}, 3);
	    unshift(@ARGV, &zfiles($file));
	    $showfname++ unless $opt_h;
	    next;
	}
	if ($file =~ /({(.+\.($zsuffix))}(.+)$)/o) {		# zip & zoo
	    $file = $1;		# XXX dirty hack for -R
	    open(STDIN, '-|') || exec("$zext '$2' '$4'") || die "$zext: $!\n";
	} else {
	    open(STDIN, $file) || do {
		$err = 2, &warn("$file: $!\n") unless -l $file; next;
	    };
	}
	if (defined(&filter) && ($filter = &filter($file))) {
	    seek(PGPPASS, 0, 0) if fileno(PGPPASS);
	    open(STDIN, '-|') || exec($filter) || die "$filter: $!\n";
	}
	if ($filter && $db_p) {
	    printf STDERR "Filter: \"$filter\" < %s.\n", $file;
	}
	return $file;
    }
    undef;
}

sub main {

    while (defined($file = &open_nextfile)) {
	$_ = '';
	$size = -1;
	$pad = 0;
	if (!$ar && !$opt_A && read(STDIN, $_, 8) && ($_ eq "!<arch>\n")) {
	    $arfile = $file;
	    $ar = 1;
	    redo;
	}
	elsif ($ar && read(STDIN, $_, 60) &&
	       &arheader(@header = unpack($ar_header_format, $_))) {
	    redo if !($size = $header[$ar_size]);
	    $file = "($arfile)$header[$ar_name]";
	    $pad = $size % 2;
	    $_ = '';
	}
	elsif (!$ar && !$opt_A && read(STDIN, $_, 512 - length, length) &&
	       /\0/ && &tarheader(@header = unpack($tar_header_format, $_))) {
	    $arfile = $file unless $arfile;
	    $size = oct($header[$tar_size]);
	    redo if ($size == 0 || $header[$tar_linkflag] =~ /[12]/);
	    ($file = "[$arfile]$header[$tar_name]") =~ s/\0+//;
	    $pad = (512 - $size) % 512;
	    $_ = '';
	}
	elsif ($arfile) {
	    undef $ar;
	    undef $arfile;
	    next;
	}
	$showfname = 1 if $arfile && !$opt_h;
	$isfile = !$arfile && !$filter && $file ne '-';

	if ($isfile && defined($/) && (!length || seek(STDIN, 0, 0))) {
	    $_ = '';
	}
	if ($opt_t && $isfile && $opt_T && ($bin = -B $file) && !$opt_B) {
	    $db_f && warn("$file: skip\n");
	    next;
	}

	$total_files++;
	($matched, $rest) = eval { &grepfile };

	if ($rest > 0) {
	    &warn("Unexpected EOF in \"$file\"\n");
	    $err = 2;
	    next;
	}
	$total_matched += $matched;
	$total_hitfiles++ if $matched;
	redo if $arfile;
    } continue {
	close STDIN; # wait;	# wait for 4.019 or earlier?
	# recover STDIN for opening '-' and some weird command which needs
	# STDIN opened (like unzip)
	open(STDIN, '<&SAVESTDIN');
    }
}

## remember start time
if ($db_t) {
    @s = times;
}

$SIG{'QUIT'} = 'QUIT';
sub QUIT { die "Interrupted\n"; }
MAIN: {
    eval { &main };
    if ($@ =~ /Interrupted/) {	# interrupted
	if (@ARGV && @dirstack) {
	    print STDERR "Interrupted\nSKIP ", shift(@dirstack), "\n";
	    1 while @ARGV && (shift(@ARGV) ne $dirend);
	    close STDIN; # wait;
	    open(STDIN, '<&SAVESTDIN');
	    redo MAIN;
	}
    } elsif ($@) {		# unexpected error has occured
	die $@;
    }
}
&chash;

## show time info
if ($db_t) {
    @e = times;
    printf STDERR "%.3fu %.3fs\n", $e[0]-$s[0], $e[1]-$s[1];
}

## show statistic info
if ($db_s) {
    printf(STDERR "%d pattern was found in %d files from total %d files\n",
	   $total_matched, $total_hitfiles, $total_files);
}

close STDOUT;

if ($db_p) {
    open(STDOUT, ">&STDERR");
    system "ps -l -p $$";
}

if ($db_p) {
    open(STDOUT, ">&STDERR");
    system "ps -l -p $$";
}

exit($err || !$total_matched);

sub grepfile {
    local($c);
    if ($readonce = ($opt_W || $opt_r || (defined($/) && !$arfile))) {
	$readsize = $size; $keepsize = 0;
    } else {
	$size -= length;
	$readsize = &max(0, $maxreadsize - length);
	$readsize = &min($size, $readsize) if $size >= 0;
    }
    $line = 1; $offset = 0; $lastmatch = -1;
    $loop = 0;
    $more = 1;
    while ($more) {
	$size -= $s = &append_data(STDIN, *_, $readsize);
	&input_filter if defined &input_filter;
	if ($loop++ == 0) {
	    printf STDERR "\r%d/%d", $hash, $total_files
		if $db_c && ++$hash > $hash_t;
	    $db_f && warn("$file: skip\n"), last if $opt_T &&
		!($opt_t && $isfile) && ($bin = &binary($_)) && !$opt_B;
	    warn $file, ' (binary)' x $bin, ":\n" if $db_f;
	    $file = $showfname ? $file : '';
	}
	$more = $readonce ? 0 : $size >= 0 ? $size : !eof(STDIN);
	$more = !&truncate(*_, $/) && $more if defined($/) && !$isfile;
	last if $opt_r && !&match(*_, $opt_r);
	if ($opt_l) {		# print only filename
	    $c = &search(*_) && do { &chash; print "$file\n"; last; };
	    next;
	}
	$c += &grep(*_, $file, *line, *offset, *lastmatch, $keepsize, $more);
	last if $c && $opt_1 && !$opt_a;
    } continue {
	last if !$more || ($readsize != 0 && $s <= 0);
	$readsize = $size >= 0 ? &min($maxreadsize, $size) : $maxreadsize;
	$offset += length;
	$_ = substr($_, -$keepsize);
	$offset -= length;
	$line -= tr/\n/\n/;
    }
    warn "$loop loops\n" if $db_l;
    while ($arfile && $size > 0 &&
	   ($s = read(STDIN, $_, &min($size, 8192)))) {
	$size -= $s;
    }
    if ($pad) {
	local($__trash__);
	if (0) {
	    ##
	    ## This part used be written as this:
	    ##
	    seek(STDIN, $pad, 1) || read(STDIN, $__trash__, $pad);
	    ##
	    ## However there was a situation this seek sets to wrong
	    ## position in the file (FreeBSD 3.4 i386 + perl5.6.0).
	    ## It seems to be a bug incorporated in 5.6.0 and fixed in
	    ## 5.6.1.  This workaround is intentionally kept in case
	    ## of used with buggy 5.6.0 and there is no strong reason
	    ## to behave differently.
	    ##
	} else {
	    read(STDIN, $__trash__, $pad);
	}
    }
    ($c, $size);
}

sub append_data {
    local($FH, *buf, $maxsize) = @_;
    local($len) = (length($buf));
    if ($maxsize >= 0) {
	read($FH, $buf, $maxsize, $len);
    } else {
	$buf .= <$FH>;
	length($buf) - $len;
    }
}

sub grep {
    local(*_, $file, *line, *offset, *lastmatch, $keepsize, $more) = @_;
    local($matched, $_matched);
    $#x = $[ - 1;		# @x = ();
    &search(*_, *x);		# @x = (offset, length, offset, length...);
    splice(@x, 0, 2) while @x && $x[$[] + $offset <= $lastmatch;
    @x = (&max(0, $lastmatch - $offset), 0) if $opt_a && !@x;
    $needinfo = length($file) || $opt_n;
    $neednlsubs = !$opt_s && $needinfo;
    $neednlqsubs = $neednlsubs || defined($opt_J);
    $file =~ s/(.$)/$1:/;
    for ($op = 0; ($p, $len) = splice(@x, 0, 2); ) {
	@out = @err = ();
	$print = $p_all;
	push(@out, $file) if $file && !$opt_a || !$offset;
	$line += (substr($_, $op, $p - $op) =~ tr/\n/\n/) if $opt_n || $opt_g;
	if ($opt_g) {
	    $pre_c = ($line - 1) % $opt_g + 1;
	    $post_c = $opt_g - $pre_c + 1;
	}
	$op = $pnl = $p;
	for ($n = $pre_c; ($opt_o || $n) && ($pnl >= 0); $n--) {
	    last if ($pnl = rindex($_, $NL, $pnl - 1)) < 0;
	    last if $opt_o && ($x = index($_, $rs, $pnl)) >= 0 && $x <= $p;
	    push(@out, "[...\n"), last if $opt_o && ($n == 1);
	}
	$pnl = $lastmatch - $offset - 1 if $opt_a && $lastmatch >= 0;
	$left = $p <= $pnl ? '' : substr($_, $pnl + 1, $p - $pnl - 1);
	if (!$opt_a || !$offset) {
	    push(@out, $opt_s ? $line : ($l = $line - ($left=~tr/\n/\n/)), ':')
		if $opt_n;
	    push(@out, $p + $offset, ':') if $opt_N;
	    push(@out, "\n") if $opt_s && $needinfo;
	}
	&nlsubs(*left, $file, *l, 0) if $neednlsubs;
	$left =~ s/^([\000-\377]*[^\b\s!-~])// && ($pnl += length($1)) if $bin;
	push(@out, $left);
	for ($_matched = 0;; ($p, $len) = splice(@x, 0, 2)) {
	    @cont = ();
	    $_matched++;
	    $match = substr($_, $p, $len);
	    $print += (index($match, "\n") >= $[) if $opt_m;
	    if ($opt_g) {
		$post_c -= ($match =~ tr/\n/\n/);
		$post_c += $opt_g while $post_c <= 0;
	    }
	    &effect($match) if $effect;
	    &nlsubs(*match, $file, *l, 1) if $neednlqsubs;
	    push(@out, $ql, $match, $qr);
	    $nnl = ($p += $len) - 1;
	    for ($n = $post_c; $n || $opt_o; $n--) {
		$nnl = length, last if ($tnnl = index($_, $NL, $nnl + 1)) < 0;
		$nnl = $tnnl;
		if ($opt_o) {
		    $nnl = (length)-1, last if ($nrs = index($_, $rs, $p)) < 0;
		    last if $nrs <= $nnl;
		    @cont = ("...]\n"), last if $n == 1;
		}
	    }
	    last if $nnl < $x[$[] || $opt_2 || @x < 2;
	    $opt_M && $print++;
	    $between = substr($_, $p, $x[$[] - $p);
	    last if $bin && $between =~ /[^\b\s!-~]/;
	    &nlsubs(*between, $file, *l, 0) if $neednlsubs;
	    $post_c -= ($between =~ tr/\n/\n/) if $opt_g;
	    push(@out, $between);
	}
	if ($more) {
	    if (!$opt_a && $offset && $pnl < 0) {
		push(@err, "!!! WARNING: Above line is not complete !!!\n");
	    }
	    if ($nnl >= length) {
		if ($opt_a) {
		    $nnl = &max($p, (length) - $keepsize);
		} elsif ($pnl >= (length) - $keepsize) {
		    last;
		} elsif (!$bin || !/[^\b\s!-~]/) {
		    push(@err, "!!! WARNING: Above line is truncated !!!\n");
		}
	    }
	}
	$right = $nnl <= $p ? '' : substr($_, $p, $nnl - $p);
	$lastnl = $opt_a && !$more && substr($right, -1, 1) eq "\n"
		? chop($right) : '';
	next if $opt_v ne '' && substr($_, $pnl + 1, $nnl - $pnl) =~ /$opt_v/o;
	$right =~ s/[^\b\s!-~][\000-\377]*// if $bin;
	&nlsubs(*right, $file, *l, 0) if $neednlsubs;
	push(@out, $right);
	push(@out, (!$opt_a || !$more) ? "\n" : $lastnl, @cont);
	unshift(@out, "\f\n") if $opt_L && $_matched;
	if ($print && ($matched += $_matched)) {
	    &chash;
	    print @out, @err;
	}
	$lastmatch = $offset + ($opt_a ? $nnl : $p);
    }
    $line += (substr($_, $op) =~ tr/\n/\n/) if $more && ($opt_n || $opt_g);
    $matched;
}
######################################################################

sub warn {
    if (!$opt_I) {
	&chash;
	warn @_;
    }
}
sub eval {
    if ($_[$[+1] || $db_e) {
	print STDERR &unctrl($_[$[]), "\n" x ($_[$[] !~ /\n$/);
    }
    eval shift;
    if ($@) {
	die sprintf("eval failed in file %s on line %s\n$@",
		    (caller)[$[+1, $[+2]);
    }
}
sub unctrl {
    local($_) = shift;
    s/([\000-\010\013-\037])/'^' . pack('c', ord($1)|0100)/ge;
    $_;
}
sub flush {
    local($save, $|, $\) = (select(shift), 1, undef);
    print '';
    select($save);
}
sub asc {
    local($_) = @_;
    $opt_E || s/\s/$delim/ || $opt_e || do {
	if ($opt_y && $opt_j eq 'euc') {
	    local(@alts) = (quotemeta($_));
	    if (/[!-~]/) {
		push(@alts, sprintf("\\%03o\\%03o",
				    unpack('CC', &asc2x0208($_, 1))));
	    }
	    if ($opt_i && /[a-zA-Z]/) {
		tr[a-zA-Z][A-Za-z];
		push(@alts, sprintf("\\%03o\\%03o",
				    unpack('CC', &asc2x0208($_, 1))));
	    }
	    $_ = '(' . join('|', @alts) . ')';
	} else {
	    $_ = quotemeta($_);
	}
    };
    $_;
}
sub mb {
    local($_) = @_;
    local($ret);

    $ret = sprintf("\\%03o\\%03o", unpack('CC', $_));

    if (!$opt_e && $opt_i && $opt_j eq 'euc' && /^\243[\301-\332\341-\372]$/) {
	tr[\301-\332\341-\372][\341-\372\301-\332];
	$ret = sprintf("(%s|\\%03o\\%03o)", $ret, unpack('CC', $_));
    }

    $ret .= $delim unless $opt_E;
    $ret;
}
sub jis {
    quotemeta(shift) . $optionalseq;
}
sub asc2x0208 {
    local($_, $msb) = @_;
    if (/[0-9a-zA-Z]/) {
	$_ = "#$_";
    }
    elsif (tr|!-/|*ItpsuGJKv\134$]%?|	||	# !"#$%&'()*+,-./
	   tr|:-@|'(cad)w|		||	# :;<=>?@
	   tr|[-`|NoO02F|		||	# [\]^_`
	   tr|{-~|PCQ1|) {			# {|}~
	$_ = "!$_";
    }
    if ($msb) {
	tr|\041-\176|\241-\376|;
    }
    $_;
}
sub getdirent {
    local($dir, $_, @ent) = @_;
    opendir(DIR, $dir) || (&warn("$dir: $!\n"), return @ent);
    @ent = grep(!m#^\.\.?$# && (-d "$dir/$_" || $opt_P eq '' || m/$opt_P/o)
		&& ($opt_V eq '' || !/$opt_V/o), readdir(DIR));
    close DIR;
    sort @ent;
}
sub wildcard {
    local($_) = @_;
    s#(\\?.)#$_ = $1; s/\\?([_0-9A-Za-z])/$1/ || /\\./ || s/[*]/.*/ ||
	s/[|]/\$|^/ || tr/?{,}[]\-/.{,}[]\-/ || s/(.)/\\$1/; $_;#ge;
    s/(^|[^\\]){(([^}\\]|\\.)+)}/"$1(" . join('|', split(',', $2)) . ')'/ge;
    length($_) ? "^$_\$" : undef;
}
sub sose {
    do 'ioctl.ph' || do 'sys/ioctl.ph';
    require('termcap.pl');
    $ospeed = 1 unless $ospeed;
    &Tgetent;
    $so = &Tputs($TC{'so'});
    $se = &Tputs($TC{'se'});
    ($so, $se, $se, $so);
}
sub arheader {
    ($_[$ar_fmag] eq "`\n")	&&	# char ar_fmag[2];
    ($_[$ar_date] =~ /^\d+ *$/)	&&	# char ar_date[12];
    ($_[$ar_uid]  =~ /^\d+ *$/)	&&	# char ar_uid[6];
    ($_[$ar_gid]  =~ /^\d+ *$/)	&&	# char ar_gid[6];
    ($_[$ar_mode] =~ /^\d+ *$/)	&&	# char ar_mode[8];
    ($_[$ar_size] =~ /^\d+ *$/);	# char ar_size[10];
}
sub tarheader {
    ($_[$tar_mode]  =~ /^[ 0-7]{7}\0$/)	&&
    ($_[$tar_uid]   =~ /^ *\d+ *\0$/)	&&
    ($_[$tar_gid]   =~ /^ *\d+ *\0$/)	&&
    ($_[$tar_size]  =~ /^ *\d+ *\0?$/)	&&
    ($_[$tar_mtime] =~ /^ *\d+[ \0]?$/);	## Is this enough?
}
sub truncate {			# emulate $/.
    local(*_, $rs, $p) = @_;
    $rs = $rs eq '' ? "\n\n" : substr($rs, 0, 1);
    if (($p = index($_, $rs)) >= $[) {
	substr($_, $p + length($rs)) = '';
    }
    $p >= $[;
}
sub mime {			# RFC1342 MIME header encoding
    s/=\?ISO-2022-JP\?B\?([\w\+\/]+)=*\?=/&decode64($1)/ige;
}
## decode BASE64 encoded string
sub decode64 {
    local($s_64) = shift;
    local($s_uu, $len, $_);

    while ($s_64 =~ s/^(.{1,60})//) {
	$_ = $1;
	
	$len = int(length($_) * 3 / 4);
	if (s/=+$//) { $len -= length($1); }
	tr[A-Za-z0-9+/=][`!-_A];

	$s_uu .= sprintf("%c%s\n", $len + 32, $_);
    }
    unpack('u', $s_uu);
}
sub chash { $hash > $hash_t && (print STDERR "\n"), $hash = 0 if $hash; }
sub max { $_[ ($_[$[] < $_[$[+1]) + $[ ]; }
sub min { $_[ ($_[$[] > $_[$[+1]) + $[ ]; }
######################################################################

##------------------------------------------------------------
## usage.pl: make a string for usage line.
##
## $Id: mg,v 5.0.1.2 2002/03/02 09:31:00 utashiro Exp $
##
## Syntax:
## &Usage($command, $option, $trailer, @arglist);
##	$command: command name (you can use $0 here)
##	$option:  option string same as &Getopt
##	$trailer: trailer string (optional)
##	@arglist: description for options which takes argument (optional)
##		  format is "option character : argument : description"
##		  where argument and description are optional.
##		  special form '-:xyz' hides options -x, -y, -z.
##
## &Usage returns list of two strings where 1st string is for usage
## line and 2nd is for description.
##
## &Mkopts(@arglist) can be used to make $option string.  If $option
## argument for &Usage is not supplied, &Usage will make it by &Mkopts.
##
## Example:
##	$opts = 'deg:u:s:x'; @arglist = (
##		'-:x',			# means -x is secret option
##		'd::debug',
##		'g:group',
##		'u:user:user name',
##	);
##	unless (&Getopts($opts)) {
##		print &Usage($0, $opts, 'file ...', @arglist);
##		exit(1);
##	}
##
## Result:
##	usage: sample [ -d ] [ -e ] [ -g group ] [ -u user ] [ -s : ] file ...
##		-d       debug
##		-u user  user name
##
sub Mkopts {
    local($opts);
    grep(/^([^-]):(.)/ && ($opts .= $1 . ':' x ($2 ne ':')), @_);
    $opts;
}
sub Usage {
    package usage; reset('a-z');
    local($cmd, $opt, $trailer, @arglist) = @_;
    $opt = &Mkopts(@arglist) unless $opt;
    for (@arglist) {
	($name, $arg, $desc) = split(/:/, $_, 3);
	$arg =~ s/\|.*//;
	if ($name eq '-') {
	    grep($hide{$_}++, split('', $arg)); next;
	}
	next if $hide{$name};
	$arg{$name} = $arg;
	$desc{$name} = $desc;
	$w = length($arg) if ($desc && $w < length($arg));
    }
    $cmd =~ s#.*/##;
    push(@usage, 'usage:', $cmd);
    while ($opt =~ s/^\s*(.)(:?)//) {
	next if $hide{$1};
	push(@opts, $1);
	push(@usage, '[', "-$1");
	push(@usage, $arg{$1} || $2) if $2;
	push(@usage, ']');
    }
    push(@usage, $trailer) if $trailer;
    for (grep($desc{$_}, @opts)) {
	push(@desc, sprintf("\t-%s %-${w}s  %s\n", $_, $arg{$_}, $desc{$_}));
    }
    (join(' ', @usage)."\n", join('', @desc));
}
1;
######################################################################

.00 ;

'di			\" finish diversion--previous line must be blank
.nr nl 0-1		\" fake up transition to first page again
.nr % 0			\" start at page 1
'; __END__ ############# From here on it's a standard manual page ####
.de XX
.ds XX \\$4\ (v\\$3)
..
.XX $Id: mg,v 5.0.1.2 2002/03/02 09:31:00 utashiro Exp $
.\"Many thanks to Ajay Shekhawat for correction of manual pages.
.TH MG 1 \*(XX
.AT 3
.\"------------------------------------------------------------
.SH NAME
mg \- multi-line grep
.\"------------------------------------------------------------
.SH SYNOPSIS
\fBmg\fP [ \fBoptions\fP ] \fIpattern\fP [ \fIfile\fP ]
.\"------------------------------------------------------------
.SH DESCRIPTION
\fIMg\fP searches the specified pattern from files or
standard input and prints lines which contain the search
pattern.  It has almost the same function as the Unix
command \fIgrep\fP(1) but is distinguished from \fIgrep\fP
because the matching is done across the line boundaries.
.PP
For example, to find a sentence ``internet control message
protocol'' from many RFC texts, you can say
.nf

	mg\ \-i 'internet control message protocol'\ *.txt

.fi
Match will occur for sequence of words `internet',
`control', `message' and `protocol' separated by any number
of whitespace characters including newline and null string
(\-w option avoids null matching).
.PP
.B [COMPRESSED FILE SEARCH]
If the file has `.Z' or `.gz' suffix, the data is
automatically uncompressed on the fly before search.  Use
\-Z option to suppress this operation.  Because this
mechanism is supported in generric framework, any kind of
suffix and preprocessor pair can be defined by \-z option,
which also allows to give default input filter.  Output
filter is specified by \-X option.
.PP
.B [EMPHASIZING and BLOCK SEARCH]
To emphasize the matched part of text, \-Q, \-u and \-b
options are supported.  This is useful especially used with
block search options.  Option \-c gives the number of
surrounding lines to be shown along with matched line.
Option \-o prints the paragraph containing matched text.
Containing page is with \-g, and entire text is printed with
\-a option.
.PP
.B [RECURSIVE SEARCH]
\fIMg\fP supports recursive directory search.  Use \-R
option to enable it.  This option can be combined with \-T
(text only search), \-B (binary data search), \-P/V (file
name pattern), \-D (descending level), \-F (symbolic link
control).  Option \-dcd gives you ongoing status.
.PP
.B [JAPANESE STRING SEARCH]
\fIMg\fP is also useful to find a word from Japanese text
because Japanese words are not separated by whitespaces, and
newline character can be inserted at any place of the text.
As a matter of fact, \fImg\fP was made for Japanese string
search originally.  Any Japanese codes including JIS,
Shift-JIS, EUC can be handled hopefully, but using JIS code
disables some features.
.PP
User should be aware that \fImg\fP doesn't recognize
character boundaries even if the search string is described
in any of Japanese code.  So some 2-byte character code may
match with 2nd byte of one character and 1st byte of next
one.  Especially when it is encoded in JIS, it may match
some sequence of ASCII string (it may be meaningless string
in uencode or base64).  From my experience, this is not a
big problem when searching some meaningful strings, but you
may get in trouble when looking for short string especially
single character.
.PP
Use \-j option to specify search string code.  Use \-z
option if you want convert file code before search.  Option
\-y enables to find JIS X 0208 representation of ASCII
characters (only EUC).
.PP
.B [ARCHIVE SEARCH]
If the file is archived file by \fIar\fP(1) or \fItar\fP(1)
command, each file in the archive will be searched
individually.  The file name is shown like ``(archive)file''
for ar format and ``[archive]file'' for tar format.  This
function works for compressed file of course.  Use \-A
option if you want to search from entire archived file.
.PP
If the file has `.zip', `.zoo' or `.lzh' suffix, the file is
treated as \fIzip\fP, \fIzoo\fP and \fIlha\fP archive
respectively and each file contained in the archive will be
the subject of search.  Option \-A disables this feature
too.  File name is shown as ``{archive}file''.
.PP
.B [HANDLE PGP ENCRYPTED FILE]
\fIMg\fP can search string from PGP-encrypted file.  It asks PGP
passphrase only once, and it is inherited to pgp decrypting
subprocesses.  Use \-k option when you specify pgp file for search.
Although many people probably want automatic execution of decription
process depending file contents, it is not supported yet.
.PP
.B [BUFFERING POLICY]
\fIMg\fP reads some amount of data at once, and the last
portion of the chunk will be searched again with next chunk
of data.  Default size of data chunk is 512k.
Search-again-data size is 2k for both.  So if the matched
segment size is more than 2K bytes, it may be truncated.
This truncation happens only when the file size is more than
data chunk size, of course.  You can use \-W option to read
whole file contents at once.  But this slows down the speed
of execution for large file.  Maximum read and search again
size can be specified by \-G option.
.PP
.B [PERFORMANCE NOTICE]
This version of \fImg\fP is free from using $`, $& and $'
only when executed by perl5.  Using these variables makes
the program much slower when processing large chunk of data.
Using perl5 doubles the performance when getting many
matches from large file.
.\"------------------------------------------------------------
.SH ENVIRONMENT
Environment variable MGOPTS is used as a default options.
.\"------------------------------------------------------------
.SH OPTIONS
.LP
.B GREP COMPATIBLE OPTIONS:
.IP \-i
Ignore case.
.IP \-l
List filename only.
.IP \-n
Show line number.
.IP \-h 
Do not display filenames even if multiple filenames are
specified by command line.
.LP
.B SLIGHTLY DIFFERENT OPTIONS:
.IP \-w 
Word sensitive.  Match occurs only when both beginning and
end of pattern is on word boundary.  Also space characters
in the pattern doesn't match to the null string.
.IP "\-v \fIpattern\fP"
Skip the line if matched with pattern.  Don't print the
matched line if it matched the pattern specified with this
option.  This option doesn't have any effect when used with
\-a or \-l option.
.LP
.B OTHER OPTIONS:
.IP "\-d \fIflags\fP"
Display informations.  Various kind of debug, diagnostic,
monitor information can be display by giving appropriate
flag to \-d option.
.nf

	f: processing file name
	d: processing directory name
	c: count of processing files
	s: statistic information
	m: misc debug information

	p: run `ps' command before termination (on Unix)

	p: run `ps' command before termination (on Unix)

.fi
You may want to use ``\-dcd'' option to monitor what is
going on during recursive search.
.IP \-N
Print byte offset of matched string.  If multiple matching
occured in one line, only the first matching offset is
printed.  Use \-2 option to avoid it.
.IP \-e 
Use the pattern as a regular expression in 
.IR perl (1)
but space is treated specially.  With this option, you can
use \fImg\fP like \fIegrep\fP(1) like this:
.nf

	mg \-e 'foo bar|\^goo car|\^hoo dar' ...

.fi
See \fIperl\fP(1) for detail of regular expression.  Slash
characters (/) in expression don't have to be escaped.
.IP ""
Option \-w puts \eb's at the beginning and the end of the
pattern.  So the pattern ``foo|bar'' becomes
``\ebfoo|bar\eb''.  You probably want to use
``foo\eb|\ebbar'' instead.
.IP \-E
Use the pattern as regular expression in \fIperl\fP
completely.
.IP \-r
Specify restriction pattern.  A file becomes a subject for
search only when the pattern specified by \-r option is
found in the file.  Next two examples are equivalent.
.nf

	mg \-e ^Subject: `mg \-le "^From: lwall" *`

	mg \-er '^From: lwall' ^Subject: *

.fi
File will be swallowed at one time even if \-W option is not
supplied.
.IP "\-c \fIn[,n]\fP"
Print n-lines before/after matched line.  Default n is 1,
which means only one line is displayed.  N is number of
newlines surrounding matched pattern.  If ``\-c\ 3''
options is suplied, two lines before and after matched
line will be displayed together.  You can see only after
two lines by ``\-c 1,3'' option.
.IP ""
Option \-c0 displays matched string only.  Next example is
almost equivalent to \fIstrings\fP(1) command with \-oa
option.
.nf

	mg \-NEc0 '[ \et\e040-\e176]{4,}' file

.fi
.IP "\-o"
Print the paragraph which contains the pattern.  Each
paragraph is delimited by two successive newline character
by default.  Be aware that an empty line is not paragraph
delimiter if which contains space characters.  Example:
.nf

	mg \-nQo 'setuid script' /usr/man/catl/perl.l

	mg \-o sockaddr /usr/include/sys/socket.h

.fi
If \-c option is also supplied and there are more lines in
the paragraph, continuous mark is displayed.
.IP "\-O \fIstring\fP"
Specify paragraph delimiter string as well as activate the
paragraph mode.  The contents of string is evaluated as is
inside double-quote not as a regular expression.  For
example, you can get lines between troff macros like this:
.nf

	mg \-QoO "\en." 'setuid script' /usr/man/manl/perl.l

.fi
.IP "\-g \fIlines\fP"
Page mode.  Provided number of lines for each pages, pages
which contains the pattern will be displayed.  For example,
you can get pages which contain some strings from nroff'ed
text like this:
.nf

	mg \-Q \-g 66 'setuid script' /usr/man/catl/perl.l

.fi
You can't use \-c with this option.  Formfeed is not
supported currently.
.IP "\-C \fIchars\fP"
Continuous character.  If you want search sentence continued
by other than white space characters, continuous characters
can be set by this options.  For example, next command finds
a sentence even if it is quoted by `>' or `|' mark.
.nf

	mg \-C '>\^|\^' 'ninja shogun fujiyama' `mhpath all`

.fi
To search a pattern in C style comments:
.nf

	mg \-C '/*' 'setuid scripts' perl.c

.fi
.IP ""
Note that continuous characters don't have to be found only
top of the string.  So ``foo\ bar'' matches a string
``foo>>bar'' on the previous example.
.IP \-u 
Underline matched string.  Makes a matched string underlined
by precede each character by ``_^H''.
.IP \-b 
Make bold matched string.  Makes a matched string
overstruck like ``f^Hfo^Hoo^Ho''.
.IP \-Q 
Use a stand-out feature of the terminal to quote the matched
string (not for JIS).  This option is ignored when the
standard output is not a terminal.  \-Q is useful
for long line.  Try
.nf

	echo $path | mg \-Q mh

.fi
.IP \-a 
Print all contents of the file.  This option makes sense
only if used with options like \-Q, \-u, \-b, otherwise
it behaves like \fIcat\fP(1).  Filenames and lines are not
printed with this option.
.IP \-s 
When multiple files are specified, each matched line is
preceded by ``filename:'' string.  With this option, a
newline character is inserted between the filename and
matched line.  This option is useful when the filenames are
very long.
.IP \-2 
Usually only one line is displayed even if multiple matching
occurrs for the same line.  With this option, each match
will be displayed in different line.
.IP \-R 
Search recursively.  Only files specified by command line
arguments are searched by default.  When invoked with this
option and arguments contain a directory, it is searched
recursively.  Usually used with \-P or \-T option.
.IP "\-D \fIlevel\fP"
Descending directory level.  This option specifies how many
levels of directory is to be searched from top directory.
.IP "\-P \fIpattern\fP"
Search file pattern.  When directories are searched
recursively, only files which match the `pattern' are
searched.  A `pattern' is specified in wildcard format same
as shell and `|\^' character can be used for alternative in
addition.  For example, you can find a string ``foobar''
from all C source files and makefiles like this:
.nf

	mg \-RP '*.c|\^[Mm]akefile' foobar /usr/src

.fi
.IP "\-V \fIpattern\fP"
Exception file pattern.  This is a counterpart of \-P.  Only
files which DO NOT match the pattern will be searched.
.nf

	mg \-RV '*.[oas]' foobar /usr/src

.fi
.IP ""
Note that the \-V option is also applied to a directory name
while \-P option has an effect only for a file.  This means
you can specify a directory name to skip, but can't specify
a directory name to search.
.IP \-F
Follow symbolic link of a directory.  Doesn't follow by
default.
.IP \-T 
Search text file only.  Binary file is skipped with this
option.  Decision is made by original method, not by perl
operator \-T, so that Japanese SJIS and EUC text is taken as
text file. See \-B option.
.IP \-B
Find string from binary file.  Only printable characters
surrounding matched pattern are printed.  If both \-T and
\-B are supplied, text file is searched in normal mode and
binary file is searched in binary mode.  Next example is
almost the same as \fIwhat\fP(1).
.nf

	mg \-Bc0,1 '@(#)' /bin/awk

.fi
.IP \-L
Print formfeed between each matchings.  Print the formfeed
character before each matched line.  This options is useful
when used with \-c option and piped to pager command.
.IP \-S 
Get filenames from standard input.  Read standard input and
use each line as a filename for searching.  You can feed the
output from other command like \fIfind\fP(1) for \fImg\fP
with this option.  Next example searches string from files
modified within 7 days:
.nf

	find . \-mtime \-7 \-print | mg \-S pattern

.fi
Next example find the files from the newest first order:
.nf

	ls -t | mg \-S pattern

.fi
.IP \-m 
Print matched line only when the pattern is across the line.
.IP \-M 
Print matched line only when multiple matching occurred for
the same line.
.IP "\-f \fIfile\fP"
Specify the file which contains search pattern.  When file
contains multiple lines, patterns on each lines are search
in OR context.  The line starting with sharp (#) character is
ignored.
.IP "\-p pattern"
Specify search pattern.  You don't have to use this option
explicitly because the first argument after options will be
treated as a pattern.
.IP \-A
Disables archive mode search (ar, tar, zip, zoo).
.IP \-Z 
Disables automatic uncompress, gunzip.
.IP "\-J \fIstring\fP"
Convert newline character(s) found in matched string to
specifed \fIstring\fP.  Using \-J with \-c0 option, you can
collect searching sentence list in one per line form.  This
is almost useless for English text but sometimes useful for
Japanese text.  For example next command prints the list of
KATAKANA words used in the Shift-JIS texts.
.nf

	set kana='\e203[\e100-\e226]|\e201\e133'
	set p="($kana)($kana|\es)*"
	mg \-Ec0 \-J '' "$p" files | sort | uniq \-c

.fi
Note that this command is confused when 2nd byte and 1st
byte of next chararacter matches KATAKANA pattern.
.IP "\-0\fIdigits\fP"
Specifies the record separator as an octal number.  It is
almost same as perl option.  But unlike perl, only the first
record is read for search.  Like perl, \-00 means paragraph
mode.  Actually I added this option only to use \-00 to
search from mail and news header portion.  When reading from
archived file, \fImg\fP emulates perl's $/ behaviour.
.IP \-W
Slurp whole file at once.
.IP "\-G \fImaxreadsize\fP[,\fIkeepsize\fP]"
Specify maximum read size and keep buffer size for next
read.  Default values for these sizes are 30K and 2K bytes.
\fIMg\fP tries to read a file upto maximum read size at a
same time.  Last part of the buffer, specified by keep
buffer size, is not thrown away but will be used as a
subject for search with next buffer.  In arguments, you can
use B, K, and M for block (512), kilo (1024) and mega (1024
* 1024) respectively.  Next example sets maxmum read size
for 100K and keep buffer size for 10K bytes.
.nf

	mg \-iBG 100K,10K unix /vmunix

.fi
.IP \-1
Print first match only.  This option doesn't work well
with \-a option.
.IP "\-j \fIcode\fP"
If you have to use different Japanese codes for search
pattern and target file, target code can be specified by \-j
option.  The code should be one of `jis', `sjis' and `euc'.
Perl library `jcode.pl' has to be installed to use this
option.
.IP "\-y"
When option \-y is specified, JIS X 0208 representation of
ASCII string is also searched even if the pattern is
supplied in ASCII.  Currently only EUC encoding is
supported, and `\-j euc' option is required to enable this
option.  If the pattern is specified in JIS X 0208 string,
only JIS X 0208 character is searched.
.IP "\-z \fIfilter\fP (or \fIEXP:filter:EXP:filter:...\fP)"
You can specify filter command which is applied to each
files before search.  If filter information include multiple
fields separated by colons, first field is perl expression
to check the filename saved in variable $_.  These
expression and command list can be repeated.  If only one
filter command is specified, it is applied to all files.
Examples:
.nf

	mg \-z 'dd conv=ascii' string spoiled_files
	mg \-z '/\e.tar$/:tar tvf \-' pattern *
.fi
.IP ""
If the command doesn't accept stndard input as processing data, you
may be able to use special device:
.nf

	mg \-Qz 'nm /dev/stdin' crypt /usr/lib/lib*.a

.fi
.IP ""
Filters for compressed and gziped file is set by default
unless \-Z option is given.  Default action is:
.nf

	mg \-z 's/\e.Z$//:zcat:s/\e.g?z$//:gunzip \-c'

.fi
.IP "\-x \fIexp\fP"
You can specify the any Perl expression to preprocess input
data.  Some subroutine will be available for this purpose
but currently only ``&mime'' is prepared.  If ``require''
operator is included in \fIexp\fP, it is executed only once.
So you can include your special perl library and use the
subroutine defined in it.
.RS
.IP &mime
Subroutine ``mime'' decodes encoded string based on RFC1342
MIME header encoding but current implementation handles only
ISO-2022-JP encoding.  Example:
.nf

	mg \-x '&mime' \-00 From ~/Mail/inbox/*

.fi
.RE
.IP ""
Note that, this process is done just before a search for the
output from input filter if \-z option is specified.  So in
the above example, MIME encoded string is converted into
ISO-2022-JP even if the input filter was specified to
convert the all data into EUC.
.\"------------------------------------------------------------
.SH APPENDIX
You may want to use \fImg\fP(1) instead of \fIgrep\fP(1)
from GNU emacs.  In that case please add following program
segment in your .emacs file.
.ne 8
.nf

	(defun mg (command)
	  "Run mg instead of grep."
	  (interactive "sRun mg (with args): ")
	  (require 'compile)
	  (compile1 (concat "mg -n " command " /dev/null")
		    "No more mg hits" "mg"))

.fi
If you are using version 19, use this.
.ne 12
.nf

	(defun mg (command-args)
	  "Run mg instead of grep."
	  (require 'compile)
	  (interactive
	   (list (read-from-minibuffer "Run mg (like this): "
				       "mg -n " nil nil 'grep-history)))
	  (compile-internal (concat command-args " " null-filename)
			    "No more mg hits" "mg"
			    ;; Give it a simpler regexp to match.
			    nil grep-regexp-alist))

.fi
You have to visit uninterested line by (next-error) when
surrounding lines are displayed by \-c or \-o option.  Use
\-s option to avoid this.
.\"------------------------------------------------------------
.SH AUTHOR
.nf
Kazumasa Utashiro <utashiro@iij.ad.jp>
Internet Initiative Japan Inc.
3-13 Kanda Nishiki-cho, Chiyoda-ku, Tokyo 101-0054, Japan
.fi
.\"------------------------------------------------------------
.SH "SEE ALSO"
grep(1), perl(1)
.br
http://www.srekcah.org/~utashiro/perl/scripts/mg/
.br
(Sorry, in Japanese...)
.\"------------------------------------------------------------
.SH BUGS
.PP
Perl5 look-behind expression can be used but it is treated
as a bare regex, because look-behind pattern is not allowed
to be variable length.  Also sinse this special treatmen is
done be very sinmple method, you can't use braces within
look-behind pattern.
.PP
When using perl older than version 5.6, actual pattern is
enclosed by parentheses, and it confuses the order of
subexpressions if it contains back-references.  The order is
fixed automaticaly but you may have some problem for certain
patterns.  Use \-dm option to check the actual pattern for
search when you doubt the behavior of this command.
.PP
Hyphenation is not supported.
.PP
No capability to find words separated by nroff footer and
header.
.PP
Very long JIS code pattern may not be processed because of a
limitation of regular expression engine.
.PP
Not enough space for new option (try undocumented option
\-U..., oops, documented here :-).
.\"------------------------------------------------------------
.SH LICENSE
.PP
Copyright (c) 1994-2002 Kazumasa Utashiro
.PP
Use and redistribution for ANY PURPOSE are granted as long as all
copyright notices are retained.  Redistribution with modification
is allowed provided that you make your modified version obviously
distinguishable from the original one.  THIS SOFTWARE IS PROVIDED
BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES ARE
DISCLAIMED.
.ex
