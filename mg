#!/usr/bin/perl
##
## mg: multi-line grep
##
## Copyright (c) 1991-2013 Kazumasa Utashiro
##
## Original: Mar 29 1991
;; my $rcsid = q$Id: mg,v 5.0.1.18 2013/10/30 04:00:06 utashiro Exp $;
##
## EXAMPLES:
##	% mg 'control message protocol' rfc*.txt.Z	# line across search
##	% mg -nRTP '*.[sch]' 'struct vnode' /sys	# recursive search
##	% mg -o sockaddr /usr/include/sys/socket.h	# paragraph mode
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

require 5.008;

use File::stat;
my $file_st;	# global File::stat object for current processing file
our $file;	# global variable for processing file name

use Getopt::Long;
use Text::ParseWords qw(shellwords);

use utf8;
use Encode;
use Encode::Guess;

my $file_code;
my $default_icode = 'utf8';	# default input encoding
my @default_icode_list = qw(euc-jp 7bit-jis);
my $output_code;
my $default_ocode = 'utf8';	# default output encoding

my @opts;
@opts =('i::ignore case',
	'l::list filename only',
	'n::print line number',
	'N::print byte offset',
	'h::do not display filenames',
	'H::display filenames',
	'w::word sensitive (foo becomes \\bfoo\\b)',
	'v:pattern:skip the line if matched with the pattern',
	'e::use pattern as regular expression (space is special)',
	'E::use pattern as regular expression completely',
	'r:pattern:specify search restriction pattern',
	'c:n[,n]|/\d+(,\d+)?/:print n tol/eol before/after matched line',
	'o::paragraph mode',
	'O:string:specify paragraph delimiter string',
	'C:chars:continuous characters',
	'u::underline matched string',
	'b::make bold (print twice) matched string',
	'Y::yield to -Q option even if stdout is not a terminal',
	'a::print whole file (no filename and line number)',
	's::output filename and line number separately',
	'2::duplicate line if multiple matching is occured',
	'R::search recursively',
	'D:level|i:descending directory level',
	'P:pattern:specify search file in wildcard (w/-R)',
	'V:pattern:specify exception file in wildcard (w/-R)',
	'F::follow symbolic link of directory (w/-R)',
	'L::print formfeed before each matching',
	'S::get filenames from stdin',
	'm::print only line across matching',
	'M::print only multiple matched line',
	'f:file:file contains search pattern',
	'x::extended pattern interpretation',
	'p:pattern:specify search pattern',
	'Z::do not uncompress automatically',
	'J:string:convert newline in matched string to the string',
	'0:digits:record separator by octal and single record search',
	'W::slurp whole file contents at once',
	'G:#[,#]:maxreadsize and keepsize',
	'1::print first match only',
	'd:flags:display info (f:file d:dir c:count m:misc s:stat)',
	'-:IU:',
	'I::ignore error',
	'U::show unused opts',
	'man::show manual page',
	'color:when:use termninal color (auto, always, never)',
	'colormode:mode:r(ed), g(reen), b(lue), R(everse), B(old), U(nderline)',
	'icode:@name:specify file encoding (repeatable)',
	'ocode:name:specify output encoding',
	'if:filter:set filter command',
	'of:filter:output filter command',
	'prep:exp:pre-processor function (&mime: RFC1342 decoding)',
	'body::search only from message body',
	'pgp::remember passphrase and use it for PGP processing',
	'pgppass:phrase:pgp passphrase',
	'mime::shortcut for -x "&mime"',
	'exclude:@pattern:specify exclude pattern or subroutine',
	'include:@pattern:oposit to --exclude',
	'require:file:require perl code',
	'file:@file:specify target files (repeatable)',
	'glob:@glob:glob target files (repeatable)',
	'chdir:dir:change directory',
	'and:@pattern:AND patterns (repeatable)',
	'or:@pattern:OR patterns (repeatable)',
	'not:@pattern:NOT patterns (repeatable)',
	'xp:pattern:extended pattern',
	'block::experimental block mode',
	'mod:module:experimental include module',
);

my @optspec = &MkoptsLong(@opts);
our @opt_file;
our @opt_glob;
our @opt_icode;
our @opt_exclude;
our @opt_include;
our @opt_and, @opt_or;
our $opt_color = 'auto';
our $opt_colormode = 'rD';


# global variables
our $rs;	# record separator


binmode STDERR, ":encoding(utf8)";

##
## User customizable option handling in ~/.mgrc
## Special option 'default' and $ENV{MGOPTS} is set by default.
##
my %user_option;
sub set_option {
    my $optname = shift;
    if (defined @{$user_option{$optname}}) {
	push(@{$user_option{$optname}}, @_);
    } else {
	$user_option{$optname} = \@_;
    }
}
if (open(MGRC, "<:encoding(utf8)", "$ENV{HOME}/.mgrc")) { # XXX
    while (<MGRC>) {
	next if /^\s*($|\#)/;
	if (/^\s*__CODE__\s*$/) {
	    local($/) = undef;
	    my $code = <MGRC>;
	    &eval($code);
	    last;
	}
	if (my($arg0, $arg1, $rest) = /^\s*(\S+)\s+(\S+)\s+(.*)/) {
	    if ($arg0 eq 'option') {
		set_option($arg1, shellwords($rest));
	    }
	    elsif ($arg0 eq 'define') {
		set_option($arg1, $rest);
	    } else {
		warn ".mgrc: parse error: $_";
	    }
	}
    }
    close MGRC;
}
for (my $i = 0; $i <= $#ARGV; $i++) {
    if ($ARGV[$i] =~ /^-:(.*)/ and defined $user_option{$1}) {
	splice(@ARGV, $i, 1, @{$user_option{$1}});
	redo;
    }
}
if (defined $user_option{default}) {
    unshift(@ARGV, @{$user_option{default}});
}
if ($ENV{'MGOPTS'}) {
    unshift(@ARGV, shellwords($ENV{'MGOPTS'}));
}

##
## Decode @ARGV
##
foreach (@ARGV) {
    $_ = decode 'utf8', $_ unless utf8::is_utf8($_); # XXX
}

my @SAVEDARGV = @ARGV;
Getopt::Long::Configure("bundling");
GetOptions(@optspec) || &usage;

sub usage {
    select STDERR;

    my($usage, $option) =
	&UsageLong($0, \@optspec, "pattern [ file ... ]", @opts);
    print "usage: mg [ -options ] pattern [ file... ]\n", $option;

    if (%user_option) {
	print "\nUser defined options:\n";
	foreach my $k (sort keys %user_option) {
	    printf "\t-:%-14s %s\n", $k, join(' ', @{$user_option{$k}});
	}
	print "\n";
    }

    print "$rcsid\n" if $rcsid =~ /:/;

    exit 2;
}

&eval(join(' ', grep($_ = "\$db_$_++;", split(//, $opt_d))), $opt_d =~ /e/);

if ($db_o) {
    warn "\@ARGV = @SAVEDARGV\n";
    warn "\@optspec = @optspec\n";
}

$output_code = $opt_ocode || $default_ocode;
binmode STDOUT, ":encoding($output_code)";

if ($opt_mod) {
    eval "use Mg::$opt_mod";
    if ($@) {
	die "$class: Module error ($@)\n";
    }
}

if (defined $opt_chdir) {
    chdir $opt_chdir or die "$!: $opt_chdir\n";
}

## --mime shortcut
$opt_prep = '&mime' if $opt_mime;

## show unused option characters
if ($opt_U) {
    $_ = join('','0'..'9',"\n",'a'..'z',"\n",'A'..'Z',"\n");
    my $opts;
    for (@optspec) {
	if (/^([0-9a-zA-Z])(?:=s)?$/) {
	    $opts .= $1;
	}
    }
    eval "tr/$opts/./";
    die $_;
}

## show man pages
if ($opt_man) {
    exec "perldoc $0";
    die;
}

## setup delimitting character
if ($opt_C) {
    $delim = sprintf('[\\s%s]', quotemeta($opt_C));
} else {
    $delim = '\s';
}
$delim .= $opt_w ? '+' : '*';
$rawdelim = quotemeta($delim);

## setup file encoding
if (@opt_icode) {
    @opt_icode = map { split /[,\s]+/ } @opt_icode;
    my $use_add;
    map { s/^\+// and ++$use_add } @opt_icode;
    if (@opt_icode == 1) {
	$file_code = $opt_icode[0];
	if ($use_add) {
	    Encode::Guess->add_suspects($file_code);
	    $file_code = 'Guess';
	}
	elsif ($file_code =~ /^guess$/i) {
	    Encode::Guess->set_suspects(@default_icode_list);
	}
    } else {
	Encode::Guess->set_suspects(@opt_icode);
	$file_code = 'Guess';
    }
}
else {
    $file_code = $default_icode;
}

sub mkpat {
    my($p) = @_;

    if ($opt_e or $opt_E) {
	$p =~ s{
		(
		 \[[^\]]*\]		# character-class
		 |
		 \(\?\<[=!][^\)]*\)	# look-behind pattern
		)
		|
		(.)			# normal characters
	}{
	    if ($1) {
		$1;
	    } else {
		length(Encode::encode_utf8($2)) > 1 ? &mb($2) : &asc($2)
	    }
	}egx;
    } else {
	$p =~ s/(.)/length(Encode::encode_utf8($1)) > 1 ? &mb($1) : &asc($1)/eg;
    }

    $p =~ s/($rawdelim)+$//;
    length($p) ? ($opt_i ? "(?i)$p" : $p) : undef;
}

## make search pattern
if ($opt_f) {
    my $patterns;
    open(my $fh, '<:encoding(utf8)', $opt_f) or die "$opt_f: $!\n";
    local $/ = undef;
    $patterns = <$fh>;
    close($fh);
    my(@opt_f) = $patterns =~ m/(.*)/g;
    @opt_f = grep { !/^#/ } @opt_f;
    for (@opt_f) { s/\n//; }
    $opt_p = join('|', grep($_ = &mkpat($_), @opt_f));
} elsif (defined $opt_x) {
    defined($opt_xp = $opt_p || shift(@ARGV)) || &usage;
} elsif (defined $opt_xp || @opt_and || @opt_or || @opt_not) {
    ;
} else {
    defined $opt_p || defined($opt_p = shift(@ARGV)) || &usage;
    $opt_p = &mkpat($opt_p);
}

## --xp, --and, --or, --not handling
if (defined $opt_xp) {
    #for my $p (shellwords($opt_xp)) {
    for my $p (split(' ', $opt_xp)) {
	next if $p eq "";
	if ($p =~ s/^-//) {
	    push(@opt_not, $p);
	} elsif ($p =~ s/^\?//) {
	    push(@opt_or, $p);
	} elsif ($p =~ s/^\+// or 1) {
	    push(@opt_and, $p);
	}
    }
}
my @xpattern;
for my $p (@opt_and) {
    $p = &mkpat($p);
    push(@xpattern, [1, qr/$p/]);
}
if (defined @opt_or) {
    my $p = join('|', map { &mkpat($_) } @opt_or);
    push(@xpattern, [1, qr/$p/]);
}
if (defined @opt_not) {
    my $p = join('|', map { &mkpat($_) } @opt_not);
    push(@xpattern, [0, qr/$p/]);
}

($opt_v, $opt_r) = (&mkpat($opt_v), &mkpat($opt_r));
($opt_P, $opt_V) = (&wildcard($opt_P), &wildcard($opt_V));
$opt_p = "\\b$opt_p\\b" if $opt_w && !$opt_E;

##
## make search functions
##
$offset = '$-[0]';
$length = '$+[0] - $-[0]';
if ($opt_1) {
    $check = 'if';
    $_g = '';
} else {
    $check = 'while';
    $_g = 'g';
}
&eval("sub search {
    local(*_, *array) = \@_;
    return /\$opt_p/smo if \@_ < 2;
    $check (/\$opt_p/smo$_g) {
	push(\@array, $offset, $length);
    }
}");
sub match {
    local(*_, $pattern) = @_;
    m/$pattern/;
}

($ql, $qr, $qd, $qe) = &sose;

$nlqsubs = $opt_n ? qq#s/\\n/"$qd\\n\$file".++\$line.":$qe"/ge#
		  : "s/\\n/$qd\\n\$file$qe/g";
$nlqsubs = q|s/\\n/++$line;"| . $opt_J . q|"/ge| if defined $opt_J;
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
	$_[0] =~ s/([\\200-\\337]?.)/' .
	'$ul[length($1)].' x $opt_u . '$1.$bs[length($1)].' x $opt_b . '$1/ge;
    }'); 
}

$opt_O = $opt_o ? '\n\n' : '\n' unless defined $opt_O;
$opt_O =~ s/"/\\"/g;
&eval("\$rs = \"$opt_O\";");	# record separator
grep(eval "warn \"opt_$_=/\$opt_$_/\n\" if \$opt_$_;", split(//, 'pvPVCO'))
    if $db_m;

if (defined($_ = $opt_c)) {
    ($pre_c, $post_c) = /,/ ? split(',') : ($_, $_);
}
$pre_c = $opt_o ? -1 : 1 unless defined($pre_c);
$post_c = $opt_o ? -1 : 1 unless defined($post_c);

unless ($opt_W) {
    my %units = ('b' => 512, 'k' => 1024, 'm' => 1024 * 1024);
    ($maxreadsize, $keepsize) = (1024 * 1024 * 2, 1024 * 2);
    $opt_G =~ s/(\d+)(?i)([kbm])/$1 * $units{lc($2)}/ge;
    if ($opt_G =~ m|([\d]+)\D*([\d]+)?|i) {
	$maxreadsize = $1+0 if $1;
	$keepsize = $2+0 if $2;
    }
    warn "maxreadsize = $maxreadsize, keepsize = $keepsize\n" if $db_m;
}

my $pgp;
if ($opt_pgp) {
    $opt_W = 1;
    $pgp = new PgpFilter;
    if ($opt_pgppass) {
	$pgp->initialize({passphrase => $opt_pgppass});
    } elsif ($opt_passphrase_fd) {
	$pgp->initialize({passphrase_fd => $opt_passphrase_fd});
    } else {
	$pgp->initialize();
    }
}

unless ($opt_Z) {
    push(@filter,
	 's/\.Z$//',   'zcat',
	 's/\.g?z$//', 'gunzip -c',
	 'm/\.pdf$/i', 'pdftotext -nopgbrk - -');
}
if ($opt_if) {
    push(@filter, split(':', $opt_if));
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
	  'zip', 'unzip -p, unzip -l,  \d\d:\d\d\s+(.+)');
$zsuffix = join('|', keys %zinfo);
sub zfiles {
    local($z, @z, *Z) = @_;	# Z will be closed on return
    open(Z, "$zlist $z|") || do { warn "$zlist: $!\n"; return undef; };
    /$zpattern/ && push(@z, "{$z}$+") while <Z>;
    ########## ^ don't put //o here!  $zpattern is set in &open_nextfile.
    @z;
}

if ($opt_require) {
    require $opt_require;
}

if ($opt_prep) {			# define input filter
    # do require/use once
    while ($opt_prep =~ s/((require|use)\b[^;]+;?)//) {
	&eval($1);
    }
    &eval("sub input_filter { $opt_prep }");
}
if ($opt_of) {			# open output filter
    open(STDOUT, "|$opt_of") || die "$opt_of: $!\n";
    $| = 1;
}

##
## Combine @opt_include and @opt_exclude into single list.
##
my @opt_clude;
if (@opt_include or @opt_exclude) {
    $opt_W = 1; # XXX is this necessary?
    for $clude (@opt_include) {
	push(@opt_clude, [1, $clude]);
    }
    for $clude (@opt_exclude) {
	push(@opt_clude, [0, $clude]);
    }
}

open(SAVESTDIN, '<&STDIN');
unshift(@ARGV, map glob, @opt_glob) if @opt_glob;
unshift(@ARGV, @opt_file) if @opt_file;
push(@ARGV, '-') unless @ARGV || $opt_S;

$hash_t = 1;
$dirend = "\0\0";
$p_all = !($opt_m || $opt_M);
$NL = $opt_a ? "\377#+%&^=(*-!" x 2 : "\n";
$showfname = $opt_H || $opt_l || !$opt_h && (@ARGV > 1 || $opt_R || $opt_S);
$/ = !defined($opt_0) ? undef : $opt_0 =~ /^0+$/ ? '' : pack('C', oct($opt_0));

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

	$file_st = stat($file);
	if (-p _ || -c _ || -b _) {	# skip FIFO and device files
	    next;
	}
	if (-d _ && $opt_R && (!-l $file || $opt_F)) {
	    if (defined($opt_D) && @dirstack >= $opt_D) {
		next;
	    }
	    ($ent = -@ARGV) += unshift(@ARGV, &getdirent($file), $dirend) - 1;
	    unshift(@dirstack, $file . ($file =~ m'/$' ? '' : '/'));
	    if ($db_d) {
		&chash;
		warn sprintf("%s> %s (%d entries).\n",
			     '=' x @dirstack, $file, $ent);
	    }
	    next;
	}
	if (!$opt_A && $file =~ /\.($zsuffix)$/o) {
	    ($zext, $zlist, $zpattern) = split(/,\s*/, $zinfo{$1}, 3);
	    unshift(@ARGV, &zfiles($file));
	    $showfname++ unless $opt_h;
	    next;
	}
	if (0) {}
	elsif ($file =~ /^http:\/\//) {
	    open(STDIN, '-|') || exec("w3m -dump $file") || die "w3m: $!\n";
	}
	elsif ($file =~ /({(.+\.($zsuffix))}(.+)$)/o) {		# zip
	    $file = $1;		# XXX dirty hack for -R
	    open(STDIN, '-|') || exec("$zext '$2' '$4'") || die "$zext: $!\n";
	}
	else {
	    open(STDIN, $file) || do {
		$err = 2, &warn("$file: $!\n") unless -l $file; next;
	    };
	}

	if (defined(&filter) && ($filter = &filter($file))) {
	    open(STDIN, '-|') || exec($filter) || die "$filter: $!\n";
	}
	if ($filter && $db_p) {
	    printf STDERR "Filter: \"$filter\" < %s.\n", $file;
	}

	binmode STDIN, ":encoding($file_code)";

	return $file;
    }
    undef;
}

sub main {

    while (defined($file = &open_nextfile)) {
	$_ = '';
	$size = -1;
	$isfile = !$filter && $file ne '-';

	$total_files++;
	($matched, $rest) = &grepfile;

	if ($rest > 0) {
	    &warn("Unexpected EOF in \"$file\"\n");
	    $err = 2;
	    next;
	}
	$total_matched += $matched;
	$total_hitfiles++ if $matched;
    } continue {
	close STDIN; # wait;	# wait for 4.019 or earlier?
	# recover STDIN for opening '-' and some weird command which needs
	# STDIN opened (like unzip)
	open(STDIN, '<&SAVESTDIN');
    }
}

## remember start time
if ($db_s) {
    @s = times;
}

$SIG{'QUIT'} = 'QUIT';
sub QUIT { die "Interrupted\n"; }
MAIN: {
    eval { &main };
    if ($@) {
	if ($@ =~ /Interrupted/) {	# interrupted
	    if (@ARGV && @dirstack) {
		print STDERR "Interrupted\nSKIP ", shift(@dirstack), "\n";
		1 while @ARGV && (shift(@ARGV) ne $dirend);
	    }
	} else {
	    warn "$file: $@";
	}
	close STDIN; # wait;
	open(STDIN, '<&SAVESTDIN');
	redo MAIN;
    }
}
&chash;

## show statistic info and consumed time
if ($db_s) {
    @e = times;
    printf(STDERR "cpu %.3fu %.3fs\n",
	   $e[0]-$s[0], $e[1]-$s[1]);
    printf(STDERR "matched %d times %d files\n",
	   $total_matched,
	   $total_hitfiles,
	   );
    printf(STDERR "total %d files %d reads\n",
	   $total_files,
	   $total_loops,
	   );
}

close STDOUT;

if ($db_p) {
    open(STDOUT, ">&STDERR");
    system "ps -lww -p $$";
}

exit($err || !$total_matched);

######################################################################

sub grepfile {
    local($c);

    if ($readonce = ($opt_W || $opt_r || (defined($/) && !$arfile))) {
	$readsize = $size;
	$keepsize = 0;
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

	&pgp_decrypt_msg(*_) if $pgp;

	if (defined &input_filter) {
	    &input_filter;
	}

	if ($loop++ == 0) {
	    if ($db_c or $db_t) {
		my $s = '';
		if ($db_c && ++$hash > $hash_t) {
		    $s .= sprintf("[%d/%d] ", $hash, $total_files);
		}
		if ($db_t) {
		    my @t = CORE::localtime($file_st->mtime);
		    $s .= sprintf("%04d-%02d-%02d %02d:%02d:%02d ",
				  $t[5] + 1900,
				  $t[4] + 1,
				  $t[3],
				  $t[2],
				  $t[1],
				  $t[0]);
		}
		print STDERR "\r$s";
	    }

	    warn $file, ":\n" if $db_f;
	    $file = $showfname ? $file : '';
	    if ($opt_body) {
		my $p;
		my $delim = "\n\n";
		if (($p = index($_, $delim)) >= 0) {
		    substr($_, 0, $p + length($delim)) = '';
		}
		$offset += $p + length($delim);
	    }
	}
	$more = $readonce ? 0 : $size >= 0 ? $size : !eof(STDIN);
	##
	## Perl's native $/ functionality does not work now! (XXX)
	## if (defined($/) and !$isfile) {
	##
	if (defined $/) {
	    $more = !&truncate(*_, $/) and $more;
	}
	if ($opt_r and !&match(*_, $opt_r)) {
	    last;
	}
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
    $total_loops += $loop;
    warn "$loop loops\n" if $db_l;
    while ($arfile and $size > 0 and
	   $s = read(STDIN, $_, &min($size, 8192))) {
	$size -= $s;
    }
    ($c, $size);
}

sub pgp_decrypt_msg {
    local(*_) = @_;
    
    if (/[\x00\xff]/) {
	$_ = $pgp->decrypt($_);
    }
    elsif (/-----BEGIN\ PGP\ MESSAGE-----/) {
	s{^(-----BEGIN\ PGP\ MESSAGE-----
	    .*?
	    -----END\ PGP\ MESSAGE-----)$
        }{
	    $pgp->decrypt($1);
	}msgex;
    }
}

sub append_data {
    local($FH, *buf, $maxsize) = @_;
    my $len = length($buf);

    if ($maxsize >= 0) {
	read($FH, $buf, $maxsize, $len);
    } else {
	if (defined $file_st and $file_st->size >= 0) {
	    read($FH, $buf, $file_st->size - $len, $len);
	} else {
	    if (0) {
		##
		## I don't know why, but this code is *veeeeery slow*.
		##
		$buf .= <$FH>;
	    } else {
		##
		## Theoretically, "1 while read(...) > 0" do same thing.
		## However, final 0 byte read consumes a lot of cpu time
		## only to do nothing.  So checking eof is very important.
		##
		my $readsize = 10 * 1024 * 1024;
		do {
		    read($FH, $buf, $readsize, length($buf));
		} while not eof $FH;
	    }
	}
	length($buf) - $len;
    }
}

sub grep {
    local(*_, $file, *line, *offset, *lastmatch, $keepsize, $more) = @_;
    my($matched, $_matched);
    my $include_sw = 0;
    $#x = $[ - 1;		# @x = ();

    if (@xpattern) {
	&xsearch(*_, \@x);
    } else {
	&search(*_, *x);	# @x = (offset, length, offset, length...);
    }
    splice(@x, 0, 2) while @x && $x[$[] + $offset <= $lastmatch;
    @x = (&max(0, $lastmatch - $offset), 0) if $opt_a && !@x;
    $needinfo = length($file) || $opt_n;
    $neednlsubs = !$opt_s && $needinfo;
    $neednlqsubs = $neednlsubs || defined($opt_J);
    $file =~ s/(.$)/$1:/;

    for my $clude (@opt_clude) {
	my($include_sw, $arg) = @$clude;
	my @select;
	if ($arg =~ /^&([^=]*)(?:=(.*))?/) {
	    my($func, @arg) = ($1, $2);
	    @select = &$func(@arg);
	}
	else {
	    @select = pattern_list($arg);
	}
####################
	if ($opt_block) {
	    my @hit = hit_block(\@x, \@select, 1);
	    while (@hit) {
		my($from, $to) = splice(@hit, 0, 2);
		my $func = "out";
		if (defined(&{$func})) {
		    &$func(substr($_, $from, $to - $from));
		} else {
		    print substr($_, $from, $to - $from), "\n";
		}
		print "\n";
	    }
	    return;
	}
####################
	if ($include_sw or @select) {
	    @x = select_list(\@x, \@select, $include_sw);
	}
    }

    for ($op = 0; ($p, $len) = splice(@x, 0, 2); ) {
	@out = @err = ();
	$print = $p_all;
	push(@out, $file) if $file && !$opt_a || !$offset;
	$line += (substr($_, $op, $p - $op) =~ tr/\n/\n/) if $opt_n;
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
	push(@out, $left);
	for ($_matched = 0;; ($p, $len) = splice(@x, 0, 2)) {
	    @cont = ();
	    $_matched++;
	    $match = substr($_, $p, $len);
	    $print += (index($match, "\n") >= $[) if $opt_m;
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
	    if ($nnl < $x[$[] || $opt_2 || @x < 2) {
		last;
	    }
	    $opt_M && $print++;
	    $between = substr($_, $p, $x[$[] - $p);
	    if ($neednlsubs) {
		&nlsubs(*between, $file, *l, 0);
	    }
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
		} else {
		    push(@err, "!!! WARNING: Above line is truncated !!!\n");
		}
	    }
	}
	$right = $nnl <= $p ? '' : substr($_, $p, $nnl - $p);
	$lastnl = $opt_a && !$more &&
	    substr($right, -1, 1) eq "\n" ? chop($right) : '';
	next if $opt_v ne '' && substr($_, $pnl + 1, $nnl - $pnl) =~ /$opt_v/o;
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
    $line += (substr($_, $op) =~ tr/\n/\n/) if $more && $opt_n;
    $matched;
}
sub xsearch {
    use strict;

    local(*_) = shift;
    my($xp) = @_;

    my @blocks;

    ##
    ## build match result list
    ##
    my @result;
    my $required_count;
    my %required;
    for (my $i = 0; $i < @xpattern; $i++) {
	my($required, $regex) = @{$xpattern[$i]};
	$required_count++ if $required;
	my $rp = $result[$i] = [];
	while ($_ =~ /$regex/g) {
	    push(@$rp, [$-[0], $+[0]]);
	    push(@blocks, [$-[0], $+[0]]);
	}
	if (@$rp) {
	    $required{$required ? 'yes' : 'no'}++;
	}
    }

    ##
    ## optimization for zero or single positive match
    ##
    return 0 if $required{yes} == 0;
    if ($required_count == 1 and $required{yes} == 1 and $required{no} == 0) {
	@{$xp} = map { $_->[0], $_->[1] - $_->[0] } map { @$_ } @result;
	return 1;
    }

    ##
    ## build block list from matched range, then sort and uniq
    ## select wider range if they begin at same position
    ##
    if (@blocks) {
    	my $min;
	my $textp = \$_;
    	@blocks =
	    grep({$_->[0] > $min and $min = $_->[0]}
		 sort({$a->[0] <=> $b->[0] || $b->[1] <=> $a->[1]}
		      map({[find_block($textp, $rs, $_->[0], $_->[1])]}
			  @blocks)));
    }

    ##
    ## build matched count list for every block
    ##
    my(@positive_hit, @negative_hit);
    my @block_matched;
    my $next_result = 0;
    for (my $bi = 0; $bi < @blocks; $bi++) {
	my($from, $to) = @{$blocks[$bi]};
	next if $to <= $next_result;
 	$next_result = 0;
	for (my $i = 0; $i < @result; $i++) {
	    my $mp = $block_matched[$bi][$i] = [];
	    my $rp = $result[$i];
	    while (@{$rp} and $rp->[0][1] < $from) {
		shift @{$rp};
	    }
	    while (@{$rp} and $rp->[0][0] < $to) {
		push(@{$mp}, shift @{$rp});
	    }
	    if (@{$mp} > 0) {
		if ($xpattern[$i][0]) {
		    $positive_hit[$bi]++;
		} else {
		    $negative_hit[$bi]++;
		}
	    }
	    # remember next minimum result postion for optimazation
	    if (@{$rp} and ($next_result == 0 or $rp->[0][0] < $next_result)) {
		$next_result = $rp->[0][0];
	    }
	}
	last if $next_result == 0;
    }

    ##
    ## build effective block list
    ##
    my @hit;
    for (my $bi = 0; $bi < @blocks; $bi++) {
	next if $negative_hit[$bi] or $positive_hit[$bi] != $required_count;
	push(@hit, map { @{$_} } @{$block_matched[$bi]});
    }

    @{$xp} = map({ $_->[0], $_->[1] - $_->[0] }
		 squeeze(sort {$a->[0]<=>$b->[0] || $a->[1]<=>$b->[1]}
			 @hit));

    1;
}
sub squeeze {
    use strict;
    my @in = @_;
    my @out;
    push(@out, shift @in) if @in;
    while (@in) {
	my $top = shift @in;
	if ($out[-1][1] >= $top->[0]) {
	    $out[-1][1] = $top->[1] if $out[-1][1] < $top->[1];
	} else {
	    push(@out, $top);
	}
    }
    @out;
}
sub find_block {
    use strict;
    local(*_) = shift;		# text
    my($delim) = shift;		# delimiter
    my($from, $to) = @_;	# range

    my($begin, $end);
    $begin = rindex($_, $delim, $from) + length($delim);
    $begin = 0 if $begin < 0;
    $end = index($_, $delim, $to) + length($delim);
    $end = length($_) if $end < 0;

    ($begin, $end);
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
	$_ = quotemeta($_);
    };
    $_;
}
sub mb {
    local($_) = @_;
    local($ret);

    $ret = $_;
    $ret .= $delim unless $opt_E;
    $ret;
}
sub asc2x0208 {
    local($_, $msb) = @_;
    if (/[0-9a-zA-Z]/) {
	$_ = "#$_";
    }
    elsif (tr|!-/|*ItpsuGJKv\134$]%?|	||	# !"#$%&'()*+,-./     "
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
    use Term::ANSIColor qw(:constants);
    my($so, $se);
    if (($opt_color eq 'always') or
	($opt_color eq 'auto' and -t STDOUT)) {
	if ($opt_colormode =~ /,/) {
	    ($so, $se) = split(/,/, $opt_colormode, 2);
	} else {
	    map {
		$so .= UNDERLINE if /U/;
		$so .= REVERSE   if /S/;
		$so .= BOLD      if /D/;
		$so .= RED       if /r/; $so .= ON_RED       if /R/;
		$so .= GREEN     if /g/; $so .= ON_GREEN     if /G/;
		$so .= BLUE      if /b/; $so .= ON_BLUE      if /B/;
		$so .= CYAN      if /c/; $so .= ON_CYAN      if /C/;
		$so .= MAGENTA   if /m/; $so .= ON_MAGENTA   if /M/;
		$so .= YELLOW    if /y/; $so .= ON_YELLOW    if /Y/;
		$so .= WHITE     if /w/; $so .= ON_WHITE     if /W/;
	    } $opt_colormode if $opt_colormode;
	    $se = RESET if $so;
	}
    }
    ($so, $se, $se, $so);
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
	if (s/=+$//) {
	    $len -= length($1);
	}
	tr[A-Za-z0-9+/=][`!-_A];

	$s_uu .= sprintf("%c%s\n", $len + 32, $_);
    }
    unpack('u', $s_uu);
}
sub chash { $hash > $hash_t && (print STDERR "\n"), $hash = 0 if $hash; }
sub max { $_[ ($_[$[] < $_[$[+1]) + $[ ]; }
sub min { $_[ ($_[$[] > $_[$[+1]) + $[ ]; }
sub pattern_list {
    use strict;
    my($pattern) = @_;
    my @list;

    my $re = qr/$pattern/m;
    while (/$re/g) {
	push(@list, $-[0], $+[0]);
    }
    @list;
}
sub select_list {
    use strict;
    my($from, $what, $how) = @_;
    my @from = @$from;
    my @what = @$what;

    my(@exclude, @include);
    while (@from) {
	while (@what and $what[1] <= $from[0]) {
	    splice(@what, 0, 2);
	}
	if (@what == 0) {
	    push(@exclude, splice(@from, 0));
	    last;
	}
	while (@from and $from[0] < $what[0]) {
	    push(@exclude, splice(@from, 0, 2));
	}
	while (@from and ($from[0] + $from[1]) <= $what[1]) {
	    push(@include, splice(@from, 0, 2));
	}
	while (@from and $from[0] < $what[1]) {
	    push(@exclude, splice(@from, 0, 2));
	}
    }	
    $how ? @include : @exclude;
}
sub hit_block {
    use strict;
    my($from, $what, $how) = @_;
    my @from = @$from;
    my @what = @$what;

    my(@exclude, @include);
    my(@hit);
    while (@from) {
	while (@what and $what[1] <= $from[0]) {
	    splice(@what, 0, 2);
	}
	if (@what == 0) {
	    push(@exclude, splice(@from, 0));
	    last;
	}
	while (@from and $from[0] < $what[0]) {
	    push(@exclude, splice(@from, 0, 2));
	}
	while (@from and ($from[0] + $from[1]) <= $what[1]) {
	    push(@include, splice(@from, 0, 2));
	    #push(@hit, $what[0], $what[1]) if $hit[1] ne $what[1];
	    push(@hit, $what[0], $what[1]);
	}
	while (@from and $from[0] < $what[1]) {
	    push(@exclude, splice(@from, 0, 2));
	}
    }	
    @hit;
}

######################################################################
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
sub MkoptsLong {
    my @o;
    foreach (@_) {
	my($opt, $arg, $desc) = split(':', $_, 3);
	if ($arg eq '') {
	    push(@o, $opt);
	} elsif ($arg eq '!') {
	    push(@o, "$opt!");
	} elsif ($arg =~ s/^\?//) {
	    push(@o, "$opt:s");
	} elsif ($arg =~ s/^\@//) {
	    push(@o, "$opt=s" => eval '\\@opt_'.$opt);
	} else {
	    push(@o, "$opt=s");
	}
    }
    @o;
}
sub UsageLong {
    my($cmd, $optref, $trailer, @arglist) = @_;
    my(@opts, %desc, $width);
    @$optref = &Mkopts(@arglist) unless $optref;
    for (@arglist) {
	my($name, $arg, $desc) = split(/:/, $_, 3);
	$arg =~ s/\|.*//;
	if ($name eq '-') {
	    map($hide{$_}++, split('', $arg));
	    next;
	}
	next if $hide{$name};
	$arg =~ s/^\@//;
	$arg{$name} = $arg;
	$desc{$name} = $desc;
	if ($desc && $width < (length($name) + length($arg))) {
	    $width = length($name) + length($arg);
	}
    }
    $width += 2;
    $cmd =~ s#.*/##;
    push(@usage, 'usage:', $cmd);
    for (@$optref) {
	if (ref($_)) {
	    next;
	}
	m/^([^:=]+)([=:]s)?/ or die "($_)";
	next if $hide{$1};
	push(@opts, $1);
	push(@usage, '[', "-$1");
	push(@usage, $arg{$1} || $2) if $2;
	push(@usage, ']');
    }
    push(@usage, $trailer) if $trailer;
    for (grep($desc{$_}, @opts)) {
	my $line = "\t-";
	my $w = $width;
	if (length($_) > 1) {
	    $line .= '-';
	    $w--;
	}
	$line .= sprintf("%-${w}s %s\n", "$_ $arg{$_}", $desc{$_});
	push(@desc, $line);
    }
    (join(' ', @usage)."\n", join('', @desc));
}
1;
######################################################################
package PgpFilter;

sub new {
    my $obj = bless {
	FH        => undef,
    }, shift;
    $obj;
}

sub initialize {
    my $obj = shift;
    my($opt) = @_;
    my $passphrase;

    if (my $fd = $opt->{passphrase_fd}) {
	$obj->fh(_openfh($fd));
    }
    else {
	if (not defined $obj->fh) {
	    $obj->fh(_openfh());
	}
	if (defined $obj->{passphrase}) {
	    $passphrase = $obj->{passphrase};
	} else {
	    _readphrase(\$passphrase);
	}
	$obj->setphrase(\$passphrase);

	##
	## Destroy data as much as possible
	##
	$passphrase =~ s/./\0/g;
	$passphrase = "";
	undef $passphrase;
    }

    $obj;
}

sub setphrase {
    my $obj = shift;
    my $fh = $obj->fh;
    my($passphrase_r) = @_;
    
    $obj->reset;
    $fh->syswrite($$passphrase_r, length($$passphrase_r));
    $obj->reset;
}

sub fh {
    my $obj = shift;
    @_ ? $obj->{FH} = shift
       : $obj->{FH};
}

sub pgppassfd {
    my $obj = shift;
    $obj->fh->fileno;
}

sub _decrypt_command {
    my $obj = shift;
    sprintf("gpg --quiet --batch --decrypt --passphrase-fd %d",
	    $obj->pgppassfd);
}

sub file_viewer {
    my $obj = shift;
    sprintf("pgpcat --passphrase-fd %d", $obj->pgppassfd);
}

sub reset {
    my $obj = shift;
    $obj->fh->sysseek(0, 0) or die;
}

sub _openfh {
    use Fcntl;
    use IO::File;

    my($fd) = @_;
    my $fh;

    if (defined $fd) {
	$fh = new IO::Handle;
	$fh->fdopen($fd, "w+");
    } else {
	$fh = new_tmpfile IO::File;
	defined $fh or die "new_tmpefile: $!";
    }

    $fh->fcntl(F_SETFD, 0) or die "fcntl F_SETFD failed: $!\n";

    return $fh;
}

sub _readphrase {
    my($passphrase_r) = @_;

    print STDERR "Enter PGP Passphrase> ";
    system "stty -echo";
    $$passphrase_r = <STDIN>;
    system "stty echo";
    chomp($$passphrase_r);
    print STDERR "\n";

    $passphrase_r;
}

sub decrypt {
    use IPC::Open2;

    my $obj = shift;

    my($enc_data) = @_;
    local($/) = undef;

    $obj->reset;

    my $pid = open2(\*RDRFH, \*WTRFH, $pgp->_decrypt_command);

    if (length($enc_data) <= 1024 * 16) {
	print WTRFH $enc_data;
    }
    else {
	## NO ERROR CHECK! XXX
	if (fork == 0) {
	    print WTRFH $enc_data;
	    close WTRFH;
	    close RDRFH;
	    exit;
	}
    }
    close WTRFH;
    my $dec_data = <RDRFH>;
    close RDRFH;

    $dec_data;
}

1;

=head1 NAME

mg - multi-line grep


=head1 B<SYNOPSIS>

B<mg> [ B<options> ] I<pattern> [ I<file> ]


=head1 B<DESCRIPTION>

I<Mg> searches the specified pattern from files or standard input and
prints lines which contain the search pattern.  It has almost the same
function as the Unix command L<grep(1)> but is distinguished from
I<grep> because the matching is done across the line boundaries.

For example, to find a sentence ``internet control message protocol''
from many RFC texts, you can say

    mg -i 'internet control message protocol' *.txt

Match will occur for sequence of words `internet', `control', message'
and `protocol' separated by any number of whitespace characters
including newline and null string (-w option avoids null matching).

B<[COMPRESSED FILE SEARCH]> If the file has `.Z' or `.gz' suffix, the
data is automatically uncompressed on the fly before search.  Use -Z
option to suppress this operation.  Because this mechanism is
supported in generric framework, any kind of suffix and preprocessor
pair can be defined by --if option, which also allows to give default
input filter.  Output filter is specified by --of option.

Now PDF file is preprocessed by ``pdftotext'' command.  Option -Z also
desables this feature.

B<[EMPHASIZING and BLOCK SEARCH]> To emphasize the matched part of
text, -Q, -u and -b options are supported.  This is useful especially
used with block search options.  Option -c gives the number of
surrounding lines to be shown along with matched line.  Option -o
prints the paragraph containing matched text.  Containing page is with
-g, and entire text is printed with -a option.

B<[RECURSIVE SEARCH]> I<Mg> supports recursive directory search.  Use
-R option to enable it.  This option can be combined with -P/V (file
name pattern), -D (descending level), -F (symbolic link control).
Option -dcd gives you ongoing status.

B<[JAPANESE STRING SEARCH]> I<Mg> is also useful to find a word from
Japanese text because Japanese words are not separated by whitespaces,
and newline character can be inserted at any place of the text.  As a
matter of fact, I<mg> was originally made for Japanese string search.

B<[MULTIBYTE CODE SET HANDLING]> Search string have to be described in
utf8 in both command line argument and pattern file (option -p).  Also
.mgrc file should be written in utf8.  Default file code is also utf8.
Use --icode option to specify file code and use code name ``guess''
for automatic code recognition.  Use --ocode option to specify output
code.

B<[HANDLE PGP ENCRYPTED FILE]> I<Mg> can search string from
PGP-encrypted file.  When option --pgp is specified, PGP passphrase is
asked only once, and it is inherited to pgp decrypting subprocesses.
You may want automatic execution of decryption process depending file
contents, but it is not supported yet.

B<[BUFFERING POLICY]> I<Mg> reads some amount of data at once, and the
last portion of the chunk will be searched again with next chunk of
data.  Default size of data chunk is 2M.  Search-again-data size is 2k
for both.  So if the matched segment size is more than 2k bytes, it
may be truncated.  This truncation happens only when the file size is
bigger than data chunk size, of course.  You can use -W option to read
whole file contents at once.  But it may slow down the speed of
execution for large file, depending on the architecture and
configuration of your system.  Maximum read and search again size can
be specified by -G option.


=head1 B<EXTENDED PATTERN SEARCH>

I<Mg> now supports completely new search method.  This feature is
invoked by -x flag, and then the specified pattern is interpreted in
the different manner.

Pattern string is treated as a colleciton of tokens separated by white
space.  Each component will be searched independently, but only the
line which contains all of them will be printed.  For example,

    mg -xp 'foo bar buz' ...

will print lines which contain all of `foo', `bar' and `buz'.  They
can be found in any order and/or any place in the string.  So this
command find all of following texts.

    foo bar buz
    buz bar foo
    the foo, bar and buz

If you want to use OR syntax, prepend question (`?') sign on each
token, or use regular expression in pattern:

    mg -xp 'foo bar buz ?yabba ?dabba ?doo'
    mg -exp 'foo bar buz yabba|dabba|doo'

This command will print the line which contains all of `foo', `bar'
and `buz' and one or more from `yabba', `dabba' or `doo'.  Note that
you need to use -e option to enable regular expression interpretation.

Please be aware that multiple `?' preceded tokens are treated all
mixed together.  That means `?A|B ?C|D' is equivalent to `?A|B|C|D'.
If you want to mean `(A or B) and (C or D)', use AND syntax instead of
OR: `A|B C|D'.

NOT operator can be specified by prefixing the token by minus (`-')
sign.  Next example will show the line which contain both `foo' and
bar' but none of `yabba' or `dabba' or `doo'.

    mg -xp 'foo bar -yabba -dabba -doo'
    mg -exp 'foo bar -yabba|dabba|doo'

It is ok to set plus (`+') sign before positive AND token, but it has
no effect.

When executed with -o option, the paragraph which contains all these
components will be found.  This style is much more useful, actually.

You can't use double quote to include white space within each token.
Separate options are prepared to specify each component individually;
--I<and>, --I<or> and --I<not>.  These options can be used multiple
times.

    mg --and 'foo bar' --and buz --or 'yabba dabba' --or doo ...

Long option --I<xp> is equivalent to the combination of -x and
(optional) -p options.  Author decided to override single character
option -x, to make it possible using in this way:

    mg -oeiQxp 'foo bar buz' ...


=head1 B<ENVIRONMENT and STARTUP FILE>

Environment variable MGOPTS is used as a default options.  They are
inserted before command line options.

Before starting execution, I<mg> reads the file named ``.mgrc'' on
user's home directory.  In .mgrc file, user can define own option
name.  There are two directives can be used in .mgrc file: `option'
and `define'.  First argument of `option' directive is user defined
option name.  The rest are processed by I<shellwords> routine defined
by Text::ParseWords module.

    option mh -RT -P '[0-9]*'

User definable option is specified by preceding option name by `-:'
string.

    mg -:mh pattern ~/Mail

Another directive `define' is almost same as `option', but argument is
not processed by I<shellwords> and treated just a simple text.  You
can include metacharacters without escaping.

    define mails [0-9]*
    option mh -RT -P -:mails

When I<mg> found `__CODE__' line in .mgrc file, the rest of the file
is evaluated as a Perl program.  You can define your own subroutines
which can be used by --prep, --include, --exclude options.  For those
subroutines, file content will be provided by global variable $_.
Expected response from the subroutine is the list of numbers, which is
made up by start and end offset pairs.

For example, suppose that the following function is defined in your
.mgrc file.

    __CODE__
    sub odd_line {
        my @list;
        my $i;
        while (/.*\n/g) {
            push(@list, $-[0], $+[0]) if ++$i % 2;
        }
        @list;
    }

You can use next command to search pattern included in odd number
lines.

    % mg --include &odd_line patten files...

If you do not want to evaluate those programs in all invocation of the
command, use --require option to include arbitrary perl program files.


=head1 B<OPTIONS>

B<GREP COMPATIBLE OPTIONS:>

=over 7

=item -i 

Ignore case.

=item -l 

List filename only.

=item -n 

Show line number.

=item -h 

Do not display filenames even if multiple filenames are specified by
command line.

=item -H 

Display filename even if single file is specified by command line.

=back

B<SLIGHTLY DIFFERENT OPTIONS:>

=over 7

=item -w 

Word sensitive.  Match occurs only when both beginning and end
of pattern is on word boundary.  Also space characters in the
pattern doesn't match to the null string.

=item -v I<pattern>

Skip the line if matched with pattern.  Don't print the matched
line if it matched the pattern specified with this option.  This
option doesn't have any effect when used with -a or -l option.

=back

B<OTHER OPTIONS:>

=over 7

=item -d I<flags>

Display informations.  Various kind of debug, diagnostic, monitor
information can be display by giving appropriate flag to -d option.

    f: processing file name
    d: processing directory name
    t: processing file modified time
    c: count of processing files
    s: statistic information
    m: misc debug information
    o: option related information
    p: run `ps' command before termination (on Unix)

You may want to use ``-dcd'' option to monitor what is going on during
recursive search.

=item -N 

Print byte offset of matched string.  If multiple matching occurred in
one line, only the first matching offset is printed.  Use -2 option to
avoid it.

=item -e 

Use the pattern as a regular expression in L<perl(1)> but space is
treated specially.  With this option, you can use I<mg> like
L<egrep(1)> like this:

    mg -e 'foo bar|goo car|hoo dar' ...

See L<perl(1)> for detail of regular expression.  Slash characters (/)
in expression don't have to be escaped.

Option -w puts \b's at the beginning and the end of the pattern.  So
the pattern ``foo|bar'' becomes ``\bfoo|bar\b''.  You probably want to
use ``foo\b|\bbar'' instead.

=item -E 

Use the pattern as regular expression in I<perl> completely.

=item -r 

Specify restriction pattern.  A file becomes a subject for search only
when the pattern specified by -r option is found in the file.  Next
two examples are equivalent.

    mg -e ^Subject: `mg -le "^From: lwall" *`

    mg -er '^From: lwall' ^Subject: *

File will be swallowed at one time even if -W option is not supplied.

=item -c I<n[,n]>

Print n-lines before/after matched line.  Default n is 1, which means
only one line is displayed.  N is number of newlines surrounding
matched pattern.  If ``-c 3'' options is supplied, two lines before
and after matched line will be displayed together.  You can see only
after two lines by ``-c 1,3'' option.

Option -c0 displays matched string only.  Next example is almost
equivalent to L<strings(1)> command with -oa option.

    mg -NEc0 '[ \t\040-\176]{4,}' file

=item -o 

Print the paragraph which contains the pattern.  Each paragraph is
delimited by two or more successive newline characters by default.  Be
aware that an empty line is not paragraph delimiter if which contains
space characters.  Example:

    mg -nQo 'setuid script' /usr/man/catl/perl.l

    mg -o sockaddr /usr/include/sys/socket.h

If -c option is also supplied and there are more lines in the
paragraph, continuous mark is displayed.

=item -O I<string>

Specify paragraph delimiter string as well as activate the paragraph
mode.  The contents of string is evaluated as is inside double-quote
not as a regular expression.  For example, you can get lines between
troff macros like this:

    mg -QoO "\n." 'setuid script' /usr/man/manl/perl.l

=item -C I<chars>

Continuous character.  If you want search sentence continued by other
than white space characters, continuous characters can be set by this
options.  For example, next command finds a sentence even if it is
quoted by `>' or `|' mark.

    mg -C '>|' 'ninja shogun fujiyama' `mhpath all`

To search a pattern in C style comments:

    mg -C '/*' 'setuid scripts' perl.c

Note that continuous characters don't have to be found only top
of the string.  So ``foo bar'' matches a string ``foo>>bar'' on
the previous example.

=item -u 

Underline matched string.  Makes a matched string underlined by
precede each character by ``_^H''.

=item -b 

Make bold matched string.  Makes a matched string overstruck
like ``f^Hfo^Hoo^Ho''.

=item -a 

Print all contents of the file.  This option makes sense only if
used with options like -Q, -u, -b, otherwise it behaves like
L<cat(1)>.

=item -s 

When multiple files are specified, each matched line is preceded by
filename:'' string.  With this option, a newline character is inserted
between the filename and matched line.  This option is useful when the
filenames are very long.

=item -2 

Usually only one line is displayed even if multiple matching occurs
for the same line.  With this option, each match will be displayed in
different line.

=item -R 

Search recursively.  Only files specified by command line arguments
are searched by default.  When invoked with this option and arguments
contain a directory, it is searched recursively.  Can be used with -P.

=item -D I<level>

Descending directory level.  This option specifies how many levels of
directory is to be searched from top directory.

=item -P I<pattern>

Search file pattern.  When directories are searched recursively, only
files which match the `pattern' are searched.  A `pattern' is
specified in wildcard format same as shell and `|' character can be
used for alternative in addition.  For example, you can find a string
foobar'' from all C source files and makefiles like this:

    mg -RP '*.c|[Mm]akefile' foobar /usr/src

=item -V I<pattern>

Exception file pattern.  This is a counterpart of -P.  Only files
which DO NOT match the pattern will be searched.

    mg -RV '*.[oas]' foobar /usr/src

Note that the -V option is also applied to a directory name while -P
option has an effect only for a file.  This means you can specify a
directory name to skip, but can't specify a directory name to search.

=item -F 

Follow symbolic link of a directory.  Doesn't follow by default.

=item -L 

Print formfeed between each matchings.  Print the formfeed
character before each matched line.  This options is useful when
used with -c option and piped to pager command.

=item -S 

Get filenames from standard input.  Read standard input and use each
line as a filename for searching.  You can feed the output from other
command like L<find(1)> for I<mg> with this option.  Next example
searches string from files modified within 7 days:

    find . -mtime -7 -print | mg -S pattern

Next example search the files from the newest first order:

    ls -t | mg -S pattern

You can use -dt option to monitor modified date of processing
files.

=item -m 

Print matched line only when the pattern is across the line.

=item -M 

Print matched line only when multiple matching occurred for the same
line.

=item -f I<file>

Specify the file which contains search pattern.  When file contains
multiple lines, patterns on each lines are search in OR context.  The
line starting with sharp (#) character is ignored.

=item -p pattern

Specify search pattern.  You don't have to use this option explicitly
because the first argument after options will be treated as a pattern.
Typical case of using this option is specifying string which can be
taken as a command option.  You can use option terminator -- for same
purpose.

    mg -p -p file
    mg -- -p file

=item -Z 

Disables automatic uncompress, gunzip.

=item -J I<string>

Convert newline character(s) found in matched string to specifed
I<string>.  Using -J with -c0 option, you can collect searching
sentence list in one per line form.  This is almost useless for
English text but sometimes useful for Japanese text.  For example next
command prints the list of KATAKANA words.

    mg -hEc0 -J '' '\p{utf8::InKatakana}+(?:\n+\p{utf8::InKatakana}+)*' ...

Another example.  If you wonder how the word ``CRC'' is used in RFCs,
you can do it like this:

    mg -h -c0 -J ' ' -ei 'Cyclic Redundancy C\w+' rfc*.txt

=item -0I<digits>

Specifies the record separator as an octal number.  It is almost same
as perl option.  But unlike perl, only the first record is read for
search.  Like perl, -00 means paragraph mode.  Actually I added this
option only to use -00 to search from mail and news header portion.
When reading from archived file, I<mg> emulates perl's $/ behaviour.

=item -W 

Slurp whole file at once.

=item -G I<maxreadsize>[,I<keepsize>]

Specify maximum read size and keep buffer size for next read.  Default
values for these sizes are 2M and 2k bytes.  I<Mg> tries to read a
file up to maximum read size at a same time.  Last part of the buffer,
specified by keep buffer size, is not thrown away but will be used as
a subject for search with next buffer.  In arguments, you can use B,
K, and M for block (512), kilo (1024) and mega (1024 * 1024)
respectively.  Next example sets maximum read size for 100K and keep
buffer size for 10K bytes.

    mg -iBG 100K,10K unix /vmunix

=item -1 

Print first match only.  This option doesn't work well with -a
option.

=item --man 

Show manual page.

=item --color I<auto>|I<always>|I<never>

Use terminal color capability to emphasize the matched text.  Default
is `auto': effective when STDOUT is termnial, not otherwise.  Option
value `always' and `never' will work as expected.

=item --colormode I<rgbUBR>

Specify color mode.  Use combination string from r(ed), g(reen),
b(lue), U(nderline), (bol)D, S(tandout).  Default is rD: red and
Bold.

=item --icode I<code>

Target file is assumed to be encoded in utf8 by default.  Use this
option to set specific encoding.  When handling Japanese text, you may
choose from 7bit-jis (jis), euc-jp or shiftjis (sjis).  Multiple code
can be supplied using multiple option or combined code names with
space or comma, then file encoding is guessed from those code sets.
Use encoding name `guess' for automatic recognition from default code
list which is euc-jp and 7bit-jis.  Following commands are all
equivalent.

    % mg --icode=guess ...
    % mg --icode=euc-jp,7bit-jis ...
    % mg --icode=euc-jp --icode=7bit-jis ...

Default code set are always included suspect code list.  If you have
just one code adding to suspect list, put + mark before the code name.
Next example does automatic code detection from euc-kr, ascii, utf8
and UTF-16/32.

    % mg --icode=+euc-kr ...

=item --ocode I<code>

Specify output code.  Default is utf8.

=item --if I<filter> (or I<EXP:filter:EXP:filter:...>)

You can specify filter command which is applied to each files before
search.  If filter information include multiple fields separated by
colons, first field is perl expression to check the filename saved in
variable $_.  These expression and command list can be repeated.  If
only one filter command is specified, it is applied to all files.
Examples:

    mg --if 'dd conv=ascii' string spoiled_files
    mg --if '/\.tar$/:tar tvf -' pattern *

If the command doesn't accept standard input as processing data, you
may be able to use special device:

    mg -Qz 'nm /dev/stdin' crypt /usr/lib/lib*.a

Filters for compressed and gzipped file is set by default unless -Z
option is given.  Default action is:

    mg --if 's/\.Z$//:zcat:s/\.g?z$//:gunzip -c'

=item --prep I<exp>

You can specify the any Perl expression to preprocess input data.
Some subroutine will be available for this purpose but currently only
&mime'' is prepared.  If ``require'' operator is included in I<exp>,
it is executed only once.  So you can include your special perl
library and use the subroutine defined in it.

&mime  Subroutine ``mime'' decodes encoded string based on
RFC1342 MIME header encoding but current implementation
handles only ISO-2022-JP encoding.  Example:

    mg --prep '&mime' -00 From ~/Mail/inbox/*

Note that, this process is done just before a search for the
output from input filter if --if option is specified.  So in the
above example, MIME encoded string is converted into ISO-2022-JP
even if the input filter was specified to convert the all data
into EUC.

=item --body

Search only from message body. 

This is the reverse of -00 option; it skips RFC822 message header
before string search.  You can find emails which contains string
"Subject" in its message body by this command:

    mg --body Subject *

Otherwise it matches all normal emails.

=item --pgp 

Invoke PGP decrypt command for all files.  PGP passphrase is asked
only once at the beginning of command execution.

=item --pgppass

You can specify PGP passphrase by this option.  Generally, it is not
recommended to use.

=item --exclude I<pattern>

Specify the pattern which should be excluded from searching.  For
example, next command searches string `if' from C source, excluding
comment part.

    mg --exclude '(?s)/\*.*?\*/' if *.c

Since this option is not implemented by preprocessor, line numbers are
still correct and excluded part can be included in surrounding area by
other option such as -o.

=item --exclude I<&function>

If the pattern name begins by ampersand (&) character, it is treated
as a name of subroutine which returns a list to exclude.  Using this
option, user can use arbitrary function to determine from what part of
the text they want to search.  User defined function is written in
.mgrc file or explicitly included by --require option.

    mg --require mycode.pl --exclude `&myfunc' pattern *

Argument can be specified after function name with = character.  Next
example is equivalent to the above example (works on 5.6 or later).

    sub myfunc {
        my($pattern) = @_;
        my @matched;
        my $re = qr/$pattern/m;
        while (/$re/g) {
            push(@matched, $-[0], $+[0]);
        }
            @matched;
    }

    mg --exclude '&myfunc=(?s)/\*.*?\*/' if *.c

--exclude and --include option can be specified simultaneously and
multiple times.

=item --include I<pattern>

Opposite for --exclude.  Next command searches string `if' only from C
source comment.

    mg --include '(?s)/\*.*?\*/' if *.c

=item --require I<filename>

Include arbitrary perl program.

=back


=head1 B<APPENDIX>

You may want to use L<mg(1)> instead of L<grep(1)> from GNU emacs.  In
that case please add following program segment in your .emacs file.

    (defun mg (command)
      "Run mg instead of grep."
      (interactive "sRun mg (with args): ")
      (require 'compile)
      (compile1 (concat "mg -n " command " /dev/null")
    	    "No more mg hits" "mg"))

If you are using version 19, use this.

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

For more recent emacs like version 21, default `grep' function takes
whole command line.

You have to visit uninterested line by (next-error) when surrounding
lines are displayed by -c or -o option.  Use -s option to avoid this.


=head1 B<AUTHOR>

Kazumasa Utashiro


=head1 B<SEE ALSO>

L<grep(1)>, L<perl(1)>


=head1 B<BUGS>

Option -l does not work well with --exclude and --include.  Option -1
may not, either.

Perl5 look-behind expression can be used but it is treated as a bare
regex, because variable length look-behind pattern is not allowed
(yet).  Also since this special treatment is done by very naive
mechanism, you can't use braces within look-behind pattern.  If you
don't like it, please debug.

When using perl older than version 5.6, actual pattern is enclosed by
parentheses, and it confuses the order of subexpressions if it
contains back-references.  The order is fixed automatically but you
may have some problem for certain patterns.  Use -dm option to check
the actual pattern for search when you doubt the behavior of this
command.

Hyphenation is not supported.

No capability to find words separated by nroff footer and header.


=head1 B<LICENSE>

Copyright (c) 1991-2013 Kazumasa Utashiro

Use and redistribution for ANY PURPOSE are granted as long as all
copyright notices are retained.  Redistribution with modification is
allowed provided that you make your modified version obviously
distinguishable from the original one.  THIS SOFTWARE IS PROVIDED BY
THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES ARE
DISCLAIMED.
