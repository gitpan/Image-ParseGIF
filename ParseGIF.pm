package Image::ParseGIF;
# 
######################################################################
# 
# Parse a GIF image into its component parts.
#  (c) 1999 University of NSW
# 
# Written by Benjamin Low <b.d.low@unsw.edu.au>
#
######################################################################
# 
# Change Log
# 1999/08/30	0.01	First release. BDL
# 1999/09/01	0.02	Fixed image version test. BDL
#
######################################################################
# 

#use diagnostics;	# turn on -w warning explanations (verbose!)
use strict;			# try and pick up silly errors at compile time

use vars qw/@ISA @EXPORT_OK %EXPORT_TAGS $VERSION/;

use Exporter ();
@ISA         = qw/Exporter/;
@EXPORT_OK   = qw//;
%EXPORT_TAGS = qw//;
# add %EXPORT_TAGS to @EXPORT_OK
Exporter::export_tags();
Exporter::export_ok_tags();

$VERSION = 0.02;

use Fcntl qw(:DEFAULT :flock);  # sysopen, flock symbolic constants

use IO::Handle;

sub new
# create a new object
{
	my ($class, $filename, @args) = @_;
	my $args;	# -> %args

	# check if %args were passed in by reference 
	#  ie. there's only one key with no value, and it's a hash ref
	if    (@args == 1 && ref($args[0]) eq 'HASH') { $args = $args[0]; }
	elsif (@args % 2 == 0)                        { $args = {@args}; }
	else  { $@ = "bad argument list"; return undef; }

	# instance data
	my $self = 
	{
		header	=> '',		# GIF header, screen desc, colour table
		parts	=> [],		# (image, control) parts
		trailer	=> '',		# trailer

		debug	=> 0,		# debug state
		output	=> undef,	# IO::Handle to send output to
	};

	# merge arguments
	while (my ($k, $v) = each %{$args})
	{
		$self->{lc($k)} = $v;
	}

	bless ($self, $class);

	# set output filehandle (STDOUT by default)
	$self->output($self->{'output'});

	# open input file if given
	if ($filename) { return $self = undef unless $self->open($filename); }

	return $self;
}

sub debug
{
	my ($self, $l) = @_;
	return defined($l) ? $self->{'debug'} = $l : $self->{'debug'};
}

sub _read
# read and confirm I got what I asked for
#  - read $len and write into buffer at $offset
#    - appends to end of buffer if offset == -1
#  - returns false on error
# e.g.
#  _read($fh, $buf, 1);		# read 1 byte at offset 0
#  _read($fh, $buf, 1, 5);	# read 1 byte at offset 5
#  _read($fh, $buf, 1, -1);	# read 1 byte at offset (len($buf) + 'offset' + 1)
#
{
	my ($io, $buf, $len, $offset) = @_;

	$offset ||= 0;	# 0, if not defined
	$offset += length($buf || '') + 1 if ($offset < 0);
	$offset = 0 if ($offset < 0);

	my $r = $io->read($_[1], $len, $offset);

	unless (defined($r)) { $@ = "read error: $!"; return undef; }
	# eof is ok
	$@ = "short read: $r / $len" unless ($r == 0 or $r == $len);

	return ($r == $len);
}

sub open
# open a gif and parse it
{
	my ($self, $filename) = @_;

	unless (sysopen(IMAGE, $filename, O_RDONLY))
	{
		$@ = $!;
		return undef;
	}

	my $r = $self->parse(*IMAGE);

	close(IMAGE);

	return $r;
}

my $_autoflush = 1;	# class variable

sub _wrap
# wrap a filehandle (a la IO::Wrap, but use IO::Handle instead of FileHandle)
{
	my ($io, $mode) = @_;
	$mode = (($io =~ /\Q*main::STD\E(OUT|ERR)/) ? 'w' : 'r') unless $mode;

	# convert raw scalar to globref, leave globrefs as they are
	no strict 'refs';
	$io = \*$io unless (ref($io) or ref(\$io) eq 'GLOB');
	use strict;

	unless ($io->isa("IO::Handle"))	# dup a filehandle to an IO::Handle
	{
		my $fh = $io;
		$io = new IO::Handle;
		# fdopen() the filehandle directly (i.e. dup the filehandle), rather 
		# than fileno($fh). Using the fileno will cause the (original) file 
		# to be closed when the IO object is destroyed.
		unless ($io->fdopen($fh, $mode))
		{
			$@ = "could not open IO::Handle on [$fh]: $!";
			return undef;
		}

		$io->autoflush($_autoflush);
	}

	return $_[0] = $io;
}

sub autoflush
# class method to turn on/off autoflush for newly created IO::Handles
#  - as per IO::Handle's autoflush, calling autoflush without parameters
# will turn on autoflush.
{
	my ($self, $v) = @_;
	return $_autoflush = (defined($v) ? $v : 1);
}

sub _read_header
{
	my ($self, $io) = @_;
	my $b;		# used to buffer 'headers' which need to be unpacked
	my $flags;	# used for flag bitmaps

	# get the GIF 'signature' (e.g. 'GIF89a')
	_read($io, $self->{'header'}, 6, 0);

	unless ($self->{'header'} =~ /^GIF(\d\d)([a-z])/)
	{
		$@ = "not a GIF - signature is [$self->{'header'}]";
		return undef;
	}

	my @spec_ver = qw/89 a/;
	my @img_ver = ($1, $2);

	# check the image version (note numbering = 87,88,...99, 00, 01, ..., 86)
	{
	local $"='';
	warn "GIF version [@img_ver] greater than [@spec_ver]\n" 
		if (($img_ver[0] < 87 ? $img_ver[0] + 100 : $img_ver[0]) > $spec_ver[0] 
		   or ($img_ver[0] == $spec_ver[0] and $img_ver[1] gt $spec_ver[1]));
	}

	# get logical screen description test for global colour table
	_read($io, $b, 7);
	$self->{'header'} .= $b;

	($flags) = unpack("x4 C x2", $b);

	if ($flags & 0x80)	# get global color table if present
	{
		warn "reading global colour table [" . 
				sprintf ("%d", (3 * 2**(($flags & 0x07) + 1))) . 
				" bytes]\n" if $self->{'debug'};
		_read($io, $self->{'header'}, 3 * 2**(($flags & 0x07) + 1), -1);
	}

	return 1;
}

sub _read_image_descriptor
{
	my ($self, $io, $part) = @_;
	my $b;		# used to buffer 'headers' which need to be unpacked

	warn "reading image descriptor\n" if $self->{'debug'};

	_read($io, $b, 9);
	$self->{'parts'}[$part] .= $b;

	# {
	# my ($lpos, $tpos, $w, $h, $flags) = unpack("v v v v C", $b);
	# warn sprintf("[%d %d %d %d 0x%.2x]\n", 
	# 	$lpos, $tpos, $w, $h, $flags);
	# }

	my ($flags) = unpack("x8 C", $b);
	if ($flags & 0x80)	# local colour map?
	{
		warn "\treading local colour table [" . 
			sprintf ("0x%x", (3 * 2**(($flags & 0x07) + 1))) . 
			" bytes]\n" if $self->{'debug'};
		_read($io, $self->{'parts'}[$part], 
			3 * 2**(($flags & 0x07) + 1), -1);
	}
	#if ($flags & 0x60) { warn "\timage is interlaced\n";}

	# get 'LZW code size' parameter
	warn "\treading LZW code size\n" if $self->{'debug'};
	_read($io, $self->{'parts'}[$part], 1, -1);

	# and now the sub-block/s
	_read($io, $b, 1);	# get block length
	$self->{'parts'}[$part] .= $b;
	while (ord($b) > 0)
	{
		warn "\t reading sub-block [" , ord($b), " bytes]\n" if 
			$self->{'debug'};
		_read($io, $self->{'parts'}[$part], ord($b), -1);
		# get either the block terminator (0x00) (ie. no more blocks), 
		#  or the block size of the next block
		_read($io, $b, 1);	# get block length
		$self->{'parts'}[$part] .= $b;
	}
	warn "\tdone sub-block, next part\n" if $self->{'debug'};

	return 1;
}

sub _read_extension
{
	my ($self, $io, $part) = @_;
	my $b;		# used to buffer 'headers' which need to be unpacked

	warn "reading extension\n" if $self->{'debug'};

	_read($io, $b, 1);	# what kind of extension?
	$self->{'parts'}[$part] .= $b;

	my $t = ord($b);

	if ($t == 0xf9)		# graphic control (precursor to an image)
	{
		# graphic control precedes an image decriptor
		warn "\tgraphic control\n" if $self->{'debug'};

		_read($io, $b, 6);	# get the 'header'

		# zero any delay
		my ($bs, $flags, $delay, $tci, $bt) = unpack("C C v C C", $b);

		$b = pack("C C v C C", $bs, $flags, 0, $tci, $bt);
		$self->{'parts'}[$part] .= $b;
	}
	elsif ($t == 0x01)	# plain text
	{
		warn "\tplain text\n" if $self->{'debug'};
		_read($io, $b, 13);	# 'header'
		_read($io, $b, 1);	# block length
		while (ord($b) > 0)
		{
			warn "\t skipping sub-block [" , ord($b), " bytes]\n" if 
				$self->{'debug'};
			_read($io, $b, ord($b));
			_read($io, $b, 1);	# next block length
		}
	}
	elsif ($t == 0xff)	# application extension - skip
	{
		warn "\tapplication extension\n" if $self->{'debug'};
		_read($io, $b, 12);	# 'header'
		_read($io, $b, 1);	# block length
		while (ord($b) > 0)
		{
			warn "\t skipping sub-block [" , ord($b), " bytes]\n" if 
				$self->{'debug'};
			_read($io, $b, ord($b));
			_read($io, $b, 1);	# next block length
		}
	}
	elsif ($t == 0xfe)	# comment
	{
		warn "\tcomment\n" if $self->{'debug'};
		# no 'header'
		_read($io, $b, 1);	# how long is the block?
		while (ord($b) > 0)
		{
			warn "\t skipping sub-block [" , ord($b), " bytes]\n" if 
				$self->{'debug'};
			_read($io, $b, ord($b));
			_read($io, $b, 1);	# how long is the next block?
		}
	}
	else
	{
		$@ = "invalid extension label found";
		return undef;
	}

	return 1;
}

sub parse
# parse a GIF, reading from a given filehandle or IO object
{
	my ($self, $io) = @_;
	my $b;		# used to buffer 'headers' which need to be unpacked

	unless (_wrap($io))
	{
		$@ ||= "could not wrap [$io]";
		return undef;
	}

	# read header, aborting if it doesn't look like a GIF
	_read_header($self, $io) or return undef;

	# parse the parts
	my $part = scalar(@{$self->{'parts'}});
	my $t;		# block type

	while (_read($io, $b, 1))
	{
		$t = ord($b);

		if ($t == 0x3b)		# trailer
		{
			$self->{'trailer'} = $b;
			$@ = "invalid trailer" unless (ord($self->{'trailer'}) == 0x3b);
			next;
		}

		$self->{'parts'}[$part] .= $b;

		if ($t == 0x2c)		# image descriptor
		{
			_read_image_descriptor($self, $io, $part);
			$part++;	# start the next part
			next;
		}

		if ($t == 0x21)		# some kind of extension
		{
			_read_extension($self, $io, $part);
			next;
		}

		# fall-through
		$@ = "invalid block label found";
		return undef;
	}

	return 1;

}

sub header
# Return the image header
{
	return shift->{'header'};
}

sub trailer
# Return the image trailer
{
	return shift->{'trailer'};
}

sub parts
# Return list of the image parts in array context, or number of parts in 
# scalar context.
{
	return wantarray ? @{shift->{'parts'}} : scalar(@{shift->{'parts'}});
}

sub part
# Return scalar ref to an image part
#  - part == undef gives header
#  - part > num_parts gives trailer
{
	my ($self, $part) = @_;

	return \$self->{'header'} unless defined($part);
	return \$self->{'trailer'} if ($part > $#{$self->{'parts'}});
	return \$self->{'parts'}->[$part];
}

sub output
# Specify output filehandle.
{
	my ($self, $io) = @_;

	$io = *STDOUT unless defined($io);
	warn "output to $io\n" if ($self->{'debug'} > 1);
	_wrap($io);

	$self->{'output'} = $io;
}

sub print_part
# print a part to given fh, or default (set via ::output()) if none supplied
{
	my ($self, $part, $io) = @_;

	# sort out the parameter order: ($io), ($part, $io) or ($io, $part)
	if (ref(\$part) eq 'GLOB') { ($io, $part) = ($part, $io) }

	$io = defined($io) ? _wrap($io) : $self->{'output'};

	warn "printing part $part to $io\n" if ($self->{'debug'} > 1);
	$io->print(${$self->part($part)});
}

sub print_parts
# print zero or more parts to given / default fh
{
	my ($self, $part, $io) = @_;

	# sort out the parameter order: ($part), ($io), ($part, $io) or ($io, $part)
	if (ref(\$part) eq 'GLOB') { ($io, $part) = ($part, $io) }

	$io = defined($io) ? _wrap($io) : $self->{'output'};

	# where were we up to?
	my $ppart = $self->{'_ppart'} || 0;
	warn "printing parts $ppart - $part to $io\n" if ($self->{'debug'} > 1);
	while ($ppart <= $part)
	{
		$io->print(${$self->part($ppart++)});
	}
	$self->{'_ppart'} = $ppart;
}

sub print_percent
{
	my ($self, $p, $io) = @_;
	$p = 1 if $p > 1;
	$self->print_parts(int($p * @{shift->{'parts'}} + 0.5));
}

sub print_header
{
	my ($self, $io) = @_;
	$io = defined($io) ? _wrap($io) : $self->{'output'};
	warn "printing header to $io\n" if ($self->{'debug'} > 1);
	$io->print($self->{'header'});
}

sub print_trailer
{
	my ($self, $io) = @_;
	$io = defined($io) ? _wrap($io) : $self->{'output'};
	warn "printing trailer to $io\n" if ($self->{'debug'} > 1);
	$io->print($self->{'trailer'});
}


1;	# return true, as require requires

__END__

#
######################################################################
#

=head1 NAME

Image::ParseGIF - Parse a GIF image into its compenent parts.

=head1 SYNOPSIS

  use Image::ParseGIF;

  $gif = new Image::ParseGIF ("image.gif") or die "failed to parse: $@\n";

  # show only the first frame
  print $gif->header;
  print $gif->part(0);
  print $gif->trailer;
  #  or, without passing scalars around:
  $gif->print_header;
  $gif->print_part(0);
  $gif->print_trailer;


  # send an animated gif frame by frame
  #  - makes for a progress bar which really means something
  $gif = new Image::ParseGIF ("progress.gif") or die "failed to parse: $@\n";

  $gif->print_header;

  $gif->print_percent(0.00);	# starting...
  do_some_work_stage1();

  $gif->print_percent(0.10);	# 10% complete
  do_some_work_stage2();

  $gif->print_percent(0.25);	# 25% complete
  do_some_work_stage3();

  $gif->print_percent(0.70);	# 70% complete
  do_some_work_stage4();

  $gif->print_percent(1.00);	# done!

  $gif->print_trailer;

=head1 DESCRIPTION

This module parses a Graphics Interchange Format (GIF) image into
its component 'parts'. A GIF is essentially made up of one or more 
images - multiple images typically are used for animated gifs.

=head2 PURPOSE

This module was written to allow a web application to display the status
of a request, without resorting to scripting or polling the server via 
client refresh.

Most web browsers (at least Netscape 2.02 & 4.05, Opera 3.21 and
Internet Explorer 4.0) show each frame as soon as it is received.
(Indeed, the GIF format is block-based so any image viewer which can
handle animated GIFs should operate similarly.) So, if we can arrange
for the parts of a progress bar image to be delivered to the client in
sympathy with the request's progress, we'll have made a dandy real-time
feedback mechanism. Contrast this with the common fixed animation used
at many sites, which are uncorrelated to the request state and not
at all realistic.

One implementation of this status mechanism involves the main application 
returning a page containing an image tag to a 'status' script (e.g. 
S<<img src="status.cgi?id="request_id">>). The status script interrogates 
the request's status (perhaps by reading from a pipe), and prints image frames 
as the request progresses.

At the moment the image is wholly read into memory. File
pointers could be used to operate from disk, however, given that most
web images are on the order of 1-100k, I don't see a lot of benefit in
bothering to do so.  Also, if a persistant application is used
(i.e. FCGI), the same image can be reused for many requests.

=head2 BACKGROUND

A gif image consists of:

=over

=item 1

a 'header'

=item 2 

a global colour map

=item 3 

zero or more 'graphic blocks' or 'extensions'

=over

=item *  

a block/extension header

=item *  

one or more sub-blocks per part

=back


=item 4  

a trailer

=back

There are two types of 'descriptor blocks/extensions' defined in the GIF
specification [1]: an image descriptor; or an extension. Extensions can
contain 'control' information for things like animated gifs. Each descriptor
block/extension has its own 'header', often followed by one or more data
blocks. This module extracts only image desciptors and graphic control
extensions. Moreover, this module treats associated descriptor blocks and
extensions as a 'part' - an image 'part' is considered to be all extensions
prior to an image descriptor, plus the image descriptor.


=head1 CONSTRUCTOR / CLASS METHODS

In general, all C<Image::ParseGIF> methods return undef on error, possibly 
with an explanatory message in $@.

=over

=item autoflush ( STATE )

Class method (i.e. Image::ParseGIF::autoflush(0)). Sets default autoflush()
state for new output streams. On by default.

=item new ( [FILENAME] )

=item new ( FILENAME [,  ARGUMENTS ] )

=item new ( FILENAME [, {ARGUMENTS}] )

Creates a new C<Image::ParseGIF> object. If FILENAME is supplied, opens and 
parses the given file. ARGUMENTS may be: 'Debug' (natural number) to set the 
debug level; 'Output' (IO::Handle/filehandle/glob) to set the default output 
stream (default STDOUT).

=back

=head1 METHODS

=over

=item debug ( LEVEL )

Sets/gets the surrent debug level. The higher the debug level, the more 
output.

=item open ( FILENAME )

Opens the given file and C<parse>s it.

=item parse ( IO )

Parse a GIF, reading from a given filehandle / IO::Handle.

=item (header|trailer)

Return the image (header|trailer) as a scalar.

=item parts

Return list of the image parts in array context, or number of parts in 
scalar context.

=item part ( PART )

Return a scalar reference to an image part. If PART == 0, returns header; if 
PART > number of parts, returns trailer.

= item output ( IO )

Specifies the default output filehandle / IO::Handle for all subsequent print_ 
calls.

=item print_(header|trailer) ( [IO] )

Prints the (header|trailer) to the supplied / default output stream.

=item print_part ( [PART] [, IO] )

Prints the given PART to the supplied / default output stream.

=item print_parts ( [PART] [, IO] )

Print multiple parts to the supplied / default output stream. Remembers 
which part it was up to (PreviousPART), prints from from PreviousPART to PART.

=item print_percent ( PERCENT [, IO ] )

Print PERCENT percent of the image frames. Remembers where it was up to, and 
will only print increasing part numbers (i.e. it won't duplicate parts).

=back

=head1 COPYRIGHT

Copyright (c) 1999 University of New South Wales 
Benjamin Low <b.d.low@unsw.edu.au>. All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Artistic License for more details.

=head1 AUTHOR

Benjamin Low <b.d.low@unsw.edu.au>

=head1 SEE ALSO

This code was based on the CGI 89a spec [1] and the Image::DeAnim module
by Ken MacFarlane.

[1] http://member.aol.com/royalef/gif89a.txt


=cut
