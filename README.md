# What is MissingH?

It's a collection of Haskell-related utilities. It is an extension of my earlier work developing MissingLib for OCaml. You can download MissingH from http://software.complete.org/missingh. There is a mirror, with a few days' lag, at http://ftp.debian.org/debian/pool/main/m/missingh.

## Major Features

 * Powerful Logging Framework for Haskell
   This framework provides a system of hierarchical loggers and
   modular handlers permitting fine-grained logging with a great deal
   of control and yet a simple and fast interface.  It's based on
   log4j for Java and logging for Python.

   Also included is a native-Haskell Syslog client.

 * Versatile modules to simplify everyday tasks:
   + FTP client library
   + E-mail client library
   + MIME types library to determine MIME types from files or URLs
   + Configuration file parser/generator

 * IO utilities make it easier to work with line-based text files
   and binary files

 * IO object virtualization so you can use one set of code to work
   on files of many different types

 * Filesystem virtualization so you can access various items with the
   same ease as your system's filesystem

 * Network utilities to streamline connections

 * List utilities including association list tools,
   list splitting, truncation, and delimiter joining

 * String utilities including removal of leading or trailing
   whitespace, joining, splitting, and truncation

 * Other utilities for threads, parsers, filenames, etc.

 * Printf utilities for formatting strings

 * GZip decompression

 * Hundreds of unit tests to verify proper functionality

 * DBM module abstraction

The following modules are are provided at this time, and more are
likely to follow:

Module name | description
------------|-------------
MissingH.AnyDBM | Generic DBM-like database infrastructure
MissingH.AnyDBM.MapDBM | Use a Map in the AnyDBM interface
MissingH.AnyDBM.StringDBM | Simple persistent mapping storage
MissingH.Bits | Obtain individual bytes from a bitfield
MissingH.Cmd | Trap errors during calls to external programs
MissingH.Checksum.* | Utilities for calculating checksums over strings
MissingH.Compression.* | Compression/decompression algorithms
MissingH.Compression.Inflate | The Inflate algorithm from unzip and gunzip
MissingH.ConfigParser | Configuration file parser<br> Interpolation supported<br> Compatible with Python and OCaml ConfigParsers
MissingH.Daemon | Support for detaching from a terminal
MissingH.Debian | Compare two Debian version numbers<br> [ does not require Debian to compile ]
MissingH.Debian.ControlParser | Parse debian/control files or output from various Debian commands that use the same format<br> [ does not require Debian to compile run ]
MissingH.Either | Utilities for the Either type/Error monad
MissingH.Email.Parser | Parse a flat string into component parts<br> Walk through the components of a message
MissingH.Email.Sendmail | Send a message via a locally-installed Sendmail program
MisssingH.FileArchive.GZip | Support for analyzing and extracting GZip archives
MissingH.Hsemail | E-mail parsers
MissingH.HUnit | Utilities for writing HUnit unit tests
MissingH.IO | Copying data between files or handles<br>Lazy operations on line input
MissingH.IO.Binary | Binary I/O with Haskell Strings or [Word8]<br>Lazy operations on binary blocks<br>Binary file copy
MissingH.IO.HVFS | Haskell Virtual File System<br>Lets you emulate real filesystems and work with multiple filesystems
MissingH.IO.StatCompat | Utilities for simulating stat(2) structures on home-grown filesystems or Windows
MissingH.IO.WindowsCompat | Provides POSIX-like functions on Windows
MissingH.List | Association list manipulation<br>List splitting and delimiter joining<br>Truncation
MissingH.Logging | Base logging types
MissingH.Logging.Handler | Base handler types
MissingH.Logging.Handler.Simple | Logging to a stream<br>Logging to a file
MissingH.Logging.Handler.Syslog | Logging to local or remote syslog<br>No C library implementation required
MissingH.Logging.Logger | Primary user interface to logging<br>Documentation for the logging system
MissingH.Map | Flip a Map and other utilities
MissingH.Maybe | forceMaybe utility function
MissingH.MIMETypes | Determine the MIME type of a file<br>Guess an extension based on MIME type
MissingH.Network | Establish TCP connections easily<br>Trap SIGPIPE to prevent odd crashes
MissingH.Network.FTP.Client | Versatile FTP client module<br>Full support for uploads and downloads<br>Passive or standard mode<br>Lazy uploads and downloads<br>Also provides low-level interface to issue advanced server commands
MissingH.Network.FTP.Parser | Parse FTP protocol replies
MissingH.Parsec | Invert the sense of a parser
MissingH.Path | Split apart filename components<br>Recursive directory scanning<br>Recursive file/directory removal (rm -r)
MissingH.Path.Glob | Expand wildcards to obtain a list of files
MissingH.Path.WildPath | Evaluate filenames or strings against wildcards<br>Convert wildcards to regular expressions
MissingH.Regex.Pesco | Perl-like regular expression operations and operators
MissingH.Str | Leading/trailing whitespace removal<br>Beginning/ending tests<br>Joining, splitting, and truncation
MissingH.Str.CSV | Parsing of comma-separated value (CSV) files
MissingH.Threads | Threaded callbacks
MissingH.Time | Utilities for working with times and dates
MissingH.Time.ParseDate | Parsing of dates, similar to strptime() in C
Wash.Mail.* | Generate or parse e-mail messages<br>Full support for headers and MIME
Wash.Utility.* | Base64 codec<br>Various Internet-related parsers

The entire library has no prerequisites save the Haskell standard
library and is designed to install without complexity on a variety of
systems.  It could also easily be embedded within your own source
trees so that users need not have it installed beforehand.

**THIS IS CURRENTLY BETA-QUALITY CODE; MAJOR API FLUCTUATIONS MAY YET OCCUR.**

## Quick Start

See the file INSTALL (Linux/Unix/BSD/Posix) or INSTALL-win.txt (Windows).

## Usage in programs

You can simply use -package MissingH in most compilers to enable
this library.  

Hugs users will need to use -98 +o to use certain modules.

GHC users will need to use this for certain modules:

- `-fallow-overlapping-instances`
- `-fallow-undecidable-instances`
- `-fglasgow-exts`

The API docs can be built with "make doc", or you can find them at:

http://quux.org/devel/missingh

## Author & Homepage

MissingH was written by John Goerzen <jgoerzen@complete.org>.

The latest version may be obtained at:

   http://software.complete.org/missingh

Documentation is also available on that page.

This program is copyrighted under the terms of the GNU General Public License.
See the COPYRIGHT and COPYING files for more details.

If the GPL is unacceptable for your uses, please e-mail me; alternative
terms can be negotiated for your project.

Certain code in MissingH was written by third parties.  Licenses of
these components may vary and are stated in COPYRIGHT.  All code in
MissingH is GPL-compatible, and the work as a whole may be distributed
as a GPL'd work.

arch-tag: general information

