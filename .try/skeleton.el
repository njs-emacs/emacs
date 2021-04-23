(define-skeleton latex-skeleton
  "Inserts a Latex letter skeleton into current buffer.
This only makes sense for empty buffers."
  "Empfänger: "
  "\\documentclass[a4paper]{letter}\n"
  "\\usepackage{german}\n"
  "\\usepackage[latin1]{inputenc}\n"
  "\\name{A. Schröder}\n"
  "\\address{Alexander Schröder \\\\ Langstrasse 104 \\\\ 8004 Zürich}\n"
  "\\begin{document}\n"
  "\\begin{letter}{" str | " *** Empfänger *** " "}\n"
  "\\opening{" _ "}\n\n"
  "\\closing{Mit freundlichen Grüssen,}\n"
  "\\end{letter}\n"
  "\\end{document}\n")

(define-skeleton fortran-skeleton-if-else-endif
  "Insert an if - else - end if region" nil
  > "if (" _ ") then" \n
  -3 "else" \n
  -3 "end if")


(define-skeleton skel-defun
  "Insert a defun template."
  "Name: "
  "(defun " str " (" @ - ")" \n
  "(" @ _ ")" \n)

(defun one (a b c)
  ()

(define-skeleton ska-skel-perl-project
   "Insert much perl code, preparing a real world project."
   (nil)
   "use Getopt::Long;\n"
   "use Pod::Usage;\n"
   "####################################################################\n"
   "##                             OPTIONS\n"
   "####################################################################\n"
   "GetOptions("
   \n "\"help|h!\" => \\my $help,"
   \n "\"version|v!\" => \\my $version"
   \n ") or pod2usage("
   \n "verbose => 0,"
   \n "exitstatus => 1"
   \n ");"
   \n "if ($help) {"
   \n "pod2usage("
   \n "verbose => 1,"
   \n "exitstatus => 0"
   \n ");"
   \n "}"
   \n "if ($version) {"
   \n "print $Version;"
   \n "exit 0;"
   \n "}"
   \n "####################################################################"
   \n "##                               MAIN"
   \n "####################################################################"
   \n ""
   \n "####################################################################"
   \n "##                               SUBS"
   \n "####################################################################" 
   \n "__END__\n"
   "####################################################################\n"
   "##                             Now Docs...\n"
   "####################################################################\n"
   "=head1 NAME"
   "\n"
   \n (file-name-nondirectory buffer-file-name) " - DESCRIBE ME"
   "\n\n"
   "=head1 SYNOPSIS"
   "\n"
   \n (file-name-nondirectory buffer-file-name) " [-h] [-v]" 
   "\n\n"
   "=head1 OPTIONS"
   "\n\n"
   "=over 1"
   "\n\n"
   "=item B<-h|--help>"
   "\n"
   \n "Print help message and exit successfully."
   "\n\n"
   "=item B<-v|--version>"
   "\n"
   \n "Print version information and exit successfully."
   "\n\n"
   "=back"
   "\n\n"
   "=cut\n"
   "")

(define-skeleton ska-skel-perl-sub
   "Insert a perl subroutine with arguments."
   "Subroutine name: "
   "sub " str " {"
   \n "my (" ("Argument name: " "$" str ", ") -2 ") = @_;"
   "\n"
   \n _
   \n "}" '(progn (indent-according-to-mode) nil)
   \n)

sub name {
my ($a, $b, $c) = @_;


}
