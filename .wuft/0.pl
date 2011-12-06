use File::Find ;

sub a {
    find sub {
	-d $_ and next ;
	my $f = $File::Find::name ;
	my @d = (split(m!/!,$f))[1..2] ;
	my $ff = "../../" . join("-",@d) . "-$_" ;
	rename $_,$ff or die ;
	print "$ff\n" ;
    },"." ;
}

sub b {
    for (glob("*.el")) {
	my $o = $_ ;
	s!(.*?)-(frame-default|frame-initial|[^-]+).el!$2--$1.el! ;
	rename $o,$_ or die ;
	print "$_\n" ;
    }
}

	
    
    
