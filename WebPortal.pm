package Biblio::WebPortal; 
use Biblio::Thesaurus;

require 5.005_62;
use strict;
use warnings;
use CGI qw/:standard/;

use XML::DT; ### DT redefines dt(

use DB_File;
use Fcntl ;
use Data::Dumper ;

require Exporter;

our @ISA = qw(Exporter);
our @EXPORT_OK = qw( &mkdiglib &see_also);
our @EXPORT = qw( );
our $VERSION = '0.04';
our $marca = '===';

sub URL {
  my $conf = shift;
  return exists($conf->{scriptname})?$conf->{scriptname}:CGI::url();
}

#
# NAVIGATE
#
sub navigate {
  my $self = shift;
  my $conf = {};
  $conf = shift if ref($_[0]);
  my %args = @_;
  my @ids;
  my $html = "";

  my $sampleSize = $args{size} || 10;

  if (exists($args{p}) && $args{p} =~/!/) {
    $args{p} = $`;
    $args{t} = $';
  }

  if (exists($args{t}) && $args{t} eq "") {
    $args{t} = $self->{the}->top_name;
  }

  $args{t} = $self->{the}->top_name if (!exists($args{t}) && !exists($args{p}));

  $html .= $self->{the}->navigate({%$self, %$conf, expand => [ 'HAS','NT','INST' ]},%args);

  my @terms = $self->{the}->tc($args{t},'BT','POF','IOF');

  $html .= "<p>" . start_form('POST');
  $html .= "procurar ". textfield('p'). "em ";
  $html .= popup_menu('t', [ @terms ]);
  $html .= submit(" Procurar "), end_form;
  $html .= "</p>";

  if (defined($args{t})) {
    if (defined($args{p})) {
      @ids = (sort {$a <=> $b } ( $self->f4($args{p},$args{t}))) ;
    } else {
      @ids = (sort{$a <=> $b }( $self->f1($args{t})));
    }
  } else {
    if (defined($args{p})) {
      @ids = sort{$a <=> $b } ( $self->f3($args{p}));
    } else {
      #...
    }
  }

  my $total = scalar @ids;

  if ($total) {
    my ($low,$high);
    if (exists($args{page})) {
      ($low,$high) = ($args{page}*$sampleSize,
                      $args{page}*$sampleSize+$sampleSize-1);
    } else {
      ($low,$high) = (0,$sampleSize-1);
      $args{page} = 0;
    }
    $high = $total-1 if $high >= $total;

    # Print navigation bar
    if (defined($args{all}) && $args{all}) {
      $html.="<small>Ver as ";
      $html.="<a href=\"".URL($self)."?".dump_url(%args,all=>0,page=>0);
      $html.="\">$sampleSize</a> primeiras entradas.";
      $html .= "</small><br><br>";
      $html .= $self->htmlofids(@ids);
    } elsif( $total > $sampleSize){
      $html.="<small>";
      $html .= "Entradas ".(1+$low)." a ".(1+$high)." de um total de $total.";
      if ($low != 0 ) {
	$html.="&nbsp;|&nbsp;";
	$html .= "<a href=\"".URL($self)."?".dump_url(%args,all=>0,page=>$args{page}-1);

	my $prv = (exists($self->{bt_prev_txt}))?$self->{bt_prev_txt}:"&lt;&lt;";
	$html .= "\">$prv</a>";
      }
      if ($high < $total-1) {
	$html.="&nbsp;|&nbsp;";
	$html .= "<a href=\"".URL($self)."?".dump_url(%args,all=>0,page=>$args{page}+1);

	my $nxt = (exists($self->{bt_next_txt}))?$self->{bt_next_txt}:"&gt;&gt;";
	$html .= "\">$nxt</a>";
      }
      $html.="&nbsp;|&nbsp;";
      $html.="<a href=\"".URL($self)."?".dump_url(%args,all=>1)."\">all</a>";
      $html .= "</small><br><br>";
      $html .= $self->htmlofids(@ids[$low..$high]);
    } else {
      $html .= $self->htmlofids(@ids);
    }
  }
  $html;
}

sub entryAsText {
  my $type = shift;
  my $entry = shift;
  return (exists($type->{asText}))?&{$type->{asText}}($entry):"";
}

sub entryAsHTML {
  my $type = shift;
  my $entry = shift;
  return (exists($type->{asHTML}))?&{$type->{asHTML}}($entry):"";
}

sub entryAsLaTeX {
  my $type = shift;
  my $entry = shift;
  return (exists($type->{asLaTeX}))?&{$type->{asLaTeX}}($entry):"";
}

sub entryAsRelations {
  my $type = shift;
  my $entry = shift;
  my ($return,@remaining) = (exists($type->{asRelations}))?&{$type->{asRelations}}($entry):{};

  if (ref($return) ne "HASH" || !ref($return)) {
    print STDERR "Function 'asRelation' should return an hash reference! Check documentation\n";
    print STDERR "Using supplied list as { rel => list }\n";
    if (ref($return)) {
      return { rel => $return };
    } else {
      return { rel => [$return, @remaining] };
    }
  } else {
    if (ref($return) eq "HASH") {
      return $return;
    } else {
      return {};
    }
  }
}

sub entry_record {
  my $catindex = shift;
  my $cid = shift;
  my $cattype = shift;
  my $entry = shift;
  my $userconf = shift;

  my $result;

  # This is the id for the catalog, a dot, and the id for the entry
  $result->[0] = "$catindex.$cid";

  # entryAsRelations now returns a reference to an hash.
  my $rels = entryAsRelations($cattype,$entry);

  # because we are still using a reference to a list, this code
  # constructs such a list;
  my @rels = ();
  for (keys %$rels) {
    next if $_ eq "id";
    next if $_ eq "type";
    if (ref($rels->{$_})) {
      push @rels, @{$rels->{$_}};
    } else {
      push @rels, $rels->{$_};
    }
  }
  $result->[1] = [ @rels ];

  # Type is the Type for the entry
  #  - if there is a relation named 'type', return it;
  #  - else, return the catalog id
  #
  # Note that at the moment, this is _not_ saved... used only
  # to calculate the entryAsHTML
  $result->[5] = (exists($rels->{type}))?$rels->{type}:$catindex;


  # This is the HTML for the entry
  $result->[2] = boxing($userconf, entryAsHTML($cattype, $entry), $result->[5]);
  # This is the Text for the entry
  $result->[3] = entryAsText($cattype,$entry);
  # This is the LaTeX for the entry
  $result->[4] = entryAsLaTeX($cattype,$entry);

  return $result;
}


#
# MAKE DIGITAL LIBRARY
#
sub mkdiglib{
  my $userconf = shift;
  my $i;

  ## First, create the directory (if it does not exists);
  mkdir $userconf->{name} unless -d $userconf->{name};

  ## Compute the thesaurus in storable format
  my $the=thesaurusLoad("$userconf->{thesaurus}");
  $the->storeOn("$userconf->{name}/thesaurus.store");

  my @a;
  my @regs = ();
  my $reftype;

  open(CATALOGS,">$userconf->{name}/catalogs.index");

  if ($reftype = ref($userconf->{catalog})) {
    ### THIS IS A MULTICATALOG
    my $catindex = 0;
    die ("Not an array of catalogs") unless $reftype eq "ARRAY";
    my @catalogs = @{$userconf->{catalog}};
    foreach my $catalog (@catalogs) {
      die "missing catalog filename" unless defined $catalog->{file};

      my @catalog_files = ();
      # check for the file name, or skip
      if (ref($catalog->{file}) eq "ARRAY") {
	@catalog_files = @{$catalog->{file}};
      } else {
	@catalog_files = ($catalog->{file});
      }
      for my $file (@catalog_files) {

	# check for the file type, or skip
	die "missing catalog type" unless defined $catalog->{type};
	my $type = $catalog->{type};

	print "Processing '$file'";
	my $count = 0;

	if (ref($type) eq "HASH") {
	  my $cid = 0;
	  # type is an hash of functions
	  my @entries = &{$type->{asList}}($file);
	  for (@entries) {
	    print "."; $count++;
	    push @regs, entry_record($catindex, $cid, $type, $_, $userconf);
	    $cid++;
	  }
	} elsif (ref($type) eq "ARRAY") {
	  #next, I dont know what to do with an array
	} else {
	  # type is an identifier
	}
	print "$count entries\n";
	print CATALOGS "$catindex:$file\n";
	$catindex++;
      }
    }
  } else {
    my @entries = &{$userconf->{catsyn}{asList}}($userconf->{catalog});
    my $cid = 0;
    for (@entries) {
      push @regs, entry_record(0,$cid,$userconf->{catsyn},$_,$userconf);
      $cid++;
    }
    print CATALOGS "0:$userconf->{catalog}\n";
  }
  close CATALOGS;

  open(IDINCAT,">$userconf->{name}/entry-catalog.index");
  open(H1,">$userconf->{name}/relation.index");
  open(H2,">$userconf->{name}/text.index");
  open(LOG, ">$userconf->{name}/thesaurus.log");

  (unlink "$userconf->{name}/html.db"||die) if -f "$userconf->{name}/html.db";
  (unlink "$userconf->{name}/relations.db"|| die) if -f "$userconf->{name}/relations.db";

  my %h = ();
  my $hand1 = tie %h, "DB_File", "$userconf->{name}/html.db",
    O_RDWR|O_CREAT, 0664, $DB_BTREE or die $!;

  my %h2 = ();
  my $hand2 = tie %h2, "DB_File", "$userconf->{name}/relations.db",
    O_RDWR|O_CREAT, 0664, $DB_BTREE or die $!;

  my %h3 = ();
  my $hand3 = tie %h3, "DB_File", "$userconf->{name}/latex.db",
    O_RDWR|O_CREAT, 0664, $DB_BTREE or die $!;

  print "Creating hashes";
  my %unknown;
  for(@regs) {
    $i++;
    print ".";
    print IDINCAT "$i:$_->[0]\n";
    my @rray = ();
    for my $_term_ (@{$_->[1]}) {
      push @{$unknown{$_term_}}, $_->[0] unless ($the->isdefined($_term_));
      push @rray, $the->translateTerm($_term_);
    }

    $h{$i}= $_->[2];
    $h3{$i}= $_->[4];
    print H1 "$i$marca", join(" / ", @rray),"\n";
    print H2 "$i$marca", $_->[3],"\n" ;

    for my $q ( @rray ) {
      $h2{$q} .= "$i|";
    }
  }
  close IDINCAT;
  print LOG "outros\n",
    join("\n",map{"# on registers ".join(",",@{$unknown{$_}})."\nNT\t$_"}sort keys %unknown),"\n";
  print "$i entries\n";

  close H1;
  close H2;

  open(H,">$userconf->{name}/relations.list");
  open(H1,">$userconf->{name}/relations.statistics");

  for(keys %h2){
    my  $howmany = $h2{$_} ;
    $howmany =~ s/\d//g;
    print H "$_\n";
    print H1 "$_$marca", length($howmany),"\n";
  }
  close H1;
  close H;

  close LOG;

  ## Compute sizes
  my $self = bless {
		    name => $userconf->{name},
		    the => thesaurusRetrieve("$userconf->{name}/thesaurus.store"),
		    db => \%h,
		    dbLaTeX => \%h3,
		    tt2 => \%h2,
		   };
  $the->setExternal("LEN");

  $the->describe( { rel=>"LEN",
		    desc => "Number of documents: ",
		    lang => "UK"});

  $the->describe( { rel=>"LEN",
		    desc => "Número de documentos: ",
		    lang => "PT"});

  for my $term (keys %{$the->{defined}}) {
    my @ids = $self->f1($term);
    my $count = scalar @ids;
    $the->addRelation($term,"LEN",$count);
  }
  $the->complete();
  $the->storeOn("$userconf->{name}/thesaurus.store");

  undef $self;

  $hand1->sync();
  undef $hand1;
  untie %h;

  $hand2->sync();
  undef $hand2;
  untie %h2;

  $hand3->sync();
  undef $hand3;
  untie %h3;

}

#
# OPEN DIGITAL LIBRARY
#
sub opendiglib{
  my $cf = shift;
  my %h;
  tie %h, "DB_File", "$cf->{name}/html.db", O_RDONLY, 0644, $DB_BTREE or die $!;
  my %h2;
  tie %h2, "DB_File", "$cf->{name}/relations.db", O_RDONLY, 0644, $DB_BTREE or die $!;
  my %h3;
  tie %h3, "DB_File", "$cf->{name}/latex.db", O_RDONLY, 0644, $DB_BTREE or die $!;

  my %self = (%$cf);
  $self{the} = thesaurusRetrieve("$cf->{name}/thesaurus.store");
  $self{db}  = \%h;
  $self{dbLaTeX}  = \%h3;
  $self{tt2} = \%h2;
  return bless \%self;
}

#
# Auxiliary function.
#  Search given a thesaurus term;
#
sub f1{
  my ($self, $tt) = @_;
  my @x = tt2ids($self, $self->{the}->tc($tt,"NT","HAS","INST") );
  return @x;
}

#
# Auxiliary function.
#
sub orf1{
  return tt2ids(@_);
}

#
# Auxiliary function.
#
sub andf1{
  my ($self, $tt1, $tt2) = @_;
  my @r1 = f1($self, $tt1);
  my @r2 = f1($self, $tt2);
  inter(\@r1,\@r2);
}

#
# Auxiliary function.
#
sub f3{
  my ($self,$er)=@_;
  return grepcut1("$self->{name}/text.index", $er);
}

#
# Auxiliary function.
#
sub f4{
  my ($self,$er, $tt)=@_;
  my @r1 = f3($self,$er);
  my @r2 = f1($self,$tt);

  return inter(\@r1,\@r2);
}

#
# Auxiliary function
#   union of a list of lists
sub union {
  my %h;
  for my $a (@_){
    @h{@$a} = @$a;
  }
  (keys %h);
}

#
# Auxiliary function
#   interception of a list of lists
sub inter {
  my %h;
  my $a = shift;
  @h{@$a} = @$a;
  my @r = keys %h;
  for $a (@_){
    @h{@r} = @r;
    @r = grep {defined ($_)} @h{@$a}; }
  @r;
}

sub tt2ids{
  my ($self,@tt)=@_;
  my @a=();
  my %a=();
  for my $tt(@tt){
    for(grepcut1("$self->{name}/relations.list",$tt)) {
      print "WARNING: \$_=$_\n" unless defined $self->{tt2}{$_};
      push(@a ,(split(/\|/,$self->{tt2}{$_})));
    }
  }
  @a{@a}=@a;
  (keys %a);
}

sub grepcut1{
  my ($f,$er)=@_; 
  $er = '\?' if $er eq "?";
  open F, "$f" or die;
  my @r = ();
  while(<F>){ push(@r,$1) if (/$er/i && /^(.+?)($marca|\n)/); }
  close F;
  @r
}

sub htmlofids{
  my $self=shift;
  join("\n",
       ( map { exists($self->{db}{$_})?$self->{db}{$_}:"<!--\n................. $_ -->\n" 
	     } grep { defined($_) && $_ !~ /^(\s|\n|\t)*$/} @_));
}

sub latexofids{
  my $self=shift;
  join("\n",
       ( map { exists($self->{dbLaTeX}{$_})?$self->{dbLaTeX}{$_}:"\n%.... $_ \n"
	     } grep { defined($_) && $_ !~ /^(\s|\n|\t)*$/} @_));
}

sub dump_url {
  my %args = @_;
  return join("&",map { $args{$_}=~s/\s/+/g;"$_=$args{$_}"} keys %args);
}

sub boxing {
  my ($userconf, $title, $body, $url) = @_;
  my $return;

  if (defined($userconf->{boxing})) {
    $return = &{$userconf->{boxing}}($title,$url,$body);
  } else {
    if (defined($body)) {
      if (defined($url) && $url !~ /^\s*$/) {
        $title = "<a href=\"$url\">$title</a>";
      }
      $return = "<b>$title</b>";
      $return.= "<div style=\"margin-left: 15px\"><small>$body</small></div>";
    } else {
      $return = $title;
    }

    $return = "<div style=\"margin: 10px;\">$return</div>";

    $return =~ s!\^(.{1,90}?)\^!see_also($userconf, $1)!ge;
  }

  return $return;
}

sub n {
  my $x = Biblio::Thesaurus::term_normalize(shift);
  $x =~ s/ /+/g;
  return $x;
}

sub search {
  my $self = shift;
  my %query = @_;
  if (exists($query{term})) {
    if (exists($query{regexp})) {
      return $self->f4($query{regexp},$query{term});
    } else {
      return $self->f1($query{term});
    }
  } else {
    if (exists($query{regexp})) {
      return $self->f3($query{regexp});
    } else {
      return ()
    }
  }
}

sub see_also {
  my $conf = shift;
  my $term;
  my $url = "";
  if (ref($conf)) {
    $url = $conf->{scriptname} || "";
    $term = shift;
  } else {
    $term = $conf;
  }

  my $string = shift || $term;
  my $query;

  if ($term =~ m/!/) {
    $query = "?t=".n($')."&p=".n($`);
    $string =~ s/^!//;
    $string =~ s/!$//;
  } else {
    $query = "?t=".n($term);
  }

  return "<a href=\"$url$query\">$string</a>";
}

sub bf_tree {
  my ($self, $terms, $visited, $tree) = @_;

  my %v = (defined($visited))?%{$visited}:();

  my @terms = @$terms;

  return $tree unless @terms;

  my @nodes = ();
  for my $sterm (@terms) {
    my @sdocs = grep {!exists($v{$_})} $self->tt2ids($sterm);
    my @sons = grep {!exists($tree->{$_})} $self->{the}->terms($sterm, qw/NT UF/);
    @nodes = (@nodes, @sons);

    $tree->{$sterm}{docs} = \@sdocs;
    $tree->{$sterm}{childs} = \@sons;

    @v{@sdocs} = @sdocs;
  }

  my %x;
  @x{@nodes} = @nodes;
  @nodes = keys %x;
  return $self->bf_tree(\@nodes, \%v, $tree);
}

sub asHTML {
  my $self = shift;
  my $term = shift || $self->{the}->top_name;
  my $size = shift;
  my $tree = shift || $self->bf_tree([$term]);

  if ($size) {
    $size = "h".($1+1) if ($size =~ m!^h(\d)$!i);
    $size = "b" if $size =~ m!^h7$!i;
  } else {
    $size = 'h1';
  }

  my @docs = @{$tree->{$term}{docs}};
  my @terms = @{$tree->{$term}{childs}};

  my $stree =  join("\n", map {$self->asHTML($_,$size,$tree)} @terms);
  if (@docs || $stree !~ /^[\s\n]*$/) {
    my $res="<$size>$term</$size>\n";
    $res .= "<blockquote>";
    $res .= $self->htmlofids(@docs);
    $res .= $stree;
    $res .= "</blockquote>";
  } else {
    return "";
  }
}

sub asLaTeX {
  my $self = shift;
  my %opt = ( size => "section",
	      levels => 0 );
  if (ref($_[0]) eq "HASH") {
    %opt = (%opt, %{shift(@_)})
  }
  my $term = shift || $self->{the}->top_name;
  my $size = $opt{size};
  my $tree = shift || $self->bf_tree([$term]);
  my $nsize = "";

  $nsize = "subsubsection" if ($size eq "subsection");
  $nsize = "subsection" if ($size eq "section");
  $nsize = "section" if ($size eq "undersection");
  $nsize = "subsection" if ($size eq "undersubsection");
  $nsize = "subsubsection" if ($size eq "undersubsubsection");

  my @docs = @{$tree->{$term}{docs}};
  my @terms = @{$tree->{$term}{childs}};

  my $stree =  join("\n", map {$self->asLaTeX({size => $nsize},$_,$tree)} @terms);
  if (@docs || $stree !~ /^[\s\n]*$/) {
    my $res = "";
    $res = "\\".$size."{". ucfirst($term)."}\n" unless ($size =~ /^under/) ;
#    $res .= "\\begin{quote}";
    $res .= $self->latexofids(@docs);
    $res .= $stree;
#    $res .= "\\end{quote}";
  } else {
    return "";
  }
}

1;
__END__

=head1 NAME

Biblio::WebPortal - Perl extension for Digital Library support

=head1 SYNOPSIS

  use Bilio::WebPortal;

  $a = mkdiglib($conf)

  $a->search( term => 'animal', regexp => 'water' );

  $a->asHTML();
  $a->asLaTeX();

  print $diglib->navigate(%vars);

=head1 DESCRIPTION

Biblio::WebPortal uses Biblio::Thesaurus and a configuration file to
manage digital libraries in a simple way. For this purpose, we define
a digital library as a set of searchable catalogs and an ontology for
that subject. Biblio::WebPortal configuration file has a list of
catalogs with their respective parse information.

To this be possible, it should be some way to access any kind of
catalog: a plain text file, XML document, SQL database or anything
else. The only method possible is to define functions to convert these
implementation techniques into a mathematical definition. So, the user
should give four functions to this module to it be capable of use the
catalog. These functions are:

=over 4

=item split the catalog

Given a string (say, a catalog identifier) the function should return
a Perl array with all catalog entries. This array should be the same
everytime the function is called for the same catalog to maintain some
type of indexing. The function can use this string as a filename, a
SQL table identifier or anything else the function can understand.

=item terms for an entry

Given an entry with the format returned by the previous function, this
function should return a list of terms related to the object
catalogued by this entry. These terms will be used latter for
thesaurus integration.

=item html from the entry

Given an entry, return a piece of HTML code to be embebed when listing
records.

=item text from the entry

Given an entry, return the searchable text it includes.

=back

The following example shows a sample configuration file:

  $userconf = {
    catalog   => "/var/library/catalog.xml",
    thesaurus => "/var/library/thesaurus",
    name => 'libraryName',
    catsyn  => {
       asList => sub{ my $file=shift;
                 my $t=`cat $file`;
                 return ($t =~ m{(<entry.*?</entry>)}gs); },
       asRelations => sub{ my $f=shift;
                 my $data;
                 while($f =~ m{<rel\s+tipo='(.*?)'>(.*?)</rel>}g)
                    { push @{$data->{$1}}, $2; }
                 $data; },
       asHTML => sub{ my $f=shift; &mp::cat::fichacat2html($f)},
       asLaTeX => sub{ ... },
       asText => sub{ my $f=shift;
                 $f =~ s{</?\w+}{ }g;
                 $f =~ s/(\s*[\n>"'])+\s*/,/g;
                 $f =~ s/\w+=//g;
                 $f =~ s/\s{2,}/ /g;
                 $f }  }  };

When using the C<mkdiglib> function with this configuration
information, the module will create a set of files with cached data
for quick response, inside a C<libraryName> directory. This function
returns a library object.

The configuration file can refer to more than one catalog file. This
is done with the following syntax:


  $userconf = {
    thesaurus => "/var/library/thesaurus",
    name => 'libraryName',
    catalog   => [
      { file => "/var/library/catalog.xml",
        type => {
           asList => sub{ ... },
           asRelations => sub{ ... },
           asHTML => sub{ ... },
           asText => sub{ ... },
           asLaTeX => sub{ ... },
        } },
      { file => ["/var/library/data1.db", "/var/library/data2.db"],
        type => {
           asList => sub{ ... },
           asRelations => sub{ ... },
           asHTML => sub{ ... },
           asText => sub{ ... },
        } },    ] }


After creating the object, we can open it on another script with the
C<opendiglib> command wich receives the base name of the digital
library. The base name is the path where it was created concatenated
with the identifier used.

The most common way to use the digital library is to build a script
like:

  use Biblio::WebPortal;
  use CGI qw/:standard :cgi-bin/;

  my $library = "/var/library/libraryName";
  my %vars = Vars();

  print header;
  my $diglib = Biblio::WebPortal::opendiglib( { name => $library } );

  print $diglib->navigate(%vars);

The following attributes can be used in conjuntion with the previous
configuration:

=over 4

=item B<scriptname>

This should be used whenever the module can't detect the correct
script name on the navigate method. Use it to point to the correct
place.

=item B<bt_next_txt>

Set this attribute to the string you want to see with the link to the
next page of search results;

=item B<bt_prev_txt>

Set this attribute to the string you want to see with the link to the
previous page of search results;

=back

Note that configuration options from C<Biblio::Thesaurus> navigate
method are allowed in the configuration file;

=head2 Module Interface

=over 4

=item B<navigate>

This method is used to navigate over a digital library. It should be called
with the hash of variables passed by the CGI;

=back


=head2 The Digital Library directory

When creating a Biblio::WebPortal object (a digital library), a
directory is created, with the name given in the configuration
file. This directory contais a set of files, each one of them with
already processed information.

=over 4

=item C<catalogs.index>

This is a text file. It contains a map between integers and processed
catalogs. Each line consists of a sequential integer (beginning in 0),
two dots and a fullpath to the catalog file.

All other databases will use that integer when referring to a catalog.

   0:/home/user/diglib/catalog1.xml
   1:/home/user/diglib/catalog2.xml

=item C<entry-catalog.index>

Another text file which maps digital library identifiers to entries in
each different catalog. Biblio::WebPortal will assign a different
integer to each entry, no matter the catalog it is from. This file
contains, in each line, the entry identifier in the digital library,
two dots, the identifier of the catalog it cames from (the identifier
defined in the C<catalogs.index> file), a dot, and a number indicating
the entry order in the respective catalog. Note that this order starts
at 0, like the catalogs identifiers.

   1:0.0
   2:0.1
   3:0.2
   4:0.3
   5:0.4
   6:1.0
   7:1.1
   8:1.2

=item C<html.db>

This is a Berkeley DB file where keys are the entry identifiers
defined in C<entry-catalog.index> file. For each key, the database
stores a pre-calculated HTML version for the entry (using the
C<asHTML> function shown in previous section).

=item C<latex.db>

This is a Berkeley DB file where keys are the entry identifiers
defined in C<entry-catalog.index> file. For each key, the database
stores a pre-calculated LaTeX version for the entry (using the
C<asLaTeX> function shown in previous section).

=item C<relation.index>

A text file mapping entry identifiers into relation terms. Each line
contains the entry identifier, a mark, and a list of classification
terms.

=item C<relations.db>

...

=item C<relations.list>

This is a text file where each line contains a term. These are the
classification terms used in all catalogs.

=item C<relations.statistics>

Contains the same thing as C<relations.list>, but each term is
followed by a mark and an occurrence number.

=item C<text.index>

This text file contains in each line the entry identifier, a mark, and
the text version for the entry, calculated using the C<asText>
function shown in the previous section.

=item C<thesaurus.log>

This is a text file in thesaurus format which maps to the term
'Others' all classification terms found on catalogs but does not
exists in the thesaurus file.

=item C<thesaurus.store>

This is a data dump format (Storable perl module) for the full
thesaurus struture. It is used as a cache for quick read when using a
navigation enabled thesaurus web page.

=back


=head1 AUTHOR

José João Almeida   <jj@di.uminho.pt>

Alberto Simões      <albie@alfarrabio.di.uminho.pt>

=head1 SEE ALSO

Manpages:
  Biblio::Thesaurus(3)
  Biblio::Catalog(3)
  perl(1)

=cut

